# Copyright 2007, 2008, 2009, 2010 Kevin Ryde

# This file is part of Gtk2-Ex-NumAxis.
#
# Gtk2-Ex-NumAxis is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any later
# version.
#
# Gtk2-Ex-NumAxis is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License along
# with Gtk2-Ex-NumAxis.  If not, see <http://www.gnu.org/licenses/>.

package Gtk2::Ex::NumAxis;
use 5.008;
use strict;
use warnings;
use Gtk2 1.220;
use List::Util qw(min max);
use Math::Round;
use POSIX qw(floor ceil);

# uncomment this to run the ### lines
#use Smart::Comments;

our $VERSION = 1;

use Glib::Ex::SignalBits;
use Glib::Ex::SignalIds;

use Glib::Object::Subclass
  'Gtk2::DrawingArea',
  signals => { expose_event      => \&_do_expose_event,
               size_request      => \&_do_size_request,
               style_set         => \&_do_style_or_direction,
               direction_changed => \&_do_style_or_direction,

               'set-scroll-adjustments'
               => { param_types => ['Gtk2::Adjustment',
                                    'Gtk2::Adjustment'],
                    return_type => undef,
                    class_closure => \&_do_set_scroll_adjustments },

               'number-to-text'
               => { param_types => ['Glib::Double','Glib::Int'],
                    return_type => 'Glib::String',
                    flags       => ['run-last'],
                    accumulator => \&Glib::Ex::SignalBits::accumulator_first_defined,
                    class_closure => \&_do_number_to_text },

               # Glib::ParamSpec->scalar
               # ('transform',
               #  'transform',
               #  '',
               #  Glib::G_PARAM_READWRITE),
               #
               # Glib::ParamSpec->scalar
               # ('untransform',
               #  'untransform',
               #  '',
               #  Glib::G_PARAM_READWRITE),
             },
  properties => [Glib::ParamSpec->object
                 ('adjustment',
                  'adjustment',
                  'Blurb.',
                  'Gtk2::Adjustment',
                  Glib::G_PARAM_READWRITE),

                 Glib::ParamSpec->int
                 ('min-decimals',
                  'min-decimals',
                  'Blurb.',
                  # range limited to 1000 decimals to try to catch garbage
                  # going in and to try to stop calculated width becoming
                  # something wild
                  0, 1000,  # min, max
                  0,        # default
                  Glib::G_PARAM_READWRITE),

                 Glib::ParamSpec->boolean
                 ('inverted',
                  'inverted',
                  'Invert the scale so numbers are drawn increasing from the bottom up, instead of the default top down.',
                  0,       # default
                  Glib::G_PARAM_READWRITE),
                ];


# as fraction of digit width
use constant { TICK_WIDTH_FRAC => 0.8,
               TICK_GAP_FRAC   => 0.5 };
# right-margin 0.2    between number and right edge of window

sub INIT_INSTANCE {
  my ($self) = @_;
  $self->{'min_decimals'} = 0; # default
  $self->{'decided_width_for'} = 'x';
}

sub SET_PROPERTY {
  my ($self, $pspec, $newval) = @_;
  my $pname = $pspec->get_name;
  ### SET_PROPERTY: $pname, $newval
  $self->{$pname} = $newval;  # per default GET_PROPERTY

  if ($pname eq 'adjustment') {
    my $adj = $newval;
    $self->{'adjustment_ids'} = $adj && do {
      Scalar::Util::weaken (my $weak_self = $self);
      Glib::Ex::SignalIds->new
          ($adj,
           $newval->signal_connect (value_changed => \&_do_adj_value_changed,
                                    \$weak_self),
           $newval->signal_connect (changed => \&_do_adj_other_changed,
                                    \$weak_self));
    };
  }

  $self->{'decided_width_for'} = 'x';
  $self->queue_resize;
  $self->queue_draw;
}

# 'set-scroll-adjustments' class closure
sub _do_set_scroll_adjustments {
  my ($self, $hadj, $vadj) = @_;
  $self->set (adjustment => $vadj);
}

# 'size-request' class closure
sub _do_size_request {
  my ($self, $req) = @_;
  ### NumAxis _do_size_request()
  _decide_width ($self);
  ### NumAxis _do_size_request() return: $self->{'decided_width'}.'x0'
  $req->width ($self->{'decided_width'});
  $req->height (0);
}

# 'number-to-text' class closure
sub _do_number_to_text {
  my ($self, $number, $decimals) = @_;
  ### _do_number_to_text()
  ### $number
  ### $decimals
  return sprintf ('%.*f', $decimals, $number);
}

sub identity {
  return $_[0];
}

sub _do_expose_event {
  my ($self, $event) = @_;
  ### NumAxis _do_expose_event(): $self->get_name
  ### decided width: $self->{'decided_width'}

  my $adj = $self->{'adjustment'} || do {
    ### no adjustment, no draw
    return Gtk2::EVENT_PROPAGATE;
  };
  my $page_size = $adj->page_size || do {
    ### zero height page, no draw
    return Gtk2::EVENT_PROPAGATE;
  };

  _decide_width($self);  # if not already done

  my $lo = $adj->get_value;
  my ($unit, $unit_decimals) = _decide_unit ($self, $lo);
  ### $unit
  ### $unit_decimals
  if ($unit == 0) {
    ### unit zero, no draw
    return Gtk2::EVENT_PROPAGATE;
  }
  my $hi = $lo + $page_size;
  ### $lo
  ### $hi

  my $layout        = $self->{'layout'};
  my $decimals      = $self->{'min_decimals'};
  my $decided_width = $self->{'decided_width'};
  my $state     = $self->state;
  my $style     = $self->style;
  my $win       = $self->window;
  my ($win_width, $win_height) = $win->get_size;

  my $factor    = $win_height / $page_size;
  my $offset    = 0;
  if ($self->{'inverted'}) {
    $factor = -$factor;
    $offset = $win_height;
  }
  $offset += -$lo * $factor;

  my $digit_height  = $self->{'digit_height'};
  $decimals = max ($decimals, $unit_decimals);
  my $widen = $digit_height / abs($factor);
  $lo -= $widen;
  $hi += $widen;
  ### digit_height pixels: $digit_height
  ### which is value: $digit_height / abs($factor)
  ### widen: "by $widen to lo=$lo hi=$hi"
  ### $unit
  ### $decimals

  my $digit_width = $self->{'digit_width'};
  my $tick_width = ceil (TICK_WIDTH_FRAC * $digit_width);
  my $tick_gc = $style->fg_gc($state);
  my $text_x = $tick_width + ceil (TICK_GAP_FRAC * $digit_width);

  my $transform   = $self->{'transform'}   || \&identity;
  my $untransform = $self->{'untransform'} || \&identity;

  $lo = $transform->($lo);
  $hi = $transform->($hi);
  my $n = Math::Round::nhimult ($unit, $lo);
  ### trans: "$lo to $hi, starting $n"

  for ( ; $n <= $hi; $n += $unit) {
    my $str = $self->signal_emit ('number-to-text', $n, $decimals);
    $layout->set_text ($str);
    my ($str_width, $str_height) = $layout->get_pixel_size;

    my $u = $untransform->($n);
    my $y = floor ($factor * $u + $offset);
    ### $str
    ### $y
    ### $str_width

    my $y_str = $y - int ($str_height/2); # top of text
    if ($y_str >= $win_height || $y + $str_height <= 0) {
      ### outside window, skip
      next;
    }

    $win->draw_rectangle ($tick_gc,
                          1,            # filled
                          0,            # x
                          $y,           # y
                          $tick_width,  # width
                          1);           # height==1
    $style->paint_layout ($win,
                          $state,
                          1,  # use_text, for the text gc instead of the fg one
                          $event->area,
                          $self,       # widget
                          __PACKAGE__, # style detail string
                          $text_x,
                          $y_str,
                          $layout);

    if ($y >= 0 && $y < $win_height  # only values more than half in window
        && ($str_width += $text_x) > $decided_width) {
      ### draw is wider than decided_width: "str=$str, width=$str_width cf decided_width=$decided_width"
      $decided_width = $self->{'decided_width'} = $str_width;
      $self->queue_resize;
    }
  }

  return Gtk2::EVENT_PROPAGATE;
}

sub _decide_width {
  my ($self) = @_;

  my $adj = $self->{'adjustment'};
  my $for = ($adj
             ? join(',', $adj->lower, $adj->upper, $adj->page_size)
             : '');
  if ($self->{'decided_width_for'} eq $for) {
    return;
  }
  ### _decide_width()
  ### old decided width: $self->{'decided_width'}
  ### was for: $self->{'decided_width_for'}
  ### now for: $for
  $self->{'decided_width_for'} = $for;

  my $layout = ($self->{'layout'} ||= $self->create_pango_layout (''));
  my ($digit_width, $digit_height) = layout_digit_size ($layout);
  $self->{'digit_width'} = $digit_width;
  $self->{'digit_height'} = $digit_height;

  my $decimals = $self->{'min_decimals'};
  my $width = $digit_width * $decimals;

  if ($adj) {
    my $lower = $adj->lower;
    my ($unit, $unit_decimals) = _decide_unit ($self, $lower);
    ### $unit
    ### $unit_decimals
    $decimals = max ($decimals, $unit_decimals);
    my $transform = $self->{'transform'} || \&identity;

    # my $n = Math::Round::nhimult ($unit, $lo);

    foreach my $un ($lower,
                    $adj->upper - $adj->page_size) {
      my $n = $transform->($un);
      # increase $n to 99.999 etc per its integer part and $decimals
      $n = ($n < 0 ? '-' : '')
        . ('9' x _num_integer_digits($n))
          . '.'
            . ('9' x $decimals);
      my $str = $self->signal_emit ('number-to-text', $n, $decimals);
      $layout->set_text ($str);
      my ($str_width, undef) = $layout->get_pixel_size;
      $width = max ($width, $str_width);
      ### this str: $str
      ### is pixels: $width
    }
  }

  $width += ceil (TICK_WIDTH_FRAC * $digit_width)
    + ceil (TICK_GAP_FRAC * $digit_width);
  ### tick width: ceil (TICK_WIDTH_FRAC * $digit_width)
  ### tick gap:   ceil (TICK_GAP_FRAC * $digit_width)
  ### _decide_width() result: $width
  $self->{'decided_width'} = $width;
}

# return ($step, $decimals)
sub _decide_unit {
  my ($self, $value) = @_;
  my $digit_height = $self->{'digit_height'};
  my $win = $self->window || return (0, 0);

  my ($win_width, $win_height) = $win->get_size;
  my $adj = $self->{'adjustment'} || return (0, 0);

  my $page_size = $adj->page_size;
  if ($page_size == 0) { return (0, 0); }

  my $transform = $self->{'transform'} || \&identity;
  my $untrans_min_step = 2.0 * $adj->page_size * $digit_height / $win_height;

  my $low_step =  abs ($transform->($value)
                       - $transform->($value + $untrans_min_step));
  my $high_step = abs ($transform->($value + $page_size)
                       - $transform->($value + $page_size - $untrans_min_step));
  return round_up_2_5_pow_10 (max ($low_step, $high_step));

  # return round_up_2_5_pow_10 (2 * $digit_height / $factor);
}

# 'style-set' and 'direction-changed' class closures
# 
sub _do_style_or_direction {
  my ($self, $prev_style) = @_;

  # update as advised by gtk_widget_create_pango_layout()
  if (my $layout = $self->{'layout'}) {
    $layout->context_changed;
  }

  $self->{'decided_width_for'} = 'x';  # possible new font or kerning
  $self->queue_resize;
  $self->queue_draw;
  return shift->signal_chain_from_overridden(@_);
}

# 'value-changed' on the adjustment
sub _do_adj_value_changed {
  my ($adj, $ref_weak_self) = @_;
  ### _do_adj_value_changed(), queue_draw
  my $self = $$ref_weak_self || return;
  $self->queue_draw;
}

# 'changed' on the adjustment
sub _do_adj_other_changed {
  my ($adj, $ref_weak_self) = @_;
  my $self = $$ref_weak_self || return;
  ### _do_adj_other_changed(), resize
  $self->queue_resize; # possible width for new upper/lower
  $self->queue_draw;   # possible new page-size
}

#------------------------------------------------------------------------------
# mostly generic

# Return ($digit_width, $digit_height) which is the size in pixels of a
# digit in the given $layout.
#
sub layout_digit_size {
  my ($layout) = @_;
  my $digit_width = 0;
  my $digit_height = 0;
  foreach my $n (0 .. 9) {
    $layout->set_text ($n);
    my ($str_width, $str_height) = $layout->get_pixel_size;
    $digit_width  = max ($digit_width,  $str_width);
    $digit_height = max ($digit_height, $str_height);
  }
  return ($digit_width, $digit_height);
}

# Round $n up to the next higher unit of the form 10^k, 2*10^k or 5*10^k
# (for an integer k, possibly negative) and return two values "($unit,
# $decimals)", where $decimals is how many decimal places are necessary to
# represent that unit.  For instance,
#
#     round_up_2_5_pow_10(0.0099) = (0.01, 2)
#     round_up_2_5_pow_10(0.15)   = (0.2, 1)
#     round_up_2_5_pow_10(3.5)    = (5, 0)
#     round_up_2_5_pow_10(60)     = (100, 0)
#
sub round_up_2_5_pow_10 {
  my ($n) = @_;
  my $k = ceil (POSIX::log10 ($n));
  my $unit = POSIX::pow (10, $k);

  # at this point $unit is the next higher value of the form 10^k, see if
  # either 5*10^(k-1) or 2*10^(k-1) would suffice to be bigger than $n
  if ($unit * 0.2 >= $n) {
    $unit *= 0.2;
    $k--;
  } elsif ($unit * 0.5 >= $n) {
    $unit *= 0.5;
    $k--;
  }
  ### $unit
  ### $k
  ### decimals: max (-$k, 0)
  return ($unit, max (-$k, 0));
}

# Return the number of digits in the integer part of $n, so for instance
#     _num_integer_digits(0) == 1
#     _num_integer_digits(99) == 2
#     _num_integer_digits(100.25) == 3
# Just the absolute value is used, so the results are the same for negatives,
#     num_integer_digits(-100.25) == 3
#
sub _num_integer_digits {
  my ($n) = @_;
  return 1 + max (0, floor (POSIX::log10 (abs ($n))));
}



#                configure_event => \&_do_configure_event,
# # 'configure-event' class closure
# sub _do_configure_event {
#   my ($self, $event) = @_;
#   $self->queue_draw;
#   return shift->signal_chain_from_overridden(@_);
# }

1;
__END__

=for stopwords NumAxis VScrollbar boolean subr resize BUILDABLE Gtk2-Ex-NumAxis Ryde

=head1 NAME

Gtk2::Ex::NumAxis -- numeric axis display widget

=for test_synopsis my ($adj)

=head1 SYNOPSIS

 use Gtk2::Ex::NumAxis;
 my $axis = Gtk2::Ex::NumAxis->new (adjustment => $adj);

=head1 WIDGET HIERARCHY

C<Gtk2::Ex::NumAxis> is a subclass of C<Gtk2::DrawingArea>, but don't rely
on more than C<Gtk2::Widget>, and probably don't rely on it being a windowed
widget.

    Gtk2::Widget
      Gtk2::DrawingArea
        Gtk2::Ex::NumAxis

=head1 DESCRIPTION

A C<Gtk2::Ex::NumAxis> widget displays a vertical axis of values like

    +---------+
    |         |
    | -- 8.5  |
    |         |
    | -- 9    |
    |         |
    | -- 9.5  |
    |         |
    | -- 10   |
    |         |
    | -- 10.5 |
    |         |
    +---------+

The numbers are the "page" portion of a C<Gtk2::Adjustment> and update with
changes in that Adjustment.  A unit step 1, 2 or 5 for the display is chosen
according to what fits in the window.

Decimal places are added as necessary and the C<min-decimals> property can
force a certain minimum decimals for example to show dollars and cents.  The
C<number-to-text> signal allows things like thousands separators or
different decimal point.

Pixel y=0 is the "value" from the adjustment and the window height is the
"page" amount, so that pixel y=winheight (one past the bottom of the window)
is "value+page".  The C<inverted> property swaps that to the "value" at the
bottom of the display.

=head2 Size Request

An axis has a desired width, but doesn't ask for any particular height.
However the height which is applied by the container parent determines the
step between displayed values, and thus how many decimal places are needed,
which can affect what width is necessary.

If the drawing code notices a number shown is wider than a width previously
requested then a resize is queued.  Hopefully this only happens if a
C<number-to-text> handler is doing strange things with values.  It
potentially means a disconcerting size increase while scrolling, but at
least ensures values are not truncated.  Of course a size request is only
ever a request, the parent container determines how much space its children
get.

A C<Gtk2::Ex::NoShrink> container can help keep a lid on resizing, if for
instance changes to the Adjustment range would often cause a different
width.

=head1 FUNCTIONS

=over 4

=item C<< $axis = Gtk2::Ex::NumAxis->new (key=>value,...) >>

Create and return a new NumAxis widget.  Optional key/value pairs set
initial properties per C<< Glib::Object->new >>.

    my $adj = Gtk2::Adjustment->new (5,0,20, 1,8, 10);
    Gtk2::Ex::NumAxis->new (adjustment => $adj,
                            min_decimals => 1);

=item C<< $axis->set_scroll_adjustments ($hadj, $vadj) >>

This usual C<Gtk2::Widget> method sets the C<adjustment> property to
C<$vadj>.

If there's a horizontal orientation option in the future then when set it
would take the C<$hadj> instead, probably just from the current orientation,
not keeping both adjustments and switching between them.

=back

=head1 PROPERTIES

=over 4

=item C<adjustment> (C<Gtk2::Adjustment>, default C<undef>)

The adjustment object giving the values to display and track.  NumAxis is
blank until an adjustment is set and has a non-zero C<page-size>.

=item C<inverted> (boolean, default 0)

Draw the adjustment C<value> at the bottom of the widget and C<page-size>
increasing up the window, as opposed to the default of C<value> at the top
going downwards.  This sense of "inverted" is the same as in a
C<Gtk2::VScrollbar>.

=item C<min-decimals> (integer, default 0)

The minimum number of decimal places to show on the numbers.  Further
decimals are shown if the unit step chosen needs more.

=back

In the future there might be an C<orientation> property for a horizontal
axis, and perhaps some transforms for log scale or similar.

=head1 SIGNALS

=over 4

=item C<number-to-text> ($number double, $decimals int -> return string)

Callback to format a number for display with a given number of decimal
places.  The default display is a plain C<sprintf("%.*f")>.  This signal
allows things like a thousands separator or different decimal point.  For
example using C<Number::Format>,

    my $nf = Number::Format->new (-thousands_sep => ' ',
                                  -decimal_point => ',');
    sub my_number_to_text_handler {
      my ($axis, $number, $decimals) = @_;
      return $nf->format_number ($number, $decimals,
                                 1); # include trailing zeros
    }
    $axis->signal_connect (number_to_text
                           => \&my_number_to_text_handler);

The C<$decimals> parameter is how many decimals are being shown.  This is at
least C<min-decimals> but can be more if a higher resolution fits in the
axis window.  The handler can decide how many trailing zeros to actually
show.

Strings formed shouldn't be too bizarre, mainly because the widget width is
established just from some representative high and low values.  Things like
+/- sign or leading and trailing zeros are fine.

=back

=head1 BUILDABLE

C<Gtk2::Ex::NumAxis> inherits the usual widget C<Gtk2::Buildable> interface
in Gtk 2.12 and up, allowing C<Gtk2::Builder> to construct a NumAxis.  The
class name is C<Gtk2__Ex__NumAxis> and properties and signal handlers can be
set in the usual way.  Here's a sample fragment, or see
F<examples/builder.pl> in the NumAxis sources for a complete program.

    <object class="Gtk2__Ex__NumAxis" id="my_axis">
      <property name="adjustment">my_adj</property>
      <signal name="number-to-text" handler="my_number_to_text"/>
    </object>

=head1 SEE ALSO

L<Gtk2::Widget>, L<Gtk2::Adjustment>, L<Gtk2::VScrollbar>, L<Gtk2::Builder>,
L<Gtk2::Ex::NoShrink>

For number formatting see L<Number::Format>, L<Locale::Currency::Format>,
L<PAB3::Utils> (has an C<strfmon>)

=head1 HOME PAGE

L<http://user42.tuxfamily.org/gtk2-ex-numaxis/index.html>

=head1 COPYRIGHT

Copyright 2007, 2008, 2009, 2010 Kevin Ryde

Gtk2-Ex-NumAxis is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any later
version.

Gtk2-Ex-NumAxis is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
more details.

You should have received a copy of the GNU General Public License along with
Gtk2-Ex-NumAxis.  If not, see L<http://www.gnu.org/licenses/>.

=cut
