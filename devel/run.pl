#!/usr/bin/perl -w

# Copyright 2008, 2009, 2010 Kevin Ryde

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

use strict;
use warnings;
use POSIX;
use Gtk2;
use Glib::Ex::ConnectProperties;
use Gtk2::Ex::NumAxis;

use FindBin;
my $progname = $FindBin::Script;

{
  my ($unit, $decimals) = Gtk2::Ex::NumAxis::round_up_2_5_pow_10 (100);
  print "$progname: $unit $decimals\n";
  ($unit, $decimals) = Gtk2::Ex::NumAxis::round_up_2_5_pow_10 (80);
  print "$progname: $unit $decimals\n";
  ($unit, $decimals) = Gtk2::Ex::NumAxis::round_up_2_5_pow_10 (45);
  print "$progname: $unit $decimals\n";
  ($unit, $decimals) = Gtk2::Ex::NumAxis::round_up_2_5_pow_10 (0.33);
  print "$progname: $unit $decimals\n";
  ($unit, $decimals) = Gtk2::Ex::NumAxis::round_up_2_5_pow_10 (0.00099);
  print "$progname: $unit $decimals\n";
}

Gtk2->disable_setlocale;  # leave LC_NUMERIC alone for version nums
Gtk2->init;

my $toplevel = Gtk2::Window->new('toplevel');
$toplevel->set_default_size (-1, 500);

my $hbox = Gtk2::HBox->new (0,0);
$toplevel->add ($hbox);

my $vbox = Gtk2::VBox->new (0,0);
$hbox->pack_start ($vbox, 0,0,0);

use constant LOG_E_10 => 2.30258509299404568402;

sub exp10 {
  my ($x) = @_;
  exp ($x * LOG_E_10);
}
if (0) {
  my $adj = Gtk2::Adjustment->new (0,
                                   -3,
                                   5,
                                   0.1, 1, 1);
  my $axis = Gtk2::Ex::NumAxis->new(adjustment => $adj,
                                    transform   => \&POSIX::exp10,
                                    untransform => \&POSIX::log10,
                                   );
  $hbox->add($axis);

  my $vscroll = Gtk2::VScrollbar->new($adj);
  $hbox->add($vscroll);
}

my $adj = Gtk2::Adjustment->new (-100, # value
                                 -1000, # lower
                                 1000,  # upper
                                 10,   # step increment
                                 100,  # page increment
                                 20); # page size
my $axis = Gtk2::Ex::NumAxis->new(adjustment => $adj,
                                  inverted => 1,
                                  min_decimals => 2);
# $axis->signal_connect (number_to_text => sub {
#                          my ($axis, $number, $decimals) = @_;
#                          return sprintf "%.*f\nblah", $decimals, $number;
#                         });
$hbox->add($axis);

if (0) {
  my $adj = Gtk2::Adjustment->new (1000, -1000, 10000, 100, 1000, 8000);
  my $vscale = Gtk2::VScale->new($adj);
  $vscale->set('digits', 2);
  $hbox->add($vscale);
}

if (1) {
  #   my $adj = Gtk2::Adjustment->new (100, -100, 1000, 10, 100, 800);
  my $vruler = Gtk2::VRuler->new();
  #  $vruler->set('digits', 2);
  $adj->signal_connect ('value-changed' => sub {
                          my ($adj) = @_;
                          # $vruler->set_range ($adj->lower, $adj->upper,
                          #                     $adj->value, 999);
                          $vruler->set_range ($adj->value,
                                              $adj->value + $adj->page_size,
                                              $adj->value, 999);
                        });
  $hbox->add($vruler);
}

my $vscroll = Gtk2::VScrollbar->new($adj);
$hbox->pack_start($vscroll, 0,0,0);

{
  my $button = Gtk2::CheckButton->new_with_label ("inverted");
  Glib::Ex::ConnectProperties->new ([$axis,'inverted'],
                                    [$vscroll,'inverted'],
                                    [$button,'active']);
  $vbox->pack_start ($button, 0,0,0);
}
{
  my $spin = Gtk2::SpinButton->new_with_range (0, 2*$adj->page_size, 10);
  Glib::Ex::ConnectProperties->new ([$adj,'page-size'],
                                    [$spin,'value']);
  $vbox->pack_start ($spin, 0,0,0);
}
{
  my $spin = Gtk2::SpinButton->new_with_range (0, 50, 1);
  Glib::Ex::ConnectProperties->new ([$axis,'min-decimals'],
                                    [$spin,'value']);
  $vbox->pack_start ($spin, 0,0,0);
}

$toplevel->show_all;

Gtk2->main();
exit 0;









#   if (my $nf = $self->{'number_format_object'}) {
#     return $nf->format_number ($num, $decimals, 1);
#   }
# #   This is
# # either a reference to a function, or a C<Number::Format> object, or C<undef>
# # for a simple default format.
# # 
# # A function is called with the number and how many decimals to show, and it
# # should return a string (possibly a Perl wide-char string).  The decimals
# # passed can be more than the C<decimals> property above if a unit smaller
# # than that has been selected.  Here's an example using C<sprintf> with a "+"
# # to put a "+" sign on positive numbers,
# # 
# #     sub my_formatter {
# #       my ($n, $decimals) = @_;
# #       return sprintf ('%+.*f', $decimals, $n);
# #     }
# #     $axis->set('number-format-object', \&my_formatter);
# # 
# # If C<formatter> is a C<Number::Format> object then 
# 
# 
# # =item C<number-format-object> (Perl C<Number::Format> object, default undef)
# # 
# # A C<Number::Format> object for how to display numbers.  The default C<undef>
# # means a plain C<sprintf> instead.
# # 
# # The C<format_number> method is used and various settings such as thousands
# # separator and decimal point in the object thus affect the display.  For
# # example,
# # 
# #     use Number::Format;
# #     my $nf = Number::Format->new (-thousands_sep => ' ',
# #                                   -decimal_point => ',');
# #     $axis->set (number_format_object => $nf);
# # 
# 
#                  Glib::ParamSpec->scalar
#                  ('number-format-object',
#                   'number-format-object',
#                   '',
#                   Glib::G_PARAM_READWRITE),

