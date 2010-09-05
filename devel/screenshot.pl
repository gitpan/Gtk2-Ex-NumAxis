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


# Usage: perl screenshot.pl [outputfile.png]
#
# Draw a numaxis widget and write it to the given output file in PNG format.
# The default output file is /tmp/screenshot.png

use strict;
use warnings;
use File::Basename;
use FindBin;
use POSIX;
use Gtk2 '-init';
use Gtk2::Ex::NumAxis;


# PNG spec 11.3.4.2 suggests RFC822 (or rather RFC1123) for CreationTime
use constant STRFTIME_FORMAT_RFC822 => '%a, %d %b %Y %H:%M:%S %z';

my $progname = $FindBin::Script; # basename part
print "progname '$progname'\n";
my $output_filename = (@ARGV >= 1 ? $ARGV[0] : '/tmp/screenshot.png');

my $toplevel = Gtk2::Window->new('toplevel');
$toplevel->set_default_size (-1, 150);
$toplevel->signal_connect (destroy => sub { Gtk2->main_quit });

my $adj = Gtk2::Adjustment->new (1.3, # value
                                 -10, # lower
                                 10,  # upper
                                 1,   # step increment
                                 1,  # page increment
                                 2); # page size
my $numaxis = Gtk2::Ex::NumAxis->new (adjustment => $adj);
$toplevel->add ($numaxis);

Glib::Timeout->add
  (2000,
   sub {
     my $window = $toplevel->window;
     my ($width, $height) = $window->get_size;
     my $pixbuf = Gtk2::Gdk::Pixbuf->get_from_drawable ($window,
                                                        undef, # colormap
                                                        0,0, 0,0,
                                                        $width, $height);
     $pixbuf->save
       ($output_filename, 'png',
        'tEXt::Title'         => 'NumAxis Screenshot',
        'tEXt::Author'        => 'Kevin Ryde',
        'tEXt::Copyright'     => 'Copyright 2010 Kevin Ryde',
        'tEXt::Creation Time' => POSIX::strftime (STRFTIME_FORMAT_RFC822,
                                                  localtime(time)),
        'tEXt::Description'   => 'A sample screenshot of a Gtk2::Ex::NumAxis widget',
        'tEXt::Software'      => "Generated by $progname",
        'tEXt::Homepage'      => 'http://user42.tuxfamily.org/gtk2-ex-numaxis/index.html',
        # must be last or gtk 2.18 botches the text keys
        compression           => 9,
       );
     print "wrote $output_filename\n";
     Gtk2->main_quit;
     return 0; # Glib::SOURCE_REMOVE
   });

$toplevel->show_all;
Gtk2->main;
exit 0
