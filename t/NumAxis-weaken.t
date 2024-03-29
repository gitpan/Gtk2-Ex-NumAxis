#!/usr/bin/perl -w

# Copyright 2007, 2008, 2009, 2010, 2011 Kevin Ryde

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


use 5.008;
use strict;
use warnings;
use Test::More;

use lib 't';
use MyTestHelpers;
BEGIN { MyTestHelpers::nowarnings() }

require Gtk2;
MyTestHelpers::glib_gtk_versions();
Gtk2->disable_setlocale;  # leave LC_NUMERIC alone for version nums
Gtk2->init_check
  or plan skip_all => 'due to no DISPLAY available';

# Test::Weaken 2.002 for "ignore"
eval "use Test::Weaken 2.002;
      use Test::Weaken::Gtk2;
      1"
  or plan skip_all => "due to Test::Weaken 2.002 and/or Test::Weaken::Gtk2 not available -- $@";

plan tests => 2;

require Gtk2::Ex::NumAxis;

#-----------------------------------------------------------------------------

{
  my $leaks = Test::Weaken::leaks (sub { Gtk2::Ex::NumAxis->new });
  is ($leaks, undef, 'deep garbage collection');
  if ($leaks && defined &explain) {
    diag "Test-Weaken ", explain($leaks);
  }
}

{
  my $leaks = Test::Weaken::leaks
    (sub {
       my $adj = Gtk2::Adjustment->new (2100, 0, 4500, 1, 10, 300);
       my $axis = Gtk2::Ex::NumAxis->new (adjustment => $adj);
       return [ $axis, $adj ];
     });
  is ($leaks, undef, 'deep garbage collection -- with adjustment');
  if ($leaks && defined &explain) {
    diag "Test-Weaken ", explain($leaks);
  }
}

exit 0;
