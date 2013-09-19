#!/usr/bin/perl -w
use strict;
use Time::Local;

# Usage: ./activityplot.pl swift.out
my $first_timestamp=0;
my %mon2num = qw(
  jan 1  feb 2  mar 3  apr 4  may 5  jun 6
  jul 7  aug 8  sep 9  oct 10 nov 11 dec 12
);

# Read file
my $filename=shift;
open(FILE, "$filename") || die "Unable to open $filename\n";
my @filedata = <FILE>;
close(FILE);

# Open file for writing
open(FILE, ">activityplot.dat") || die "Unable too open activityplot.dat for writing\n";

sub get_value
{
   my $statement = $_[0];
   my $value = (split /:/, $statement)[1];
   return $value;
}

sub timestamp_to_seconds
{
   (my $progress, my $tme, my $dayname, my $monthday, my $month, my $year, my $hhmmss, my @junk) = split('\s+', $_[0]);
   (my $hh, my $mm, my $ss) = split(':', $hhmmss);
   my $time = timelocal($ss, $mm, $hh, $monthday, $mon2num{lc substr($month, 0, 3)}, $year);
   return $time;
}

# Progress:  time: Sat, 12 Jan 2013 05:26:32 +0000  Stage in:913  Submitting:5  Submitted:443  Active:678  Stage out:9  Finished successfully:82

foreach my $line(@filedata) {
   if($line !~ /^Progress/) { next; }
   my @words = split('\s+', $line);
   my $timestamp = timestamp_to_seconds($line);

   if($first_timestamp == 0) { $first_timestamp = $timestamp; $timestamp=0; } 
   else { $timestamp = $timestamp - $first_timestamp; }

   my $stagein=0, my $submitting=0, my $submitted=0;
   my $active=0, my $stageout=0, my $finished=0;

   foreach my $word(@words) {
      if ($word =~ /^in:/)             { $stagein    = get_value($word); }
      elsif ($word =~ /^Submitting:/)  { $submitting = get_value($word); }
      elsif ($word =~ /^Submitted:/)   { $submitted  = get_value($word); }
      elsif ($word =~ /^Active:/)      { $active     = get_value($word); }
      elsif ($word =~ /^out:/)         { $stageout   = get_value($word); }
      elsif ($word =~ /^successfully/) { $finished   = get_value($word); }
   } 

   # Adjust values for plotting
   $stagein = $stagein + $active + $stageout;
   $active = $active + $stageout;
   print FILE "$timestamp $stagein $active $stageout $finished\n";
}

close(FILE);
system("gnuplot activityplot.gp");
