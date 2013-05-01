#!/usr/bin/perl -w

use strict;

$SIG{'INT'} = \&sigIntHandler;

my @part_files=`find parts -type f`;
my %accumulated_values = (); 
my %csv_file_handles = ();
my $filecount=0;
my $sync_frequency=100;

sub sync {
   print "Syncing data\n";
   for my $filename ( keys %accumulated_values ) {
      if(!defined($csv_file_handles{$filename})) {
         open($csv_file_handles{$filename}, ">$filename") || die "Can't open $filename: $!";
      }
      print {$csv_file_handles{$filename}} @{$accumulated_values{$filename}};
   } 
   %accumulated_values = ();
}

sub close_files {
   print "Closing " . (keys %csv_file_handles) . " files..\n";
   for my $fh ( values %csv_file_handles ) {
      close($fh);
   }
   print "Done\n";
}

sub sigIntHandler {
    print "Ctrl-C caught, closing files and exiting\n";
    close_files(); 
    exit;
}

foreach my $part_file(@part_files) {
   
   open(FILE, $part_file) || die "Unable to open $part_file!\n";
   my @summary_data = <FILE>;
   close(FILE);
   print "Reading $part_file";

   foreach my $row(@summary_data) {
      (my $file_to_create, my @rdata) = split(',', $row);
      my $columns = join(',', @rdata);
      push(@{$accumulated_values{$file_to_create}}, $columns);
   }

   $filecount++;

   if($filecount >= $sync_frequency ) { 
      sync(); 
      $filecount=0;   
   }
}

sync();
close_files();

