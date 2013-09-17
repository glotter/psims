#!/usr/bin/perl -w

use strict;

# Extract and reformat the data
my @part_files=`find parts -type f`;
my %accumulated_values = (); 

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
}

for my $filename ( keys %accumulated_values ) {
    print "Writing $filename\n";
    open(FILE, ">$filename") || die "Unable to open $filename\n";
    print FILE @{$accumulated_values{$filename}};
    close(FILE);
}
