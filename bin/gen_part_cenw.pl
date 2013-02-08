#!/usr/bin/perl -w

use strict;
use Getopt::Long;
use File::Basename;

# Process command line arguments
# --num_scenarios (number of scenarios)
# --years (number of years)
# --variables (list of comma separated variables) 
# --directory (location of summaries directory)

our($num_scenarios, $years, $variables, $crop, $grid, $outfile);
GetOptions (
   'num_scenarios=i' => \$num_scenarios, 
   'years=i'         => \$years,
   'variables=s'     => \$variables,
   'crop=s'          => \$crop,
   'grid=s'          => \$grid,
   'outfile=s'       => \$outfile,
);

my @variable_list = split(',', $variables);
print "Num_scenarios: $num_scenarios\n";
print "Years: $years\n";
print "Variables: $variables\n";
print "Crop: $crop\n";
print "Grid: $grid\n";

$grid = basename($grid);
my $summary_file = "output/CenW-101.DT!";
my $field_width = 9; 

# We need to store variables for N number of years before printing anything here - store it in a hash
my %accumulated_values = (); 

# Store summary file in an array
open(FILE, $summary_file) || die "Unable to open $summary_file!\n";
my @summary_data = <FILE>;
close(FILE);

# Table to correlate field names to column numbers
my %column_names = ();
my $num_columns = length($summary_data[0]) / $field_width;
my @column_definitions = unpack(("A" . $field_width) x $num_columns , $summary_data[0]);

for (my $count=0; $count < @column_definitions; $count++ ) {
   # Some of the column identifiers have trailing dots (for example: "soil_id..") remove them
   $column_definitions[$count] =~ s/\.//g;
   $column_definitions[$count] =~ s/ //g;
   $column_names{ $column_definitions[$count] } = $count;
}

# Verify that requested variable names are defined
foreach my $v(@variable_list) {
   if(!defined($column_names{$v})) {
      die "Undefined variable $v\n";
   }
}

my $current_year = 1;
my $current_scenario = 1;
my $line_number = 0;

# There are $years*$num_scenarios grids + 4 lines of header data
foreach my $line_number(0..(($years*$num_scenarios)-1+4)) {
   my $row;
   if($line_number < 4) { next; }

   # Handle missing data
   if(scalar(@summary_data) < $line_number) { $row = 'NaN' x $num_columns; }
   else { $row = $summary_data[$line_number]; }      

   my @columns = unpack(("A" . $field_width) x $num_columns , $row);
   foreach my $variable(@variable_list) {
      my $file_to_create = $crop . "_" . $variable . "_" . $current_scenario . ".csv"; 
      if(defined($accumulated_values{$file_to_create})) {
         $accumulated_values{ $file_to_create } .= "," . $columns[$column_names{$variable}];
      }
      else {
         $accumulated_values{ $file_to_create } = "$file_to_create,$grid,$columns[$column_names{$variable}]";
      }
   }

   if($current_year == $years) {
      $current_year = 1;
      $current_scenario++;
   }
   else {
      $current_year++;
   }
}

my $dirname=dirname($outfile);
system("mkdir -p $dirname");
open(FILE, ">$outfile") || die "Unable to create file $outfile\n";
while ( my ($key, $value) = each(%accumulated_values) ) {
   print FILE "$value\n";
}
close(FILE);
