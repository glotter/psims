#!/usr/bin/python

import filecmp
import os
import sys

# Command line arguments
if len(sys.argv) != 3:
    sys.exit("Usage: %s <run_directory> <test_directory>" % sys.argv[0])

run_directory = os.path.normpath(sys.argv[1])
test_directory = os.path.normpath(sys.argv[2])

# Open list of files to compare
file_list_filename = os.path.join(test_directory, "files.list")
if not os.path.isfile(file_list_filename):
	sys.exit("Error: Unable to find file %s" % file_list_filename)

file_list = open(file_list_filename, "r")

# Compare files in list
for file_to_compare in iter(file_list):  
   test_file = os.path.abspath(os.path.join(test_directory, file_to_compare.rstrip()))
   run_file = os.path.abspath(os.path.join(run_directory, file_to_compare.rstrip()))
   if not filecmp.cmp(test_file, run_file):
	print "File %s does not match!" % run_file
