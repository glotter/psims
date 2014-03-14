#!/usr/bin/python

import filecmp
import os
import sys
import tarfile
import hashlib

# Given a tar file, return a list of checksums for files contained within
def tar_checksums(tar_fn):
    tar = tarfile.open(tar_fn)
    sums = {} 
    chunk = 1024*1024
    for member in tar:
        if not member.isfile():
            continue
        f = tar.extractfile(member)
        h = hashlib.md5()
        data = f.read(chunk)
        while data:
            h.update(data)
            data = f.read(chunk)
        sums[member.name] = h.hexdigest()
    return sums

# Compare two tar files (return True if they're identical)
def tarcmp(tar1, tar2):
   tar1_sums = tar_checksums(tar1)
   tar2_sums = tar_checksums(tar2)
   retval = True

   for key in set(tar1_sums.keys()).union(tar2_sums.keys()):
       if key not in tar1_sums:
           print "%s doesn't contain file %s" % (tar1, key)
           retval = False
       elif key not in tar2_sums:
           print "%s doesn't contain file %s" % (tar2, key)
           retval = False
       elif tar1_sums[key] != tar2_sums[key]:
           print "File %s (in %s) differs" % (key, os.path.basename(tar1))
           retval = False
   return retval

# Command line arguments
if len(sys.argv) != 3:
    sys.exit("Usage: %s <run_directory> <test_directory>" % sys.argv[0])

run_dir  = os.path.normpath(sys.argv[1])
test_dir = os.path.normpath(sys.argv[2])

# Verify test.txt exists
test_fn = os.path.join(test_dir, "test.txt")
if not os.path.isfile(test_fn):
	sys.exit("Error: Unable to find test file %s" % test_file_fn)
test_file = open(test_fn, "r")

# Compare files in test.txt
ec = 0
for fn in iter(test_file):  
   fn = fn.rstrip()
   filecmp1 = os.path.abspath(os.path.join(test_dir, fn))
   filecmp2 = os.path.abspath(os.path.join(run_dir, fn))

   if fn.endswith(".tar.gz"):
      if not tarcmp(filecmp1, filecmp2):
         ec=1
   else:
       if not filecmp.cmp(filecmp1, filecmp2):
           ec=1
           print "File %s does not match!" % filecmp2

sys.exit(ec)
