Prerequisites
=============
This README will assume that runs will be started from 
communicado.ci.uchicago.edu and bridled.ci.uchicago.edu.
Communicado and bridled are behind a firewall. In order
to connect to those machines, you must first connect
to login.ci.uchicago.edu. This README assumes that the 
work will be done on either the UC3 cluster or on OSG.

You must be able to SSH from communicado/bridled to UC3
and OSG. Test your setup by running the following commands:

ssh yourusername@uc3-sub.uchicago.edu
ssh yourusername@engage-submit3.renci.org

When you run these commands, you should be able to immediately
connect and get a shell prompt without being asked for a password.
If this does not happen, check to make sure that the SSH client
you are using has agent forwarding enabled. The second thing is
to add your public SSH key to .ssh/authorized_keys on each of these
remote hosts.

Communicado, uc3-mgt and engage-submit3 must also have Swift installed 
on it. To download swift, please run the following command:

wget http://www.ci.uchicago.edu/~davidk/DSSAT/swift-r5801.tar.gz

Extract this file in your home directory and add the swift-trunk/bin directory 
to your PATH via .bashrc. More detailed instructions on how to do this can 
be found at 
http://www.ci.uchicago.edu/swift/guides/release-0.93/quickstart/quickstart.html.

Download the psims scripts from SVN onto communicado/bridled. These
scripts should go into somewhere in GPFS to avoid quota restrictions.
Run the following command to download the latest copy:

svn co https://svn.ci.uchicago.edu/svn/see/frameworks/impacts/

How to Run
==========
The swiftopt.sh script is used to start a pSIMS run. The options you pass to 
this script will determine which pSIMS runs will be done (including which models) 
and where they will run.

Usage: swiftopt.sh [-s sitename] [-p paramfile] [-g gridfile]

The sitename option determines where a run will take place. Currently, the 
three valid options are "osg", "uc3", and "local".

The paramfile and gridfile determine which run will get done. These files are typically stored 
in psims/params/ psims/gridList/

Here are a few examples:

To run CenW on midway with a small grid file you might do something like:
-----
./swiftopt.sh -s midway -p ./params/pcenw.default.a1b -g ./gridLists/nzsmalltest323
-----

To run DSSAT scenario US0007 on OSG
-----
./swiftopt.sh -s osg -p ./params/US0007 -g ./gridList/US0007
-----

For the gen_part extraction code for cenw, variable names should be specified as in 
the cenw output head, except with spaces removed. valid variable names are thus 
Day
LAI
Height
DBH
Canopyc
Bsarea
Stocking
SapWood
Ht-Wood
Bark
Foliage
FineRt
Crs-Rt
Branch
Reprod
CH2O
Reserves
TotalN
pi
CAI
NPP
NEE
CAssim
Respn
DCFlux
NCFlux
TDamage
TDUnits
Transp