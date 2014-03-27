#!/usr/bin/python
import argparse

#file = open('newfile.txt', 'r')
#print file.readline():
#print "LIPOSUCTION"

def liposuction(tfile, includes, dryRun):
   #if (isValidTtcn3File(file)):
   pass;

class PlasticSurgeon:
   def __init__(self, includes, dryRun=False):
      self.dryRun = dryRun;
      self.includes = includes;

   def performLiposuctionOnPacient(self, ttcn3File):
      pass;

def parseArguments(): 
   # Creating the parser.
   argumentParser = argparse.ArgumentParser(
                        description="Perform plastic surgery over fatty TTCN3 files",
                        epilog="eartesc March-2014");

   # Setting up the available Options
   argumentParser.add_argument("-I", "--include", action="append", type=dir,
                             help="Add directory to the Include search path");
   argumentParser.add_argument("--dryRun", action="store_true", default=False, 
                             help="Execute in dry-run mode, i.e.: not proceeding with real actions.");
   argumentParser.add_argument("ttcn3_files", metavar='TTCN_File', type=file, 
                               nargs="+", help="TTCN-3 Files to proceed over.");

   # Parsing argv 
   options = argumentParser.parse_args();

   # returning the options
   return options

if (__name__ == '__main__'):
   # Parse command line arguments
   options = parseArguments();

   # Call and prepae the plastic surgeon
   doctor  = PlasticSurgeon(options.include,
                            options.dryRun)

   # In the operation room
   for ttcn3File in options.ttcn3_files:
      doctor.performLiposuctionOnPacient(ttcn3File);
