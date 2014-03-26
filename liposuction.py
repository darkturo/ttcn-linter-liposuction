#!/usr/bin/python
from optparse import OptionParser 

#file = open('newfile.txt', 'r')
#print file.readline():
#print "LIPOSUCTION"

def parseArguments(): 
   # Creating the parser.
   argumentParser = OptionParser("usage: %prog [options] [ttcn_file(s)]");

   # Setting up the available Options
   argumentParser.add_option("-I", action="append", type="string", dest="includes",
                             "Add directory to the Include search path");
   argumentParser.add_option("--dry-run", type="boolean", dest="dryRun",
                             "Execute in dry-run mode, i.e.: not proceeding with real actions.");

   # Parsing argv 
   (options, args) = argumentParser.parse_args();

   ttcn3_files = ();
   if ( len(args) == 0): 
      # print error and exit with status 2.
      argumentParser.error("Need at least one ttcn-3 fatty file to proceed on.");
   else:
      for file in args:
         if (isValidTtcn3File(file)):
            ttcn3_files.append(file);
         else:
            argumentParser.error("Incorrect file: " + file + ". Liposuct only operates on ttcn3 files");
      options.ttcn3_files = ttcn3_files;

   # returning the options
   return options


if (__name__ == '__main__'):
   options = parseArguments();
   liposuction(options.ttcn3_files, option.includes, options);
