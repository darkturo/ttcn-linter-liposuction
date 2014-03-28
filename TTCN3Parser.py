from lepl import *;

class TTCN3Parser:
   def __init__(self):
      self.TTCN3Module = __defineTTCN3Grammar();

   def parse(self, ttcn3File):
      ''' Will return an AST (represented with lists).'''
      fileHandle = open(ttcn3_file);
      self.TTCN3Module.parse_file(fileHandle);


   def __defineTTCN3Grammar();
      ''' Defines the grammar with lepl, and returns the root element, which
          will be used later to parse a given file.
      '''
      TTCN3Module = String();
      return TTCN3Module;
