from lepl import *;
import TTCN3SymbolTable;

class TTCN3Parser:
   def __init__(self, includeDirs=[]):
      self.includeDirs = includeDirs;
      self.TTCN3Module = __defineTTCN3Grammar();
      self.symbolTable = ();

   def parse(self, ttcn3File):
      ''' 
      Will return a tuple pair with the resulting AST tree after the parsing,
      and a symbol table containing all the elements fond during the parsing.
      '''
      self.symbolTable = TTCN3SymbolTable();
      fileHandle = open(ttcn3File);
      ast = self.TTCN3Module.parse_file(fileHandle);
      return (ast, self.symbolTable);


   def __defineTTCN3Grammar();
      ''' 
      Defines the grammar with lepl, and returns the root element, which
      will be used later to parse a given file.
      '''
      TTCN3Module = String();
      return TTCN3Module;
