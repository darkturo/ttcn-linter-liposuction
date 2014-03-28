import TTCN3SymbolTable
import TTCN3ActionRules

class TTCN3Compiler:
   def __init__(self, ast, symbolTable, dryRun=False):
      self.dryRun = dryRun;
      self.includeDirs = includeDirs;
      self.ast = ast;
      self.stable = symbolTable;
      self.rules = [];

   def addRule(self, rule):
      self.rules.append(rule)

   def applyRules(self):
      ''' explore the AST and apply rules node by node. This will go through
      the AST tree, and will check if any of the rules apply or not, as a
      result, the node will be annotated with the action to do.'''

   def emitTTCN3(self):
      ''' go through the AST tree and print the nodes, according to the annotated rules'''
      pass

   def printAST(self):
      ''' debugging purposes '''
      #TODO: overwrite print operator instead?
      print(self.ast);

