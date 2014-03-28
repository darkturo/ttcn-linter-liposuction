import re;

class TTCN3SymbolTable:
   def __init__(self):
      self.table = dict();
      self.current_scope = "";

   def newScope(self, name):
      self.current_scope = self.current_scope + "." + name;

   def exitScope(self):
      self.current_scope = re.sub(r'\.[^.]+$', '', self.current_scope);

   def addSymbol(self, type, name):
      fullName = self.current_scope + "." + name;
      self.table[fullName] = TTCN3SymbolElement(type, name, self.current_scope);

   def existSymbol(self, name):
      # search in the current scope first, and go recursively until the top.
      try:
         self.searchSymbolFromScope(name, self.current_scope);
         return True;
      except TTCN3SymbolElementNonExistent, e:
         return False;

   def referenceSymbol(self, name):
      # search for a symbol in the table and increment the number of times the
      # symbol has been called by 1.
      symbol = self.searchSymbolFromScope(name, self.current_scope);
      symbol.timesReferenced += 1;

   def searchSymbol(self, name):
      # Return the symbol if found, otherwise, it will throw an exception.
      return self.searchSymbolFromScope(name, self.current_scope);     

   def searchSymbolFromScope(self, name, scope):
      # Return the symbol if found, otherwise, it will throw an exception.
      # This method is intended to be used to analyse the ttcn3 file after is parsed.
      raise TTCN3SymbolElementNonExistent("The symbol does not exist");

class TTCN3SymbolElement:
   def __init__(self, type, name, familyName):
      self.type = type;
      self.name = name;
      self.fullName= familyName + "." + name;
      self.timesReferenced = 0;

class TTCN3SymbolElementNonExistent(Exception):

