class TTCN3ActionRules:
   def __init__(self, defaultAction = "keep"):
      self.mcondition = lambda x : False;
      self.action = "keep";
      self.defaultAction = "keep";

   def setMatchingCondition(self, mcond):
      self.mcondition = mcond;

   def setAction(self, action):
      self.action = action;

   def setDefaultAction(self, action):
      self.defaultAction = action;

   def doesRuleApply(self, astNode):
      return False

   def getAction(self, astNode):
      if (self.mcondition(astNode)):
         return self.action;
      else:
         return self.defaultAction


