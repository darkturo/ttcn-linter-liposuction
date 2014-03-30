import unittest;
from grammar_evolution import *;
class GrammarTest(unittest.TestCase):
   def test_029939_is_a_valid_decimal_number(self):
      result = DecimalNumber.parseString("029939");
      self.assertEqual(result[0], '029939');

   def test_666_is_a_valid_decimal_number(self):
      result = DecimalNumber.parseString("666");
      self.assertEqual(result[0], '666');

   def test_029a_is_NOT_a_valid_decimal_number(self):
      self.assertRaises(ParseException, (DecimalNumber + stringEnd).parseString, "029a");

   def test_0_is_a_valid_Number(self):
      result = Number.parseString("0");
      self.assertEqual(result[0], '0');
   
   def test_33_is_a_valid_Number(self):
      result = Number.parseString("33");
      self.assertEqual(result[0], '33');
           
   def test_033_is_NOT_a_valid_Number(self):
      self.assertRaises(ParseException, (Number + stringEnd).parseString, "033");

   # TODO: use this later on
   def sketch_for_the_parse_function_based_on_pyparsing(self):
      txt="0666";
      try:
         #result = (Number + stringEnd).parseString("0666");
         result = Number.parseString("0666", parseAll=True);
      except Exception, e:
         msg = str(e)
         print("\nerror: %s: %s" % (msg, txt))
         print(" " * (len("error: %s: " % msg) + (e.loc)) + "^")

code='''select(expression) { case (inlineTemplate) example_block }'''
#grammar_evolution.parse(code);


if __name__ == '__main__':
   unittest.main()
