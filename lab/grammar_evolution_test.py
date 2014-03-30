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

   def test_29a_is_a_valid_HexString(self):
      result = Hstring.parseString("'29a'H");
      self.assertEqual(result[0], "'29a'H");

   def test_029a_is_also_a_valid_HexString(self):
      result = Hstring.parseString("'029a'H");
      self.assertEqual(result[0], "'029a'H");

   def test_47ak_is_NOT_a_valid_HexString(self):
      self.assertRaises(ParseException, (Hstring + stringEnd).parseString, "'47ak'H");

   def test_ab_is_a_valid_Oct(self):
      result = Oct.parseString("ab");
      self.assertEqual("".join(result), "ab");

   def test_abcd_is_NOT_a_valid_Oct(self):
      self.assertRaises(ParseException, (Oct + stringEnd).parseString, "abcd");

   def test_abcd_is_a_valid_Ostring(self):
      result = Ostring.parseString("'abcd'O");
      self.assertEqual(result[0], "'abcd'O");

   def test_12abcd_is_also_a_valid_Ostring(self):
      result = Ostring.parseString("'12abcd'O");
      self.assertEqual(result[0], "'12abcd'O");

   def test_12abcd8_is_NOT_a_valid_Ostring(self):
      self.assertRaises(ParseException, (Ostring + stringEnd).parseString, "'12abcd8'O");

   def test_0101_is_a_valid_Bstring(self):
      result = Bstring.parseString("'0101'B");
      self.assertEqual(result[0], "'0101'B");

   def test_01012_is_NOT_a_valid_Bstring(self):
      self.assertRaises(ParseException, (Bstring + stringEnd).parseString, "'01012'B");

   def test_pl_1293_is_a_valid_identifier(self):
      result = Identifier.parseString('pl_1293');
      self.assertEqual(result[0], "pl_1293");
      
   def test_f_superduper_is_a_valid_identifier(self):
      result = Identifier.parseString('f_superduper');
      self.assertEqual(result[0], "f_superduper");

   def test_9_val_is_NOT_a_valid_identifier(self):
      self.assertRaises(ParseException, (Identifier + stringEnd).parseString, "9_val");

   def test_valid_qualified_identifier(self):
      result = QualifiedIdentifier.parseString('CommonFunctions.f_abracadabra');
      self.assertEqual(result[0], "CommonFunctions.f_abracadabra");

   def test_valid_qualified_identifier_more_than_two_elements(self):
      result = QualifiedIdentifier.parseString('CommonFunctions.Magic.f_abracadabra');
      self.assertEqual(result[0], "CommonFunctions.Magic.f_abracadabra");

   def test_NOT_Valid_qualified_identifier(self):
      self.assertRaises(ParseException, (QualifiedIdentifier + stringEnd).parseString, "19CommonFunctions.f_abracadabra");

   def test_valid_qualified_identifier_list(self):
      result = QualifiedIdentifierList.parseString('CommonFunctions.f_abracadabra, CommonFunctions.f_trampolin');
      self.assertEqual(result.asList(), ["CommonFunctions.f_abracadabra", "CommonFunctions.f_trampolin"]);

   def test_NOT_valid_qualified_identifier_list(self):
      self.assertRaises(ParseException, (QualifiedIdentifierList + stringEnd).parseString, 
                       'CommonFunctions.f_abracadabra, CommonFunctions.3_trampolin');

   def test_valid_identifier_list(self):
      result = IdentifierList.parseString('f_abracadabra, pl_trampolin, result, prueba');
      self.assertEqual(result.asList(), ['f_abracadabra', 'pl_trampolin', 'result', 'prueba']);

   def test_NOT_valid_identifier_list_id_should_not_start_with_number(self):
      self.assertRaises(ParseException, (IdentifierList + stringEnd).parseString, 
                       'f_abracadabra, pl_trampolin, result, 3m, hummer');

   def test_NOT_valid_identifier_list_mix_of_ids_and_qualifiedids(self):
      self.assertRaises(ParseException, (IdentifierList + stringEnd).parseString, 
                       'f_abracadabra, pl_trampolin, result, Common.f_myfunc, hummer');

   def test_valid_extended_identifier(self):
      result = ExtendedIdentifier.parseString('CommonFunctions.f_abracadabra2');
      self.assertEqual(result[0], "CommonFunctions.f_abracadabra2");

   def test_NOT_valid_extended_identifier(self):
      self.assertRaises(ParseException, (ExtendedIdentifier + stringEnd).parseString, 
                       'CommonFunctions.MoreMagic.f_abracadabra2');
      
   def test_SelectCase_simple(self):
      result = SelectCase.parseString('case (42) {}');
      self.assertEqual(result.asList(), ['case', '(', '42', ')', '{', '}']);
   
   def test_SelectCase_simple_mlines(self):
      result = SelectCase.parseString('''
case ( t_someSpecificTemplate ) 
{
}
''');
      self.assertEqual(result.asList(), ['case', '(', 't_someSpecificTemplate', ')', '{', '}']);

   def test_SelectCase_else(self):
      result = SelectCase.parseString('case else { }');
      self.assertEqual(result.asList(), ['case', 'else', '{', '}']);

   def test_SelectCaseConstruct_single_case(self):
      result = SelectCaseConstruct.parseString('''
select (expression)
{
   case ( t_someSpecificTemplate ) 
   {
   }
}
''');
      self.assertEqual(result.asList(), ['select', '(', 'expression', ')', '{', [ [ 'case', '(', 't_someSpecificTemplate', ')', '{', '}'] ], '}']);

   def test_SelectCaseConstruct_multiple_cases(self):
      result = SelectCaseConstruct.parseString('''
select (expression)
{
   case ( t_someSpecificTemplate ) 
   {
   }
   case ( 42 ) 
   {
   }
   case else
   {
   }
}
''');
      self.assertEqual(result.asList(), ['select', '(', 'expression', ')', '{', [
                                          [ 'case', '(', 't_someSpecificTemplate', ')', '{', '}'],
                                          ['case', '(', '42', ')', '{', '}'],
                                          ['case', 'else', '{', '}']
                                         ], '}']);

   def test_simple_example_with_if(self):
      result = ConditionalConstruct.parseString('''if (isExpression) { }''');
      self.assertEqual(result.asList(), [['if', '(', 'isExpression', ')', '{', '}']]);

   def test_example_with_if_else(self):
      result = ConditionalConstruct.parseString('''if (isExpression) { } else { }''');
      self.assertEqual(result.asList(), [['if', '(', 'isExpression', ')', '{', '}'], ['else', '{', '}']]);

   def test_example_with_if_ifelse(self):
      result = ConditionalConstruct.parseString('''if (isExpression) { } else if (isExpression) { }''');
      self.assertEqual(result.asList(), [['if', '(', 'isExpression', ')', '{', '}'], ['else', 'if', '(', 'isExpression', ')', '{', '}']]);

   def test_example_with_if_ifelse_else(self):
      result = ConditionalConstruct.parseString('''
      if (isExpression) 
      { 
      } 
      else if (isExpression) 
      { 
      }
      else
      {
      }''');
      self.assertEqual(result.asList(), [['if', '(', 'isExpression', ')', '{', '}'], 
                                         ['else', 'if', '(', 'isExpression', ')', '{', '}'],
                                         ['else', '{', '}']]);

   def test_example_do_while(self):
      result = DoWhileStatement.parseString('''do { } while(isExpression)''');
      self.assertEqual(result.asList(), ['do', '{', '}', 'while', '(', 'isExpression', ')']);

   def test_example_while_loop(self):
      result = WhileStatement.parseString('''
      while(isExpression) 
      {
      }               ''');
      self.assertEqual(result.asList(), ['while', '(', 'isExpression', ')', '{', '}']);


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

#code='''select(expression) { case (inlineTemplate) example_block }'''
#grammar_evolution.parse(code);


if __name__ == '__main__':
   unittest.main()
