#!/usr/bin/python
import sys
import unittest
from liposuction import parseArguments

class liposuctionTest(unittest.TestCase):
   def test_parseArguments_without_any_option_nor_ttcn3_files_should_exit_with_error(self):
      ''' Calling parseArguments without options nor files '''
      sys.argv = 'test_liposuction.py'.split()
      self.assertRaises(SystemExit, parseArguments)

   def test_parseArguments_using_options_but_without_ttcn3_files_should_exit_with_error(self):
      ''' Calling parseArguments with optional arguments, but no files '''
      sys.argv = 'test_liposuction.py --dryRun'.split()
      self.assertRaises(SystemExit, parseArguments)

   def test_parseArguments_using_no_options_and_one_ttcn3_file_that_does_not_exist_should_exit_with_error(self):
      ''' Calling parseArguments without options, but with a non-existent file'''
      sys.argv = 'test_liposuction.py dont_exist.ttcn'.split()
      self.assertRaises(IOError, parseArguments)

   def test_parseArguments_using_no_options_and_one_ttcn3_file_should_have_one_element_on_ttcn3_files(self):
      ''' Calling parseArguments without options, but with a file'''
      sys.argv = 'test_liposuction.py prueba.ttcn'.split()
      options = parseArguments()
      self.assertEqual(len(options.ttcn3_files), 1)

   def test_parseArguments_using_no_options_and_two_ttcn3_file_should_have_one_element_on_ttcn3_files(self):
      ''' Calling parseArguments without options, but with a file'''
      sys.argv = 'test_liposuction.py prueba.ttcn prueba.ttcn'.split()
      options = parseArguments()
      self.assertEqual(len(options.ttcn3_files), 2)

   def test_parseArguments_using_one_ttcn3_file_and_one_dir_as_include_should_fail(self):
      ''' Calling parseArguments, -I with a valid directory should pass'''
      sys.argv = 'test_liposuction.py -I /tmp prueba.ttcn'.split()
      options = parseArguments()
      self.assertEqual(len(options.include), 1)

   def test_parseArguments_using_ttcn3_file_and_two_dirs_as_include_should_fail(self):
      ''' Calling parseArguments, using -I several times should pass'''
      sys.argv = 'test_liposuction.py -I /tmp -I /usr/include prueba.ttcn'.split()
      options = parseArguments()
      self.assertEqual(len(options.include), 2)

if __name__ == '__main__':
   unittest.main()
