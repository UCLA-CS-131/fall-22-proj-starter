# Base class for our interpreter
from enum import Enum

class ErrorType(Enum):
  TYPE_ERROR = 1
  NAME_ERROR = 2    # if a variable or function name can't be found
  SYNTAX_ERROR = 3  # used for syntax errors
  # Add others here


class InterpreterBase:

  # constants
  MAIN_FUNC = 'main'
  FUNC_DEF = 'func'
  ENDFUNC_DEF = 'endfunc'
  WHILE_DEF = 'while'
  ELSE_DEF = 'else'
  ENDWHILE_DEF = 'endwhile'
  ENDIF_DEF = 'endif'
  ASSIGN_DEF = 'assign'
  FUNCCALL_DEF = 'funccall'
  IF_DEF = 'if'
  RETURN_DEF = 'return'
  VAR_DEF = 'var'
  INT_DEF = 'int'
  BOOL_DEF = 'bool'
  STRING_DEF = 'string'
  REFINT_DEF = 'refint'        # only usable for param passing
  REFSTRING_DEF = 'refstring'  # only usable for param passing
  REFBOOL_DEF = 'refbool'      # only usable for param passing
  VOID_DEF = 'void'
  TRUE_DEF = 'True'
  FALSE_DEF = 'False'
  PRINT_DEF = 'print'
  STRTOINT_DEF = 'strtoint'
  INPUT_DEF = 'input'
  COMMENT_DEF = '#'
  RESULT_DEF = 'result'
  OBJECT_DEF = 'object'
  THIS_DEF = 'this'

  # v3 defs
  LAMBDA_DEF = 'lambda'
  ENDLAMBDA_DEF = 'endlambda'

  # methods
  def __init__(self, console_output=True, input=None):
    self.console_output = console_output
    self.input = input  # if not none, then read input from passed-in list
    self.reset()

  # Call to reset I/O for another run of the program
  def reset(self):
    self.output_log = []
    self.input_cursor = 0
    self.error_type = None
    self.error_line = None

  # Students must implement this
  def run(self, program):
    pass

  def get_input(self):
    if not self.input:
      return input()  # Get input from keyboard if not input list provided

    if self.input_cursor < len(self.input):
      cur_input = self.input[self.input_cursor]
      self.input_cursor += 1
      return cur_input
    else:
      return None

  # students must call this for any errors that they run into
  def error(self, error_type, description=None, line_num=None):
    # log the error before we throw
    self.error_line = line_num
    self.error_type = error_type

    if description:
       description = ': ' + description
    else:
       description = ''
    if line_num is None:
      raise Exception(f'{error_type}{description}')
    else:
      raise Exception(f'{error_type} on line {line_num}{description}')

  def output(self, v):
    if self.console_output:
      print(v)
    self.output_log.append(v)

  def get_output(self):
    return self.output_log

  def get_error_type_and_line(self):
    return self.error_type, self.error_line

  def validate_program(self, program):
   first_tokens = [tokens[0] if tokens else '' for tokens in
                   [line.split(InterpreterBase.COMMENT_DEF)[0].split() for line in program]]
   indents = [len(line) - len(line.lstrip(' ')) for line in program]
   self.__validate_blocks(first_tokens,indents)
   self.__validate_indentation(first_tokens,indents)

  def __validate_blocks(self, first_tokens, indents):
    stack = []
    for i in range(0,len(first_tokens)):
      if not first_tokens[i]:
        continue

      if first_tokens[i] == InterpreterBase.FUNC_DEF:
        stack.append((i, InterpreterBase.ENDFUNC_DEF, indents[i]))
        continue
      if first_tokens[i] == InterpreterBase.IF_DEF:
        stack.append((i, InterpreterBase.ENDIF_DEF, indents[i]))
        continue
      if first_tokens[i] == InterpreterBase.WHILE_DEF:
        stack.append((i, InterpreterBase.ENDWHILE_DEF, indents[i]))
        continue

      if first_tokens[i] in [InterpreterBase.ENDFUNC_DEF, InterpreterBase.ENDIF_DEF,
                             InterpreterBase.ELSE_DEF, InterpreterBase.ENDWHILE_DEF]:
        if not stack:
          self.error(ErrorType.SYNTAX_ERROR,f'Mismatched {first_tokens[i]} on line {i}', i)
        top_item = stack.pop()
        if first_tokens[i] == InterpreterBase.ELSE_DEF:
          # valdiate else and then put the endif back on the stack to be found for real endif
          if top_item[1] == InterpreterBase.ENDIF_DEF and top_item[2] == indents[i]:
            stack.append(top_item) # reappend endif for later
            continue
          self.error(ErrorType.SYNTAX_ERROR,f'Mismatched else', i)

        if top_item[1] != first_tokens[i] or top_item[2] != indents[i]:
          self.error(ErrorType.SYNTAX_ERROR,f'Missing {top_item[1]} for block on line {top_item[0]}', top_item[0])

  def __validate_indentation(self, first_tokens, indents):
    stack = []
    for i in range(0,len(first_tokens)):
      if not first_tokens[i]:
        continue

      if first_tokens[i] in [InterpreterBase.FUNC_DEF,InterpreterBase.IF_DEF,InterpreterBase.WHILE_DEF]:
        if stack and indents[i] <= stack[-1]:
          break
        stack.append(indents[i])
      elif first_tokens[i] in [InterpreterBase.ENDFUNC_DEF, InterpreterBase.ENDIF_DEF,
                               InterpreterBase.ELSE_DEF, InterpreterBase.ENDWHILE_DEF]:
        if indents[i] != stack[-1]:
          break
        if first_tokens[i] != InterpreterBase.ELSE_DEF:
          stack.pop()
      elif indents[i] <= stack[-1]:
        break

    if i < len(first_tokens)-1:
      self.error(ErrorType.SYNTAX_ERROR,f'Bad indentation on line {i}', i)
