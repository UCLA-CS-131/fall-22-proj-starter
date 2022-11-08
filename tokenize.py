from intbase import InterpreterBase, ErrorType

# Tokenzies a program, e.g., "assign var + 5 10" --> ["assign","var","+","5","10"] for each line of the input program
# Input: A list of strings, e.g.: ["func main", " assign x 10", " funccall print x","endfunc"]
# Output: A list of lists of tokens, e.g.: [["func","main"],["assign","x","10"],["funccall","print","x"],["endfunc"]]
class Tokenizer:
  # Performs tokenization and returns the tokenized program
  def tokenize_program(program):
    tokenized_program = []
    for line_num, line in enumerate(program):
      tokens = Tokenizer._tokenize(line_num, line.rstrip())
      tokenized_program.append(tokens)
    return tokenized_program

  def _remove_comment(s):
   in_quote = False
   for i in range(0,len(s)):
     if s[i] == '"':
      in_quote = not in_quote
     elif s[i] == InterpreterBase.COMMENT_DEF and not in_quote:
      return s[:i]
   return s

  def _tokenize(line_num, s):
    s = Tokenizer._remove_comment(s)

    tokens = []
    search_from = 0
    while True:
      start_quote = end_quote = None
      try:
        start_quote = s.index('"', search_from)
        end_quote = s.index('"', start_quote+1)
      except:
        if start_quote and not end_quote:
          super().error(ErrorType.SYNTAX_ERROR,f"Mismatched quotes",line_num) #no
      if start_quote is None:
        break
      else:
        tokens += s[search_from:start_quote].split()
        tokens.append(s[start_quote:end_quote+1])
        search_from = end_quote + 1
    # no more quotes found, tokenize remaining string
    tokens += s[search_from:].split()
    return tokens
