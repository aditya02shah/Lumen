  $ ../../bin/main.exe ../../examples/for.sui
  DATATYPE: INT
  IDENTIFIER: main
  '('
  ')'
  '{'
  WHITESPACE
  FOR
  '('
  MUT
  DATATYPE: INT
  IDENTIFIER: i
  '='
  NUMBER 0
  ';'
  IDENTIFIER: i
  '<'
  NUMBER 10
  ';'
  IDENTIFIER: i
  'INCREMENT'
  ')'
  '{'
  WHITESPACE
  IDENTIFIER: printf
  '('
  IDENTIFIER: i
  ')'
  ';'
  WHITESPACE
  '}'
  WHITESPACE
  RETURN
  NUMBER 0
  ';'
  WHITESPACE
  '}'

  $ ../../bin/main.exe ../../examples/func.sui
  DATATYPE: INT
  IDENTIFIER: add
  '('
  DATATYPE: INT
  IDENTIFIER: x
  ','
  DATATYPE: INT
  IDENTIFIER: y
  ')'
  '{'
  WHITESPACE
  RETURN
  IDENTIFIER: x
  +
  IDENTIFIER: y
  ';'
  WHITESPACE
  '}'
  WHITESPACE
  DATATYPE: INT
  IDENTIFIER: main
  '('
  ')'
  '{'
  WHITESPACE
  DATATYPE: INT
  IDENTIFIER: res
  '='
  IDENTIFIER: add
  '('
  NUMBER 2
  ','
  NUMBER 3
  ')'
  ';'
  WHITESPACE
  RETURN
  NUMBER 0
  ';'
  WHITESPACE
  '}'

  $ ../../bin/main.exe ../../examples/mut.sui
  DATATYPE: INT
  IDENTIFIER: main
  '('
  ')'
  '{'
  WHITESPACE
  MUT
  DATATYPE: INT
  IDENTIFIER: x
  '='
  NUMBER 5
  ';'
  WHITESPACE
  IDENTIFIER: x
  '='
  NUMBER 10
  ';'
  WHITESPACE
  RETURN
  NUMBER 0
  ';'
  WHITESPACE
  '}'
  WHITESPACE

  $ ../../bin/main.exe ../../examples/option.sui
  'OPTION'
  '<'
  IDENTIFIER: int
  '>'
  IDENTIFIER: divide
  '('
  DATATYPE: INT
  IDENTIFIER: a
  ','
  DATATYPE: INT
  IDENTIFIER: b
  ')'
  '{'
  WHITESPACE
  IDENTIFIER: if
  '('
  IDENTIFIER: b
  '='
  '='
  NUMBER 0
  ')'
  '{'
  WHITESPACE
  RETURN
  'NONE'
  ';'
  WHITESPACE
  '}'
  WHITESPACE
  RETURN
  'SOME'
  '('
  IDENTIFIER: a
  '/'
  IDENTIFIER: b
  ')'
  ';'
  WHITESPACE
  '}'

  $ ../../bin/main.exe ../../examples/while.sui
  DATATYPE: INT
  IDENTIFIER: main
  '('
  ')'
  '{'
  WHITESPACE
  MUT
  DATATYPE: INT
  IDENTIFIER: i
  '='
  NUMBER 0
  ';'
  WHITESPACE
  WHILE
  '('
  IDENTIFIER: i
  '<'
  NUMBER 10
  ')'
  '{'
  WHITESPACE
  IDENTIFIER: printf
  '('
  IDENTIFIER: i
  ')'
  ';'
  WHITESPACE
  IDENTIFIER: i
  'INCREMENT'
  ';'
  WHITESPACE
  '}'
  WHITESPACE
  RETURN
  NUMBER 0
  ';'
  WHITESPACE
  '}'
