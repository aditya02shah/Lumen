let prog = Io.read_file "examples/mut.sui"
let tokens = Lexer.tokenize prog
let () = Lexer.print_list Lexer.format_token tokens
