let path = Sys.argv.(1)
let prog = Io.read_file path
let tokens = Lexer.tokenize prog
let () = Lexer.print_list Lexer.format_token tokens
