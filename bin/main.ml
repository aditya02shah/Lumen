let prog =
  {| 
int main(){
    mut int x = 5;
    x = 10;
    
    return 0;
}
|}

let tokens = Lexer.tokenize prog
let () = Lexer.print_list Lexer.format_token tokens
