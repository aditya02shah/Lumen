let prog =
  {| 
int main(){
    mut int x = 5;
    x = 10;
    
    return 0;
}
|}

let strings = String.split_on_char ' ' prog

(* let () = Lexer.print_list (fun x -> x) strings *)
let tokens = Lexer.tokenize strings
let () = Lexer.print_list Lexer.format_token tokens
