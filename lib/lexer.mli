type token

exception Unknown_token

val tokenize : string -> token list
val print_list : ('a -> string) -> 'a list -> unit
val format_token : token -> string
