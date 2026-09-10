type semantic_errors = string list

val check_prog : Parser.prog -> semantic_errors
