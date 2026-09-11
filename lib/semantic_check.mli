type semantic_errors = string list

val check_prog : Parser.prog -> semantic_errors
val check_cond_expr : Parser.cond_expr -> semantic_errors
