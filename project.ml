(** Part 1: Parsing *)
let parse_exp_tests : (string * exp option) list = [
  ("", None)
]

let rec exp_parser i =
  let open Parser in
  (** Use [identifier] and [keyword] in your implementation,
      not [identifier_except] and [keyword_among] directly *)
  let identifier, keyword =
    let keywords = ["true"; "false"; "let"; "in"; "end"; "if"; "then"; "else"; "fn"; "rec"] in
    identifier_except keywords, keyword_among keywords
  in
  let atomic_exp_parser =
    first_of [
      map (fun i -> ConstB true) (keyword "true");
      map (fun i -> ConstB false) (keyword "false");
      map (fun i -> ConstI i) int_digits;
      map (fun i -> Var i) identifier;
      between (symbol "(") (symbol ")") exp_parser;
    ]
  in
  let applicative_exp_parser =
    left_assoc_op 
      (of_value (fun f a -> Apply(f, a)))
      atomic_exp_parser 
      (fun e1 op e2 -> Apply (e1, e2))
  in
  let negatable_exp_parser =
    first_of [
      (keyword "let" |>>
       between (symbol "(") (symbol ")")
         (identifier |*> fun x -> symbol "," |>>
             identifier |*> fun y ->
               of_value (x, y))
       |*> fun (x, y) ->
         symbol "=" |>>
         exp_parser |*> fun e1 -> keyword "in" |>>
           exp_parser |*> fun e2 -> symbol "end" |>>
             of_value (LetComma (x, y, e1, e2)));
      (keyword "let" |>>
       identifier |*> fun x -> symbol "=" |>>
         exp_parser |*> fun e1 -> keyword "in" |>>
           exp_parser |*> fun e2 -> symbol "end" |>>
             of_value (Let (x, e1, e2)));

      (keyword "if" |>>
       exp_parser |*> fun cond -> keyword "then" |>>
         exp_parser |*> fun e1 -> keyword "else" |>>
           exp_parser |*> fun e2 -> of_value (If (cond, e1, e2)));

      (keyword "fn" |>>
       identifier |*> fun x ->
         optional (symbol ":" |>> typ_parser) |*> fun t ->
           symbol "=>" |>>
           exp_parser |*> fun e -> of_value (Fn (x, t, e)));

      (keyword "rec" |>>
       identifier |*> fun f ->
         optional (symbol ":" |>> typ_parser) |*> fun t ->
           symbol "=>" |>>
           exp_parser |*> fun e -> of_value (Rec (f, t, e)));

      applicative_exp_parser;
    ]
  in
  let negation_exp_parser =
    prefix_op
      (symbol "-" |>> of_value Negate)
      negatable_exp_parser
      (fun op e -> PrimUop (op, e))
  in
  let multiplicative_exp_parser =
    left_assoc_op
     (symbol "*" |>> of_value Times)
      negation_exp_parser
      (fun e1 op e2 -> PrimBop (e1, op, e2))
  in
  let additive_exp_parser =
    left_assoc_op
      (first_of_2
         (symbol "+" |>> of_value Plus)
         (symbol "-" |>> of_value Minus))
      multiplicative_exp_parser
      (fun e1 op e2 -> PrimBop (e1, op, e2))
  in
  let comparative_exp_parser =
    left_assoc_op
      (first_of_2
         (symbol "=" |>> of_value Equals)
         (symbol "<" |>> of_value LessThan))
      additive_exp_parser
      (fun e1 op e2 -> PrimBop (e1, op, e2))
  in
  (** You may need to define helper parsers depending on [exp_parser] here *)
  let exp_parser_impl =
    first_of_2
      (map2 (fun e1 e2 -> Comma(e1, e2))
         comparative_exp_parser
         (symbol "," |>> comparative_exp_parser))
      (comparative_exp_parser)
  in
  exp_parser_impl i

(** DO NOT Change This Definition *)
let parse_exp : string -> exp option =
  let open Parser in
  run (between spaces eof exp_parser)
