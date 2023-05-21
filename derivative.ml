open Str

#load "str.cma";; (* Interpreter directive to use str.cma library, with regular expressions *)

let () = Printexc.record_backtrace true

(* Expression creation and derivation logic *)
 
type expr =
  Const of float
  |Var of string
  |Sum of expr * expr
  |Prod of expr * expr
  |Sub of expr * expr
  |Exp of expr * expr
  |Neg of expr
  |Div of expr * expr

let neg e = match e with
  |Neg(Const(0.0)) -> Const(0.0)
  |Neg(exp) -> exp
  |Sub(e1,e2) -> Sub(e2,e1)
  |_ -> Neg(e)

let prod e1 e2 = match (e1,e2) with 
  |(Const(0.0), _) -> Const(0.0)
  |(_, Const(0.0)) -> Const(0.0)
  |(Const(1.0) , x) | (x, Const(1.0))  -> x
  |(Const(c1), Const(c2)) -> Const(c1 *. c2)
  |_ -> Prod(e1,e2)

let sum e1 e2 = match (e1,e2) with 
  |(Const(0.0), x) -> x
  |(x, Const(0.0)) -> x
  |(Const(x), Const(y)) -> Const(x +. y)
  |_ -> Sum(e1,e2)

let sub e1 e2 = match (e1,e2) with
  |(Const(0.0), x) -> Neg(x)
  |(x, Const(0.0)) -> x
  |(Const(x), Const(y)) -> Const(x -. y)
  |_ -> Sub(e1,e2)


let exp e1 e2 = match (e1,e2) with 
  |(Const(0.0), x) -> Const(0.0)
  |(_, Const(0.0)) -> Const(1.0)
  |(x, Const(1.0)) -> x
  |(Const(1.0), x) -> Const(1.0)
  |(Const(x), Const(y)) -> Const(x ** y)
  |_ -> Exp(e1,e2)

let div e1 e2 = match (e1,e2) with
  |(_, Const(0.0)) -> raise Division_by_zero
  |(Const(0.0), _) -> Const(0.0)
  |(x, Const(1.0)) -> x
  |(Const(c1), Const(c2)) -> Const(c1 /. c2)
  |_ -> Div(e1,e2)

let rec derivative v expr = 
  match expr with 
    |Const(_) -> Const(0.0)
    |Var(x) -> if v = x then Const(1.0) else Const(0.0)
    |Sum(e1,e2) -> sum (derivative v e1) (derivative v e2)
    |Sub(e1,e2) -> sub (derivative v e1) (derivative v e2)
    |Prod(e1,e2) -> sum (prod e1 (derivative v e2)) (prod (derivative v e1) e2)
    |Exp(e1,e2) -> prod (prod e2 (exp e1 (sub e2 (Const(1.0))))) (derivative v e1)
    |Neg(e) -> neg (derivative v e)
    |Div(e1,e2) -> div (sub (prod e2 (derivative v e1)) (prod e1 (derivative v e2))) (exp e2 (Const(2.0)))

(* Helper functions *)

let rec toString exp = match exp with 
  |Var(name) -> name
  |Const(num) -> Printf.sprintf "%.2f" num
  |Sum(e1,e2) -> (toString e1) ^ " + " ^ (toString e2)
  |Prod(Const(cst), e) -> (toString (Const(cst)))^(toString e)
  |Prod(e, Const(cst)) -> (toString (Const(cst)))^(toString e)
  |Prod(e1,e2) -> (toString e1) ^ " * " ^ (toString e2)
  |Sub(e1,e2) -> (toString e1) ^ " - " ^ (toString e2)
  |Neg(e) -> "-"^(toString e)
  |Exp(e1,e2) -> (toString e1)^"**"^(toString e2)
  |Div(e1,e2) -> (toString e1)^" / "^(toString e2)


let rec lookup (env : (string * float) list) (element: string): float option = 
  match env with
    |[] -> None
    |(x,y)::rest -> if x = element then Some y else lookup rest element 

let collect_vars expression = 
  let rec collect e acc = match e with 
    |Var(name) -> name::acc
    |Const(_) -> acc
    |Sum(e1, e2) -> collect e1 (collect e2 acc) 
    |Prod(e1, e2) -> collect e1 (collect e2 acc) 
    |Sub(e1, e2) -> collect e1 (collect e2 acc)
    |Div(e1, e2) -> collect e1 (collect e2 acc)
    |Exp(e1, e2) -> collect e1 (collect e2 acc)
    |Neg(e) -> collect e acc
  in
    collect expression []

let is_member env element = match lookup env element with
  |None -> false
  |Some(_) -> true

(* Evaliation logic *)


exception EvalError of string


let rec eval_with expression env = match expression with
  |Var(name) -> begin match lookup env name with 
                |Some(value) -> value
                |None -> raise (EvalError ("Unknown variable : "^name)) end
  |Const(value) -> value
  |Sum(e1, e2) -> (eval_with e1 env) +. (eval_with e2 env)
  |Prod(e1,e2) -> (eval_with e1 env) *. (eval_with e2 env)
  |Exp (e1, e2) -> (eval_with e1 env) ** (eval_with e2 env)
  |Div (e1, e2) -> (eval_with e1 env) /. (eval_with e2 env)
  |Sub (e1, e2) -> (eval_with e1 env) -. (eval_with e2 env)
  |Neg(e) -> -.(eval_with e env)


(* Parsing helper functions and algorithm *)


let rest list = match list with 
  |_::rest -> rest
  |[] -> []

let first lst = match lst with 
  |x::_ -> Some x
  |[] -> None

let rec skip_blank contents = match contents with
  |[] -> []
  |c::rest -> match c with
    |' ' | '\t' | '\r' | '\n' -> skip_blank rest
    |_ -> contents 

let advance input = input := skip_blank (rest !input) 

let explode s =
    let rec expl i l =
      if i < 0 then l else expl (i - 1) (s.[i] :: l) 
    in
  expl (String.length s - 1) []

let implode l =
  let result = Bytes.create (List.length l) in
  let rec imp i = function
  | [] -> result
  | c :: l -> (Bytes.set result i c); imp (i + 1) l in
  Bytes.to_string (imp 0 l)

let consumeVar chars_ref = 
    let rec consume input acc = match !input with 
      |[] |' '::_ | '*'::_ | '+'::_ | '-'::_ | '/'::_ |'\x00'::_ -> Var(implode (List.rev acc))
      |c::_ -> begin advance input; consume input (c::acc) end
    in 
      consume chars_ref []

exception ParseError of string

let is_num c = Str.string_match (Str.regexp "[0-9]+$") (String.make 1 c) 0

let is_var chr = match chr with 
  |'*' | '+' | '/' | '-' | '(' | ')' -> false
  |c when is_num c -> false 
  |_ -> true


let consumeNum chars_ref = 
  let rec consume input acc dotFound = 
    match first !input with 
      |Some c when is_num c -> begin 
        advance input;
        consume input (c::acc) dotFound end
      |Some c when c = '.' -> if dotFound then raise (ParseError "Invalid float literal")
          else begin 
            advance input;
            consume input (c::acc) true end
      |_ -> (float_of_string (implode (List.rev acc)))
    in
    Const(consume chars_ref [] false)


let peek_no_blank input : char = match (skip_blank !input) with 
  |c::_ -> c
  |[] -> '\x00'

let peek input : char = match !input with 
  |c::_ -> c
  |[]-> '\x00'

let (^*) (s: string) (c: char) : string = s ^ String.make 1 c 

let rec getE input = getE2 input (getT input)

and getE2 input t = match peek_no_blank input with 
  |'+' -> begin advance input; getE2 input (sum t (getT input)) end
  |'-' -> begin advance input; getE2 input (sub t (getT input)) end
  |_ -> t

and getT input = getT2 input (getF input)

and getT2 input f = match peek_no_blank input with 
  |'*' -> begin 
            advance input; 
            if peek input = '*' then begin 
              advance input;
              getT2 input (exp f (getF input)) end
            else getT2 input (prod f (getF input)) end
  |'/' -> begin advance input; getT2 input (div f (getF input)) end
  |_ -> f

and getF input = match peek_no_blank input with
  |'-' -> begin advance input; neg (getF input) end
  |c when is_num c -> consumeNum input
  |c when is_var c -> consumeVar input
  |'(' -> begin
            advance input; 
            let e = getE input in
            let tok = peek input in 
            if tok = ')' 
              then begin 
                  advance input;
                  e
              end
            else
              raise (ParseError (("Unexpected token :"^* tok)^". Expected ')'"))
          end 
  |other -> raise (ParseError ("Unexpected token : "^*other))


let parse expr_string = getE (ref (explode expr_string))

let df var inputStr = toString (derivative var (parse inputStr))

(* User prompts and main function *)

let prompt_user_df () = 
  print_endline "Enter the expression you want to derive or evaluate";
  let input = read_line () in
    print_endline "Enter the name of the variable you want to derive with respect to";
    let var = read_line () in
      print_endline ("Derivative : "^(df var input))

let build_env var_list = 
  let rec aux vlist acc = match vlist with 
    |[] -> acc
    |x::xs -> begin 
                print_endline ("Enter a value for variable "^x^":");
                let value = read_float () in 
                aux xs ((x,value)::acc)
              end
  in
    aux var_list []
    

let prompt_user_eval () = 
  print_endline "Enter the expression you want to evaluate";
  let input = read_line () in
  let expression = parse input in 
  let env = build_env (collect_vars expression) in 
    print_endline ("Result: "^(string_of_float (eval_with expression env)))

let rec ask () = 
  print_endline "Enter d to derive, e to evaluate or x to quit the program :";
  match read_line () with
    |"d" -> prompt_user_df ()
    |"e" -> prompt_user_eval () 
    |"x" -> print_endline "Goodbye!"
    |_ -> print_endline "Sorry, I did not understand."; ask ()

let main = 
  print_endline "Welcome to the derivation machine!";
  ask ()
    
