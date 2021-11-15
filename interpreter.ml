

open Printf

let explode s =
  List.of_seq (String.to_seq s)

let implode ls =
  String.of_seq (List.to_seq ls)

(* Parser Time Baby *)
type 'a parser = Parser of (string -> ('a * string ) list)

let parse p s = 
  match p with 
    Parser f -> f s 

let charP  = 
  Parser (
    fun s ->
      match (explode s) with 
        []->[]
      |
        h::rest->[(h,implode rest)]
  )

let returnP a = 
  Parser 
    (
      fun s -> [a,s]
    )

let failP = 
  Parser
    (
      fun s->[]
    )


let (>>=) p f = 
  Parser (
    fun s ->
      match (parse p s ) with 
        []->[]     (* this is where the parser p has has failed and you make the bind fail as well *)
      |
        (h,rest)::_->  let parser2 = f h in 
        match (parse parser2 rest) with (* you can clean up the 2nd pattern matching significantly *)
          []->[]   (* this is where the parser parser2 has has failed and you make the bind fail as well *)
        |
          (h2,rest2)::_->[(h2,rest2)]
  )

(* Choice parser *)
let (<|>) a b = 
  Parser (
    fun s->  match (parse a s) with 
        []-> parse b s 
      |
        r->r
  )

let satcP (c:char)= 
  charP>>=fun x->
  if x=c then returnP c 
  else failP

let satsP s = 
  if s="" then failP else
    let rec asats (s:string)= 
      match (explode s) with 
        h::rest->satcP h >>=fun _->asats (implode rest)
      |
        []-> returnP([])
    in 
    asats (s:string)

let rec many0 p =
  (p >>= fun a -> 
   many0 p >>= fun b-> 
   returnP (a::b))
  <|>
  returnP []


let rec many1 p =
  p >>= fun a -> 
  many0 p >>= fun b-> 
  returnP (a::b)

let rec many' (p : unit -> 'a parser)  =
  (p () >>= fun a -> 
   many' p >>= fun b-> 
   returnP (a::b))
  <|>
  returnP []

let rec many1' (p : unit -> 'a parser)  =
  p () >>= fun a -> 
  many' p >>= fun b-> 
  returnP (a::b)

(* whitespaceP *)
let whitespaceP = 
  charP >>= fun c ->
  if String.contains " \012\n\r\t" c
  then returnP c
  else failP


let whitespaceP'() = 
  charP >>= fun c ->
  if String.contains " \012\n\r\t" c
  then returnP c
  else failP

(* digitP *)
let digitP = 
  charP >>= fun x -> 
  if '0' <= x && x <= '9' 
  then returnP x
  else failP

let natP = 
  (many1 digitP >>= fun a-> 
   returnP (int_of_string (implode a)))

(* Integer Parser *)
let integerP = 
  natP
  <|>
  (satcP '-' >>= fun _->
   natP >>= fun v -> 
   returnP ((-1)*v)) 


(* Helper for stringP, goes until it hits a quotation mark *)
let notquoteP =
  charP >>= fun x -> 
  if x = '"' then failP 
  else returnP x 


(* String Parser *)
let stringP =
  (satcP '"' >>= fun _ ->
   satcP '"' >>= fun _ ->
   returnP ("\"\""))
  <|>
  (satcP '"' >>= fun _ ->
   notquoteP >>= fun h -> (* so that it can be just one character *)
   many0 notquoteP >>= fun str ->
   satcP '"' >>= fun _ ->
   returnP ("\""^(implode (h::str))^"\"")) (* adding back quotes around the string *)


let letterP = 
  charP >>= fun x -> 
  if ('a' <= x && x <= 'z') || ('A' <= x && x <= 'Z')
  then returnP x
  else failP

let namecharP =
  satcP '_' <|> satcP '\'' <|> letterP <|> digitP


(* Name Parser- no quotation marks *)
let nameP =
  letterP >>= fun h ->
  many0 namecharP >>= fun str ->
  returnP (implode (h::str)) 

(* Boolean Parser *)
let boolP =
  (satcP '<' >>= fun _ ->
   satsP "true" >>= fun _ ->
   satcP '>' >>= fun _->
   returnP (true))
  <|>
  (satcP '<' >>= fun _ ->
   satsP "false" >>= fun _ ->
   satcP '>' >>= fun _->
   returnP (false))


(* Unit Parser *)
let unitP =
  satcP '<' >>= fun _ ->
  satsP "unit" >>= fun _ ->
  satcP '>' >>= fun _->
  returnP (())

type sval = 
  |I of int
  |S of string
  |N of string 
  |B of bool
  |U of unit
  |Func of (sval * sval * command list * env) (* Function!: name, arg, coms, env *)
  |Empty

and env = (sval * sval) list (* Name and binding value *)

and command = 
  |Push of sval
  |Pop  
  |Log 
  |Swap  
  |Add 
  |Sub 
  |Mul 
  |Div 
  |Rem 
  |Neg 
  |Cat
  | And | Or | Not
  | Eq
  | Lte | Lt | Gte | Gt
  | Let 
  | Ask 
  | Call
  | Throw
  | BeginEnd of command list 
  | IfElse of (command list * command list)
  | DefFun of sval (* Func sval *)
  | TryCatch of (command list * command list)




(*Individual Command Parsers!*)

(* Push *)
let pushP = 
  (many0 whitespaceP >>= fun _->
   satsP "Push" >>= fun _-> 
   many1 whitespaceP >>= fun _->
   integerP >>= fun i -> 
   satcP ';' >>= fun _ -> 
   returnP (Push (I i)) )
  <|>
  (many0 whitespaceP >>= fun _->
   satsP "Push" >>= fun _-> 
   many1 whitespaceP >>= fun _->
   stringP >>= fun s -> 
   satcP ';' >>= fun _ -> 
   returnP (Push (S s)))
  <|>
  (many0 whitespaceP >>= fun _-> 
   satsP "Push" >>= fun _-> 
   many1 whitespaceP >>= fun _->
   nameP >>= fun n -> 
   satcP ';' >>= fun _ -> 
   returnP (Push (N n)))
  <|>
  (many0 whitespaceP >>= fun _->
   satsP "Push" >>= fun _-> 
   many1 whitespaceP >>= fun _->
   boolP >>= fun b -> 
   satcP ';' >>= fun _ -> 
   returnP (Push (B b)))
  <|>
  (many0 whitespaceP >>= fun _->
   satsP "Push" >>= fun _-> 
   many1 whitespaceP >>= fun _->
   unitP >>= fun u ->
   satcP ';' >>= fun _ -> 
   returnP (Push (U u)))

(* Note: in these I am checking for semicolons right after *)
(* Pop *)
let popP = 
  many0 whitespaceP >>= fun _->
  satsP "Pop;" >>= fun _-> 
  returnP (Pop)

(* Log *)
let logP = 
  many0 whitespaceP >>= fun _->
  satsP "Log;" >>= fun _-> 
  returnP (Log)

(* Swap *)
let swapP = 
  many0 whitespaceP >>= fun _->
  satsP "Swap;" >>= fun _-> 
  returnP (Swap)

(* Add *)
let addP = 
  many0 whitespaceP >>= fun _->
  satsP "Add;" >>= fun _-> 
  returnP (Add)

(* Sub *)
let subP = 
  many0 whitespaceP >>= fun _->
  satsP "Sub;" >>= fun _-> 
  returnP (Sub)

(* Mul *)
let mulP = 
  many0 whitespaceP >>= fun _->
  satsP "Mul;" >>= fun _-> 
  returnP (Mul)

(* Div *)
let divP = 
  many0 whitespaceP >>= fun _->
  satsP "Div;" >>= fun _-> 
  returnP (Div)

(* Rem *)
let remP = 
  many0 whitespaceP >>= fun _->
  satsP "Rem;" >>= fun _-> 
  returnP (Rem)

(* Neg *)
let negP = 
  many0 whitespaceP >>= fun _->
  satsP "Neg;" >>= fun _-> 
  returnP (Neg)

(* Cat *)
let catP = 
  many0 whitespaceP >>= fun _->
  satsP "Cat;" >>= fun _-> 
  returnP (Cat)

(* And *)
let andP = 
  many0 whitespaceP >>= fun _->
  satsP "And;" >>= fun _-> 
  returnP (And)

(* Or *)
let orP = 
  many0 whitespaceP >>= fun _->
  satsP "Or;" >>= fun _-> 
  returnP (Or)

(* Not *)
let notP = 
  many0 whitespaceP >>= fun _->
  satsP "Not;" >>= fun _-> 
  returnP (Not)

(* Eq *)
let eqP = 
  many0 whitespaceP >>= fun _->
  satsP "Eq;" >>= fun _-> 
  returnP (Eq)

(* Lte *)
let lteP = 
  many0 whitespaceP >>= fun _->
  satsP "Lte;" >>= fun _-> 
  returnP (Lte)

(* Lt *)
let ltP = 
  many0 whitespaceP >>= fun _->
  satsP "Lt;" >>= fun _-> 
  returnP (Lt)

(* Gte *)
let gteP = 
  many0 whitespaceP >>= fun _->
  satsP "Gte;" >>= fun _-> 
  returnP (Gte)

(* Gt *)
let gtP = 
  many0 whitespaceP >>= fun _->
  satsP "Gt;" >>= fun _-> 
  returnP (Gt)

(* Let *)
let letP =
  many0 whitespaceP >>= fun _->
  satsP "Let;" >>= fun _-> 
  returnP (Let)

(* Ask *)
let askP =
  many0 whitespaceP >>= fun _->
  satsP "Ask;" >>= fun _-> 
  returnP (Ask)

(* Call *)
let callP =
  many0 whitespaceP >>= fun _->
  satsP "Call;" >>= fun _-> 
  returnP (Call)

(* Throw *)
let throwP =
  many0 whitespaceP >>= fun _->
  satsP "Throw;" >>= fun _-> 
  returnP (Throw)

(* BeginEnd *)
let rec beginendP() = 
  many0 whitespaceP >>= fun _->
  satsP "Begin" >>= fun _-> 
  many1 whitespaceP >>= fun _ ->
  many1 (commandP()) >>= fun ls ->
  many0 whitespaceP >>= fun _->
  satsP "End;" >>= fun _ ->
  returnP (BeginEnd ls)

(* IfElseEnd *)
and ifelseP() =
  many0 whitespaceP >>= fun _->
  satsP "If" >>= fun _-> 
  many1 whitespaceP >>= fun _ ->
  many1 (commandP()) >>= fun ls1 ->
  many0 whitespaceP >>= fun _->
  satsP "Else" >>= fun _-> 
  many1 whitespaceP >>= fun _ ->
  many1 (commandP()) >>= fun ls2 ->
  many0 whitespaceP >>= fun _-> 
  satsP "End;" >>= fun _ ->
  returnP (IfElse (ls1,ls2))

(*Deffun parser*)
and deffunP() = 
  many0 whitespaceP >>= fun _->
  satsP "DefFun" >>= fun _-> 
  many1 whitespaceP >>= fun _ ->
  nameP >>= fun fname ->
  many1 whitespaceP >>= fun _ ->
  nameP >>= fun arg ->
  many1 whitespaceP >>= fun _ ->
  many1 (commandP()) >>= fun ls ->
  many0 whitespaceP >>= fun _->
  satsP "End;" >>= fun _ ->
  returnP (DefFun (Func ((N fname), (N arg) ,ls, []))) 

(* TryCatch *)
and trycatchP() =
  many0 whitespaceP >>= fun _->
  satsP "Try" >>= fun _-> 
  many1 whitespaceP >>= fun _ ->
  many1 (commandP()) >>= fun ls1 ->
  many0 whitespaceP >>= fun _->
  satsP "Catch" >>= fun _-> 
  many1 whitespaceP >>= fun _ ->
  many1 (commandP()) >>= fun ls2 ->
  many0 whitespaceP >>= fun _-> 
  satsP "End;" >>= fun _ ->
  returnP (TryCatch (ls1,ls2))


(* All the commands *)
and commandP() =
  pushP <|> popP <|> swapP <|> logP <|> 
  addP <|> subP <|> mulP <|> divP <|> remP <|> negP <|>
  catP  <|> andP  <|> orP  <|> notP  <|> eqP  <|> lteP  <|>
  ltP  <|> gteP  <|> gtP  <|> letP  <|> askP <|> callP <|> throwP
  <|> beginendP() <|> ifelseP() <|> deffunP() <|> trycatchP()

(* Generate the entire list of commands *)
let commandsP = 
  many1 (commandP()) 



(* Helper functions for eval *)

let log stack strlist = 
  match stack with 
  | (I i)::rest -> (string_of_int i)::strlist
  | (S s)::rest -> s::strlist 
  | (N n)::rest -> n::strlist
  | (B b)::rest -> ("<"^(string_of_bool b)^">")::strlist
  | (U u)::rest -> "<unit>"::strlist
  | (Func f)::rest -> "<fun>"::strlist
  | _ -> strlist


let rec find name bindls =
  match name, bindls with
  | n1, (n2, x)::rest -> if n1 = n2 then (true, x) else find name rest
  | _, [] -> (false, Empty)


let extractstringP =
  (satcP '"' >>= fun _ ->
   satcP '"' >>= fun _ ->
   returnP ("\"\""))
  <|>
  (satcP '"' >>= fun _ ->
   notquoteP >>= fun h -> (* so that it can be just one character *)
   many0 notquoteP >>= fun str ->
   satcP '"' >>= fun _ ->
   returnP (implode (h::str)) )(* not adding back quotes around the string *)

let extract str = parse extractstringP str

let cat str1 str2 = 
  match str1,str2 with
  |S "\"\"", S "\"\"" -> S "\"\""
  |S "\"\"", S s2 -> S s2
  |S s1, S "\"\"" -> S s1
  |S s1, S s2 -> (match extract s1, extract s2 with 
      | (x1, _)::_, (x2, _)::_ -> S ("\""^(x1^x2)^"\"")
      | _, _ -> Empty )
  | _ ,_ -> Empty 


(* Eval function - executes the list of commands *)
let rec eval (coms: command list) (stack: sval list) (strlist: string list) (bindls)
  : string list * int * sval list= 
  match coms, stack with 
  | Push v::rest, _ -> eval rest (v::stack) strlist bindls
  | BeginEnd comls::rest, stack -> (match eval comls [] strlist bindls with
      |(localstr, 0, [x]) -> eval rest (x::stack) localstr bindls (* could I just pass in strlist??*)
      |(localstr, 0, []) -> (localstr, 2, []) 
      |(localstr, x, _) -> (localstr, x, []))
  | DefFun (Func (fname, arg, comls, fenv))::rest, stack -> eval rest stack strlist ((fname, (Func (fname, arg, comls, bindls)))::bindls) (*pass bindls (current env) intp function so it captures env when it is defined/created *)
  | TryCatch (comls1, comls2)::rest, stack -> 
    (match eval comls1 stack strlist bindls with
     |(localstr, 0, [x]) -> eval rest (x::stack) localstr bindls
     |(localstr, 0, []) -> (localstr, 2, []) 
     |(localstr, error, _) -> 
       (match eval comls2 (I error::stack) localstr bindls with
        |(localstr2, 0, [x]) -> eval rest (x::stack) localstr2 bindls
        |(localstr2, 0, []) -> (localstr2, 2, []) 
        |(localstr2, x, _) -> (localstr2, x, [])))
  (* Need at least one item on the stack, error code 2*)
  | _::rest, [] -> (strlist, 2, []) 
  | Ask::rest, (N n)::srest -> (match find (N n) bindls with
      |(true, x) -> eval rest (x::srest) strlist bindls
      |(false,_) -> (strlist,4,[]))
  | Ask::rest, _::srest -> (strlist, 1, [])
  | IfElse (comls1, comls2)::rest, (B b)::srest -> if b = true then eval (comls1 @ rest) srest strlist bindls else eval (comls2 @ rest) srest strlist bindls (*list in correct order??*)
  | IfElse (comls1, comls2)::rest, _::srest -> (strlist, 1, []) (*ifElse is called but not a boolean*)
  | Pop::rest, h::srest -> eval rest srest strlist bindls
  | Log::rest, h::srest -> eval rest srest (log stack strlist) bindls (* keep log helper bc easier?*)
  | Neg::rest, (I x)::srest -> eval rest ((I (-1 * x))::srest) strlist bindls
  | Neg::rest, x::srest -> (strlist, 1, []) (*Neg is called but top value is not int*)
  | Not::rest, (B b)::srest -> eval rest (B (not b)::srest) strlist bindls(*works right?*)
  | Not::rest, x::srest -> (strlist, 1, []) (*Not is called but top value is not a bool*)
  | Throw::rest, (I x)::srest -> (strlist, x, [])
  | Throw::rest, x::srest -> (strlist, 1, []) (*Throw called but top value is not an int*)
  (* Need at least two items on the stack, error code 2*)
  | _::rest, [x] -> (strlist, 2, []) 
  | Call::rest, x::(Func (fname, arg, comls, fenv))::srest -> (match eval comls [] strlist ((arg, x)::(fname, (Func (fname, arg, comls, bindls)))::fenv) with (* when function is called must add itself into env so it can call itself, too complicated to do in deffun?? *)
      |(localstr, 0, [x]) -> eval rest (x::srest) localstr bindls 
      |(localstr, 0, []) -> (localstr, 2, []) (*terminate if function fails?*)
      |(localstr, x, _) -> (localstr, x, []))
  | Let::rest, (N n)::x::srest -> eval rest srest strlist ((N n, x)::bindls)
  | Cat::rest, (S s1)::(S s2)::srest -> eval rest ((cat (S s1) (S s2))::srest) strlist bindls
  | Swap::rest, h1::h2::srest -> eval rest (h2::h1::srest) strlist bindls
  | Add::rest, (I x)::(I y)::srest -> eval rest ((I (x + y))::srest) strlist bindls
  | Sub::rest, (I x)::(I y)::srest -> eval rest ((I (x - y))::srest) strlist bindls
  | Mul::rest, (I x)::(I y)::srest -> eval rest ((I (x * y))::srest) strlist bindls
  | Div::rest, (I x)::(I y)::srest -> if y = 0 then (strlist, 3, []) else eval rest ((I (x / y))::srest) strlist bindls
  | Rem::rest, (I x)::(I y)::srest -> if y = 0 then (strlist, 3, []) else eval rest ((I (x mod y))::srest) strlist bindls
  | Eq::rest, (I x)::(I y)::srest ->  if x = y then eval rest (B true::srest) strlist bindls else eval rest (B false::srest) strlist bindls
  | Lte::rest, (I x)::(I y)::srest ->  if x <= y then eval rest (B true::srest) strlist bindls else eval rest (B false::srest) strlist bindls
  | Lt::rest, (I x)::(I y)::srest ->  if x < y then eval rest (B true::srest) strlist bindls else eval rest (B false::srest) strlist bindls
  | Gte::rest, (I x)::(I y)::srest ->  if x >= y then eval rest (B true::srest) strlist bindls else eval rest (B false::srest) strlist bindls
  | Gt::rest, (I x)::(I y)::srest ->  if x > y then eval rest (B true::srest) strlist bindls else eval rest (B false::srest) strlist bindls
  | And::rest, (B b1)::(B b2)::srest -> eval rest (B (b1 && b2)::srest) strlist bindls
  | Or::rest, (B b1)::(B b2)::srest -> eval rest (B (b1 || b2)::srest) strlist bindls
  | _::rest, x::y::srest -> (strlist, 1, []) (*any of them are called is called but top2 are not correct types*)
  | [], x::_ -> (strlist, 0, [x]) (*make it through all commands*)
  | [], [] -> (strlist, 0, []) (*make it through all commands*)


let interpreter (s : string) : string list * int = 
  match (parse commandsP s) with 
  | [comlist, unstr] -> (match eval comlist [] [] [] with 
      | (strlist, ec, _) -> ((List.rev strlist), ec))
  | (_, _)::_ -> ([], 0) 
  | [] -> ([], 0) 


(*how do I open the test files?? Test eval on its own first please!*)
let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

let runfile (file : string) : string list * int =
  let s = readlines file in
  interpreter s

(* For testing *)
(*let rec print_list_string ls =
  match ls with 
  |[] -> print_string "[]"
  |h::rest -> print_string h;  print_list_string rest


  let print_interpreter sli =
  match sli with
  | ([], x) -> print_string "[]"; print_int x
  | (sl, x) -> print_list_string sl; print_int x

*)


