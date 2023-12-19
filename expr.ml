(* 
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Fl_plus
  | Minus
  | Fl_minus
  | Times
  | Fl_times
  | Divides
  | Fl_divides
  | Equals
  | LessThan
  | GreaterThan
  | Concat
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
  | Float of float                       (* floats *)
  | String of string                     (* strings *)
  | Unit of unit                         (* unit type *)
  | Ref of varid * expr                  (* references *)
  | Deref of varid                       (* dereferencing *)
;;
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal ;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Var x -> SS.add x SS.empty
  | Unop (_, ex) -> free_vars ex
  | Binop (_, ex1, ex2)
  | App (ex1, ex2) -> SS.union (free_vars ex1) (free_vars ex2)
  | Conditional (ex1, ex2, ex3) -> SS.union (free_vars ex1) 
                                      (SS.union (free_vars ex2) (free_vars ex3))
  | Fun (var, ex) -> SS.remove var (free_vars ex)
  | Let (var, ex1, ex2) -> SS.union (free_vars ex1) 
                                (SS.diff (free_vars ex2) (SS.singleton var)) 
  | Letrec (var, ex1, ex2) -> SS.diff (free_vars (Let (var, ex1, ex2))) 
                                                          (SS.singleton var)
  | _expr -> SS.empty ;;

(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no other variable names
   use the prefix "var". (Otherwise, they might accidentally be the
   same as a generated variable name.) *)
let new_varname : unit -> varid =
  let counter = ref ~-1 in
  fun () -> 
    counter := !counter + 1;
    "var" ^ string_of_int !counter ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
   
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let subpartial = subst var_name repl in
  match exp with
  | Var var -> if var = var_name then repl 
               else exp
  | Unop (unop, ex) -> Unop (unop, subpartial ex)
  | Binop (bi, ex1, ex2) -> Binop (bi, subpartial ex1, subpartial ex2)
  | Conditional (ex1, ex2, ex3) -> Conditional (subpartial ex1, subpartial ex2,
                                                subpartial ex3)
  | Fun (var, ex) -> if var = var_name then exp
                     else if SS.mem var (free_vars repl) then 
                      let nextvar = new_varname () in
                      Fun (nextvar, subpartial ex)
                     else Fun (var, subpartial ex)
  | Let (var, def, body) -> if var = var_name then Let (var, subpartial def, 
                                                                          body)
                            else if SS.mem var (free_vars repl) then
                              let nextvar = new_varname () in
                              Let (nextvar, subpartial def, 
                                    subpartial (subst var (Var(nextvar)) body))
                            else Let(var, subpartial def, subpartial body)
  | Letrec (var, def, body) -> if var = var_name then exp
                               else if SS.mem var (free_vars repl) then
                                let nextvar = new_varname () in
                                Letrec (nextvar, subpartial 
                                (subst var (Var nextvar) def), 
                                subpartial (subst var (Var nextvar) body))
                              else Letrec (var, subpartial def, subpartial body)
  | App (ex1, ex2) -> App (subpartial ex1, subpartial ex2)
  | ex -> ex ;;

(*......................................................................
  String representations of expressions
 *)

let binop_to_string (bi: binop) : string =
  match bi with
  | Plus 
  | Fl_plus -> "Plus"
  | Minus 
  | Fl_minus -> "Minus"
  | Times 
  | Fl_times -> "Times"
  | Divides 
  | Fl_divides -> "Divided by"
  | Equals -> "Equals"
  | LessThan -> "Less Than"
  | GreaterThan -> "Greater Than"
  | Concat -> "Concatenate" ;;


(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  | Var var -> var
  | Num x -> string_of_int x
  | Bool x -> string_of_bool x
  | Unop (unop, ex) -> 
    (match unop with
    | Negate -> "Negate " ^ exp_to_concrete_string ex)
  | Binop (bi, ex1, ex2) -> (exp_to_concrete_string ex1) ^ " " 
                      ^ binop_to_string bi ^ " " ^ (exp_to_concrete_string ex2)
  | Conditional (ex1, ex2, ex3) -> 
      "if " ^ exp_to_concrete_string ex1 ^ " then " ^ exp_to_concrete_string ex2
      ^ " else " ^ exp_to_concrete_string ex3
  | Fun (var, ex) -> "Fun " ^ var ^ " -> " ^ exp_to_concrete_string ex
  | Let (var, def, body) -> "Let " ^ var ^ " = " ^ exp_to_concrete_string def
      ^ " in " ^ exp_to_concrete_string body  
  | Letrec (var, def, body) -> "rec Let " ^ var ^ " = " 
      ^ exp_to_concrete_string def ^ " in " ^ exp_to_concrete_string body
  | Raise -> "raise"
  | Unassigned -> "unassigned"
  | App (ex1, ex2) -> "(" ^ exp_to_concrete_string ex1 ^ ") (" 
                                          ^ exp_to_concrete_string ex2 ^ ")"
  | Float x -> string_of_float x
  | String str -> str
  | Unit () -> "()"
  | Ref (r, ex) -> "Let " ^ r ^ " = ref " ^ exp_to_concrete_string ex
  | Deref r -> "!" ^ r ;;

     
(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var var -> "Var(" ^ var ^ ")" 
  | Num x -> "Num(" ^ string_of_int x ^ ")"
  | Bool x -> "Bool(" ^ string_of_bool x ^ ")"
  | Unop (unop, ex) -> 
    (match unop with
    | Negate -> "Unop(Negate, " ^ exp_to_abstract_string ex ^ ")")
  | Binop (bi, ex1, ex2) -> "Binop(" ^ binop_to_string bi ^ ", " 
      ^ exp_to_abstract_string ex1 ^ ", " ^ exp_to_abstract_string ex2 ^ ")"
  | Conditional (ex1, ex2, ex3) -> "Conditional(" ^ exp_to_abstract_string ex1 
    ^ ", " ^ exp_to_abstract_string ex2 ^ ", " ^ exp_to_abstract_string ex3
                                                                          ^ ")"
  | Fun (var, ex) -> "Fun(" ^ var ^ ", " ^ exp_to_abstract_string ex ^ ")"
  | Let (var, def, body) -> "Let(" ^ var ^ ", " ^ exp_to_abstract_string def 
                            ^ ", " ^ exp_to_abstract_string body ^ ")"
  | Letrec (var, def, body) -> "Letrec (" ^ var ^ ", " 
        ^ exp_to_abstract_string def ^ ", " ^ exp_to_abstract_string body ^ ")"
  | Raise -> "raise"
  | Unassigned -> "unassigned"
  | App (ex1, ex2) -> "App(" ^ exp_to_abstract_string ex1 ^ ", " 
                                          ^ exp_to_abstract_string ex2 ^ ")"
  | Float x -> "Float(" ^ string_of_float x ^ ")"
  | String str -> "String(" ^ str ^ ")"
  | Unit () -> "Unit()"
  | Ref (r, ex) -> "LetRef(" ^ r ^ ", " ^ exp_to_abstract_string ex ^ ")"
  | Deref r -> "Deref(" ^ r ^ ")" ;;
