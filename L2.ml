type bop =  
  | Sum | Sub | Mul | Div   (* operações aritméticas *)
  | Eq  | Neq | Lt | Gt   (* operações relacionais  *)
  | And | Or   (* operações lógicas *)

type tipo = 
  | TyInt
  | TyBool
  | TyRef of tipo
  | TyUnit

type expr = 
  | Num of int
  | Bool of bool 
  | Id of string
  | If of expr * expr * expr 
  | Binop of bop * expr * expr
  | Wh of expr * expr 
  | Asg of expr * expr 
  | Let of string * tipo * expr * expr 
  | Loc of int
  | New of expr
  | Deref of expr 
  | Unit
  | Seq of expr * expr
  | Read
  | Print of expr
  | For of string * expr * expr * expr * expr
  (* For(var, start, end_, step, body) *)

let is_value (e: expr): bool =
  match e with
    | Num _ -> true
    | Bool _ -> true
    | Unit -> true
    | Loc _ -> true
    | _ -> false

(*O context é um ambiente que associa
variáveis a seus tipos, Γ (Gama) *)
type context = (string * tipo) list 
      
let rec typeinfer (ctx: context) (e: expr) : tipo option = 
  match e with
    | Num _ -> Some(TyInt)    (* T-int  *)
    | Bool _ -> Some(TyBool)  (* T-bool *)
    | Unit -> Some(TyUnit)    (* T-unit *)
    
    | Id x -> List.assoc_opt x ctx (* T-var *)

    | Binop (op, e1, e2) -> (match op, typeinfer ctx e1, typeinfer ctx e2 with (* T-op+ e T-op< *)
        | (Sum | Sub | Mul | Div), Some(TyInt), Some(TyInt) -> Some(TyInt)       (* Seria a T-op+ do pdf mas aqui incluo todas ARITMÉTICAS(tem que resolver a exceção do div???) *)
        | (Eq | Neq | Lt | Gt), Some(TyInt), Some(TyInt) -> Some(TyBool)         (*Seria a T-op< mas aqui incluo todas RELACIONAIS *)
        | (And | Or), Some(TyBool), Some(TyBool) -> Some(TyBool)                 (*Não tem uma regra na especificação pras LÓGICAS *)
        | _ -> None)

    | If (e1, e2, e3) -> (match typeinfer ctx e1, typeinfer ctx e2, typeinfer ctx e3 with (* T-if *)
        | (Some(TyBool), Some(tipo_e2), Some(tipo_e3)) when tipo_e2 == tipo_e3 -> Some(tipo_e2)
        | _ -> None)

    | Let (x, tipo_x, e1, e2) -> (match typeinfer ctx e1 with  (* T-let *)
        | Some(tipo_x) -> typeinfer ((x, tipo_x) :: ctx) e2
        | _ -> None)

    | New (e1) -> (match typeinfer ctx e1 with (* T-new *)
        | Some(tipo) -> Some(TyRef(tipo))
        | None -> None)

    | Deref (e1) -> (match typeinfer ctx e1 with (* T-deref *)
        | Some(TyRef(tipo)) -> Some(tipo)
        | _ -> None)
    
    | Asg (e1, e2) -> (match typeinfer ctx e1, typeinfer ctx e2 with (* T-atr *)
        | Some(TyRef(tipo_referenciado)), Some(tipo_e2) when tipo_e2 == tipo_referenciado -> Some(TyUnit)
        | _ -> None)

    | Wh (e1, e2) -> (match typeinfer ctx e1, typeinfer ctx e2 with (* T-while *)
        | Some(TyBool), Some(TyUnit) -> Some(TyUnit)
        | _ -> None)
    
    | Seq (e1, e2) -> (match typeinfer ctx e1 with (* (T-seq) *)
        | Some(TyUnit) -> typeinfer ctx e2
        | _ -> None)

    | Read -> Some(TyInt)  (* T-read *)
    
    | Print (e1) -> (match typeinfer ctx e1 with (* T-print *)
        | Some(TyInt) -> Some(TyUnit)
        | _ -> None)

    | For (var, start, end_, step, body) -> (match typeinfer ctx start, typeinfer ctx end_, typeinfer ctx step, typeinfer ((var, (TyRef TyInt)) :: ctx) body  with
      | (Some(TyInt), Some(TyInt), Some(TyInt), Some(TyUnit)) -> Some(TyUnit)
      | _ -> None)

    | _ -> None

let typeinfer_init (e: expr) =
  typeinfer [] e

let rec set_nth_opt (list: 'a list) (index: int) (value: 'a): ('a list) option =
  match (index, list) with
  | (0, old_value :: tail) -> Some(value :: tail)
  | (x, head :: tail) when x > 0 -> (match set_nth_opt tail (x - 1) value with
    | Some(new_tail) -> Some(head :: new_tail)
    | None -> None)
  | _ -> None

let rec subs (e1: expr) (x: string) (e2: expr): expr =
  match e2 with
  | Num (i) -> Num (i)
  | Bool (b) -> Bool (b)
  | Id (y) when String.equal x y -> e1
  | Id (y) -> Id (y)
  | If (le1, le2, le3) -> If (subs e1 x le1, subs e1 x le2, subs e1 x le3)
  | Binop (bop, le1, le2) -> Binop (bop, subs e1 x le1, subs e1 x le2)
  | Wh (le1, le2) -> Wh (subs e1 x le1, subs e1 x le2)
  | Asg (le1, le2) -> Asg (subs e1 x le1, subs e1 x le2)
  | Let (y, t, le1, le2) when not (String.equal x y) -> Let (y, t, subs e1 x le1, subs e1 x le2)
  | Let (y, t, le1, le2) -> Let (y, t, subs e1 x le1, le2)
  | Loc (i) -> Loc(i)
  | New (le) -> New (subs e1 x le)
  | Deref (le) -> Deref (subs e1 x le)
  | Unit -> Unit
  | Seq (le1, le2) -> Seq (subs e1 x le1, subs e1 x le2)
  | Read -> Read
  | Print (le) -> Print (subs e1 x le)
  | For (var, start, end_, step_expr, body) ->
    For (var,
         subs e1 x start,
         subs e1 x end_,
         subs e1 x step_expr,
         subs e1 x body)

let rec step (e: expr) (mem: expr list) (input: int list) (output: int list): (expr * expr list * int list * int list) option =
  match e with
    | Binop (op, op1, op2) when is_value op1 && is_value op2 -> (match (op, op1, op2) with (* op* *)
      | (Sum, Num int1, Num int2) -> Some(Num (int1 + int2), mem, input, output)
      | (Sub, Num int1, Num int2) -> Some(Num (int1 - int2), mem, input, output)
      | (Mul, Num int1, Num int2) -> Some(Num (int1 * int2), mem, input, output)
      (* | (Div, Num int1, Num int2) -> Some(Num (int1 / int2), mem, input, output) *)

      | (Eq, Num int1, Num int2) -> Some(Bool (int1 == int2), mem, input, output)
      | (Neq, Num int1, Num int2) -> Some(Bool (int1 != int2), mem, input, output)
      | (Lt, Num int1, Num int2) -> Some(Bool (int1 < int2), mem, input, output)
      | (Gt, Num int1, Num int2) -> Some(Bool (int1 > int2), mem, input, output)

      | (And, Bool bool1, Bool bool2) -> Some(Bool (bool1 && bool2), mem, input, output)
      | (Or, Bool bool1, Bool bool2) -> Some(Bool (bool1 || bool2), mem, input, output)

      | _ -> None)

    | Binop (op, op1, op2) when is_value op1 -> (match step op2 mem input output with (* op2 *)
      | Some(op2', mem', input', output') -> Some(Binop (op, op1, op2'), mem', input', output')
      | None -> None)

    | Binop (op, op1, op2) -> (match step op1 mem input output with (* op1 *)
      | Some(op1', mem', input', output') -> Some(Binop (op, op1', op2), mem', input', output')
      | None -> None)
      
    | If (Bool true, e2, e3) -> Some(e2, mem, input, output) (* if1 *)

    | If (Bool false, e2, e3) -> Some(e3, mem, input, output) (* if2 *)

    | If (e1, e2, e3) -> (match step e1 mem input output with (* if3 *)
      | Some(e1', mem', input', output') -> Some(If (e1', e2, e3), mem', input', output')
      | None -> None)

    | Let (x, t, e1, e2) when is_value e1 -> Some(subs e1 x e2, mem, input, output) (* e-let2 *)
    
    | Let (x, t, e1, e2) -> (match step e1 mem input output with (* e-let1 *)
        | Some(e1', mem', input', output') -> Some(Let (x, t, e1', e2), mem', input', output')
        | None -> None)

    | Asg (Loc index, e2) when is_value e2 -> (match set_nth_opt mem index e2 with (* atr1 *)
        | Some(mem') -> Some(Unit, mem', input, output)
        | None -> None) 

    | Asg (Loc index, e2) -> (match step e2 mem input output with (* atr2 *)
      | Some(e2', mem', input', output') -> Some(Asg (Loc index, e2'), mem', input', output')
      | None -> None)

    | Asg (e1, e2) -> (match step e1 mem input output with (* atr *)
      | Some(e1', mem', input', output') -> Some(Asg (e1', e2), mem', input', output')
      | None -> None)

    | Deref (Loc index) -> (match List.nth_opt mem index with (* deref1 *)
      | Some(value) -> Some(value, mem, input, output)
      | None -> None)

    | Deref (e) -> (match step e mem input output with (* deref *)
      | Some(e', mem', input', output') -> Some(Deref (e'), mem', input', output')
      | None -> None)

    | New (e) when is_value e -> Some(Loc (List.length mem), List.append mem [e], input, output) (* new1 *)

    | New (e) -> (match step e mem input output with (* new *)
      | Some(e', mem', input', output') -> Some(New (e'), mem', input', output')
      | None -> None)

    | Seq (Unit, e2) -> Some(e2, mem, input, output) (* seq1 *)

    | Seq (e1, e2) -> (match step e1 mem input output with (* seq *)
      | Some(e1', mem', input', output') -> Some(Seq(e1', e2), mem', input', output')
      | None -> None)

    | Wh (e1, e2) -> Some(If (e1, Seq (e2, Wh (e1, e2)), Unit), mem, input, output) (* e-while *)
    
    | Print (Num int) -> Some(Unit, mem, input, output @ [int]) (* print-n *)

    | Print (e) -> (match step e mem input output with (* print *)
      | Some(e', mem', input', output') -> Some(Print(e'), mem', input', output')
      | None -> None)

    | Read -> (match input with (* read *)
      | value :: tail -> Some(Num value, mem, tail, output)
      | _ -> None)

    | For (var, start, end_, step_expr, body) ->
      (* let var = ref start in while (!var <= end_) do body; var := !var + step done 
      ref_init representa uma tradução para expressões que já existiam na linguagem
      *)
      let ref_init = Let(var, TyRef TyInt, New(start),
          Wh(
            Binop(Lt, Deref(Id var), Binop(Sum, end_, Num 1)), (* inclui end_ *)
            Seq(
              body,
              Asg(Id var, Binop(Sum, Deref(Id var), step_expr))
            )
          )
        )
      in Some(ref_init, mem, input, output)


    | _ -> None

let rec steps (e: expr) (mem: expr list) (input: int list) (output: int list): (expr * expr list * int list * int list) =
  match step e mem input output with
  | Some(e', mem', input', output') -> steps e' mem' input' output'
  | None -> e, mem, input, output

let rec steps_list (e: expr) (mem: expr list) (input: int list) (output: int list): (expr * expr list * int list * int list) list =
  (e, mem, input, output) :: match step e mem input output with
  | Some(e', mem', input', output') ->  (steps_list e' mem' input' output')
  | None -> []

let call_expr (e: expr) (input: int list): (int list) option =
  let (e', final_mem, input', output) = steps e [] input [] in
    if e' == Unit then Some(output)
    else None

let cndwhi = Binop(Gt, Deref (Id "z"),Num 0)
let asgny = Asg(Id "y", Binop(Mul, Deref (Id "y"),Deref(Id "z")))
let asgnz = Asg(Id "z", Binop(Sub, Deref (Id "z"),Num 1))
let bdwhi = Seq(asgny, asgnz) 
let whi = Wh(cndwhi, bdwhi)
let prt = Print(Deref (Id "y"))
let seq = Seq(whi, prt)
    
let fat = Let("x", TyInt, Read, 
              Let("z", TyRef TyInt, New (Id "x"), 
                  Let("y", TyRef TyInt, New (Num 1),
                      seq)))
        

  
          (*         
           
            let  x: int     =  read() in 
            let  z: ref int = new x in 
            let  y: ref int = new 1 in 
            
            (while (!z > 0) (
                   y :=  !y * !z;
                   z :=  !z - 1);
            print (! y))     

*)    

let for_example =
  For("i", Num 0, Num 5, Num 1,
    Print(Deref(Id "i"))
  )

let for_sum_example =
  Let("acc", TyRef TyInt, New(Num 0),
    Seq(
      For("i", Num 1, Num 5, Num 1,
        Asg(Id "acc", Binop(Sum, Deref(Id "acc"), Deref(Id "i")))
      ),
      Print(Deref(Id "acc"))
    )
  )


