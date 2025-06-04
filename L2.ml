type bop =  
  | Sum | Sub | Mul | Div   (* operações aritméticas *)
  | Eq  | Neq | Lt | Gt   (* operações relacionais  *)
  | And | Or   (* operações lógicas *) 

type tipo = 
  | TyInt
  | TyBool
  | TyRef of tipo
  | TyUnit
  
(*Serve pra converter um tipo do tipo tipo em
uma string legível e facilitar testes*)
let rec string_of_tipo t =
  match t with
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyUnit -> "unit"
  | TyRef t' -> "ref " ^ string_of_tipo t'




(*O context é um ambiente que associa
variáveis a seus tipos, Γ (Gama) *)
type context = (string * tipo) list

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

let is_value (e: expr): bool =
  match e with
    | Num _ -> true
    | Bool _ -> true
    | Unit -> true
    | Loc _ -> true
    | _ -> false
  
      
let rec typeinfer (ctx: context) (e: expr) : tipo = 
  match e with
    | Num _ -> TyInt    (* (T-int)  *)
    | Bool _ -> TyBool  (* (T-bool) *)
    | Unit -> TyUnit    (* (T-unit) *)
    
    
    | Id x -> (* (T-var) *)
      (*List.assoc is a standard function in OCaml, List module*)
      (try List.assoc x ctx 
        with Not_found -> failwith ("Variável "^ x ^ " não declarada"))

    | Binop (op, e1, e2) -> (* (T-op+) e (T-op<) *)
      let tipo_e1 = typeinfer ctx e1 in
      let tipo_e2 = typeinfer ctx e2 in
      (match op, tipo_e1, tipo_e2 with
        | (Sum | Sub | Mul | Div), TyInt, TyInt -> TyInt       (* Seria a (T-op+) do pdf mas aqui incluo todas ARITMÉTICAS(tem que resolver a exceção do div???) *)
        | (Eq | Neq | Lt | Gt), TyInt, TyInt -> TyBool         (*Seria a (T-op<) mas aqui incluo todas RELACIONAIS *)
        | (And | Or), TyBool, TyBool -> TyBool                 (*Não tem uma regra na especificação pras LÓGICAS *)
        | _ -> failwith "Erro de tipo na operação binária")      

    | If (e1, e2, e3) -> (* (T-if) *)
      let tipo_e1 = typeinfer ctx e1 in 
        if tipo_e1 <> TyBool then failwith "Condição do if deve ser bool";
      let tipo_e2 = typeinfer ctx e2 in 
      let tipo_e3 = typeinfer ctx e3 in 
      
      if tipo_e2 = tipo_e3 then tipo_e2 else failwith "Os dois ramos precisam ser do mesmo tipo";

    | Let (x, tipo_x, e1, e2) -> (* (T-let)*)
      let tipo_e1 = typeinfer ctx e1 in
      if tipo_e1 <> tipo_x then
        failwith ("Let: tipo declarado "^ string_of_tipo tipo_x ^
                " não bate com tipo inferido " ^ string_of_tipo tipo_e1)
      else
        let ctx' = (x, tipo_x) :: ctx in 
        typeinfer ctx' e2
    | New (e1) ->   (* (T-new)*)
        let tipo_e1  = typeinfer ctx e1 in 
        TyRef tipo_e1

    | Deref (e1) ->   (* (T-deref) *)
        let tipo_e1 = typeinfer ctx e1 in
        (match tipo_e1 with
          | TyRef tipo_referenciado -> tipo_referenciado
          | _ -> failwith "Erro: espera uma referência (ref T)"
        )
    
    | Asg (e1, e2) ->   (* (T-atr) *)
        let tipo_e1 = typeinfer ctx e1 in
        let tipo_e2 = typeinfer ctx e2 in
        (match tipo_e1 with
          | TyRef tipo_referenciado ->
              if tipo_referenciado = tipo_e2 then TyUnit
              else failwith "Tipo incompatível: valor não corresponde ao tipo da referência"
          | _ -> failwith "Erro: tentativa de atribuir a algo que não é referência")

    | Wh (e1, e2) ->    (* (T-while) *)
        let tipo_e1 = typeinfer ctx e1 in
        let tipo_e2 = typeinfer ctx e2 in
        if (tipo_e1 <> TyBool) then failwith "condição deve ser do tipo TyBool"
          else
            if (tipo_e2 <> TyUnit) then failwith "o corpo do while deve ser Unit "
            else TyUnit
    
    | Seq (e1, e2) ->  (* (T-seq) *)
        let tipo_e1 = typeinfer ctx e1 in
        if tipo_e1 <> TyUnit then failwith "O primeiro termo da sequência deve ser unit"
        else typeinfer ctx e2

    | Read -> TyInt  (* (T-read) *)
    
    | Print (e1) ->   (* (T-print) *)
        let tipo_e1 = typeinfer ctx e1 in
        if tipo_e1 <> TyInt then failwith "termo a imprimir deve ser int"
        else TyUnit
    | Loc (_) -> failwith "Localizão sem tipo"

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
      | Some(new_op2, new_mem, new_input, new_output) -> Some(Binop (op, op1, new_op2), new_mem, new_input, new_output)
      | None -> None)

    | Binop (op, op1, op2) -> (match step op2 mem input output with (* op1 *)
      | Some(new_op2, new_mem, new_input, new_output) -> Some(Binop (op, op1, new_op2), new_mem, new_input, new_output)
      | None -> None)
      
    | If (Bool true, e2, e3) -> Some(e2, mem, input, output) (* if1 *)

    | If (Bool false, e2, e3) -> Some(e3, mem, input, output) (* if2 *)

    | If (e1, e2, e3) -> (match step e1 mem input output with (* if3 *)
      | Some(new_e1, new_mem, new_input, new_output) -> Some(If (new_e1, e2, e3), new_mem, new_input, new_output)
      | None -> None)

    (* e-let2 *)
    
    | Let (id, tipo, e1, e2) -> (match step e1 mem input output with (* e-let1 *)
        | Some(new_e1, new_mem, new_input, new_output) -> Some(Let (id, tipo, new_e1, e2), new_mem, new_input, new_output)
        | None -> None)

    (* atr1 *)

    | Asg (Loc index, e2) -> (match step e2 mem input output with (* atr2 *)
      | Some(new_e2, new_mem, new_input, new_output) -> Some(Asg (Loc index, new_e2), new_mem, new_input, new_output)
      | None -> None)

    | Asg (e1, e2) -> (match step e1 mem input output with (* atr *)
      | Some(new_e1, new_mem, new_input, new_output) -> Some(Asg (new_e1, e2), new_mem, new_input, new_output)
      | None -> None)

    (* deref1 *)

    | Deref (e) -> (match step e mem input output with (* deref *)
      | Some(new_e, new_mem, new_input, new_output) -> Some(Deref (new_e), new_mem, new_input, new_output)
      | None -> None)

    (* new1 *)

    | New (e) -> (match step e mem input output with (* new *)
      | Some(new_e, new_mem, new_input, new_output) -> Some(New (new_e), new_mem, new_input, new_output)
      | None -> None)

    | Seq (Unit, e2) -> Some(e2, mem, input, output) (* seq1 *)

    | Seq (e1, e2) -> (match step e1 mem input output with (* seq *)
      | Some(new_e1, new_mem, new_input, new_output) -> Some(Seq(new_e1, e2), new_mem, new_input, new_output)
      | None -> None)

    | Wh (e1, e2) -> Some(If (e1, Seq (e2, Wh (e1, e2)), Unit), mem, input, output) (* e-while *)
    
    | Print (Num int) -> Some(Unit, mem, input, output @ [int]) (* print-n *)

    | Print (e) -> (match step e mem input output with (* print *)
      | Some(new_e, new_mem, new_input, new_output) -> Some(Print(new_e), new_mem, new_input, new_output)
      | None -> None)

    (* read *)

    | _ -> None

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

