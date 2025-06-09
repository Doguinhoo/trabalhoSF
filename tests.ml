open L2

(*Serve pra converter um tipo do tipo tipo em
uma string legível e facilitar testes*)
let rec string_of_tipo t =
  match t with
  | Some(TyInt) -> "int"
  | Some(TyBool) -> "bool"
  | Some(TyUnit) -> "unit"
  | Some(TyRef t') -> "ref " ^ string_of_tipo (Some(t'))
  | None -> "mal-tipado"

(*Função para testar e imprimir*)
let test_infer e = 
  let t = typeinfer [] e in
  print_endline (string_of_tipo t) 

(* Resolvi comentar esse trecho e deixar só o check_expect
      (* Testes simples *)
      let () =
        test_infer (Num 42);      (* deve imprimir: int *)
        test_infer (Bool true);   (* deve imprimir: bool *)
        test_infer Unit           (* deve imprimir: unit *)
*)

(*Função para checar e imprimir só se o teste falhar*)
(* ? é argumento opcional e nomeado *)
let check_expect ?(ctx=[]) expr expected =
  let inferred = typeinfer ctx expr in
    if inferred <> expected then
      failwith
        (Printf.sprintf "Erro: tipo é %s mas deveria ser %s"
          (string_of_tipo inferred) (string_of_tipo expected))
  (* não imprime nada se estiver ok *)

let () =
  (* Testes simples *)
  check_expect (Num 42) (Some TyInt);;
  check_expect (Bool true) (Some TyBool);;
  check_expect Unit (Some TyUnit);;


  
  (* Teste de variável (Id) com contexto *)
  check_expect ~ctx:[("x", TyInt); ("y", TyBool)] (Id "x") (Some TyInt);;
  check_expect ~ctx:[("x", TyInt); ("y", TyBool)] (Id "y") (Some TyBool);;

  
  (* Testes de Binop aritméticas *)
  check_expect (Binop (Sum, Num 1, Num 2)) (Some TyInt);;
  check_expect (Binop (Sub, Num 10, Num 3)) (Some TyInt);;

  (* Testes de Binop relacionais *)
  check_expect (Binop (Lt, Num 1, Num 2)) (Some TyBool);;
  check_expect (Binop (Eq, Num 1, Num 1)) (Some TyBool);;

  (* Testes de Binop lógicas *)
  check_expect (Binop (And, Bool true, Bool false)) (Some TyBool);;
  check_expect (Binop (Or, Bool false, Bool true)) (Some TyBool);;

  (* Testes de if *)
  check_expect (If (Bool true, Num 1, Num 2)) (Some TyInt);;
  check_expect (If (Bool false, Bool true, Bool false)) (Some TyBool);;

  (* Testes de let *)
  (* let x: int = 3 in x + 2 --> int*)
  check_expect (Let ("x", TyInt, Num 3, Binop (Sum, Id "x", Num 2))) (Some TyInt);;

  (* let b: bool = true in if b then 1 else 2 --> int *)
  check_expect (Let ("b", TyBool, Bool true, If (Id "b", Num 1, Num 2))) (Some TyInt);;

  (* let x: int = 1 in let y: int = 2 in x + y --> int  *) 
  check_expect (Let ("x", TyInt, Num 1,Let ("y", TyInt, Num 2,Binop (Sum, Id "x", Id "y")))) (Some TyInt);;

  (* Testes New *)
  check_expect (New (Num 42)) (Some (TyRef TyInt));;
  check_expect (New (Bool true)) (Some (TyRef TyBool));;


  (* Testes Deref *)
  check_expect (Deref (New (Num 42))) (Some TyInt);;
  check_expect (Deref(New (Bool true))) (Some TyBool);;

  (* Testes Asg *)
  check_expect (Asg (New (Num 1), Num 2)) (Some TyUnit);;
  check_expect (Asg (New (Bool true), Bool false)) (Some TyUnit);;

  (* Testes While *)
  check_expect (Wh (Bool true, Unit)) (Some TyUnit);;

  (* Teste seq *)
  check_expect (Seq (Unit, Num 5)) (Some TyInt);;
  check_expect (Seq (Asg (New (Num 10), Num 20),Seq (Unit,Bool true))) (Some TyBool);;

  (* Teste read *)
  check_expect Read (Some TyInt);;

  (* Teste print*)
  check_expect (Print (Num 42)) (Some TyUnit);;


  (* TESTES DE ERRO *)

  (* condição não booleana *)
  (* 
  check_expect (If (Num 1, Num 2, Num 3)) (Some TyInt);;
  *)
  
  (* ramos diferentes *)
  (*   
  check_expect (If (Bool true, Num 1, Bool false)) TyInt;;
  *)
  
  (* tipos de atribuição e ref incompatíveis*)
  (*
  check_expect (Asg (New (Num 1), Bool true)) TyUnit;;  
  *)

  (* condicao do while nao é bool*)
  (*
  check_expect(Wh (Num 1, Unit)) TyUnit;;
  *)

  (* corpo não é unit *)
  (*
  check_expect (Wh (Bool true, Num 1)) TyUnit;;
  *)

  (* primeiro termo nao é unit*)
  (*
  check_expect (Seq (Bool true, Num 1)) TyInt;;
  *)

  (* tentar imprimir algo que não é int*)
  (*
  check_expect (Print (Bool true)) TyUnit;;
  *)
  
  print_endline "Todos Testes OK";;
