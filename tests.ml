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
let check_expect_tipo ?(ctx=[]) expr expected =
  let inferred = typeinfer ctx expr in
    if inferred <> expected then
      failwith
        (Printf.sprintf "Erro: tipo é %s mas deveria ser %s"
          (string_of_tipo inferred) (string_of_tipo expected))
  (* não imprime nada se estiver ok *)

let check_expect (result: 'a) (expected: 'b)  id =
    if Stdlib.compare result expected != 0 then
      failwith
        (Printf.sprintf "Erro: teste %s falhou" id)
  (* não imprime nada se estiver ok *)

(* Testes do sistema de tipos *)
let () =
  (* Testes simples *)
  check_expect_tipo (Num 42) (Some TyInt);;
  check_expect_tipo (Bool true) (Some TyBool);;
  check_expect_tipo Unit (Some TyUnit);;
  
  (* Teste de variável (Id) com contexto *)
  check_expect_tipo ~ctx:[("x", TyInt); ("y", TyBool)] (Id "x") (Some TyInt);;
  check_expect_tipo ~ctx:[("x", TyInt); ("y", TyBool)] (Id "y") (Some TyBool);;
  
  (* Testes de Binop aritméticas *)
  check_expect_tipo (Binop (Sum, Num 1, Num 2)) (Some TyInt);;
  check_expect_tipo (Binop (Sub, Num 10, Num 3)) (Some TyInt);;

  (* Testes de Binop relacionais *)
  check_expect_tipo (Binop (Lt, Num 1, Num 2)) (Some TyBool);;
  check_expect_tipo (Binop (Eq, Num 1, Num 1)) (Some TyBool);;

  (* Testes de Binop lógicas *)
  check_expect_tipo (Binop (And, Bool true, Bool false)) (Some TyBool);;
  check_expect_tipo (Binop (Or, Bool false, Bool true)) (Some TyBool);;

  (* Testes de if *)
  check_expect_tipo (If (Bool true, Num 1, Num 2)) (Some TyInt);;
  check_expect_tipo (If (Bool false, Bool true, Bool false)) (Some TyBool);;

  (* Testes de let *)
  (* let x: int = 3 in x + 2 --> int*)
  check_expect_tipo (Let ("x", TyInt, Num 3, Binop (Sum, Id "x", Num 2))) (Some TyInt);;

  (* let b: bool = true in if b then 1 else 2 --> int *)
  check_expect_tipo (Let ("b", TyBool, Bool true, If (Id "b", Num 1, Num 2))) (Some TyInt);;

  (* let x: int = 1 in let y: int = 2 in x + y --> int  *) 
  check_expect_tipo (Let ("x", TyInt, Num 1,Let ("y", TyInt, Num 2,Binop (Sum, Id "x", Id "y")))) (Some TyInt);;

  (* Testes New *)
  check_expect_tipo (New (Num 42)) (Some (TyRef TyInt));;
  check_expect_tipo (New (Bool true)) (Some (TyRef TyBool));;

  (* Testes Deref *)
  check_expect_tipo (Deref (New (Num 42))) (Some TyInt);;
  check_expect_tipo (Deref(New (Bool true))) (Some TyBool);;

  (* Testes Asg *)
  check_expect_tipo (Asg (New (Num 1), Num 2)) (Some TyUnit);;
  check_expect_tipo (Asg (New (Bool true), Bool false)) (Some TyUnit);;

  (* Testes While *)
  check_expect_tipo (Wh (Bool true, Unit)) (Some TyUnit);;

  (* Teste seq *)
  check_expect_tipo (Seq (Unit, Num 5)) (Some TyInt);;
  check_expect_tipo (Seq (Asg (New (Num 10), Num 20),Seq (Unit,Bool true))) (Some TyBool);;

  (* Teste read *)
  check_expect_tipo Read (Some TyInt);;

  (* Teste print*)
  check_expect_tipo (Print (Num 42)) (Some TyUnit);;

  check_expect_tipo for_example (Some TyUnit);;
  check_expect_tipo for_sum_example (Some TyUnit);;

  (* TESTES DE ERRO *)

  (* condição não booleana *)
  (* 
  check_expect_tipo (If (Num 1, Num 2, Num 3)) (Some TyInt);;
  *)
  
  (* ramos diferentes *)
  (*   
  check_expect_tipo (If (Bool true, Num 1, Bool false)) TyInt;;
  *)
  
  (* tipos de atribuição e ref incompatíveis*)
  (*
  check_expect_tipo (Asg (New (Num 1), Bool true)) TyUnit;;  
  *)

  (* condicao do while nao é bool*)
  (*
  check_expect_tipo(Wh (Num 1, Unit)) TyUnit;;
  *)

  (* corpo não é unit *)
  (*
  check_expect_tipo (Wh (Bool true, Num 1)) TyUnit;;
  *)

  (* primeiro termo nao é unit*)
  (*
  check_expect_tipo (Seq (Bool true, Num 1)) TyInt;;
  *)

  (* tentar imprimir algo que não é int*)
  (*
  check_expect_tipo (Print (Bool true)) TyUnit;;
  *)

(* Testes da semântica small-step *)
let () =
  (* op* *)
  check_expect (step (Binop (Sum, Num 1, Num 2)) [] [] []) (Some ((Num 3), [], [], [])) "sem-op-sum";;
  check_expect (step (Binop (Sub, Num 1, Num 2)) [] [] []) (Some ((Num (-1)), [], [], [])) "sem-op-sub";;
  check_expect (step (Binop (Mul, Num 2, Num 3)) [] [] []) (Some ((Num 6), [], [], [])) "sem-op-mul";;

  check_expect (step (Binop (Eq, Num 1, Num 1)) [] [] []) (Some ((Bool true), [], [], [])) "sem-op-eq1";;
  check_expect (step (Binop (Eq, Num 1, Num 2)) [] [] []) (Some ((Bool false), [], [], [])) "sem-op-eq2";;
  check_expect (step (Binop (Eq, Num 2, Num 1)) [] [] []) (Some ((Bool false), [], [], [])) "sem-op-eq3";;

  check_expect (step (Binop (Neq, Num 1, Num 1)) [] [] []) (Some ((Bool false), [], [], [])) "sem-op-neq1";;
  check_expect (step (Binop (Neq, Num 1, Num 2)) [] [] []) (Some ((Bool true), [], [], [])) "sem-op-neq2";;
  check_expect (step (Binop (Neq, Num 2, Num 1)) [] [] []) (Some ((Bool true), [], [], [])) "sem-op-neq3";;

  check_expect (step (Binop (Lt, Num 1, Num 1)) [] [] []) (Some ((Bool false), [], [], [])) "sem-op-lt1";;
  check_expect (step (Binop (Lt, Num 1, Num 2)) [] [] []) (Some ((Bool true), [], [], [])) "sem-op-lt2";;
  check_expect (step (Binop (Lt, Num 2, Num 1)) [] [] []) (Some ((Bool false), [], [], [])) "sem-op-lt3";;

  check_expect (step (Binop (Gt, Num 1, Num 1)) [] [] []) (Some ((Bool false), [], [], [])) "sem-op-gt1";;
  check_expect (step (Binop (Gt, Num 1, Num 2)) [] [] []) (Some ((Bool false), [], [], [])) "sem-op-gt2";;
  check_expect (step (Binop (Gt, Num 2, Num 1)) [] [] []) (Some ((Bool true), [], [], [])) "sem-op-gt3";;

  check_expect (step (Binop (And, Bool false, Bool false)) [] [] []) (Some ((Bool false), [], [], [])) "sem-op-and1";;
  check_expect (step (Binop (And, Bool false, Bool true)) [] [] []) (Some ((Bool false), [], [], [])) "sem-op-and2";;
  check_expect (step (Binop (And, Bool true, Bool false)) [] [] []) (Some ((Bool false), [], [], [])) "sem-op-and3";;
  check_expect (step (Binop (And, Bool true, Bool true)) [] [] []) (Some ((Bool true), [], [], [])) "sem-op-and4";;

  check_expect (step (Binop (Or, Bool false, Bool false)) [] [] []) (Some ((Bool false), [], [], [])) "sem-op-or1";;
  check_expect (step (Binop (Or, Bool false, Bool true)) [] [] []) (Some ((Bool true), [], [], [])) "sem-op-or2";;
  check_expect (step (Binop (Or, Bool true, Bool false)) [] [] []) (Some ((Bool true), [], [], [])) "sem-op-or3";;
  check_expect (step (Binop (Or, Bool true, Bool true)) [] [] []) (Some ((Bool true), [], [], [])) "sem-op-or4";;

  (* op1 *)
  check_expect (step (Binop (Sum, (Binop (Sum, Num 2, Num 3)), (Binop (Sum, Num 4, Num 5)))) [] [] [])
    (Some ((Binop (Sum, (Num 5), (Binop (Sum, Num 4, Num 5)))), [], [], []))
    "sem-op1";;

  (* op2 *)
  check_expect (step (Binop (Sum, (Num 5), (Binop (Sum, Num 4, Num 5)))) [] [] [])
    (Some ((Binop (Sum, (Num 5), (Num 9))), [], [], []))
    "sem-op2";;

  (* if1 *)
  check_expect (step (If (Bool true, (Num 1), (Num 2))) [] [] [])
    (Some ((Num 1), [], [], [])) 
    "sem-if1";;

  (* if2 *)
  check_expect (step (If (Bool false, (Num 1), (Num 2))) [] [] [])
    (Some ((Num 2), [], [], [])) 
    "sem-if2";;

  (* if3 *)
  check_expect (step (If ((Binop (Lt, Num 1, Num 1)), (Num 1), (Num 2))) [] [] [])
    (Some ((If ((Bool false), (Num 1), (Num 2))), [], [], [])) 
    "sem-if3";; 

  (* e-let1 *)
  check_expect (step (Let ("x", TyInt, (Binop (Sum, Num 2, Num 3)), (Id "x"))) [] [] [])
    (Some ((Let ("x", TyInt, (Num 5), (Id "x"))), [], [], []))
    "sem-let1";;

  (* e-let2 *)
  check_expect (step (Let ("x", TyInt, (Num 5), (Id "x"))) [] [] [])
    (Some ((Num 5), [], [], []))
    "sem-let2";;

  (* atr1 *)
  check_expect (step (Asg (Loc 0, Num 5)) [Num 1] [] [])
    (Some (Unit, [Num 5], [], []))
    "sem-atr1";;

  (* atr2 *)
  check_expect (step (Asg (Loc 0, (Binop (Sum, Num 2, Num 3)))) [] [] [])
    (Some ((Asg (Loc 0, Num 5)), [], [], []))
    "sem-atr1";;

  (* atr *)
  check_expect (step (Asg ((If (Bool true, Loc 0, Loc 1)), (Binop (Sum, Num 2, Num 3)))) [] [] [])
    (Some ((Asg (Loc 0, (Binop (Sum, Num 2, Num 3)))), [], [], []))
    "sem-atr";;

  (* deref1 *)
  check_expect (step (Deref (Loc 0)) [Num 5] [] [])
    (Some ((Num 5), [Num 5], [], []))
    "sem-deref1";;

  (* deref *)
  check_expect (step (Deref (If (Bool true, Loc 0, Loc 1))) [Num 5] [] [])
    (Some ((Deref (Loc 0)), [Num 5], [], []))
    "sem-deref";;

  (* new1 *)
  check_expect (step (New (Num 5)) [] [] [])
    (Some ((Loc 0), [Num 5], [], []))
    "sem-new1'";;

  (* new *)
  check_expect (step (New (Binop (Sum, Num 2, Num 3))) [] [] [])
    (Some ((New (Num 5)), [], [], []))
    "sem-new'";;

  (* seq1 *)
  check_expect (step (Seq (Unit, (New (Binop (Sum, Num 2, Num 3))))) [] [] [])
    (Some ((New (Binop (Sum, Num 2, Num 3))), [], [], []))
    "sem-seq1'";; 

  (* seq *)
  check_expect (step (Seq ((Asg (Loc 0, Num 5)), (New (Binop (Sum, Num 2, Num 3))))) [Num 1] [] [])
    (Some ((Seq (Unit, (New (Binop (Sum, Num 2, Num 3))))), [Num 5], [], []))
    "sem-seq'";;

  (* e-while *)
  check_expect (step (Wh ((Binop (Lt, (Deref (Id "x")), (Deref (Id "y")))), (Asg ((Id "x"), (Binop (Mul, (Deref (Id "x")), (Num 2))))))) [] [] [])
    (Some ((If ((Binop (Lt, (Deref (Id "x")), (Deref (Id "y")))), (Seq ((Asg ((Id "x"), (Binop (Mul, (Deref (Id "x")), (Num 2))))), (Wh ((Binop (Lt, (Deref (Id "x")), (Deref (Id "y")))), (Asg ((Id "x"), (Binop (Mul, (Deref (Id "x")), (Num 2))))))))), Unit)), [], [], []))
    "sem-print-n'";; 

  (* print-n *)
  check_expect (step (Print (Num 5)) [] [] [])
    (Some (Unit, [], [], [5]))
    "sem-print-n'";;

  (* print *)
  check_expect (step (Print (Binop (Sum, Num 2, Num 3))) [] [] [])
    (Some ((Print (Num 5)), [], [], []))
    "sem-print-n'";;

  (* read *)
  check_expect (step (Read) [] [5] [])
    (Some ((Num 5), [], [], []))
    "sem-print-n'";;

  check_expect (step for_example [] [] [])
    (Some
     (Let ("i", TyRef TyInt, New (Num 0),
      Wh (Binop (Lt, Deref (Id "i"), Binop (Sum, Num 5, Num 1)),
        Seq (Print (Deref (Id "i")),
          Asg (Id "i", Binop (Sum, Deref (Id "i"), Num 1))))),
      [], [], []))
    "sem-for";;

  (* extra *)
  check_expect (call_expr fat [3]) (Some [6]) "sem-fat1";;
  check_expect (call_expr fat [4]) (Some [24]) "sem-fat1";;
  check_expect (call_expr fat [5]) (Some [120]) "sem-fat1";;

  check_expect (call_expr for_example []) (Some [0; 1; 2; 3; 4; 5]) "sem-for-print";;
  check_expect (call_expr for_sum_example []) (Some [15]) "sem-for-sum";;



  print_endline "Todos Testes OK";;
