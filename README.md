# 🧠 Mini-Interpretador Imperativo com Tipagem Estática (OCaml)
Este projeto implementa um interpretador small-step para uma linguagem imperativa simplificada, inspirado em linguagens como ML e C. Ele suporta variáveis, referências (ref), laços (while), leitura/escrita de memória, operações aritméticas, relacionais e booleanas — tudo com um sistema de tipagem estática.

> Objetivo: Explorar conceitos fundamentais de linguagens de programação e sistemas de tipos, como substituição, ambientes tipados (contextos), memória mutável e avaliação passo a passo (small-step operational semantics).

# ✨ Funcionalidades
✅ Sistema de tipos com verificação estática (antes da execução)
✅ Tipos: int, bool, unit, ref tipo
✅ Expressões aritméticas, booleanas e relacionais
✅ Comandos imperativos: let, :=, while, seq, print, read
✅ Memória mutável simulada como lista
✅ Avaliação com função step e execução total com steps
✅ Casos de teste automatizados para inferência de tipo e semântica operacional

# 🔧Exemplo
O trecho abaixo define uma simples função fatorial na nossa linguagem :

```ocaml
let seq = Seq(whi, prt)

let fat = Let("x", TyInt, Read, 
              Let("z", TyRef TyInt, New (Id "x"), 
                  Let("y", TyRef TyInt, New (Num 1),
                      seq)))
```
Que está funcionando OK, de acordo com os testes em tests.ml:
```ocaml
  (* 3! = 6, 4! = 24, 5! = 120 *)
  check_expect (call_expr fat [3]) (Some [6]) "sem1";;
  check_expect (call_expr fat [4]) (Some [24]) "sem1";;
  check_expect (call_expr fat [5]) (Some [120]) "sem1";;
```

# 🔄 For
O nosso For é um açúcar sintático para Let + While + Seq + Asg

- Foi adicionado na função step uma tradução do for para essas estruturas presentes na especificação original;
- Na função subs foi adicionado um caso específico novo;
- Foi adicionado o For nas expr, bem no começo do código

Este trecho deve imprimir 0;1;2;3;4;5:
```ocaml
let for_example =
  For("i", Num 0, Num 5, Num 1,
    Print(Deref(Id "i"))
  )
```
```ocaml
check_expect (call_expr for_example []) (Some [0; 1; 2; 3; 4; 5]) "sem-for-print";;
```

```ocaml
let for_sum_example =
  Let("acc", TyRef TyInt, New(Num 0),
    Seq(
      For("i", Num 1, Num 5, Num 1,
        Asg(Id "acc", Binop(Sum, Deref(Id "acc"), Deref(Id "i")))
      ),
      Print(Deref(Id "acc"))
    )
  )
```
```ocaml
check_expect (call_expr for_sum_example []) (Some [15]) "sem-for-sum";;
```

# 🔛 Rodando o código

```bash
$ ocamlc -o run L2.ml tests.ml
$ ./run
```


