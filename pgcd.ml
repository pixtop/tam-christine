let tam_pgcd =
"__pgcd__ ; Fonction pour reduire rat
LOAD (1) -1[LB]
LOAD (1) -2[LB]
SUBR ILss
JUMPIF (1) __no_reverse__
LOAD (1) -1[LB]
LOAD (1) -2[LB]
JUMP __while_pgcd__
__no_reverse__
LOAD (1) -2[LB]
LOAD (1) -1[LB]
__while_pgcd__
LOAD (1) 4[LB]
JUMPIF (0) __fin_pgcd__
LOAD (1) 3[LB]
LOAD (1) 4[LB]
SUBR IMod
LOAD (1) 4[LB]
STORE (1) 3[LB]
STORE (1) 4[LB]
JUMP __while_pgcd__
__fin_pgcd__
POP (0) 1
LOAD (1) -2[LB]
LOAD (1) 3[LB]
SUBR IDiv
LOAD (1) -1[LB]
LOAD (1) 3[LB]
SUBR IDiv
POP (2) 1
RETURN (2) 2
";;
