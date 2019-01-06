/* Imports. */

%{

open Type
open Ast.AstSyntax
%}


%token <int> ENTIER
%token <string> ID
%token RETURN
%token PV
%token AO
%token AF
%token PF
%token PO
%token EQUAL
%token CONST
%token PRINT
%token IF
%token ELSE
%token WHILE
%token BOOL
%token INT
%token RAT
%token CALL
%token CO
%token CF
%token SLASH
%token NUM
%token DENOM
%token TRUE
%token FALSE
%token PLUS
%token MULT
%token INF
%token NEW
%token NULL
%token AND
%token EOF

(* Type de l'attribut synthétisé des non-terminaux *)
%type <programme> prog
%type <instruction list> bloc
%type <fonction> fonc
%type <instruction list> is
%type <instruction> i
%type <typ> typ
%type <(typ*string) list> dp
%type <expression> e
%type <expression list> cp
%type <affectable> af

(* Type et définition de l'axiome *)
%start <Ast.AstSyntax.programme> main

%%

main : lfi = prog EOF     {lfi}

prog :
| lf = fonc  lfi = prog   {let (Programme (lf1,li))=lfi in (Programme (lf::lf1,li))}
| ID li = bloc            {Programme ([],li)}

fonc : t=typ n=ID PO p=dp PF AO li=is RETURN exp=e PV AF {Fonction(t,n,p,li,exp)}

bloc : AO li = is AF      {li}

is :
|                         {[]}
| i1=i li=is              {i1::li}

i :
| t=typ id=ID EQUAL e1=e PV          {Declaration (t,id,e1)}
| a=af EQUAL e1=e PV                {Affectation (a,e1)}
| CONST n=ID EQUAL e=ENTIER PV      {Constante (n,e)}
| PRINT e1=e PV                     {Affichage (e1)}
| IF exp=e li1=bloc ELSE li2=bloc   {Conditionnelle (exp,li1,li2)}
| WHILE exp=e li=bloc               {TantQue (exp,li)}

dp :
|                         {[]}
| t=typ n=ID lp=dp        {(t,n)::lp}

typ :
| BOOL    {Bool}
| INT     {Int}
| RAT     {Rat}
| t=typ MULT {Pt t}
| t=typ CO CF {Tab t}
| PO t=typ PF {t}

e :
| CALL n=ID PO lp=cp PF   {AppelFonction (n,lp)}
| CO e1=e SLASH e2=e CF   {Rationnel(e1,e2)}
| NUM e1=e                {Numerateur e1}
| DENOM e1=e              {Denominateur e1}
| TRUE                    {True}
| FALSE                   {False}
| e=ENTIER                {Entier e}
| PO e1=e PLUS e2=e PF    {Binaire (Plus,e1,e2)}
| PO e1=e MULT e2=e PF    {Binaire (Mult,e1,e2)}
| PO e1=e EQUAL e2=e PF   {Binaire (Equ,e1,e2)}
| PO e1=e INF e2=e PF     {Binaire (Inf,e1,e2)}
| a=af                    {Acces a}
| NULL                    {Vide}
| PO NEW t=typ PF         {Allocation t}
| PO NEW t=typ CO exp=e CF PF {Array_Allocation (t, exp)}
| AND n=ID                {Adresse n}

cp :
|               {[]}
| e1=e le=cp    {e1::le}

af :
| PO MULT a=af PF   {Valeur a}
| n=ID              {Ident n}
| PO a=af CO exp=e CF PF {Indice (a,exp)}
