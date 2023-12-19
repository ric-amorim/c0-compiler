{
module Parser where 
import Lexer
import AST
}

%name parser 
%tokentype {Token}
%error {parseError}

%token

num         {Tnum $$}
str         {Str $$}
var         {Tvar $$}
bol         {Bol $$}
int         {Tint}
bool        {Tbool}
string      {Tstring}
if          {Tif}
else        {Telse}
while       {Twhile}
for         {Tfor}
continue    {Tcontinue}
break       {Tbreak}
print_int   {Tprint_int}
print_str   {Tprint_str}
scan_int    {Tscan_int}
return      {Treturn}
'!'         {Tbang}
'&&'        {Tand}
'||'        {Tor}
'++'        {Tplusplus}
'--'        {Tminusminus}
'=='        {Tequal}
'='         {Tassign}
'<'         {Tless}
'>'         {Tgreater}
'<='        {TlessEq}
'>='        {TgreaterEq}
'!='        {Tnequal}
'+'         {Tplus}
'-'         {Tminus}
'*'         {Ttimes}
'/'         {Tdiv}
'%'         {Tmod}
'('         {Tlparen}
')'         {Trparen}
';'         {Tsemicolon}
','         {Tcomma}
'{'         {Tlbrace}
'}'         {Trbrace}

%right '='
%left '||'
%left '&&'
%left '==' '!='
%left '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/' '%'
%right '!' '++' '--'
%left '(' ')'

%%
Fns : Fn                                                   {[$1]}      
    | Fn Fns                                                {$1 : $2}

Fn : Type var '(' Param ')' '{' Stmt '}'                    {Func $1 $2 $4 $7}
   | Type var '(' Param ')' '{' '}'                         {Func $1 $2 $4 []}


Type : int                                                  {Int}
     | bool                                                 {Bool}
     | string                                               {String}

Param : Par                                                 {[$1]}
      | Par ',' Param                                      {$1 : $3}
      | {- empty -}                                          {[]}

Par : Type var                                              {($1,$2)}

   
Stmt : Stm                                                  {[$1]} 
     | Stm Stmt                                             {$1 : $2}


Stm  : Simple ';'                                           {Simple $1}
     | print_int '(' Exp ')' ';'                            {PrintInt $3}   
     | print_str '(' Exp ')' ';'                            {PrintStr $3}
     | while '(' Exp ')' Stm                                {While $3 $5}
     | for '('OptSimple ';' Exp ';'OptSimple ')' Stm       {For $3 $5 $7 $9}
     | if '(' Exp ')' Stm                                   {If $3 $5}  
     | if '(' Exp ')' Stm else Stm                         {IfElse $3 $5 $7} 
     | return ';'                                           {Ret Nothing}
     | return Exp ';'                                       {Ret (Just $2)}
     | '{' Stmt '}'                                         {Block $2}
     | '{' '}'                                              {Block []}
     | continue ';'                                         {Continue}
     | break ';'                                            {Break}

Simple : Exp                                                {Expression $1}
       | var '=' Exp                                        {Assign $1 $3}
       | var '++'                                           {Incr $1}
       | var '--'                                           {Decr $1}
       | Type var '=' Exp                                   {DefVar $1 $2 (Just $4)}
       | Type var                                           {DefVar $1 $2 Nothing}

OptSimple : Simple                                          {[$1]}
          | {- empty -}                                     {[]}

ParamCall : Exp                                             {[$1]}
          | Exp ',' ParamCall                               {$1 : $3}
          |                                                 {[]}


Exp : num                                                   {Num $1}
    | str                                                   {StringVal $1}
    | var                                                   {Ident $1}
    | bol                                                   {BoolVal $1}
    | '(' Exp ')'                                           {$2}
    | var '(' ParamCall ')'                                 {FnCall $1 $3}     
    | scan_int '(' ')'                                      {ScanInt}
    | Exp '+' Exp                                           {Op Add $1 $3}
    | Exp '-' Exp                                           {Op Sub $1 $3}
    | Exp '*' Exp                                           {Op Mult $1 $3}
    | Exp '/' Exp                                           {Op Div $1 $3}
    | Exp '%' Exp                                           {Op Mod $1 $3}
    | Exp '&&' Exp                                          {Op And $1 $3}
    | Exp '||' Exp                                          {Op Or $1 $3}
    | '!' Exp                                               {Op1 Not $2}
    | '-' Exp                                               {Op1 Neg $2}
    | Exp '<' Exp                                           {Op Lt $1 $3}
    | Exp '>' Exp                                           {Op Gt $1 $3}
    | Exp '==' Exp                                          {Op Eq $1 $3}
    | Exp '<=' Exp                                          {Op Le $1 $3}
    | Exp '>=' Exp                                          {Op Ge $1 $3}
    | Exp '!=' Exp                                          {Op Ne $1 $3}


{
parseError :: [Token] -> a
parseError toks = error "parse error"
}


