{

module Lexer where
}

%wrapper "basic"
$alpha = [_a-zA-Z]
$digit = [0-9]

tokens :-
$white+                         ;
"/*"($white|[^\*]|\*[^\/])*"*/"                     ;
"//".*                                  ;       
"if"                            {\_ -> Tif}
"int"                           {\_ -> Tint}
"bool"                          {\_ -> Tbool}
"string"                        {\_ -> Tstring}
"else"                          {\_ -> Telse}
"return"                        {\_ -> Treturn}
"while"                         {\_ -> Twhile}
"for"                           {\_ -> Tfor}
"break"                         {\_ -> Tbreak}
"continue"                      {\_ -> Tcontinue}
"true"                          {\s -> Bol True}
"false"                         {\s -> Bol False}
"scan_int"                      {\_ -> Tscan_int}
"print_int"                     {\_ -> Tprint_int}
"print_str"                     {\_ -> Tprint_str}
[\"]($alpha|$digit|$white)*[\"] {\s -> Str s}
$alpha($alpha|$digit)*          {\s -> Tvar s}
$digit+                         {\s -> Tnum (read s)}
"+"                             {\_ -> Tplus}
"++"                            {\_ -> Tplusplus}
"-"                             {\_ -> Tminus}
"--"                            {\_ -> Tminusminus}
"*"                             {\_ -> Ttimes}
"/"                             {\_ -> Tdiv}
"%"                             {\_ -> Tmod}
"("                             {\_ -> Tlparen}
")"                             {\_ -> Trparen}
"{"                             {\_ -> Tlbrace}
"}"                             {\_ -> Trbrace}
"=="                            {\_ -> Tequal}
"="                             {\_ -> Tassign}
"!="                            {\_ -> Tnequal}
"<"                             {\_ -> Tless}
"<="                            {\_ -> TlessEq}
">"                             {\_ -> Tgreater}
">="                            {\_ -> TgreaterEq}
"!"                             {\_ -> Tbang}
"&&"                            {\_ -> Tand}
"||"                            {\_ -> Tor}
";"                             {\_ -> Tsemicolon}
","                             {\_ -> Tcomma}

{
data Token = Tif 
           | Telse
           | Treturn
           | Twhile
           | Tscan_int
           | Tprint_int
           | Tprint_str
           | Tvar String
           | Tnum Int 
           | Str String
           | Tplus
           | Tminus 
           | Ttimes 
           | Tdiv 
           | Tmod
           | Tlparen 
           | Trparen 
           | Tlbrace 
           | Trbrace 
           | Tequal
           | Tnequal
           | Tless 
           | TlessEq 
           | Tgreater
           | TgreaterEq
           | Tbang 
           | Tand 
           | Tor 
           | Tfor 
           | Tbreak
           | Tcontinue 
           | Tsemicolon
           | Tint
           | Tstring
           | Bol Bool
           | Tbool
           | Tplusplus
           | Tminusminus
           | Tassign
           | Tcomma
           deriving (Show,Eq)
}
