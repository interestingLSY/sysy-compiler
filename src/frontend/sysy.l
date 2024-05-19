%option noyywrap
%option nounput
%option noinput

%{

#include <cstdlib>
#include <string>
#include "sysy.tab.hpp"

using std::string;

%}

WhiteSpace    [ \t\n\r]*
LineComment   "//".*
BlockComment  \/\*([^*]|(\*+[^*/]))*\*+\/

Identifier    [a-zA-Z_][a-zA-Z0-9_]*

Decimal       [1-9][0-9]*
Octal         0[0-7]*
Hexadecimal   0[xX][0-9a-fA-F]+

%%

{WhiteSpace}    {}
{LineComment}   {}
{BlockComment}	{}

"const"			{ return CONST; }
"int"           { return INT; }
"void"			{ return VOID; }

"return"        { return RETURN; }
"if"			{ return IF; }
"else"			{ return ELSE; }
"while"			{ return WHILE; }
"break"			{ return BREAK; }
"continue"		{ return CONTINUE; }

"||"			{ return LOGICAL_OR; }
"&&"			{ return LOGICAL_AND; }
"=="			{ return EQ; }
"!="			{ return NEQ; }
"<="			{ return LEQ; }
">="			{ return GEQ; }

{Identifier}    { yylval.str_val = new string(yytext); return IDENT; }

{Decimal}       { yylval.int_val = strtol(yytext, nullptr, 0); return INT_CONST; }
{Octal}         { yylval.int_val = strtol(yytext, nullptr, 0); return INT_CONST; }
{Hexadecimal}   { yylval.int_val = strtol(yytext, nullptr, 0); return INT_CONST; }

.               { return yytext[0]; }

%%
