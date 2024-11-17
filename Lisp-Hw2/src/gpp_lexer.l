%{
#include <stdio.h>
#include <string.h>

void yyerror(const char *s);
int yycolumn = 1; // Column number
%}

%option noyywrap

VALUEI [0-9]+
VALUEF {VALUEI}:{VALUEI}
IDENTIFIER [a-zA-Z_][a-zA-Z0-9_]*
COMMENT ;;.*

KW_AND ([aA][nN][dD])
KW_OR ([oO][rR])
KW_NOT ([nN][oO][tT])
KW_EQUAL ([eE][qQ][uU][aA][lL])
KW_LESS ([lL][eE][sS][sS])
KW_NIL ([nN][iI][lL])
KW_LIST ([lL][iI][sS][tT])
KW_APPEND ([aA][pP][pP][eE][nN][dD])
KW_CONCAT ([cC][oO][nN][cC][aA][tT])
KW_SET ([sS][eE][tT])
KW_DEFFUN ([dD][eE][fF][fF][uU][nN])
KW_FOR ([fF][oO][rR])
KW_IF ([iI][fF])
KW_EXIT ([eE][xX][iI][tT])
KW_LOAD ([lL][oO][aA][dD])
KW_DISP ([pP][rR][iI][nN][tT])
KW_TRUE ([tT][rR][uU][eE])
KW_FALSE ([fF][aA][lL][sS][eE])

%%

{KW_AND}       { printf("KW_AND\n"); }
{KW_OR}        { printf("KW_OR\n"); }
{KW_NOT}       { printf("KW_NOT\n"); }
{KW_EQUAL}     { printf("KW_EQUAL\n"); }
{KW_LESS}      { printf("KW_LESS\n"); }
{KW_NIL}       { printf("KW_NIL\n"); }
{KW_LIST}      { printf("KW_LIST\n"); }
{KW_APPEND}    { printf("KW_APPEND\n"); }
{KW_CONCAT}    { printf("KW_CONCAT\n"); }
{KW_SET}       { printf("KW_SET\n"); }
{KW_DEFFUN}    { printf("KW_DEFFUN\n"); }
{KW_FOR}       { printf("KW_FOR\n"); }
{KW_IF}        { printf("KW_IF\n"); }
{KW_EXIT}      { printf("KW_EXIT\n"); }
{KW_LOAD}      { printf("KW_LOAD\n"); }
{KW_DISP}      { printf("KW_DISP\n"); }
{KW_TRUE}      { printf("KW_TRUE\n"); }
{KW_FALSE}     { printf("KW_FALSE\n"); }

"+"            { printf("OP_PLUS\n"); }
"-"            { printf("OP_MINUS\n"); }
"/"            { printf("OP_DIV\n"); }
"*"            { printf("OP_MULT\n"); }
"("            { printf("OP_OP\n"); }
")"            { printf("OP_CP\n"); }
","            { printf("OP_COMMA\n"); }

{VALUEF}       { printf("VALUEF\n"); }
{VALUEI}       { printf("VALUEI\n"); }

{IDENTIFIER}   { printf("IDENTIFIER\n"); }

{COMMENT}      { printf("COMMENT\n"); }

[ \t\n]+       { yycolumn += yyleng; }  // Ignore whitespace and update column

[^\n\t\(\)\+\-\/\*\, ]+ { yyerror("SYNTAX_ERROR"); }  // Invalid token

%%

void yyerror(const char *s) {
    fprintf(stderr, "Error: %s at line %d, column %d: %s\n", s, yylineno, yycolumn, yytext);
}

int main(int argc, char **argv) {
    if (argc > 1) {
        FILE *file = fopen(argv[1], "r");
        if (!file) {
            perror(argv[1]);
            return 1;
        }
        yyin = file;
    }
    yylex();
    return 0;
}