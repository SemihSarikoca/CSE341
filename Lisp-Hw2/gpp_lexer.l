%{
#include <stdio.h>
#include <string.h>

void yyerror(const char *s);
int yycolumn = 1; // Column number
%}

%option noyywrap

%%

"and"       { printf("KW_AND\n"); }
"or"        { printf("KW_OR\n"); }
"not"       { printf("KW_NOT\n"); }
"equal"     { printf("KW_EQUAL\n"); }
"less"      { printf("KW_LESS\n"); }
"nil"       { printf("KW_NIL\n"); }
"list"      { printf("KW_LIST\n"); }
"append"    { printf("KW_APPEND\n"); }
"concat"    { printf("KW_CONCAT\n"); }
"set"       { printf("KW_SET\n"); }
"deffun"    { printf("KW_DEFFUN\n"); }
"for"       { printf("KW_FOR\n"); }
"if"        { printf("KW_IF\n"); }
"exit"      { printf("KW_EXIT\n"); }
"load"      { printf("KW_LOAD\n"); }
"print"     { printf("KW_DISP\n"); }
"true"      { printf("KW_TRUE\n"); }
"false"     { printf("KW_FALSE\n"); }

"+"         { printf("OP_PLUS\n"); }
"-"         { printf("OP_MINUS\n"); }
"/"         { printf("OP_DIV\n"); }
"*"         { printf("OP_MULT\n"); }
"("         { printf("OP_OP\n"); }
")"         { printf("OP_CP\n"); }
","         { printf("OP_COMMA\n"); }

[0-9]+      { printf("VALUEI\n"); }
[0-9]+":"[0-9]+ { printf("VALUEF\n"); }

[a-zA-Z_][a-zA-Z0-9_]* { printf("IDENTIFIER\n"); }

";;".*      { printf("COMMENT\n"); }

[ \t\n]+    { yycolumn += yyleng; }  // Ignore whitespace and update column

.           { yyerror("SYNTAX_ERROR"); }

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