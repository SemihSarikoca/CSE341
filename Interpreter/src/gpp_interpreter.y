%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void yyerror(const char *s);
int yylex(void);
extern FILE *yyin;  // Declare yyin

typedef union {
    float fval;
    char *sval;
} YYSTYPE;

#define YYSTYPE YYSTYPE

typedef struct {
    char *name;
    float value;
} symbol_t;

symbol_t symbol_table[100];
int symbol_count = 0;

float lookup_symbol(char *name) {
    for (int i = 0; i < symbol_count; i++) {
        if (strcmp(symbol_table[i].name, name) == 0) {
            return symbol_table[i].value;
        }
    }
    return 0.0;
}

void add_symbol(char *name, float value) {
    for (int i = 0; i < symbol_count; i++) {
        if (strcmp(symbol_table[i].name, name) == 0) {
            symbol_table[i].value = value;
            return;
        }
    }
    symbol_table[symbol_count].name = strdup(name);
    symbol_table[symbol_count].value = value;
    symbol_count++;
}

%}

%union {
    float fval;
    char *sval;
}

%token <fval> VALUEF
%token <sval> IDENTIFIER
%token OP_PLUS OP_MINUS OP_MULT OP_DIV OP_OP OP_CP OP_COMMA
%token KW_SET KW_DEFFUN KW_IF KW_WHILE KW_FOR KW_LIST KW_EXIT KW_APPEND KW_CONCAT KW_DISP KW_LOAD
%token KW_TRUE KW_FALSE
%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL

%type <fval> EXP EXPLIST ARITHMETIC_EXP LOGICAL_EXP BOOLEAN 
%type <fval> FUNCTION_DEF FCALL
%type <fval> DISPLAY CONTROL_STATEMENT EXIT ASSIGNMENT

%left OP_PLUS OP_MINUS
%left OP_MULT OP_DIV

%start START

%%

START:
    INPUT
    ;

INPUT:
    EXPLIST
    | LOAD INPUT
    ;

EXPLIST:
    EXPLIST EXP { $$ = $2; }
    | EXP
    ;

EXP:
    IDENTIFIER { $$ = lookup_symbol($1); }
    | VALUEF
    | ARITHMETIC_EXP
    | BOOLEAN
    | LOGICAL_EXP
    | FCALL
    | FUNCTION_DEF
    | LIST { $$ = 0.0; }
    | ASSIGNMENT
    | CONTROL_STATEMENT
    | DISPLAY
    | EXIT
    ;

ARITHMETIC_EXP:
    OP_OP OP_PLUS EXP EXP OP_CP { $$ = $3 + $4; }
    | OP_OP OP_MINUS EXP EXP OP_CP { $$ = $3 - $4; }
    | OP_OP OP_MULT EXP EXP OP_CP { $$ = $3 * $4; }
    | OP_OP OP_DIV EXP EXP OP_CP { $$ = $3 / $4; }
    ;

LOGICAL_EXP:
    OP_OP KW_AND EXP EXP OP_CP { $$ = $3 && $4; }
    | OP_OP KW_OR EXP EXP OP_CP { $$ = $3 || $4; }
    | OP_OP KW_NOT EXP OP_CP { $$ = !$3; }
    | OP_OP KW_EQUAL EXP EXP OP_CP { $$ = $3 == $4; }
    | OP_OP KW_LESS EXP EXP OP_CP { $$ = $3 < $4; }
    ;

BOOLEAN:
    KW_TRUE { $$ = 1.0; }
    | KW_FALSE { $$ = 0.0; }
    ;

ASSIGNMENT:
    OP_OP KW_SET IDENTIFIER EXPLIST OP_CP { add_symbol($3, $4); $$ = $4; }
    ;

FCALL:
    OP_OP IDENTIFIER EXP OP_CP { $$ = 0.0; }
    ;

FUNCTION_DEF:
    OP_OP KW_DEFFUN IDENTIFIER OP_OP IDENTIFIER OP_CP EXP OP_CP { $$ = 0.0; }
    ;

CONTROL_STATEMENT:
    OP_OP KW_IF EXP EXP OP_CP { $$ = 0.0; }
    | OP_OP KW_IF EXP EXP EXP OP_CP { $$ = 0.0; }
    | OP_OP KW_WHILE OP_OP EXP OP_CP EXPLIST OP_CP { 
        while ($4) { 
            yyparse();
            $$ = $6;
        } 
    }
    | OP_OP KW_FOR OP_OP IDENTIFIER EXP EXP OP_CP EXPLIST OP_CP { 
        float result = 0;
        add_symbol($4, $5);
        for(float i = $5; i < $6; i++) {
            add_symbol($4, i);
            result = $8;
        }
        $$ = result;
    }
    ;

DISPLAY:
    OP_OP KW_DISP EXP OP_CP { printf("Display: %f\n", $3); $$ = $3;}
    ;

EXIT:
    OP_OP KW_EXIT OP_CP { exit(0); $$ = 0.0; }
    ;

LIST:
    KW_NIL
    | OP_OP KW_LIST OP_CP
    | OP_OP KW_LIST OP_OP VALUES OP_CP OP_CP
    | OP_OP KW_APPEND LIST EXP OP_CP
    | OP_OP KW_CONCAT LIST LIST OP_CP
    ;

VALUES:
    VALUES OP_COMMA VALUEF
    | VALUEF
    ;

LOAD:
    OP_OP KW_LOAD IDENTIFIER OP_CP
    ;

%%

void yyerror(const char *s) {
    fprintf(stderr, "Error: %s\n", s);
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
    yyparse();
    return 0;
}