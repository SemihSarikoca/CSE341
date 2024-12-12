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

typedef struct {
    char *name;       // Fonksiyon adı
    char *param;      // Fonksiyon parametresi
    float body;       // Fonksiyon gövdesi (şimdilik sabit bir değer)
} function_t;

function_t function_table[100];
int function_count = 0;

float lookup_symbol(char *name) {
    for (int i = 0; i < symbol_count; i++) {
        if (strcmp(symbol_table[i].name, name) == 0) {
            return symbol_table[i].value;
        }
    }
    yyerror("Undefined identifier");
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

void add_function(char *name, char *param, float body) {
    function_t *f = &function_table[function_count++];
    f->name = strdup(name);
    f->param = strdup(param);
    f->body = body;
}

function_t *lookup_function(char *name) {
    for (int i = 0; i < function_count; i++) {
        if (strcmp(function_table[i].name, name) == 0) {
            return &function_table[i];
        }
    }
    return NULL; // Tanımsız fonksiyon
}

%}

%union {
    float fval;
    char *sval;
}

%token <fval> VALUEF
%token <sval> IDENTIFIER
%token OP_PLUS OP_MINUS OP_MULT OP_DIV OP_OP OP_CP
%token KW_SET KW_DEFFUN KW_IF KW_WHILE KW_FOR
%token KW_TRUE KW_FALSE
%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS
%token KW_EXIT
%token KW_DISP

%type <fval> EXP EXPLIST ARITHMETIC_EXP LOGICAL_EXP BOOLEAN 
%type <fval> FUNCTION_DEF FCALL
%type <fval> DISPLAY CONTROL_STATEMENT QUIT ASSIGNMENT

%left OP_PLUS OP_MINUS
%left OP_MULT OP_DIV

%start START

%%

START:
    INPUT
    ;

INPUT:
    EXPLIST
    ;

EXPLIST:
    EXPLIST EXP
    | EXP
    ;

EXP:
    ARITHMETIC_EXP
    | IDENTIFIER { $$ = lookup_symbol($1); }
    | VALUEF
    | FCALL
    | LOGICAL_EXP
    | BOOLEAN
    | FUNCTION_DEF
    | DISPLAY
    | CONTROL_STATEMENT
    | ASSIGNMENT
    | QUIT
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
    OP_OP KW_SET IDENTIFIER EXP OP_CP { add_symbol($3, $4); $$ = $4; }
    ;

FCALL:
    OP_OP IDENTIFIER EXP OP_CP {
        function_t *f = lookup_function($2);
        if (!f) yyerror("Undefined function");
        $$ = f->body + $3; // Örnek olarak gövde ve argüman toplandı
    }
    ;

FUNCTION_DEF:
    OP_OP KW_DEFFUN IDENTIFIER OP_OP IDENTIFIER OP_CP EXP OP_CP {
        add_function($3, $5, $7); // Fonksiyonu tabloya ekle
    }
    ;

CONTROL_STATEMENT:
    OP_OP KW_IF EXP EXP OP_CP { if ($3) { $$ = $4; } }
    | OP_OP KW_WHILE OP_OP EXP OP_CP EXP OP_CP { while ($4) { $$ = $6; } }
    | OP_OP KW_FOR OP_OP IDENTIFIER EXP EXP OP_CP EXP OP_CP { add_symbol($4, $5); while (lookup_symbol($4) < $6) { $$ = $8; add_symbol($4, lookup_symbol($4) + 1.0); } }
    ;

DISPLAY:
    OP_OP KW_DISP EXP OP_CP { printf("Display: %f\n", $3); $$ = 1.0;}
    ;

QUIT:
    OP_OP KW_EXIT OP_CP { exit(0); $$ = 0.0; }
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