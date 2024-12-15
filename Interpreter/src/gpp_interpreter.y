%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

// Forward declarations
int yylex(void);
void yyerror(const char *s);
extern FILE *yyin;

// Debug levels
#define DEBUG_NONE 0
#define DEBUG_BASIC 1
#define DEBUG_VERBOSE 2
int debug_level = DEBUG_BASIC;

#define MAX_INCLUDE_DEPTH 10

FILE* input_stack[MAX_INCLUDE_DEPTH];
int input_stack_top = -1;

// Node types
typedef enum {
    NODE_VALUE,
    NODE_IDENTIFIER,
    NODE_OPERATOR,
    NODE_CONTROL,
    NODE_FUNCTION,
    NODE_LIST
} NodeType;

// Node structure
typedef struct Node {
    NodeType type;
    char* name;
    float value;
    struct Node* left;
    struct Node* right;
    struct Node* next;  // For list nodes
} Node;

// Global variables
Node* root_node = NULL;

// Add value accessor functions
float get_node_value(Node* node) {
    if (!node) return 0.0;
    return node->value;
}
char* get_node_name(Node* node) {
    if (!node) return NULL;
    return node->name;
}
// Debug functions
void print_indent(int depth) {
    for(int i = 0; i < depth; i++) printf("  ");
}

void print_node(Node* node, int depth) {
    if (!node) return;
    
    print_indent(depth);
    switch(node->type) {
        case NODE_VALUE:
            printf("VALUE: %f\n", node->value);
            break;
        case NODE_IDENTIFIER:
            printf("ID: %s = %f\n", node->name, node->value);
            break;
        case NODE_OPERATOR:
            printf("OP: %s\n", node->name);
            break;
        case NODE_CONTROL:
            printf("CONTROL: %s\n", node->name);
            break;
        case NODE_FUNCTION:
            printf("FUNCTION: %s\n", node->name);
            break;
        case NODE_LIST:
            printf("LIST: %s\n", node->name);
            break;
    }
    
    if (node->left) {
        print_indent(depth);
        printf("LEFT:\n");
        print_node(node->left, depth + 1);
    }
    if (node->right) {
        print_indent(depth);
        printf("RIGHT:\n");
        print_node(node->right, depth + 1);
    }
    if (node->next) {
        print_indent(depth);
        printf("NEXT:\n");
        print_node(node->next, depth + 1);
    }
}

// Node creation helper
Node* create_node(NodeType type, const char* name, float value) {
    Node* node = malloc(sizeof(Node));
    node->type = type;
    node->name = name ? strdup(name) : NULL;
    node->value = value;
    node->left = node->right = node->next = NULL;
    if (debug_level >= DEBUG_VERBOSE) {
        printf("Created node: ");
        print_node(node, 0);
    }
    return node;
}

void debug_print(const char* msg, ...) {
    if (debug_level >= DEBUG_BASIC) {
        va_list args;
        va_start(args, msg);
        vprintf(msg, args);
        va_end(args);
    }
}

// Original symbol table code
typedef struct {
    char *name;
    float value;
    Node* node;  // Add reference to parse tree node
} symbol_t;

symbol_t symbol_table[100];
int symbol_count = 0;

float lookup_symbol(char *name) {
    debug_print("Looking up symbol: %s\n", name);
    for (int i = 0; i < symbol_count; i++) {
        if (strcmp(symbol_table[i].name, name) == 0) {
            debug_print("Found symbol %s = %f\n", name, symbol_table[i].value);
            return symbol_table[i].value;
        }
    }
    debug_print("Symbol not found: %s\n", name);
    return 0.0;
}

void add_symbol(char *name, float value) {
    debug_print("Adding/updating symbol: %s = %f\n", name, value);
    for (int i = 0; i < symbol_count; i++) {
        if (strcmp(symbol_table[i].name, name) == 0) {
            symbol_table[i].value = value;
            return;
        }
    }
    symbol_table[symbol_count].name = strdup(name);
    symbol_table[symbol_count].value = value;
    symbol_table[symbol_count].node = create_node(NODE_IDENTIFIER, name, value);
    symbol_count++;
}

Node* parse_file(FILE* file);
Node* load_file(const char* filename);

%}

// Modify union to include Node*
%union {
    float fval;
    char *sval;
    struct Node* node;
}

// Update type declarations
%token <fval> VALUEF
%token <sval> IDENTIFIER
%token OP_PLUS OP_MINUS OP_MULT OP_DIV OP_OP OP_CP OP_COMMA
%token KW_SET KW_DEFFUN KW_IF KW_WHILE KW_FOR KW_LIST KW_EXIT KW_APPEND KW_CONCAT KW_DISP KW_LOAD
%token KW_TRUE KW_FALSE
%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL

%type <node> EXP EXPLIST ARITHMETIC_EXP LOGICAL_EXP BOOLEAN FUNCTION_DEF FCALL
%type <node> DISPLAY CONTROL_STATEMENT EXIT ASSIGNMENT LIST VALUES LOAD INPUT START

%%
START:
    INPUT { 
        if (!root_node) {
            root_node = create_node(NODE_LIST, "PROGRAM", 0);
        }
        
        // Add new input to root node
        if ($1) {
            if (root_node->left == NULL) {
                root_node->left = $1;
            } else {
                Node* current = root_node->left;
                while (current->next != NULL) {
                    current = current->next;
                }
                current->next = $1;
            }
        }

        $$ = root_node;
    }
    ;

INPUT:
    EXPLIST { $$ = $1; }
    ;

EXPLIST:
    EXPLIST EXP { 
        $$ = create_node(NODE_LIST, "EXPLIST", 0);
        $$->left = $1;
        $$->right = $2;
    }
    | EXP { $$ = $1; }
    ;

EXP:
    IDENTIFIER { 
        $$ = create_node(NODE_IDENTIFIER, $1, lookup_symbol($1));
    }
    | VALUEF { 
        $$ = create_node(NODE_VALUE, NULL, $1);
    }
    | ARITHMETIC_EXP { $$ = $1; }
    | BOOLEAN { $$ = $1; }
    | LOGICAL_EXP { $$ = $1; }
    | FCALL { $$ = $1; }
    | FUNCTION_DEF { $$ = $1; }
    | LIST { $$ = $1; }
    | ASSIGNMENT { $$ = $1; }
    | CONTROL_STATEMENT { $$ = $1; }
    | DISPLAY { $$ = $1; }
    | EXIT { $$ = $1; }
    | LOAD { $$ = $1; }
    ;
    
ARITHMETIC_EXP:
    OP_OP OP_PLUS EXP EXP OP_CP { 
        $$ = create_node(NODE_OPERATOR, "OP_PLUS", get_node_value($3) + get_node_value($4));
        $$->left = $3;
        $$->right = $4;
    }
    | OP_OP OP_MINUS EXP EXP OP_CP {
        $$ = create_node(NODE_OPERATOR, "OP_MINUS", get_node_value($3) - get_node_value($4));
        $$->left = $3;
        $$->right = $4;
    }
    | OP_OP OP_MULT EXP EXP OP_CP {
        $$ = create_node(NODE_OPERATOR, "OP_MULT", get_node_value($3) * get_node_value($4));
        $$->left = $3;
        $$->right = $4;
    }
    | OP_OP OP_DIV EXP EXP OP_CP {
        $$ = create_node(NODE_OPERATOR, "OP_DIV", get_node_value($3) / get_node_value($4));
        $$->left = $3;
        $$->right = $4;
    }
    ;

LOGICAL_EXP:
    OP_OP KW_AND EXP EXP OP_CP {
        $$ = create_node(NODE_OPERATOR, "AND", get_node_value($3) && get_node_value($4));
        $$->left = $3;
        $$->right = $4;
    }
    | OP_OP KW_OR EXP EXP OP_CP {
        $$ = create_node(NODE_OPERATOR, "OR", get_node_value($3) || get_node_value($4));
        $$->left = $3;
        $$->right = $4;
    }
    | OP_OP KW_NOT EXP OP_CP {
        $$ = create_node(NODE_OPERATOR, "NOT", !get_node_value($3));
        $$->left = $3;
    }
    | OP_OP KW_EQUAL EXP EXP OP_CP {
        $$ = create_node(NODE_OPERATOR, "EQUAL", get_node_value($3) == get_node_value($4));
        $$->left = $3;
        $$->right = $4;
    }
    | OP_OP KW_LESS EXP EXP OP_CP {
        $$ = create_node(NODE_OPERATOR, "LESS", get_node_value($3) < get_node_value($4));
        $$->left = $3;
        $$->right = $4;
    }
    ;

BOOLEAN:
    KW_TRUE { $$ = create_node(NODE_VALUE, "TRUE", 1.0); }
    | KW_FALSE { $$ = create_node(NODE_VALUE, "FALSE", 0.0); }
    ;

ASSIGNMENT:
    OP_OP KW_SET IDENTIFIER EXPLIST OP_CP { 
        add_symbol($3, get_node_value($4));
        $$ = create_node(NODE_CONTROL, "SET", get_node_value($4));
        $$->left = create_node(NODE_IDENTIFIER, $3, get_node_value($4));
        $$->right = $4;
    }
    ;

FCALL:
    OP_OP IDENTIFIER EXP OP_CP {
        $$ = create_node(NODE_FUNCTION, $2, 0.0);
        $$->left = $3;
    }
    ;

FUNCTION_DEF:
    OP_OP KW_DEFFUN IDENTIFIER OP_OP IDENTIFIER OP_CP EXP OP_CP {
        $$ = create_node(NODE_FUNCTION, $3, 0.0);
        $$->left = create_node(NODE_IDENTIFIER, $5, 0.0);
        $$->right = $7;
    }
    ;

CONTROL_STATEMENT:
    OP_OP KW_IF EXP EXP OP_CP {
        $$ = create_node(NODE_CONTROL, "IF", 0.0);
        $$->left = $3;
        if ($3->value) {
            $$->right = $4;
        } else {
            $$->right = NULL;
        }
    }
    | OP_OP KW_IF EXP EXP EXP OP_CP {
        $$ = create_node(NODE_CONTROL, "IF-ELSE", 0.0);
        $$->left = $3;
        if ($3->value) {
            $$->right = $4;
        } else {
            $$->right = $5;
        }
    }
    | OP_OP KW_WHILE OP_OP EXP OP_CP EXPLIST OP_CP {
        $$ = create_node(NODE_CONTROL, "WHILE", 0.0);
        $$->left = $4;
        $$->right = $6;
    }
    | OP_OP KW_FOR OP_OP IDENTIFIER EXP EXP OP_CP EXPLIST OP_CP {
        $$ = create_node(NODE_CONTROL, "FOR", 0.0);
        $$->left = create_node(NODE_IDENTIFIER, $4, get_node_value($5));
        $$->right = $8;
        $$->next = create_node(NODE_VALUE, NULL, get_node_value($6));
    }
    ;

DISPLAY:
    OP_OP KW_DISP EXP OP_CP {
        $$ = create_node(NODE_CONTROL, "DISPLAY", get_node_value($3));
        $$->left = $3;
        printf("Display: %f\n", get_node_value($3));
    }
    ;

EXIT:
    OP_OP KW_EXIT OP_CP {
        $$ = create_node(NODE_CONTROL, "EXIT", 0.0);
    }
    ;

LIST:
    KW_NIL { $$ = create_node(NODE_LIST, "NIL", 0.0); }
    | OP_OP KW_LIST OP_CP { $$ = create_node(NODE_LIST, "EMPTY", 0.0); }
    | OP_OP KW_LIST OP_OP VALUES OP_CP OP_CP {
        $$ = create_node(NODE_LIST, "LIST", 0.0);
        $$->left = $4;
    }
    | OP_OP KW_APPEND LIST EXP OP_CP {
        $$ = create_node(NODE_LIST, "APPEND", 0.0);
        $$->left = $3;
        $$->right = $4;
    }
    | OP_OP KW_CONCAT LIST LIST OP_CP {
        $$ = create_node(NODE_LIST, "CONCAT", 0.0);
        $$->left = $3;
        $$->right = $4;
    }
    ;

VALUES:
    VALUES OP_COMMA VALUEF {
        $$ = create_node(NODE_LIST, "VALUES", $3);
        $$->next = $1;
    }
    | VALUEF {
        $$ = create_node(NODE_VALUE, NULL, $1);
    }
    ;

LOAD:
    OP_OP KW_LOAD IDENTIFIER OP_CP {
        $$ = create_node(NODE_CONTROL, "LOAD", 0.0);
        Node* loaded_tree = load_file($3);
        if (!loaded_tree) {
            YYERROR;
        }
        $$->left = loaded_tree;
    }
    ;

%%

void yyerror(const char *s) {
    fprintf(stderr, "Error: %s\n", s);
    print_node(root_node, 0);
}

int main(int argc, char **argv) {
    // Parse command line arguments
    for(int i = 1; i < argc; i++) {
        if(strcmp(argv[i], "-d0") == 0) {
            debug_level = DEBUG_NONE;
        } else if(strcmp(argv[i], "-d1") == 0) {
            debug_level = DEBUG_BASIC;
        } else if(strcmp(argv[i], "-d2") == 0) {
            debug_level = DEBUG_VERBOSE;
        } else {
            // Try to open as input file
            FILE *file = fopen(argv[i], "r");
            if (!file) {
                perror(argv[i]);
                return 1;
            }
            yyin = file;
        }
    }

    // If no file specified, read from stdin
    if (!yyin) {
        yyin = stdin;
    }

    printf("GPP Interpreter starting (debug level: %d)\n", debug_level);

    // Start parsing
    yyparse();
    printf("Parsing complete\n");
    print_node(root_node, 0);

    return 0;
}

Node* parse_file(FILE* file) {
    // Mevcut durumu kaydet
    FILE* old_yyin = yyin;
    Node* old_root = root_node;
    
    // Yeni parse işlemi için hazırlık
    yyin = file;
    root_node = NULL;
    
    // Dosyayı parse et
    yyparse();
    
    // Sonuçları kaydet
    Node* result = root_node;
    
    // Eski duruma geri dön
    root_node = old_root;
    yyin = old_yyin;
    
    return result;
}

Node* load_file(const char* filename) {
    FILE* new_file = fopen(filename, "r");
    if (!new_file) {
        yyerror("Could not open file for loading");
        return NULL;
    }

    if (input_stack_top >= MAX_INCLUDE_DEPTH - 1) {
        yyerror("Maximum include depth exceeded");
        fclose(new_file);
        return NULL;
    }

    // Dosyayı parse et ve sonucu al
    Node* loaded_tree = parse_file(new_file);
    fclose(new_file);
    
    return loaded_tree;
}