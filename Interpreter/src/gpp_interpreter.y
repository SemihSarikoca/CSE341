%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

int yylex(void);
void yyerror(const char *s);
extern FILE *yyin;

// Debug levels
#define DEBUG_NONE 0
#define DEBUG_BASIC 1
#define DEBUG_VERBOSE 2
int debug_level = DEBUG_NONE;

#define MAX_INCLUDE_DEPTH 10
#define MAX_SYMBOLS 100
#define MAX_CHILDREN 100

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
    struct Node* next[MAX_CHILDREN];  // For list nodes
    int next_count;
} Node;

// Symbol table structure
typedef struct {
    char *name;
    float value;
    Node* node;  // Reference to parse tree node
} symbol_t;

typedef struct ParseState {
    FILE* file;
    Node* root;
    int in_load;
    symbol_t symbols[MAX_SYMBOLS];  // Symbol table
    int symbol_count;       // Symbol count
} ParseState;

ParseState current_state = {
    .file = NULL,
    .root = NULL,
    .in_load = 0,
    .symbol_count = 0
};

// Add value accessor functions
float get_node_value(Node* node) {
    if (!node) return 0.0;
    return node->value;
}
char* get_node_name(Node* node) {
    if (!node) return NULL;
    return node->name;
}

void debug_print(const char* msg, ...);
Node* create_node(NodeType type, const char* name, float value);
float lookup_symbol(char *name);
void add_symbol(char *name, float value);
Node* load_file(const char* filename);
Node* parse_file(FILE* file);
void print_node(Node* node, int depth);


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

%type <node> EXP EXPLIST ARITHMETIC_EXP LOGICAL_EXP FUNCTION_DEF FCALL
%type <node> DISPLAY CONTROL_STATEMENT EXIT ASSIGNMENT LIST VALUES LOAD INPUT START

%%
START:
    INPUT { 
        if (!current_state.in_load) {
            if (!current_state.root) {
                current_state.root = create_node(NODE_LIST, "PROGRAM", 0);
            }
            if ($1) {
                if (!current_state.root->left) {
                    current_state.root->left = $1;
                } else {
                    Node* temp = current_state.root->left;
                    while (temp->right) temp = temp->right;
                    temp->right = $1;
                }
            }
            $$ = current_state.root;
        } else {
            $$ = $1;
        }
    }
    ;

INPUT:
    EXPLIST { $$ = $1; }
    | { $$ = NULL; }
    ;

EXPLIST:
    EXPLIST EXP { 
        $$ = create_node(NODE_LIST, "EXPLIST", get_node_value($2));
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
    KW_TRUE { $$ = create_node(NODE_VALUE, "TRUE", 1.0); }
    | KW_FALSE { $$ = create_node(NODE_VALUE, "FALSE", 0.0); }
    | OP_OP KW_AND EXP EXP OP_CP {
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

ASSIGNMENT:
    OP_OP KW_SET IDENTIFIER EXP OP_CP { 
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
    | OP_OP KW_WHILE OP_OP EXP OP_CP EXP OP_CP {
        $$ = create_node(NODE_CONTROL, "WHILE", 0.0);
        $$->left = $4;
        $$->next[0] = $6;
        $$->next_count = 1;
    }
    | OP_OP KW_FOR OP_OP IDENTIFIER EXP EXP OP_CP EXPLIST OP_CP {
        $$ = create_node(NODE_CONTROL, "FOR", 0.0);
        $$->left = $5;
        $$->right = $6;
        for (float val = get_node_value($5); val < get_node_value($6); val++) {
            $$->next[$$->next_count++] = $8;
        }
    }
    ;

DISPLAY:
    OP_OP KW_DISP EXPLIST OP_CP {
        $$ = create_node(NODE_CONTROL, "DISPLAY", get_node_value($3));
        $$->left = $3;
        printf("Display: %f\n", get_node_value($3));
    }
    ;

EXIT:
    OP_OP KW_EXIT OP_CP {
        $$ = create_node(NODE_CONTROL, "EXIT", -1);
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
    }
    | VALUEF {
        $$ = create_node(NODE_VALUE, NULL, $1);
    }
    ;

LOAD:
    OP_OP KW_LOAD IDENTIFIER OP_CP {
        $$ = create_node(NODE_CONTROL, "LOAD", -1);
        $$->left = create_node(NODE_IDENTIFIER, $3, -1);
        Node* loaded_tree = load_file($3);
        if (!loaded_tree) {
            YYERROR;
        }
        $$->right = loaded_tree;
        yyin = current_state.file;
        yyparse();

    }
    ;

%%

void yyerror(const char *s) {
    fprintf(stderr, "Error: %s\n", s);
}

int main(int argc, char **argv) {
    current_state.file = stdin;
    current_state.root = NULL;
    current_state.in_load = 0;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-d0") == 0) {
            debug_level = DEBUG_NONE;
        } else if (strcmp(argv[i], "-d1") == 0) {
            debug_level = DEBUG_BASIC;
        } else if (strcmp(argv[i], "-d2") == 0) {
            debug_level = DEBUG_VERBOSE;
        } else {
            FILE *file = fopen(argv[i], "r");
            if (!file) {
                perror(argv[i]);
                return 1;
            }
            yyin = file;
        }
    }

    if (!yyin) {
        yyin = stdin;
    }

    printf("GPP Interpreter starting (debug level: %d)\n", debug_level);

   
    int parse_result = yyparse();
    if (parse_result != 0) {
        fprintf(stderr, "Error parsing input\n");
    }

    print_node(current_state.root, 0);

    return 0;
}

void debug_print(const char* msg, ...) {
    if (debug_level >= DEBUG_BASIC) {
        va_list args;
        va_start(args, msg);
        vprintf(msg, args);
        va_end(args);
    }
}

Node* create_node(NodeType type, const char* name, float value) {
    Node* node = malloc(sizeof(Node));
    node->type = type;
    node->name = name ? strdup(name) : NULL;
    node->value = value;
    node->left = NULL;
    node->right = NULL;
    node->next_count = 0;  // Initialize the counter
    // No need to initialize next[]; it will be set when adding nodes
    return node;
}

float lookup_symbol(char *name) {
    for (int i = 0; i < current_state.symbol_count; i++) {
        if (strcmp(current_state.symbols[i].name, name) == 0) {
            debug_print("Found symbol %s = %f\n", name, current_state.symbols[i].value);
            return current_state.symbols[i].value;
        }
    }
    debug_print("Symbol not found: %s\n", name);
    return 0.0;
}

void add_symbol(char *name, float value) {
    for (int i = 0; i < current_state.symbol_count; i++) {
        if (strcmp(current_state.symbols[i].name, name) == 0) {
            current_state.symbols[i].value = value;
            return;
        }
    }
    current_state.symbols[current_state.symbol_count].name = strdup(name);
    current_state.symbols[current_state.symbol_count].value = value;
    current_state.symbols[current_state.symbol_count].node = create_node(NODE_IDENTIFIER, name, value);
    current_state.symbol_count++;
}

Node* parse_file(FILE* file) {
    // Save the current parsing state
    ParseState saved_state = current_state;
    FILE* saved_yyin = yyin;

    // Set up the new parsing state
    yyin = file;
    current_state.file = file;
    current_state.root = NULL;
    current_state.in_load = 0;

    // Parse the file
    int parse_result = yyparse();
    if (parse_result != 0) {
        fprintf(stderr, "Error parsing loaded file\n");
        // Restore the previous state
        current_state = saved_state;
        yyin = saved_yyin;
        return NULL;
    }

    // Collect the result
    Node* result = current_state.root;

    // Restore the previous parsing state
    current_state.file = saved_state.file;
    current_state.root = saved_state.root;
    current_state.in_load = saved_state.in_load;
    yyin = saved_yyin;

    return result;
}

Node* load_file(const char* filename) {
    FILE* new_file = fopen(filename, "r");
    if (!new_file) {
        fprintf(stderr, "Could not open file '%s' for loading\n", filename);
        return NULL;
    }

    // Parse the new file
    Node* loaded_tree = parse_file(new_file);
    fclose(new_file);

    if (!loaded_tree) {
        fprintf(stderr, "Failed to load file: %s\n", filename);
        return NULL;
    }

    return loaded_tree;
}

void print_node(Node* node, int depth) {
    if (!node) return;

    for (int i = 0; i < depth; i++) printf("  ");
    switch (node->type) {
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
    
    // Printing left and right children
    if (node->left) {
        for (int i = 0; i < depth; i++) printf("  ");
        printf("LEFT:\n");
        print_node(node->left, depth + 1);
    }
    if (node->right) {
        for (int i = 0; i < depth; i++) printf("  ");
        printf("RIGHT:\n");
        print_node(node->right, depth + 1);
    }

    // Printing next states
    if (node->next_count > 0) {
        for (int i = 0; i < depth; i++) printf("  ");
        printf("NEXT[%d]:\n", node->next_count);
        for (int i = 0; i < node->next_count; i++) {
            print_node(node->next[i], depth + 1);
        }
    }
}