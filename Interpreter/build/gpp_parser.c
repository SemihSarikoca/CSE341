/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     VALUEF = 258,
     IDENTIFIER = 259,
     OP_PLUS = 260,
     OP_MINUS = 261,
     OP_MULT = 262,
     OP_DIV = 263,
     OP_OP = 264,
     OP_CP = 265,
     OP_COMMA = 266,
     KW_SET = 267,
     KW_DEFFUN = 268,
     KW_IF = 269,
     KW_WHILE = 270,
     KW_FOR = 271,
     KW_LIST = 272,
     KW_EXIT = 273,
     KW_APPEND = 274,
     KW_CONCAT = 275,
     KW_DISP = 276,
     KW_LOAD = 277,
     KW_TRUE = 278,
     KW_FALSE = 279,
     KW_AND = 280,
     KW_OR = 281,
     KW_NOT = 282,
     KW_EQUAL = 283,
     KW_LESS = 284,
     KW_NIL = 285
   };
#endif
/* Tokens.  */
#define VALUEF 258
#define IDENTIFIER 259
#define OP_PLUS 260
#define OP_MINUS 261
#define OP_MULT 262
#define OP_DIV 263
#define OP_OP 264
#define OP_CP 265
#define OP_COMMA 266
#define KW_SET 267
#define KW_DEFFUN 268
#define KW_IF 269
#define KW_WHILE 270
#define KW_FOR 271
#define KW_LIST 272
#define KW_EXIT 273
#define KW_APPEND 274
#define KW_CONCAT 275
#define KW_DISP 276
#define KW_LOAD 277
#define KW_TRUE 278
#define KW_FALSE 279
#define KW_AND 280
#define KW_OR 281
#define KW_NOT 282
#define KW_EQUAL 283
#define KW_LESS 284
#define KW_NIL 285




/* Copy the first part of user declarations.  */
#line 1 "src/gpp_interpreter.y"

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



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 167 "src/gpp_interpreter.y"
{
    float fval;
    char *sval;
    struct Node* node;
}
/* Line 193 of yacc.c.  */
#line 327 "build/gpp_parser.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 340 "build/gpp_parser.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  43
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   158

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  31
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  17
/* YYNRULES -- Number of rules.  */
#define YYNRULES  46
/* YYNRULES -- Number of states.  */
#define YYNSTATES  119

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   285

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     5,     7,    10,    12,    14,    16,    18,
      20,    22,    24,    26,    28,    30,    32,    34,    36,    38,
      44,    50,    56,    62,    68,    74,    79,    85,    91,    93,
      95,   101,   106,   115,   121,   128,   136,   146,   151,   155,
     157,   161,   168,   174,   180,   184,   186
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      32,     0,    -1,    33,    -1,    34,    -1,    34,    35,    -1,
      35,    -1,     4,    -1,     3,    -1,    36,    -1,    38,    -1,
      37,    -1,    40,    -1,    41,    -1,    45,    -1,    39,    -1,
      42,    -1,    43,    -1,    44,    -1,    47,    -1,     9,     5,
      35,    35,    10,    -1,     9,     6,    35,    35,    10,    -1,
       9,     7,    35,    35,    10,    -1,     9,     8,    35,    35,
      10,    -1,     9,    25,    35,    35,    10,    -1,     9,    26,
      35,    35,    10,    -1,     9,    27,    35,    10,    -1,     9,
      28,    35,    35,    10,    -1,     9,    29,    35,    35,    10,
      -1,    23,    -1,    24,    -1,     9,    12,     4,    34,    10,
      -1,     9,     4,    35,    10,    -1,     9,    13,     4,     9,
       4,    10,    35,    10,    -1,     9,    14,    35,    35,    10,
      -1,     9,    14,    35,    35,    35,    10,    -1,     9,    15,
       9,    35,    10,    34,    10,    -1,     9,    16,     9,     4,
      35,    35,    10,    34,    10,    -1,     9,    21,    35,    10,
      -1,     9,    18,    10,    -1,    30,    -1,     9,    17,    10,
      -1,     9,    17,     9,    46,    10,    10,    -1,     9,    19,
      45,    35,    10,    -1,     9,    20,    45,    45,    10,    -1,
      46,    11,     3,    -1,     3,    -1,     9,    22,     4,    10,
      -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   186,   186,   209,   213,   218,   222,   225,   228,   229,
     230,   231,   232,   233,   234,   235,   236,   237,   238,   242,
     247,   252,   257,   265,   270,   275,   279,   284,   292,   293,
     297,   306,   313,   321,   330,   339,   344,   353,   361,   367,
     368,   369,   373,   378,   386,   390,   396
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "VALUEF", "IDENTIFIER", "OP_PLUS",
  "OP_MINUS", "OP_MULT", "OP_DIV", "OP_OP", "OP_CP", "OP_COMMA", "KW_SET",
  "KW_DEFFUN", "KW_IF", "KW_WHILE", "KW_FOR", "KW_LIST", "KW_EXIT",
  "KW_APPEND", "KW_CONCAT", "KW_DISP", "KW_LOAD", "KW_TRUE", "KW_FALSE",
  "KW_AND", "KW_OR", "KW_NOT", "KW_EQUAL", "KW_LESS", "KW_NIL", "$accept",
  "START", "INPUT", "EXPLIST", "EXP", "ARITHMETIC_EXP", "LOGICAL_EXP",
  "BOOLEAN", "ASSIGNMENT", "FCALL", "FUNCTION_DEF", "CONTROL_STATEMENT",
  "DISPLAY", "EXIT", "LIST", "VALUES", "LOAD", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    31,    32,    33,    34,    34,    35,    35,    35,    35,
      35,    35,    35,    35,    35,    35,    35,    35,    35,    36,
      36,    36,    36,    37,    37,    37,    37,    37,    38,    38,
      39,    40,    41,    42,    42,    42,    42,    43,    44,    45,
      45,    45,    45,    45,    46,    46,    47
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     1,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     5,
       5,     5,     5,     5,     5,     4,     5,     5,     1,     1,
       5,     4,     8,     5,     6,     7,     9,     4,     3,     1,
       3,     6,     5,     5,     3,     1,     4
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     7,     6,     0,    28,    29,    39,     0,     2,     3,
       5,     8,    10,     9,    14,    11,    12,    15,    16,    17,
      13,    18,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     1,     4,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    40,    38,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    31,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    45,     0,
       0,     0,    37,    46,     0,     0,    25,     0,     0,    19,
      20,    21,    22,    30,     0,    33,     0,     0,     0,     0,
       0,    42,    43,    23,    24,    26,    27,     0,    34,     0,
       0,    41,    44,     0,    35,     0,    32,     0,    36
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    79,    21
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -49
static const yytype_int16 yypact[] =
{
     102,   -49,   -49,   129,   -49,   -49,   -49,     4,   -49,   102,
     -49,   -49,   -49,   -49,   -49,   -49,   -49,   -49,   -49,   -49,
     -49,   -49,   102,   102,   102,   102,   102,     3,    17,   102,
      14,    15,     1,     2,    -8,    -8,   102,    21,   102,   102,
     102,   102,   102,   -49,   -49,    16,   102,   102,   102,   102,
     102,    19,   102,   102,    31,    33,   -49,   -49,   -14,   102,
      -8,    32,    35,   102,   102,    36,   102,   102,   -49,    37,
      38,    41,    42,    67,    49,    69,    46,   102,   -49,     8,
      50,    51,   -49,   -49,    52,    53,   -49,    55,    59,   -49,
     -49,   -49,   -49,   -49,    72,   -49,    73,   102,   102,    74,
      56,   -49,   -49,   -49,   -49,   -49,   -49,   102,   -49,    71,
      75,   -49,   -49,    76,   -49,   102,   -49,   100,   -49
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -49,   -49,   -49,   -48,    -9,   -49,   -49,   -49,   -49,   -49,
     -49,   -49,   -49,   -49,   -26,   -49,   -49
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      44,    58,    73,    32,    43,    34,    35,    50,    59,    60,
      55,    56,    57,    45,    46,    47,    48,    49,    99,   100,
      52,    51,     6,    53,    54,    62,    68,    61,    74,    63,
      64,    65,    66,    67,    81,    77,    78,    69,    70,    71,
      72,     0,    82,    75,    76,    83,    86,    89,    90,   109,
      80,    91,    92,    94,    84,    85,    97,    87,    88,   112,
     101,   102,   103,   104,    44,   105,    96,   117,    98,   106,
       1,     2,     1,     2,     1,     2,     3,    93,     3,    95,
       3,   114,   107,   108,   111,   115,   116,     0,     0,   110,
       4,     5,     4,     5,     4,     5,     0,     6,   113,     6,
      44,     6,     0,     1,     2,     1,     2,     0,    44,     3,
     118,     3,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     4,     5,     4,     5,     0,     0,     0,
       6,     0,     6,    22,    23,    24,    25,    26,     0,     0,
       0,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,     0,     0,    38,    39,    40,    41,    42
};

static const yytype_int8 yycheck[] =
{
       9,     9,    50,    17,     0,    19,    20,     4,    34,    35,
       9,    10,    10,    22,    23,    24,    25,    26,    10,    11,
      29,     4,    30,     9,     9,     4,    10,    36,     9,    38,
      39,    40,    41,    42,    60,     4,     3,    46,    47,    48,
      49,    -1,    10,    52,    53,    10,    10,    10,    10,    97,
      59,    10,    10,     4,    63,    64,    10,    66,    67,     3,
      10,    10,    10,    10,    73,    10,    75,   115,    77,    10,
       3,     4,     3,     4,     3,     4,     9,    10,     9,    10,
       9,    10,    10,    10,    10,    10,    10,    -1,    -1,    98,
      23,    24,    23,    24,    23,    24,    -1,    30,   107,    30,
     109,    30,    -1,     3,     4,     3,     4,    -1,   117,     9,
      10,     9,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    23,    24,    23,    24,    -1,    -1,    -1,
      30,    -1,    30,     4,     5,     6,     7,     8,    -1,    -1,
      -1,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    -1,    -1,    25,    26,    27,    28,    29
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     9,    23,    24,    30,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    47,     4,     5,     6,     7,     8,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    25,    26,
      27,    28,    29,     0,    35,    35,    35,    35,    35,    35,
       4,     4,    35,     9,     9,     9,    10,    10,     9,    45,
      45,    35,     4,    35,    35,    35,    35,    35,    10,    35,
      35,    35,    35,    34,     9,    35,    35,     4,     3,    46,
      35,    45,    10,    10,    35,    35,    10,    35,    35,    10,
      10,    10,    10,    10,     4,    10,    35,    10,    35,    10,
      11,    10,    10,    10,    10,    10,    10,    10,    10,    34,
      35,    10,     3,    35,    10,    10,    10,    34,    10
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 186 "src/gpp_interpreter.y"
    { 
        if (!root_node) {
            root_node = create_node(NODE_LIST, "PROGRAM", 0);
        }
        
        // Add new input to root node
        if ((yyvsp[(1) - (1)].node)) {
            if (root_node->left == NULL) {
                root_node->left = (yyvsp[(1) - (1)].node);
            } else {
                Node* current = root_node->left;
                while (current->next != NULL) {
                    current = current->next;
                }
                current->next = (yyvsp[(1) - (1)].node);
            }
        }

        (yyval.node) = root_node;
    ;}
    break;

  case 3:
#line 209 "src/gpp_interpreter.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); ;}
    break;

  case 4:
#line 213 "src/gpp_interpreter.y"
    { 
        (yyval.node) = create_node(NODE_LIST, "EXPLIST", 0);
        (yyval.node)->left = (yyvsp[(1) - (2)].node);
        (yyval.node)->right = (yyvsp[(2) - (2)].node);
    ;}
    break;

  case 5:
#line 218 "src/gpp_interpreter.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); ;}
    break;

  case 6:
#line 222 "src/gpp_interpreter.y"
    { 
        (yyval.node) = create_node(NODE_IDENTIFIER, (yyvsp[(1) - (1)].sval), lookup_symbol((yyvsp[(1) - (1)].sval)));
    ;}
    break;

  case 7:
#line 225 "src/gpp_interpreter.y"
    { 
        (yyval.node) = create_node(NODE_VALUE, NULL, (yyvsp[(1) - (1)].fval));
    ;}
    break;

  case 8:
#line 228 "src/gpp_interpreter.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); ;}
    break;

  case 9:
#line 229 "src/gpp_interpreter.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); ;}
    break;

  case 10:
#line 230 "src/gpp_interpreter.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); ;}
    break;

  case 11:
#line 231 "src/gpp_interpreter.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); ;}
    break;

  case 12:
#line 232 "src/gpp_interpreter.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); ;}
    break;

  case 13:
#line 233 "src/gpp_interpreter.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); ;}
    break;

  case 14:
#line 234 "src/gpp_interpreter.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); ;}
    break;

  case 15:
#line 235 "src/gpp_interpreter.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); ;}
    break;

  case 16:
#line 236 "src/gpp_interpreter.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); ;}
    break;

  case 17:
#line 237 "src/gpp_interpreter.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); ;}
    break;

  case 18:
#line 238 "src/gpp_interpreter.y"
    { (yyval.node) = (yyvsp[(1) - (1)].node); ;}
    break;

  case 19:
#line 242 "src/gpp_interpreter.y"
    { 
        (yyval.node) = create_node(NODE_OPERATOR, "OP_PLUS", get_node_value((yyvsp[(3) - (5)].node)) + get_node_value((yyvsp[(4) - (5)].node)));
        (yyval.node)->left = (yyvsp[(3) - (5)].node);
        (yyval.node)->right = (yyvsp[(4) - (5)].node);
    ;}
    break;

  case 20:
#line 247 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_OPERATOR, "OP_MINUS", get_node_value((yyvsp[(3) - (5)].node)) - get_node_value((yyvsp[(4) - (5)].node)));
        (yyval.node)->left = (yyvsp[(3) - (5)].node);
        (yyval.node)->right = (yyvsp[(4) - (5)].node);
    ;}
    break;

  case 21:
#line 252 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_OPERATOR, "OP_MULT", get_node_value((yyvsp[(3) - (5)].node)) * get_node_value((yyvsp[(4) - (5)].node)));
        (yyval.node)->left = (yyvsp[(3) - (5)].node);
        (yyval.node)->right = (yyvsp[(4) - (5)].node);
    ;}
    break;

  case 22:
#line 257 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_OPERATOR, "OP_DIV", get_node_value((yyvsp[(3) - (5)].node)) / get_node_value((yyvsp[(4) - (5)].node)));
        (yyval.node)->left = (yyvsp[(3) - (5)].node);
        (yyval.node)->right = (yyvsp[(4) - (5)].node);
    ;}
    break;

  case 23:
#line 265 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_OPERATOR, "AND", get_node_value((yyvsp[(3) - (5)].node)) && get_node_value((yyvsp[(4) - (5)].node)));
        (yyval.node)->left = (yyvsp[(3) - (5)].node);
        (yyval.node)->right = (yyvsp[(4) - (5)].node);
    ;}
    break;

  case 24:
#line 270 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_OPERATOR, "OR", get_node_value((yyvsp[(3) - (5)].node)) || get_node_value((yyvsp[(4) - (5)].node)));
        (yyval.node)->left = (yyvsp[(3) - (5)].node);
        (yyval.node)->right = (yyvsp[(4) - (5)].node);
    ;}
    break;

  case 25:
#line 275 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_OPERATOR, "NOT", !get_node_value((yyvsp[(3) - (4)].node)));
        (yyval.node)->left = (yyvsp[(3) - (4)].node);
    ;}
    break;

  case 26:
#line 279 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_OPERATOR, "EQUAL", get_node_value((yyvsp[(3) - (5)].node)) == get_node_value((yyvsp[(4) - (5)].node)));
        (yyval.node)->left = (yyvsp[(3) - (5)].node);
        (yyval.node)->right = (yyvsp[(4) - (5)].node);
    ;}
    break;

  case 27:
#line 284 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_OPERATOR, "LESS", get_node_value((yyvsp[(3) - (5)].node)) < get_node_value((yyvsp[(4) - (5)].node)));
        (yyval.node)->left = (yyvsp[(3) - (5)].node);
        (yyval.node)->right = (yyvsp[(4) - (5)].node);
    ;}
    break;

  case 28:
#line 292 "src/gpp_interpreter.y"
    { (yyval.node) = create_node(NODE_VALUE, "TRUE", 1.0); ;}
    break;

  case 29:
#line 293 "src/gpp_interpreter.y"
    { (yyval.node) = create_node(NODE_VALUE, "FALSE", 0.0); ;}
    break;

  case 30:
#line 297 "src/gpp_interpreter.y"
    { 
        add_symbol((yyvsp[(3) - (5)].sval), get_node_value((yyvsp[(4) - (5)].node)));
        (yyval.node) = create_node(NODE_CONTROL, "SET", get_node_value((yyvsp[(4) - (5)].node)));
        (yyval.node)->left = create_node(NODE_IDENTIFIER, (yyvsp[(3) - (5)].sval), get_node_value((yyvsp[(4) - (5)].node)));
        (yyval.node)->right = (yyvsp[(4) - (5)].node);
    ;}
    break;

  case 31:
#line 306 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_FUNCTION, (yyvsp[(2) - (4)].sval), 0.0);
        (yyval.node)->left = (yyvsp[(3) - (4)].node);
    ;}
    break;

  case 32:
#line 313 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_FUNCTION, (yyvsp[(3) - (8)].sval), 0.0);
        (yyval.node)->left = create_node(NODE_IDENTIFIER, (yyvsp[(5) - (8)].sval), 0.0);
        (yyval.node)->right = (yyvsp[(7) - (8)].node);
    ;}
    break;

  case 33:
#line 321 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_CONTROL, "IF", 0.0);
        (yyval.node)->left = (yyvsp[(3) - (5)].node);
        if ((yyvsp[(3) - (5)].node)->value) {
            (yyval.node)->right = (yyvsp[(4) - (5)].node);
        } else {
            (yyval.node)->right = NULL;
        }
    ;}
    break;

  case 34:
#line 330 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_CONTROL, "IF-ELSE", 0.0);
        (yyval.node)->left = (yyvsp[(3) - (6)].node);
        if ((yyvsp[(3) - (6)].node)->value) {
            (yyval.node)->right = (yyvsp[(4) - (6)].node);
        } else {
            (yyval.node)->right = (yyvsp[(5) - (6)].node);
        }
    ;}
    break;

  case 35:
#line 339 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_CONTROL, "WHILE", 0.0);
        (yyval.node)->left = (yyvsp[(4) - (7)].node);
        (yyval.node)->right = (yyvsp[(6) - (7)].node);
    ;}
    break;

  case 36:
#line 344 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_CONTROL, "FOR", 0.0);
        (yyval.node)->left = create_node(NODE_IDENTIFIER, (yyvsp[(4) - (9)].sval), get_node_value((yyvsp[(5) - (9)].node)));
        (yyval.node)->right = (yyvsp[(8) - (9)].node);
        (yyval.node)->next = create_node(NODE_VALUE, NULL, get_node_value((yyvsp[(6) - (9)].node)));
    ;}
    break;

  case 37:
#line 353 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_CONTROL, "DISPLAY", get_node_value((yyvsp[(3) - (4)].node)));
        (yyval.node)->left = (yyvsp[(3) - (4)].node);
        printf("Display: %f\n", get_node_value((yyvsp[(3) - (4)].node)));
    ;}
    break;

  case 38:
#line 361 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_CONTROL, "EXIT", 0.0);
    ;}
    break;

  case 39:
#line 367 "src/gpp_interpreter.y"
    { (yyval.node) = create_node(NODE_LIST, "NIL", 0.0); ;}
    break;

  case 40:
#line 368 "src/gpp_interpreter.y"
    { (yyval.node) = create_node(NODE_LIST, "EMPTY", 0.0); ;}
    break;

  case 41:
#line 369 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_LIST, "LIST", 0.0);
        (yyval.node)->left = (yyvsp[(4) - (6)].node);
    ;}
    break;

  case 42:
#line 373 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_LIST, "APPEND", 0.0);
        (yyval.node)->left = (yyvsp[(3) - (5)].node);
        (yyval.node)->right = (yyvsp[(4) - (5)].node);
    ;}
    break;

  case 43:
#line 378 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_LIST, "CONCAT", 0.0);
        (yyval.node)->left = (yyvsp[(3) - (5)].node);
        (yyval.node)->right = (yyvsp[(4) - (5)].node);
    ;}
    break;

  case 44:
#line 386 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_LIST, "VALUES", (yyvsp[(3) - (3)].fval));
        (yyval.node)->next = (yyvsp[(1) - (3)].node);
    ;}
    break;

  case 45:
#line 390 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_VALUE, NULL, (yyvsp[(1) - (1)].fval));
    ;}
    break;

  case 46:
#line 396 "src/gpp_interpreter.y"
    {
        (yyval.node) = create_node(NODE_CONTROL, "LOAD", 0.0);
        Node* loaded_tree = load_file((yyvsp[(3) - (4)].sval));
        if (!loaded_tree) {
            YYERROR;
        }
        (yyval.node)->left = loaded_tree;
    ;}
    break;


/* Line 1267 of yacc.c.  */
#line 1984 "build/gpp_parser.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 406 "src/gpp_interpreter.y"


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
