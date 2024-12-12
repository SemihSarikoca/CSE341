/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

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
     KW_SET = 266,
     KW_DEFFUN = 267,
     KW_IF = 268,
     KW_WHILE = 269,
     KW_FOR = 270,
     KW_TRUE = 271,
     KW_FALSE = 272,
     KW_AND = 273,
     KW_OR = 274,
     KW_NOT = 275,
     KW_EQUAL = 276,
     KW_LESS = 277,
     KW_EXIT = 278,
     KW_DISP = 279
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
#define KW_SET 266
#define KW_DEFFUN 267
#define KW_IF 268
#define KW_WHILE 269
#define KW_FOR 270
#define KW_TRUE 271
#define KW_FALSE 272
#define KW_AND 273
#define KW_OR 274
#define KW_NOT 275
#define KW_EQUAL 276
#define KW_LESS 277
#define KW_EXIT 278
#define KW_DISP 279




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 74 "src/gpp_interpreter.y"
{
    float fval;
    char *sval;
}
/* Line 1529 of yacc.c.  */
#line 102 "build/gpp_parser.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

