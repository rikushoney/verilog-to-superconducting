/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_RTLIL_FRONTEND_YY_RTLIL_PARSER_TAB_HH_INCLUDED
# define YY_RTLIL_FRONTEND_YY_RTLIL_PARSER_TAB_HH_INCLUDED
/* Debug traces.  */
#ifndef RTLIL_FRONTEND_YYDEBUG
# if defined YYDEBUG
#if YYDEBUG
#   define RTLIL_FRONTEND_YYDEBUG 1
#  else
#   define RTLIL_FRONTEND_YYDEBUG 0
#  endif
# else /* ! defined YYDEBUG */
#  define RTLIL_FRONTEND_YYDEBUG 1
# endif /* ! defined YYDEBUG */
#endif  /* ! defined RTLIL_FRONTEND_YYDEBUG */
#if RTLIL_FRONTEND_YYDEBUG
extern int rtlil_frontend_yydebug;
#endif
/* "%code requires" blocks.  */
#line 55 "yosys/frontends/rtlil/rtlil_parser.y"

#include <string>
#include <vector>
#include "frontends/rtlil/rtlil_frontend.h"

#line 63 "rtlil_parser.tab.hh"

/* Token kinds.  */
#ifndef RTLIL_FRONTEND_YYTOKENTYPE
# define RTLIL_FRONTEND_YYTOKENTYPE
  enum rtlil_frontend_yytokentype
  {
    RTLIL_FRONTEND_YYEMPTY = -2,
    RTLIL_FRONTEND_YYEOF = 0,      /* "end of file"  */
    RTLIL_FRONTEND_YYerror = 256,  /* error  */
    RTLIL_FRONTEND_YYUNDEF = 257,  /* "invalid token"  */
    TOK_ID = 258,                  /* TOK_ID  */
    TOK_VALUE = 259,               /* TOK_VALUE  */
    TOK_STRING = 260,              /* TOK_STRING  */
    TOK_INT = 261,                 /* TOK_INT  */
    TOK_AUTOIDX = 262,             /* TOK_AUTOIDX  */
    TOK_MODULE = 263,              /* TOK_MODULE  */
    TOK_WIRE = 264,                /* TOK_WIRE  */
    TOK_WIDTH = 265,               /* TOK_WIDTH  */
    TOK_INPUT = 266,               /* TOK_INPUT  */
    TOK_OUTPUT = 267,              /* TOK_OUTPUT  */
    TOK_INOUT = 268,               /* TOK_INOUT  */
    TOK_CELL = 269,                /* TOK_CELL  */
    TOK_CONNECT = 270,             /* TOK_CONNECT  */
    TOK_SWITCH = 271,              /* TOK_SWITCH  */
    TOK_CASE = 272,                /* TOK_CASE  */
    TOK_ASSIGN = 273,              /* TOK_ASSIGN  */
    TOK_SYNC = 274,                /* TOK_SYNC  */
    TOK_LOW = 275,                 /* TOK_LOW  */
    TOK_HIGH = 276,                /* TOK_HIGH  */
    TOK_POSEDGE = 277,             /* TOK_POSEDGE  */
    TOK_NEGEDGE = 278,             /* TOK_NEGEDGE  */
    TOK_EDGE = 279,                /* TOK_EDGE  */
    TOK_ALWAYS = 280,              /* TOK_ALWAYS  */
    TOK_GLOBAL = 281,              /* TOK_GLOBAL  */
    TOK_INIT = 282,                /* TOK_INIT  */
    TOK_UPDATE = 283,              /* TOK_UPDATE  */
    TOK_MEMWR = 284,               /* TOK_MEMWR  */
    TOK_PROCESS = 285,             /* TOK_PROCESS  */
    TOK_END = 286,                 /* TOK_END  */
    TOK_INVALID = 287,             /* TOK_INVALID  */
    TOK_EOL = 288,                 /* TOK_EOL  */
    TOK_OFFSET = 289,              /* TOK_OFFSET  */
    TOK_PARAMETER = 290,           /* TOK_PARAMETER  */
    TOK_ATTRIBUTE = 291,           /* TOK_ATTRIBUTE  */
    TOK_MEMORY = 292,              /* TOK_MEMORY  */
    TOK_SIZE = 293,                /* TOK_SIZE  */
    TOK_SIGNED = 294,              /* TOK_SIGNED  */
    TOK_REAL = 295,                /* TOK_REAL  */
    TOK_UPTO = 296                 /* TOK_UPTO  */
  };
  typedef enum rtlil_frontend_yytokentype rtlil_frontend_yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined RTLIL_FRONTEND_YYSTYPE && ! defined RTLIL_FRONTEND_YYSTYPE_IS_DECLARED
union RTLIL_FRONTEND_YYSTYPE
{
#line 61 "yosys/frontends/rtlil/rtlil_parser.y"

	char *string;
	int integer;
	YOSYS_NAMESPACE_PREFIX RTLIL::Const *data;
	YOSYS_NAMESPACE_PREFIX RTLIL::SigSpec *sigspec;
	std::vector<YOSYS_NAMESPACE_PREFIX RTLIL::SigSpec> *rsigspec;

#line 129 "rtlil_parser.tab.hh"

};
typedef union RTLIL_FRONTEND_YYSTYPE RTLIL_FRONTEND_YYSTYPE;
# define RTLIL_FRONTEND_YYSTYPE_IS_TRIVIAL 1
# define RTLIL_FRONTEND_YYSTYPE_IS_DECLARED 1
#endif


extern RTLIL_FRONTEND_YYSTYPE rtlil_frontend_yylval;


int rtlil_frontend_yyparse (void);


#endif /* !YY_RTLIL_FRONTEND_YY_RTLIL_PARSER_TAB_HH_INCLUDED  */
