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

#ifndef YY_FRONTEND_VERILOG_YY_FRONTENDS_VERILOG_VERILOG_PARSER_TAB_HH_INCLUDED
# define YY_FRONTEND_VERILOG_YY_FRONTENDS_VERILOG_VERILOG_PARSER_TAB_HH_INCLUDED
/* Debug traces.  */
#ifndef FRONTEND_VERILOG_YYDEBUG
# if defined YYDEBUG
#if YYDEBUG
#   define FRONTEND_VERILOG_YYDEBUG 1
#  else
#   define FRONTEND_VERILOG_YYDEBUG 0
#  endif
# else /* ! defined YYDEBUG */
#  define FRONTEND_VERILOG_YYDEBUG 1
# endif /* ! defined YYDEBUG */
#endif  /* ! defined FRONTEND_VERILOG_YYDEBUG */
#if FRONTEND_VERILOG_YYDEBUG
extern int frontend_verilog_yydebug;
#endif
/* "%code requires" blocks.  */
#line 362 "../frontends/verilog/verilog_parser.y"

#include <map>
#include <string>
#include "verilog.h"

#line 63 "frontends/verilog/verilog_parser.tab.hh"

/* Token kinds.  */
#ifndef FRONTEND_VERILOG_YYTOKENTYPE
# define FRONTEND_VERILOG_YYTOKENTYPE
  enum frontend_verilog_yytokentype
  {
    FRONTEND_VERILOG_YYEMPTY = -2,
    FRONTEND_VERILOG_YYEOF = 0,    /* "end of file"  */
    FRONTEND_VERILOG_YYerror = 256, /* error  */
    FRONTEND_VERILOG_YYUNDEF = 257, /* "invalid token"  */
    TOK_STRING = 258,              /* TOK_STRING  */
    TOK_ID = 259,                  /* TOK_ID  */
    TOK_CONSTVAL = 260,            /* TOK_CONSTVAL  */
    TOK_REALVAL = 261,             /* TOK_REALVAL  */
    TOK_PRIMITIVE = 262,           /* TOK_PRIMITIVE  */
    TOK_SVA_LABEL = 263,           /* TOK_SVA_LABEL  */
    TOK_SPECIFY_OPER = 264,        /* TOK_SPECIFY_OPER  */
    TOK_MSG_TASKS = 265,           /* TOK_MSG_TASKS  */
    TOK_BASE = 266,                /* TOK_BASE  */
    TOK_BASED_CONSTVAL = 267,      /* TOK_BASED_CONSTVAL  */
    TOK_UNBASED_UNSIZED_CONSTVAL = 268, /* TOK_UNBASED_UNSIZED_CONSTVAL  */
    TOK_USER_TYPE = 269,           /* TOK_USER_TYPE  */
    TOK_PKG_USER_TYPE = 270,       /* TOK_PKG_USER_TYPE  */
    TOK_ASSERT = 271,              /* TOK_ASSERT  */
    TOK_ASSUME = 272,              /* TOK_ASSUME  */
    TOK_RESTRICT = 273,            /* TOK_RESTRICT  */
    TOK_COVER = 274,               /* TOK_COVER  */
    TOK_FINAL = 275,               /* TOK_FINAL  */
    ATTR_BEGIN = 276,              /* ATTR_BEGIN  */
    ATTR_END = 277,                /* ATTR_END  */
    DEFATTR_BEGIN = 278,           /* DEFATTR_BEGIN  */
    DEFATTR_END = 279,             /* DEFATTR_END  */
    TOK_MODULE = 280,              /* TOK_MODULE  */
    TOK_ENDMODULE = 281,           /* TOK_ENDMODULE  */
    TOK_PARAMETER = 282,           /* TOK_PARAMETER  */
    TOK_LOCALPARAM = 283,          /* TOK_LOCALPARAM  */
    TOK_DEFPARAM = 284,            /* TOK_DEFPARAM  */
    TOK_PACKAGE = 285,             /* TOK_PACKAGE  */
    TOK_ENDPACKAGE = 286,          /* TOK_ENDPACKAGE  */
    TOK_PACKAGESEP = 287,          /* TOK_PACKAGESEP  */
    TOK_INTERFACE = 288,           /* TOK_INTERFACE  */
    TOK_ENDINTERFACE = 289,        /* TOK_ENDINTERFACE  */
    TOK_MODPORT = 290,             /* TOK_MODPORT  */
    TOK_VAR = 291,                 /* TOK_VAR  */
    TOK_WILDCARD_CONNECT = 292,    /* TOK_WILDCARD_CONNECT  */
    TOK_INPUT = 293,               /* TOK_INPUT  */
    TOK_OUTPUT = 294,              /* TOK_OUTPUT  */
    TOK_INOUT = 295,               /* TOK_INOUT  */
    TOK_WIRE = 296,                /* TOK_WIRE  */
    TOK_WAND = 297,                /* TOK_WAND  */
    TOK_WOR = 298,                 /* TOK_WOR  */
    TOK_REG = 299,                 /* TOK_REG  */
    TOK_LOGIC = 300,               /* TOK_LOGIC  */
    TOK_INTEGER = 301,             /* TOK_INTEGER  */
    TOK_SIGNED = 302,              /* TOK_SIGNED  */
    TOK_ASSIGN = 303,              /* TOK_ASSIGN  */
    TOK_ALWAYS = 304,              /* TOK_ALWAYS  */
    TOK_INITIAL = 305,             /* TOK_INITIAL  */
    TOK_ALWAYS_FF = 306,           /* TOK_ALWAYS_FF  */
    TOK_ALWAYS_COMB = 307,         /* TOK_ALWAYS_COMB  */
    TOK_ALWAYS_LATCH = 308,        /* TOK_ALWAYS_LATCH  */
    TOK_BEGIN = 309,               /* TOK_BEGIN  */
    TOK_END = 310,                 /* TOK_END  */
    TOK_IF = 311,                  /* TOK_IF  */
    TOK_ELSE = 312,                /* TOK_ELSE  */
    TOK_FOR = 313,                 /* TOK_FOR  */
    TOK_WHILE = 314,               /* TOK_WHILE  */
    TOK_REPEAT = 315,              /* TOK_REPEAT  */
    TOK_DPI_FUNCTION = 316,        /* TOK_DPI_FUNCTION  */
    TOK_POSEDGE = 317,             /* TOK_POSEDGE  */
    TOK_NEGEDGE = 318,             /* TOK_NEGEDGE  */
    TOK_OR = 319,                  /* TOK_OR  */
    TOK_AUTOMATIC = 320,           /* TOK_AUTOMATIC  */
    TOK_CASE = 321,                /* TOK_CASE  */
    TOK_CASEX = 322,               /* TOK_CASEX  */
    TOK_CASEZ = 323,               /* TOK_CASEZ  */
    TOK_ENDCASE = 324,             /* TOK_ENDCASE  */
    TOK_DEFAULT = 325,             /* TOK_DEFAULT  */
    TOK_FUNCTION = 326,            /* TOK_FUNCTION  */
    TOK_ENDFUNCTION = 327,         /* TOK_ENDFUNCTION  */
    TOK_TASK = 328,                /* TOK_TASK  */
    TOK_ENDTASK = 329,             /* TOK_ENDTASK  */
    TOK_SPECIFY = 330,             /* TOK_SPECIFY  */
    TOK_IGNORED_SPECIFY = 331,     /* TOK_IGNORED_SPECIFY  */
    TOK_ENDSPECIFY = 332,          /* TOK_ENDSPECIFY  */
    TOK_SPECPARAM = 333,           /* TOK_SPECPARAM  */
    TOK_SPECIFY_AND = 334,         /* TOK_SPECIFY_AND  */
    TOK_IGNORED_SPECIFY_AND = 335, /* TOK_IGNORED_SPECIFY_AND  */
    TOK_GENERATE = 336,            /* TOK_GENERATE  */
    TOK_ENDGENERATE = 337,         /* TOK_ENDGENERATE  */
    TOK_GENVAR = 338,              /* TOK_GENVAR  */
    TOK_REAL = 339,                /* TOK_REAL  */
    TOK_SYNOPSYS_FULL_CASE = 340,  /* TOK_SYNOPSYS_FULL_CASE  */
    TOK_SYNOPSYS_PARALLEL_CASE = 341, /* TOK_SYNOPSYS_PARALLEL_CASE  */
    TOK_SUPPLY0 = 342,             /* TOK_SUPPLY0  */
    TOK_SUPPLY1 = 343,             /* TOK_SUPPLY1  */
    TOK_TO_SIGNED = 344,           /* TOK_TO_SIGNED  */
    TOK_TO_UNSIGNED = 345,         /* TOK_TO_UNSIGNED  */
    TOK_POS_INDEXED = 346,         /* TOK_POS_INDEXED  */
    TOK_NEG_INDEXED = 347,         /* TOK_NEG_INDEXED  */
    TOK_PROPERTY = 348,            /* TOK_PROPERTY  */
    TOK_ENUM = 349,                /* TOK_ENUM  */
    TOK_TYPEDEF = 350,             /* TOK_TYPEDEF  */
    TOK_RAND = 351,                /* TOK_RAND  */
    TOK_CONST = 352,               /* TOK_CONST  */
    TOK_CHECKER = 353,             /* TOK_CHECKER  */
    TOK_ENDCHECKER = 354,          /* TOK_ENDCHECKER  */
    TOK_EVENTUALLY = 355,          /* TOK_EVENTUALLY  */
    TOK_INCREMENT = 356,           /* TOK_INCREMENT  */
    TOK_DECREMENT = 357,           /* TOK_DECREMENT  */
    TOK_UNIQUE = 358,              /* TOK_UNIQUE  */
    TOK_UNIQUE0 = 359,             /* TOK_UNIQUE0  */
    TOK_PRIORITY = 360,            /* TOK_PRIORITY  */
    TOK_STRUCT = 361,              /* TOK_STRUCT  */
    TOK_PACKED = 362,              /* TOK_PACKED  */
    TOK_UNSIGNED = 363,            /* TOK_UNSIGNED  */
    TOK_INT = 364,                 /* TOK_INT  */
    TOK_BYTE = 365,                /* TOK_BYTE  */
    TOK_SHORTINT = 366,            /* TOK_SHORTINT  */
    TOK_LONGINT = 367,             /* TOK_LONGINT  */
    TOK_VOID = 368,                /* TOK_VOID  */
    TOK_UNION = 369,               /* TOK_UNION  */
    TOK_BIT_OR_ASSIGN = 370,       /* TOK_BIT_OR_ASSIGN  */
    TOK_BIT_AND_ASSIGN = 371,      /* TOK_BIT_AND_ASSIGN  */
    TOK_BIT_XOR_ASSIGN = 372,      /* TOK_BIT_XOR_ASSIGN  */
    TOK_ADD_ASSIGN = 373,          /* TOK_ADD_ASSIGN  */
    TOK_SUB_ASSIGN = 374,          /* TOK_SUB_ASSIGN  */
    TOK_DIV_ASSIGN = 375,          /* TOK_DIV_ASSIGN  */
    TOK_MOD_ASSIGN = 376,          /* TOK_MOD_ASSIGN  */
    TOK_MUL_ASSIGN = 377,          /* TOK_MUL_ASSIGN  */
    TOK_SHL_ASSIGN = 378,          /* TOK_SHL_ASSIGN  */
    TOK_SHR_ASSIGN = 379,          /* TOK_SHR_ASSIGN  */
    TOK_SSHL_ASSIGN = 380,         /* TOK_SSHL_ASSIGN  */
    TOK_SSHR_ASSIGN = 381,         /* TOK_SSHR_ASSIGN  */
    TOK_BIND = 382,                /* TOK_BIND  */
    TOK_TIME_SCALE = 383,          /* TOK_TIME_SCALE  */
    OP_LOR = 384,                  /* OP_LOR  */
    OP_LAND = 385,                 /* OP_LAND  */
    OP_NOR = 386,                  /* OP_NOR  */
    OP_XNOR = 387,                 /* OP_XNOR  */
    OP_NAND = 388,                 /* OP_NAND  */
    OP_EQ = 389,                   /* OP_EQ  */
    OP_NE = 390,                   /* OP_NE  */
    OP_EQX = 391,                  /* OP_EQX  */
    OP_NEX = 392,                  /* OP_NEX  */
    OP_LE = 393,                   /* OP_LE  */
    OP_GE = 394,                   /* OP_GE  */
    OP_SHL = 395,                  /* OP_SHL  */
    OP_SHR = 396,                  /* OP_SHR  */
    OP_SSHL = 397,                 /* OP_SSHL  */
    OP_SSHR = 398,                 /* OP_SSHR  */
    OP_POW = 399,                  /* OP_POW  */
    OP_CAST = 400,                 /* OP_CAST  */
    UNARY_OPS = 401,               /* UNARY_OPS  */
    FAKE_THEN = 402                /* FAKE_THEN  */
  };
  typedef enum frontend_verilog_yytokentype frontend_verilog_yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined FRONTEND_VERILOG_YYSTYPE && ! defined FRONTEND_VERILOG_YYSTYPE_IS_DECLARED
union FRONTEND_VERILOG_YYSTYPE
{
#line 368 "../frontends/verilog/verilog_parser.y"

	std::string *string;
	struct yosys_mini::AST::AstNode *ast;
	yosys_mini::hashlib::dict<yosys_mini::RTLIL::IdString, yosys_mini::AST::AstNode*> *al;
	struct specify_target *specify_target_ptr;
	struct specify_triple *specify_triple_ptr;
	struct specify_rise_fall *specify_rise_fall_ptr;
	bool boolean;
	char ch;
	int integer;
	yosys_mini::AST::AstNodeType ast_node_type;

#line 240 "frontends/verilog/verilog_parser.tab.hh"

};
typedef union FRONTEND_VERILOG_YYSTYPE FRONTEND_VERILOG_YYSTYPE;
# define FRONTEND_VERILOG_YYSTYPE_IS_TRIVIAL 1
# define FRONTEND_VERILOG_YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined FRONTEND_VERILOG_YYLTYPE && ! defined FRONTEND_VERILOG_YYLTYPE_IS_DECLARED
typedef struct FRONTEND_VERILOG_YYLTYPE FRONTEND_VERILOG_YYLTYPE;
struct FRONTEND_VERILOG_YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define FRONTEND_VERILOG_YYLTYPE_IS_DECLARED 1
# define FRONTEND_VERILOG_YYLTYPE_IS_TRIVIAL 1
#endif




int frontend_verilog_yyparse (void);


#endif /* !YY_FRONTEND_VERILOG_YY_FRONTENDS_VERILOG_VERILOG_PARSER_TAB_HH_INCLUDED  */
