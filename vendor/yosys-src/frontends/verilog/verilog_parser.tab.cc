/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Substitute the type names.  */
#define YYSTYPE         FRONTEND_VERILOG_YYSTYPE
#define YYLTYPE         FRONTEND_VERILOG_YYLTYPE
/* Substitute the variable and function names.  */
#define yyparse         frontend_verilog_yyparse
#define yylex           frontend_verilog_yylex
#define yyerror         frontend_verilog_yyerror
#define yydebug         frontend_verilog_yydebug
#define yynerrs         frontend_verilog_yynerrs

/* First part of user prologue.  */
#line 38 "verilog_parser.y"

#include <list>
#include <stack>
#include <string.h>
#include "frontends/verilog/verilog_frontend.h"
#include "frontends/verilog/verilog_parser.tab.hh"
#include "kernel/log.h"

#define YYLEX_PARAM &yylval, &yylloc

USING_YOSYS_NAMESPACE
using namespace AST;
using namespace VERILOG_FRONTEND;

YOSYS_NAMESPACE_BEGIN
namespace VERILOG_FRONTEND {
	int port_counter;
	dict<std::string, int> port_stubs;
	dict<IdString, AstNode*> *attr_list, default_attr_list;
	std::stack<dict<IdString, AstNode*> *> attr_list_stack;
	dict<IdString, AstNode*> *albuf;
	std::vector<UserTypeMap> user_type_stack;
	dict<std::string, AstNode*> pkg_user_types;
	std::vector<AstNode*> ast_stack;
	struct AstNode *astbuf1, *astbuf2, *astbuf3;
	struct AstNode *current_function_or_task;
	struct AstNode *current_ast, *current_ast_mod;
	int current_function_or_task_port_id;
	std::vector<char> case_type_stack;
	bool do_not_require_port_stubs;
	bool default_nettype_wire;
	bool sv_mode, formal_mode, lib_mode, specify_mode;
	bool noassert_mode, noassume_mode, norestrict_mode;
	bool assume_asserts_mode, assert_assumes_mode;
	bool current_wire_rand, current_wire_const;
	bool current_modport_input, current_modport_output;
	std::istream *lexin;
}
YOSYS_NAMESPACE_END

#define SET_AST_NODE_LOC(WHICH, BEGIN, END) \
    do { (WHICH)->location.first_line = (BEGIN).first_line; \
    (WHICH)->location.first_column = (BEGIN).first_column; \
    (WHICH)->location.last_line = (END).last_line; \
    (WHICH)->location.last_column = (END).last_column; } while(0)

#define SET_RULE_LOC(LHS, BEGIN, END) \
    do { (LHS).first_line = (BEGIN).first_line; \
    (LHS).first_column = (BEGIN).first_column; \
    (LHS).last_line = (END).last_line; \
    (LHS).last_column = (END).last_column; } while(0)

int frontend_verilog_yylex(YYSTYPE *yylval_param, YYLTYPE *yyloc_param);

static void append_attr(AstNode *ast, dict<IdString, AstNode*> *al)
{
	for (auto &it : *al) {
		if (ast->attributes.count(it.first) > 0)
			delete ast->attributes[it.first];
		ast->attributes[it.first] = it.second;
	}
	delete al;
}

static void append_attr_clone(AstNode *ast, dict<IdString, AstNode*> *al)
{
	for (auto &it : *al) {
		if (ast->attributes.count(it.first) > 0)
			delete ast->attributes[it.first];
		ast->attributes[it.first] = it.second->clone();
	}
}

static void free_attr(dict<IdString, AstNode*> *al)
{
	for (auto &it : *al)
		delete it.second;
	delete al;
}

struct specify_target {
	char polarity_op;
	AstNode *dst, *dat;
};

struct specify_triple {
	AstNode *t_min, *t_avg, *t_max;
};

struct specify_rise_fall {
	specify_triple rise;
	specify_triple fall;
};

static void addWiretypeNode(std::string *name, AstNode *node)
{
	log_assert(node);
	node->is_custom_type = true;
	node->children.push_back(new AstNode(AST_WIRETYPE));
	node->children.back()->str = *name;
	delete name;
}

static void addTypedefNode(std::string *name, AstNode *node)
{
	log_assert(node);
	auto *tnode = new AstNode(AST_TYPEDEF, node);
	tnode->str = *name;
	auto &user_types = user_type_stack.back();
	user_types[*name] = tnode;
	if (current_ast_mod && current_ast_mod->type == AST_PACKAGE) {
		// typedef inside a package so we need the qualified name
		auto qname = current_ast_mod->str + "::" + (*name).substr(1);
		pkg_user_types[qname] = tnode;
	}
	delete name;
	ast_stack.back()->children.push_back(tnode);
}

static void enterTypeScope()
{
	user_type_stack.push_back(UserTypeMap());
}

static void exitTypeScope()
{
	user_type_stack.pop_back();
}

static bool isInLocalScope(const std::string *name)
{
	// tests if a name was declared in the current block scope
	auto &user_types = user_type_stack.back();
	return (user_types.count(*name) > 0);
}

static AstNode *makeRange(int msb = 31, int lsb = 0, bool isSigned = true)
{
	auto range = new AstNode(AST_RANGE);
	range->children.push_back(AstNode::mkconst_int(msb, true));
	range->children.push_back(AstNode::mkconst_int(lsb, true));
	range->is_signed = isSigned;
	return range;
}

static void addRange(AstNode *parent, int msb = 31, int lsb = 0, bool isSigned = true)
{
	auto range = makeRange(msb, lsb, isSigned);
	parent->children.push_back(range);
}

static AstNode *checkRange(AstNode *type_node, AstNode *range_node)
{
	if (type_node->range_left >= 0 && type_node->range_right >= 0) {
		// type already restricts the range
		if (range_node) {
			frontend_verilog_yyerror("integer/genvar types cannot have packed dimensions.");
		}
		else {
			range_node = makeRange(type_node->range_left, type_node->range_right, false);
		}
	}
	if (range_node && range_node->children.size() != 2) {
		frontend_verilog_yyerror("wire/reg/logic packed dimension must be of the form: [<expr>:<expr>], [<expr>+:<expr>], or [<expr>-:<expr>]");
	}
	return range_node;
}

static void rewriteRange(AstNode *rangeNode)
{
	if (rangeNode->type == AST_RANGE && rangeNode->children.size() == 1) {
		// SV array size [n], rewrite as [0:n-1]
		rangeNode->children.push_back(new AstNode(AST_SUB, rangeNode->children[0], AstNode::mkconst_int(1, true)));
		rangeNode->children[0] = AstNode::mkconst_int(0, false);
	}
}

static void rewriteAsMemoryNode(AstNode *node, AstNode *rangeNode)
{
	node->type = AST_MEMORY;
	if (rangeNode->type == AST_MULTIRANGE) {
		for (auto *itr : rangeNode->children)
			rewriteRange(itr);
	} else
		rewriteRange(rangeNode);
	node->children.push_back(rangeNode);
}

static void checkLabelsMatch(const char *element, const std::string *before, const std::string *after)
{
	if (!before && after)
		frontend_verilog_yyerror("%s missing where end label (%s) was given.",
			element, after->c_str() + 1);
	if (before && after && *before != *after)
		frontend_verilog_yyerror("%s (%s) and end label (%s) don't match.",
			element, before->c_str() + 1, after->c_str() + 1);
}

// This transforms a loop like
//   for (genvar i = 0; i < 10; i++) begin : blk
// to
//   genvar _i;
//   for (_i = 0; _i < 10; _i++) begin : blk
//     localparam i = _i;
// where `_i` is actually some auto-generated name.
static void rewriteGenForDeclInit(AstNode *loop)
{
	// check if this generate for loop contains an inline declaration
	log_assert(loop->type == AST_GENFOR);
	AstNode *decl = loop->children[0];
	if (decl->type == AST_ASSIGN_EQ)
		return;
	log_assert(decl->type == AST_GENVAR);
	log_assert(loop->children.size() == 5);

	// identify each component of the loop
	AstNode *init = loop->children[1];
	AstNode *cond = loop->children[2];
	AstNode *incr = loop->children[3];
	AstNode *body = loop->children[4];
	log_assert(init->type == AST_ASSIGN_EQ);
	log_assert(incr->type == AST_ASSIGN_EQ);
	log_assert(body->type == AST_GENBLOCK);

	// create a unique name for the genvar
	std::string old_str = decl->str;
	std::string new_str = stringf("$genfordecl$%d$%s", autoidx++, old_str.c_str());

	// rename and move the genvar declaration to the containing description
	decl->str = new_str;
	loop->children.erase(loop->children.begin());
	log_assert(current_ast_mod != nullptr);
	current_ast_mod->children.push_back(decl);

	// create a new localparam with old name so that the items in the loop
	// can simply use the old name and shadow it as necessary
	AstNode *indirect = new AstNode(AST_LOCALPARAM);
	indirect->str = old_str;
	AstNode *ident = new AstNode(AST_IDENTIFIER);
	ident->str = new_str;
	indirect->children.push_back(ident);

	body->children.insert(body->children.begin(), indirect);

	// only perform the renaming for the initialization, guard, and
	// incrementation to enable proper shadowing of the synthetic localparam
	std::function<void(AstNode*)> substitute = [&](AstNode *node) {
		if (node->type == AST_IDENTIFIER && node->str == old_str)
			node->str = new_str;
		for (AstNode *child : node->children)
			substitute(child);
	};
	substitute(init);
	substitute(cond);
	substitute(incr);
}


#line 337 "verilog_parser.tab.cc"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "verilog_parser.tab.hh"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_TOK_STRING = 3,                 /* TOK_STRING  */
  YYSYMBOL_TOK_ID = 4,                     /* TOK_ID  */
  YYSYMBOL_TOK_CONSTVAL = 5,               /* TOK_CONSTVAL  */
  YYSYMBOL_TOK_REALVAL = 6,                /* TOK_REALVAL  */
  YYSYMBOL_TOK_PRIMITIVE = 7,              /* TOK_PRIMITIVE  */
  YYSYMBOL_TOK_SVA_LABEL = 8,              /* TOK_SVA_LABEL  */
  YYSYMBOL_TOK_SPECIFY_OPER = 9,           /* TOK_SPECIFY_OPER  */
  YYSYMBOL_TOK_MSG_TASKS = 10,             /* TOK_MSG_TASKS  */
  YYSYMBOL_TOK_BASE = 11,                  /* TOK_BASE  */
  YYSYMBOL_TOK_BASED_CONSTVAL = 12,        /* TOK_BASED_CONSTVAL  */
  YYSYMBOL_TOK_UNBASED_UNSIZED_CONSTVAL = 13, /* TOK_UNBASED_UNSIZED_CONSTVAL  */
  YYSYMBOL_TOK_USER_TYPE = 14,             /* TOK_USER_TYPE  */
  YYSYMBOL_TOK_PKG_USER_TYPE = 15,         /* TOK_PKG_USER_TYPE  */
  YYSYMBOL_TOK_ASSERT = 16,                /* TOK_ASSERT  */
  YYSYMBOL_TOK_ASSUME = 17,                /* TOK_ASSUME  */
  YYSYMBOL_TOK_RESTRICT = 18,              /* TOK_RESTRICT  */
  YYSYMBOL_TOK_COVER = 19,                 /* TOK_COVER  */
  YYSYMBOL_TOK_FINAL = 20,                 /* TOK_FINAL  */
  YYSYMBOL_ATTR_BEGIN = 21,                /* ATTR_BEGIN  */
  YYSYMBOL_ATTR_END = 22,                  /* ATTR_END  */
  YYSYMBOL_DEFATTR_BEGIN = 23,             /* DEFATTR_BEGIN  */
  YYSYMBOL_DEFATTR_END = 24,               /* DEFATTR_END  */
  YYSYMBOL_TOK_MODULE = 25,                /* TOK_MODULE  */
  YYSYMBOL_TOK_ENDMODULE = 26,             /* TOK_ENDMODULE  */
  YYSYMBOL_TOK_PARAMETER = 27,             /* TOK_PARAMETER  */
  YYSYMBOL_TOK_LOCALPARAM = 28,            /* TOK_LOCALPARAM  */
  YYSYMBOL_TOK_DEFPARAM = 29,              /* TOK_DEFPARAM  */
  YYSYMBOL_TOK_PACKAGE = 30,               /* TOK_PACKAGE  */
  YYSYMBOL_TOK_ENDPACKAGE = 31,            /* TOK_ENDPACKAGE  */
  YYSYMBOL_TOK_PACKAGESEP = 32,            /* TOK_PACKAGESEP  */
  YYSYMBOL_TOK_INTERFACE = 33,             /* TOK_INTERFACE  */
  YYSYMBOL_TOK_ENDINTERFACE = 34,          /* TOK_ENDINTERFACE  */
  YYSYMBOL_TOK_MODPORT = 35,               /* TOK_MODPORT  */
  YYSYMBOL_TOK_VAR = 36,                   /* TOK_VAR  */
  YYSYMBOL_TOK_WILDCARD_CONNECT = 37,      /* TOK_WILDCARD_CONNECT  */
  YYSYMBOL_TOK_INPUT = 38,                 /* TOK_INPUT  */
  YYSYMBOL_TOK_OUTPUT = 39,                /* TOK_OUTPUT  */
  YYSYMBOL_TOK_INOUT = 40,                 /* TOK_INOUT  */
  YYSYMBOL_TOK_WIRE = 41,                  /* TOK_WIRE  */
  YYSYMBOL_TOK_WAND = 42,                  /* TOK_WAND  */
  YYSYMBOL_TOK_WOR = 43,                   /* TOK_WOR  */
  YYSYMBOL_TOK_REG = 44,                   /* TOK_REG  */
  YYSYMBOL_TOK_LOGIC = 45,                 /* TOK_LOGIC  */
  YYSYMBOL_TOK_INTEGER = 46,               /* TOK_INTEGER  */
  YYSYMBOL_TOK_SIGNED = 47,                /* TOK_SIGNED  */
  YYSYMBOL_TOK_ASSIGN = 48,                /* TOK_ASSIGN  */
  YYSYMBOL_TOK_ALWAYS = 49,                /* TOK_ALWAYS  */
  YYSYMBOL_TOK_INITIAL = 50,               /* TOK_INITIAL  */
  YYSYMBOL_TOK_ALWAYS_FF = 51,             /* TOK_ALWAYS_FF  */
  YYSYMBOL_TOK_ALWAYS_COMB = 52,           /* TOK_ALWAYS_COMB  */
  YYSYMBOL_TOK_ALWAYS_LATCH = 53,          /* TOK_ALWAYS_LATCH  */
  YYSYMBOL_TOK_BEGIN = 54,                 /* TOK_BEGIN  */
  YYSYMBOL_TOK_END = 55,                   /* TOK_END  */
  YYSYMBOL_TOK_IF = 56,                    /* TOK_IF  */
  YYSYMBOL_TOK_ELSE = 57,                  /* TOK_ELSE  */
  YYSYMBOL_TOK_FOR = 58,                   /* TOK_FOR  */
  YYSYMBOL_TOK_WHILE = 59,                 /* TOK_WHILE  */
  YYSYMBOL_TOK_REPEAT = 60,                /* TOK_REPEAT  */
  YYSYMBOL_TOK_DPI_FUNCTION = 61,          /* TOK_DPI_FUNCTION  */
  YYSYMBOL_TOK_POSEDGE = 62,               /* TOK_POSEDGE  */
  YYSYMBOL_TOK_NEGEDGE = 63,               /* TOK_NEGEDGE  */
  YYSYMBOL_TOK_OR = 64,                    /* TOK_OR  */
  YYSYMBOL_TOK_AUTOMATIC = 65,             /* TOK_AUTOMATIC  */
  YYSYMBOL_TOK_CASE = 66,                  /* TOK_CASE  */
  YYSYMBOL_TOK_CASEX = 67,                 /* TOK_CASEX  */
  YYSYMBOL_TOK_CASEZ = 68,                 /* TOK_CASEZ  */
  YYSYMBOL_TOK_ENDCASE = 69,               /* TOK_ENDCASE  */
  YYSYMBOL_TOK_DEFAULT = 70,               /* TOK_DEFAULT  */
  YYSYMBOL_TOK_FUNCTION = 71,              /* TOK_FUNCTION  */
  YYSYMBOL_TOK_ENDFUNCTION = 72,           /* TOK_ENDFUNCTION  */
  YYSYMBOL_TOK_TASK = 73,                  /* TOK_TASK  */
  YYSYMBOL_TOK_ENDTASK = 74,               /* TOK_ENDTASK  */
  YYSYMBOL_TOK_SPECIFY = 75,               /* TOK_SPECIFY  */
  YYSYMBOL_TOK_IGNORED_SPECIFY = 76,       /* TOK_IGNORED_SPECIFY  */
  YYSYMBOL_TOK_ENDSPECIFY = 77,            /* TOK_ENDSPECIFY  */
  YYSYMBOL_TOK_SPECPARAM = 78,             /* TOK_SPECPARAM  */
  YYSYMBOL_TOK_SPECIFY_AND = 79,           /* TOK_SPECIFY_AND  */
  YYSYMBOL_TOK_IGNORED_SPECIFY_AND = 80,   /* TOK_IGNORED_SPECIFY_AND  */
  YYSYMBOL_TOK_GENERATE = 81,              /* TOK_GENERATE  */
  YYSYMBOL_TOK_ENDGENERATE = 82,           /* TOK_ENDGENERATE  */
  YYSYMBOL_TOK_GENVAR = 83,                /* TOK_GENVAR  */
  YYSYMBOL_TOK_REAL = 84,                  /* TOK_REAL  */
  YYSYMBOL_TOK_SYNOPSYS_FULL_CASE = 85,    /* TOK_SYNOPSYS_FULL_CASE  */
  YYSYMBOL_TOK_SYNOPSYS_PARALLEL_CASE = 86, /* TOK_SYNOPSYS_PARALLEL_CASE  */
  YYSYMBOL_TOK_SUPPLY0 = 87,               /* TOK_SUPPLY0  */
  YYSYMBOL_TOK_SUPPLY1 = 88,               /* TOK_SUPPLY1  */
  YYSYMBOL_TOK_TO_SIGNED = 89,             /* TOK_TO_SIGNED  */
  YYSYMBOL_TOK_TO_UNSIGNED = 90,           /* TOK_TO_UNSIGNED  */
  YYSYMBOL_TOK_POS_INDEXED = 91,           /* TOK_POS_INDEXED  */
  YYSYMBOL_TOK_NEG_INDEXED = 92,           /* TOK_NEG_INDEXED  */
  YYSYMBOL_TOK_PROPERTY = 93,              /* TOK_PROPERTY  */
  YYSYMBOL_TOK_ENUM = 94,                  /* TOK_ENUM  */
  YYSYMBOL_TOK_TYPEDEF = 95,               /* TOK_TYPEDEF  */
  YYSYMBOL_TOK_RAND = 96,                  /* TOK_RAND  */
  YYSYMBOL_TOK_CONST = 97,                 /* TOK_CONST  */
  YYSYMBOL_TOK_CHECKER = 98,               /* TOK_CHECKER  */
  YYSYMBOL_TOK_ENDCHECKER = 99,            /* TOK_ENDCHECKER  */
  YYSYMBOL_TOK_EVENTUALLY = 100,           /* TOK_EVENTUALLY  */
  YYSYMBOL_TOK_INCREMENT = 101,            /* TOK_INCREMENT  */
  YYSYMBOL_TOK_DECREMENT = 102,            /* TOK_DECREMENT  */
  YYSYMBOL_TOK_UNIQUE = 103,               /* TOK_UNIQUE  */
  YYSYMBOL_TOK_UNIQUE0 = 104,              /* TOK_UNIQUE0  */
  YYSYMBOL_TOK_PRIORITY = 105,             /* TOK_PRIORITY  */
  YYSYMBOL_TOK_STRUCT = 106,               /* TOK_STRUCT  */
  YYSYMBOL_TOK_PACKED = 107,               /* TOK_PACKED  */
  YYSYMBOL_TOK_UNSIGNED = 108,             /* TOK_UNSIGNED  */
  YYSYMBOL_TOK_INT = 109,                  /* TOK_INT  */
  YYSYMBOL_TOK_BYTE = 110,                 /* TOK_BYTE  */
  YYSYMBOL_TOK_SHORTINT = 111,             /* TOK_SHORTINT  */
  YYSYMBOL_TOK_LONGINT = 112,              /* TOK_LONGINT  */
  YYSYMBOL_TOK_VOID = 113,                 /* TOK_VOID  */
  YYSYMBOL_TOK_UNION = 114,                /* TOK_UNION  */
  YYSYMBOL_TOK_BIT_OR_ASSIGN = 115,        /* TOK_BIT_OR_ASSIGN  */
  YYSYMBOL_TOK_BIT_AND_ASSIGN = 116,       /* TOK_BIT_AND_ASSIGN  */
  YYSYMBOL_TOK_BIT_XOR_ASSIGN = 117,       /* TOK_BIT_XOR_ASSIGN  */
  YYSYMBOL_TOK_ADD_ASSIGN = 118,           /* TOK_ADD_ASSIGN  */
  YYSYMBOL_TOK_SUB_ASSIGN = 119,           /* TOK_SUB_ASSIGN  */
  YYSYMBOL_TOK_DIV_ASSIGN = 120,           /* TOK_DIV_ASSIGN  */
  YYSYMBOL_TOK_MOD_ASSIGN = 121,           /* TOK_MOD_ASSIGN  */
  YYSYMBOL_TOK_MUL_ASSIGN = 122,           /* TOK_MUL_ASSIGN  */
  YYSYMBOL_TOK_SHL_ASSIGN = 123,           /* TOK_SHL_ASSIGN  */
  YYSYMBOL_TOK_SHR_ASSIGN = 124,           /* TOK_SHR_ASSIGN  */
  YYSYMBOL_TOK_SSHL_ASSIGN = 125,          /* TOK_SSHL_ASSIGN  */
  YYSYMBOL_TOK_SSHR_ASSIGN = 126,          /* TOK_SSHR_ASSIGN  */
  YYSYMBOL_TOK_BIND = 127,                 /* TOK_BIND  */
  YYSYMBOL_TOK_TIME_SCALE = 128,           /* TOK_TIME_SCALE  */
  YYSYMBOL_OP_LOR = 129,                   /* OP_LOR  */
  YYSYMBOL_OP_LAND = 130,                  /* OP_LAND  */
  YYSYMBOL_131_ = 131,                     /* '|'  */
  YYSYMBOL_OP_NOR = 132,                   /* OP_NOR  */
  YYSYMBOL_133_ = 133,                     /* '^'  */
  YYSYMBOL_OP_XNOR = 134,                  /* OP_XNOR  */
  YYSYMBOL_135_ = 135,                     /* '&'  */
  YYSYMBOL_OP_NAND = 136,                  /* OP_NAND  */
  YYSYMBOL_OP_EQ = 137,                    /* OP_EQ  */
  YYSYMBOL_OP_NE = 138,                    /* OP_NE  */
  YYSYMBOL_OP_EQX = 139,                   /* OP_EQX  */
  YYSYMBOL_OP_NEX = 140,                   /* OP_NEX  */
  YYSYMBOL_141_ = 141,                     /* '<'  */
  YYSYMBOL_OP_LE = 142,                    /* OP_LE  */
  YYSYMBOL_OP_GE = 143,                    /* OP_GE  */
  YYSYMBOL_144_ = 144,                     /* '>'  */
  YYSYMBOL_OP_SHL = 145,                   /* OP_SHL  */
  YYSYMBOL_OP_SHR = 146,                   /* OP_SHR  */
  YYSYMBOL_OP_SSHL = 147,                  /* OP_SSHL  */
  YYSYMBOL_OP_SSHR = 148,                  /* OP_SSHR  */
  YYSYMBOL_149_ = 149,                     /* '+'  */
  YYSYMBOL_150_ = 150,                     /* '-'  */
  YYSYMBOL_151_ = 151,                     /* '*'  */
  YYSYMBOL_152_ = 152,                     /* '/'  */
  YYSYMBOL_153_ = 153,                     /* '%'  */
  YYSYMBOL_OP_POW = 154,                   /* OP_POW  */
  YYSYMBOL_OP_CAST = 155,                  /* OP_CAST  */
  YYSYMBOL_UNARY_OPS = 156,                /* UNARY_OPS  */
  YYSYMBOL_FAKE_THEN = 157,                /* FAKE_THEN  */
  YYSYMBOL_158_ = 158,                     /* ','  */
  YYSYMBOL_159_ = 159,                     /* '='  */
  YYSYMBOL_160_ = 160,                     /* '.'  */
  YYSYMBOL_161_ = 161,                     /* '('  */
  YYSYMBOL_162_ = 162,                     /* ')'  */
  YYSYMBOL_163_ = 163,                     /* ';'  */
  YYSYMBOL_164_ = 164,                     /* '#'  */
  YYSYMBOL_165_ = 165,                     /* ':'  */
  YYSYMBOL_166_ = 166,                     /* '['  */
  YYSYMBOL_167_ = 167,                     /* ']'  */
  YYSYMBOL_168_ = 168,                     /* '{'  */
  YYSYMBOL_169_ = 169,                     /* '}'  */
  YYSYMBOL_170_ = 170,                     /* '@'  */
  YYSYMBOL_171_ = 171,                     /* '?'  */
  YYSYMBOL_172_ = 172,                     /* '~'  */
  YYSYMBOL_173_ = 173,                     /* '!'  */
  YYSYMBOL_YYACCEPT = 174,                 /* $accept  */
  YYSYMBOL_input = 175,                    /* input  */
  YYSYMBOL_176_1 = 176,                    /* $@1  */
  YYSYMBOL_design = 177,                   /* design  */
  YYSYMBOL_attr = 178,                     /* attr  */
  YYSYMBOL_179_2 = 179,                    /* $@2  */
  YYSYMBOL_attr_opt = 180,                 /* attr_opt  */
  YYSYMBOL_defattr = 181,                  /* defattr  */
  YYSYMBOL_182_3 = 182,                    /* $@3  */
  YYSYMBOL_183_4 = 183,                    /* $@4  */
  YYSYMBOL_opt_attr_list = 184,            /* opt_attr_list  */
  YYSYMBOL_attr_list = 185,                /* attr_list  */
  YYSYMBOL_attr_assign = 186,              /* attr_assign  */
  YYSYMBOL_hierarchical_id = 187,          /* hierarchical_id  */
  YYSYMBOL_hierarchical_type_id = 188,     /* hierarchical_type_id  */
  YYSYMBOL_module = 189,                   /* module  */
  YYSYMBOL_190_5 = 190,                    /* $@5  */
  YYSYMBOL_191_6 = 191,                    /* $@6  */
  YYSYMBOL_module_para_opt = 192,          /* module_para_opt  */
  YYSYMBOL_193_7 = 193,                    /* $@7  */
  YYSYMBOL_194_8 = 194,                    /* $@8  */
  YYSYMBOL_module_para_list = 195,         /* module_para_list  */
  YYSYMBOL_single_module_para = 196,       /* single_module_para  */
  YYSYMBOL_197_9 = 197,                    /* $@9  */
  YYSYMBOL_198_10 = 198,                   /* $@10  */
  YYSYMBOL_module_args_opt = 199,          /* module_args_opt  */
  YYSYMBOL_module_args = 200,              /* module_args  */
  YYSYMBOL_optional_comma = 201,           /* optional_comma  */
  YYSYMBOL_module_arg_opt_assignment = 202, /* module_arg_opt_assignment  */
  YYSYMBOL_module_arg = 203,               /* module_arg  */
  YYSYMBOL_204_11 = 204,                   /* $@11  */
  YYSYMBOL_205_12 = 205,                   /* $@12  */
  YYSYMBOL_206_13 = 206,                   /* $@13  */
  YYSYMBOL_207_14 = 207,                   /* $@14  */
  YYSYMBOL_package = 208,                  /* package  */
  YYSYMBOL_209_15 = 209,                   /* $@15  */
  YYSYMBOL_210_16 = 210,                   /* $@16  */
  YYSYMBOL_package_body = 211,             /* package_body  */
  YYSYMBOL_package_body_stmt = 212,        /* package_body_stmt  */
  YYSYMBOL_interface = 213,                /* interface  */
  YYSYMBOL_214_17 = 214,                   /* $@17  */
  YYSYMBOL_215_18 = 215,                   /* $@18  */
  YYSYMBOL_interface_body = 216,           /* interface_body  */
  YYSYMBOL_interface_body_stmt = 217,      /* interface_body_stmt  */
  YYSYMBOL_bind_directive = 218,           /* bind_directive  */
  YYSYMBOL_219_19 = 219,                   /* $@19  */
  YYSYMBOL_220_20 = 220,                   /* $@20  */
  YYSYMBOL_221_21 = 221,                   /* $@21  */
  YYSYMBOL_bind_target = 222,              /* bind_target  */
  YYSYMBOL_opt_bind_target_instance_list = 223, /* opt_bind_target_instance_list  */
  YYSYMBOL_bind_target_instance_list = 224, /* bind_target_instance_list  */
  YYSYMBOL_bind_target_instance = 225,     /* bind_target_instance  */
  YYSYMBOL_mintypmax_expr = 226,           /* mintypmax_expr  */
  YYSYMBOL_non_opt_delay = 227,            /* non_opt_delay  */
  YYSYMBOL_delay = 228,                    /* delay  */
  YYSYMBOL_io_wire_type = 229,             /* io_wire_type  */
  YYSYMBOL_230_22 = 230,                   /* $@22  */
  YYSYMBOL_non_io_wire_type = 231,         /* non_io_wire_type  */
  YYSYMBOL_232_23 = 232,                   /* $@23  */
  YYSYMBOL_wire_type = 233,                /* wire_type  */
  YYSYMBOL_wire_type_token_io = 234,       /* wire_type_token_io  */
  YYSYMBOL_wire_type_signedness = 235,     /* wire_type_signedness  */
  YYSYMBOL_wire_type_const_rand = 236,     /* wire_type_const_rand  */
  YYSYMBOL_opt_wire_type_token = 237,      /* opt_wire_type_token  */
  YYSYMBOL_wire_type_token = 238,          /* wire_type_token  */
  YYSYMBOL_net_type = 239,                 /* net_type  */
  YYSYMBOL_logic_type = 240,               /* logic_type  */
  YYSYMBOL_integer_atom_type = 241,        /* integer_atom_type  */
  YYSYMBOL_integer_vector_type = 242,      /* integer_vector_type  */
  YYSYMBOL_non_opt_range = 243,            /* non_opt_range  */
  YYSYMBOL_non_opt_multirange = 244,       /* non_opt_multirange  */
  YYSYMBOL_range = 245,                    /* range  */
  YYSYMBOL_range_or_multirange = 246,      /* range_or_multirange  */
  YYSYMBOL_module_body = 247,              /* module_body  */
  YYSYMBOL_module_body_stmt = 248,         /* module_body_stmt  */
  YYSYMBOL_checker_decl = 249,             /* checker_decl  */
  YYSYMBOL_250_24 = 250,                   /* $@24  */
  YYSYMBOL_task_func_decl = 251,           /* task_func_decl  */
  YYSYMBOL_252_25 = 252,                   /* $@25  */
  YYSYMBOL_253_26 = 253,                   /* $@26  */
  YYSYMBOL_254_27 = 254,                   /* $@27  */
  YYSYMBOL_255_28 = 255,                   /* $@28  */
  YYSYMBOL_256_29 = 256,                   /* $@29  */
  YYSYMBOL_257_30 = 257,                   /* $@30  */
  YYSYMBOL_func_return_type = 258,         /* func_return_type  */
  YYSYMBOL_opt_type_vec = 259,             /* opt_type_vec  */
  YYSYMBOL_opt_signedness_default_signed = 260, /* opt_signedness_default_signed  */
  YYSYMBOL_opt_signedness_default_unsigned = 261, /* opt_signedness_default_unsigned  */
  YYSYMBOL_dpi_function_arg = 262,         /* dpi_function_arg  */
  YYSYMBOL_opt_dpi_function_args = 263,    /* opt_dpi_function_args  */
  YYSYMBOL_dpi_function_args = 264,        /* dpi_function_args  */
  YYSYMBOL_opt_automatic = 265,            /* opt_automatic  */
  YYSYMBOL_task_func_args_opt = 266,       /* task_func_args_opt  */
  YYSYMBOL_267_31 = 267,                   /* $@31  */
  YYSYMBOL_268_32 = 268,                   /* $@32  */
  YYSYMBOL_task_func_args = 269,           /* task_func_args  */
  YYSYMBOL_task_func_port = 270,           /* task_func_port  */
  YYSYMBOL_271_33 = 271,                   /* $@33  */
  YYSYMBOL_272_34 = 272,                   /* $@34  */
  YYSYMBOL_task_func_body = 273,           /* task_func_body  */
  YYSYMBOL_specify_block = 274,            /* specify_block  */
  YYSYMBOL_specify_item_list = 275,        /* specify_item_list  */
  YYSYMBOL_specify_item = 276,             /* specify_item  */
  YYSYMBOL_specify_opt_triple = 277,       /* specify_opt_triple  */
  YYSYMBOL_specify_if = 278,               /* specify_if  */
  YYSYMBOL_specify_condition = 279,        /* specify_condition  */
  YYSYMBOL_specify_target = 280,           /* specify_target  */
  YYSYMBOL_specify_edge = 281,             /* specify_edge  */
  YYSYMBOL_specify_rise_fall = 282,        /* specify_rise_fall  */
  YYSYMBOL_specify_triple = 283,           /* specify_triple  */
  YYSYMBOL_ignored_specify_block = 284,    /* ignored_specify_block  */
  YYSYMBOL_ignored_specify_item_opt = 285, /* ignored_specify_item_opt  */
  YYSYMBOL_ignored_specify_item = 286,     /* ignored_specify_item  */
  YYSYMBOL_specparam_declaration = 287,    /* specparam_declaration  */
  YYSYMBOL_specparam_range = 288,          /* specparam_range  */
  YYSYMBOL_list_of_specparam_assignments = 289, /* list_of_specparam_assignments  */
  YYSYMBOL_specparam_assignment = 290,     /* specparam_assignment  */
  YYSYMBOL_ignspec_opt_cond = 291,         /* ignspec_opt_cond  */
  YYSYMBOL_path_declaration = 292,         /* path_declaration  */
  YYSYMBOL_simple_path_declaration = 293,  /* simple_path_declaration  */
  YYSYMBOL_path_delay_value = 294,         /* path_delay_value  */
  YYSYMBOL_list_of_path_delay_extra_expressions = 295, /* list_of_path_delay_extra_expressions  */
  YYSYMBOL_specify_edge_identifier = 296,  /* specify_edge_identifier  */
  YYSYMBOL_parallel_path_description = 297, /* parallel_path_description  */
  YYSYMBOL_full_path_description = 298,    /* full_path_description  */
  YYSYMBOL_list_of_path_inputs = 299,      /* list_of_path_inputs  */
  YYSYMBOL_more_path_inputs = 300,         /* more_path_inputs  */
  YYSYMBOL_list_of_path_outputs = 301,     /* list_of_path_outputs  */
  YYSYMBOL_opt_polarity_operator = 302,    /* opt_polarity_operator  */
  YYSYMBOL_specify_input_terminal_descriptor = 303, /* specify_input_terminal_descriptor  */
  YYSYMBOL_specify_output_terminal_descriptor = 304, /* specify_output_terminal_descriptor  */
  YYSYMBOL_system_timing_declaration = 305, /* system_timing_declaration  */
  YYSYMBOL_system_timing_arg = 306,        /* system_timing_arg  */
  YYSYMBOL_system_timing_args = 307,       /* system_timing_args  */
  YYSYMBOL_ignspec_constant_expression = 308, /* ignspec_constant_expression  */
  YYSYMBOL_ignspec_expr = 309,             /* ignspec_expr  */
  YYSYMBOL_ignspec_id = 310,               /* ignspec_id  */
  YYSYMBOL_311_35 = 311,                   /* $@35  */
  YYSYMBOL_param_signed = 312,             /* param_signed  */
  YYSYMBOL_param_integer = 313,            /* param_integer  */
  YYSYMBOL_param_real = 314,               /* param_real  */
  YYSYMBOL_param_range = 315,              /* param_range  */
  YYSYMBOL_param_integer_type = 316,       /* param_integer_type  */
  YYSYMBOL_param_range_type = 317,         /* param_range_type  */
  YYSYMBOL_param_implicit_type = 318,      /* param_implicit_type  */
  YYSYMBOL_param_type = 319,               /* param_type  */
  YYSYMBOL_param_decl = 320,               /* param_decl  */
  YYSYMBOL_321_36 = 321,                   /* $@36  */
  YYSYMBOL_localparam_decl = 322,          /* localparam_decl  */
  YYSYMBOL_323_37 = 323,                   /* $@37  */
  YYSYMBOL_param_decl_list = 324,          /* param_decl_list  */
  YYSYMBOL_single_param_decl = 325,        /* single_param_decl  */
  YYSYMBOL_single_param_decl_ident = 326,  /* single_param_decl_ident  */
  YYSYMBOL_defparam_decl = 327,            /* defparam_decl  */
  YYSYMBOL_defparam_decl_list = 328,       /* defparam_decl_list  */
  YYSYMBOL_single_defparam_decl = 329,     /* single_defparam_decl  */
  YYSYMBOL_enum_type = 330,                /* enum_type  */
  YYSYMBOL_331_38 = 331,                   /* $@38  */
  YYSYMBOL_enum_base_type = 332,           /* enum_base_type  */
  YYSYMBOL_type_atom = 333,                /* type_atom  */
  YYSYMBOL_type_vec = 334,                 /* type_vec  */
  YYSYMBOL_type_signing = 335,             /* type_signing  */
  YYSYMBOL_enum_name_list = 336,           /* enum_name_list  */
  YYSYMBOL_enum_name_decl = 337,           /* enum_name_decl  */
  YYSYMBOL_opt_enum_init = 338,            /* opt_enum_init  */
  YYSYMBOL_enum_var_list = 339,            /* enum_var_list  */
  YYSYMBOL_enum_var = 340,                 /* enum_var  */
  YYSYMBOL_enum_decl = 341,                /* enum_decl  */
  YYSYMBOL_struct_decl = 342,              /* struct_decl  */
  YYSYMBOL_343_39 = 343,                   /* $@39  */
  YYSYMBOL_struct_type = 344,              /* struct_type  */
  YYSYMBOL_345_40 = 345,                   /* $@40  */
  YYSYMBOL_struct_union = 346,             /* struct_union  */
  YYSYMBOL_struct_body = 347,              /* struct_body  */
  YYSYMBOL_opt_packed = 348,               /* opt_packed  */
  YYSYMBOL_opt_signed_struct = 349,        /* opt_signed_struct  */
  YYSYMBOL_struct_member_list = 350,       /* struct_member_list  */
  YYSYMBOL_struct_member = 351,            /* struct_member  */
  YYSYMBOL_member_name_list = 352,         /* member_name_list  */
  YYSYMBOL_member_name = 353,              /* member_name  */
  YYSYMBOL_354_41 = 354,                   /* $@41  */
  YYSYMBOL_struct_member_type = 355,       /* struct_member_type  */
  YYSYMBOL_356_42 = 356,                   /* $@42  */
  YYSYMBOL_member_type_token = 357,        /* member_type_token  */
  YYSYMBOL_358_43 = 358,                   /* $@43  */
  YYSYMBOL_359_44 = 359,                   /* $@44  */
  YYSYMBOL_member_type = 360,              /* member_type  */
  YYSYMBOL_struct_var_list = 361,          /* struct_var_list  */
  YYSYMBOL_struct_var = 362,               /* struct_var  */
  YYSYMBOL_wire_decl = 363,                /* wire_decl  */
  YYSYMBOL_364_45 = 364,                   /* $@45  */
  YYSYMBOL_365_46 = 365,                   /* $@46  */
  YYSYMBOL_366_47 = 366,                   /* $@47  */
  YYSYMBOL_367_48 = 367,                   /* $@48  */
  YYSYMBOL_opt_supply_wires = 368,         /* opt_supply_wires  */
  YYSYMBOL_wire_name_list = 369,           /* wire_name_list  */
  YYSYMBOL_wire_name_and_opt_assign = 370, /* wire_name_and_opt_assign  */
  YYSYMBOL_wire_name = 371,                /* wire_name  */
  YYSYMBOL_assign_stmt = 372,              /* assign_stmt  */
  YYSYMBOL_assign_expr_list = 373,         /* assign_expr_list  */
  YYSYMBOL_assign_expr = 374,              /* assign_expr  */
  YYSYMBOL_type_name = 375,                /* type_name  */
  YYSYMBOL_typedef_decl = 376,             /* typedef_decl  */
  YYSYMBOL_typedef_base_type = 377,        /* typedef_base_type  */
  YYSYMBOL_enum_struct_type = 378,         /* enum_struct_type  */
  YYSYMBOL_cell_stmt = 379,                /* cell_stmt  */
  YYSYMBOL_380_49 = 380,                   /* $@49  */
  YYSYMBOL_381_50 = 381,                   /* $@50  */
  YYSYMBOL_tok_prim_wrapper = 382,         /* tok_prim_wrapper  */
  YYSYMBOL_cell_list = 383,                /* cell_list  */
  YYSYMBOL_single_cell = 384,              /* single_cell  */
  YYSYMBOL_single_cell_no_array = 385,     /* single_cell_no_array  */
  YYSYMBOL_386_51 = 386,                   /* $@51  */
  YYSYMBOL_single_cell_arraylist = 387,    /* single_cell_arraylist  */
  YYSYMBOL_388_52 = 388,                   /* $@52  */
  YYSYMBOL_cell_list_no_array = 389,       /* cell_list_no_array  */
  YYSYMBOL_prim_list = 390,                /* prim_list  */
  YYSYMBOL_single_prim = 391,              /* single_prim  */
  YYSYMBOL_392_53 = 392,                   /* $@53  */
  YYSYMBOL_cell_parameter_list_opt = 393,  /* cell_parameter_list_opt  */
  YYSYMBOL_cell_parameter_list = 394,      /* cell_parameter_list  */
  YYSYMBOL_cell_parameter = 395,           /* cell_parameter  */
  YYSYMBOL_cell_port_list = 396,           /* cell_port_list  */
  YYSYMBOL_cell_port_list_rules = 397,     /* cell_port_list_rules  */
  YYSYMBOL_cell_port = 398,                /* cell_port  */
  YYSYMBOL_always_comb_or_latch = 399,     /* always_comb_or_latch  */
  YYSYMBOL_always_or_always_ff = 400,      /* always_or_always_ff  */
  YYSYMBOL_always_stmt = 401,              /* always_stmt  */
  YYSYMBOL_402_54 = 402,                   /* $@54  */
  YYSYMBOL_403_55 = 403,                   /* $@55  */
  YYSYMBOL_404_56 = 404,                   /* $@56  */
  YYSYMBOL_405_57 = 405,                   /* $@57  */
  YYSYMBOL_always_cond = 406,              /* always_cond  */
  YYSYMBOL_always_events = 407,            /* always_events  */
  YYSYMBOL_always_event = 408,             /* always_event  */
  YYSYMBOL_opt_label = 409,                /* opt_label  */
  YYSYMBOL_opt_sva_label = 410,            /* opt_sva_label  */
  YYSYMBOL_opt_property = 411,             /* opt_property  */
  YYSYMBOL_modport_stmt = 412,             /* modport_stmt  */
  YYSYMBOL_413_58 = 413,                   /* $@58  */
  YYSYMBOL_414_59 = 414,                   /* $@59  */
  YYSYMBOL_modport_args_opt = 415,         /* modport_args_opt  */
  YYSYMBOL_modport_args = 416,             /* modport_args  */
  YYSYMBOL_modport_arg = 417,              /* modport_arg  */
  YYSYMBOL_modport_member = 418,           /* modport_member  */
  YYSYMBOL_modport_type_token = 419,       /* modport_type_token  */
  YYSYMBOL_assert = 420,                   /* assert  */
  YYSYMBOL_assert_property = 421,          /* assert_property  */
  YYSYMBOL_simple_behavioral_stmt = 422,   /* simple_behavioral_stmt  */
  YYSYMBOL_asgn_binop = 423,               /* asgn_binop  */
  YYSYMBOL_for_initialization = 424,       /* for_initialization  */
  YYSYMBOL_behavioral_stmt = 425,          /* behavioral_stmt  */
  YYSYMBOL_426_60 = 426,                   /* $@60  */
  YYSYMBOL_427_61 = 427,                   /* $@61  */
  YYSYMBOL_428_62 = 428,                   /* $@62  */
  YYSYMBOL_429_63 = 429,                   /* $@63  */
  YYSYMBOL_430_64 = 430,                   /* $@64  */
  YYSYMBOL_431_65 = 431,                   /* $@65  */
  YYSYMBOL_432_66 = 432,                   /* $@66  */
  YYSYMBOL_433_67 = 433,                   /* $@67  */
  YYSYMBOL_434_68 = 434,                   /* $@68  */
  YYSYMBOL_435_69 = 435,                   /* $@69  */
  YYSYMBOL_436_70 = 436,                   /* $@70  */
  YYSYMBOL_437_71 = 437,                   /* $@71  */
  YYSYMBOL_case_attr = 438,                /* case_attr  */
  YYSYMBOL_case_type = 439,                /* case_type  */
  YYSYMBOL_opt_synopsys_attr = 440,        /* opt_synopsys_attr  */
  YYSYMBOL_behavioral_stmt_list = 441,     /* behavioral_stmt_list  */
  YYSYMBOL_optional_else = 442,            /* optional_else  */
  YYSYMBOL_443_72 = 443,                   /* $@72  */
  YYSYMBOL_case_body = 444,                /* case_body  */
  YYSYMBOL_case_item = 445,                /* case_item  */
  YYSYMBOL_446_73 = 446,                   /* $@73  */
  YYSYMBOL_447_74 = 447,                   /* $@74  */
  YYSYMBOL_gen_case_body = 448,            /* gen_case_body  */
  YYSYMBOL_gen_case_item = 449,            /* gen_case_item  */
  YYSYMBOL_450_75 = 450,                   /* $@75  */
  YYSYMBOL_451_76 = 451,                   /* $@76  */
  YYSYMBOL_case_select = 452,              /* case_select  */
  YYSYMBOL_case_expr_list = 453,           /* case_expr_list  */
  YYSYMBOL_rvalue = 454,                   /* rvalue  */
  YYSYMBOL_lvalue = 455,                   /* lvalue  */
  YYSYMBOL_lvalue_concat_list = 456,       /* lvalue_concat_list  */
  YYSYMBOL_opt_arg_list = 457,             /* opt_arg_list  */
  YYSYMBOL_arg_list = 458,                 /* arg_list  */
  YYSYMBOL_arg_list2 = 459,                /* arg_list2  */
  YYSYMBOL_single_arg = 460,               /* single_arg  */
  YYSYMBOL_module_gen_body = 461,          /* module_gen_body  */
  YYSYMBOL_gen_stmt_or_module_body_stmt = 462, /* gen_stmt_or_module_body_stmt  */
  YYSYMBOL_genvar_identifier = 463,        /* genvar_identifier  */
  YYSYMBOL_genvar_initialization = 464,    /* genvar_initialization  */
  YYSYMBOL_gen_stmt = 465,                 /* gen_stmt  */
  YYSYMBOL_466_77 = 466,                   /* $@77  */
  YYSYMBOL_467_78 = 467,                   /* $@78  */
  YYSYMBOL_468_79 = 468,                   /* $@79  */
  YYSYMBOL_469_80 = 469,                   /* $@80  */
  YYSYMBOL_470_81 = 470,                   /* $@81  */
  YYSYMBOL_gen_block = 471,                /* gen_block  */
  YYSYMBOL_472_82 = 472,                   /* $@82  */
  YYSYMBOL_473_83 = 473,                   /* $@83  */
  YYSYMBOL_gen_stmt_block = 474,           /* gen_stmt_block  */
  YYSYMBOL_475_84 = 475,                   /* $@84  */
  YYSYMBOL_opt_gen_else = 476,             /* opt_gen_else  */
  YYSYMBOL_expr = 477,                     /* expr  */
  YYSYMBOL_basic_expr = 478,               /* basic_expr  */
  YYSYMBOL_479_85 = 479,                   /* $@85  */
  YYSYMBOL_concat_list = 480,              /* concat_list  */
  YYSYMBOL_integral_number = 481           /* integral_number  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if 1

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
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
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
# define YYCOPY_NEEDED 1
#endif /* 1 */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined FRONTEND_VERILOG_YYLTYPE_IS_TRIVIAL && FRONTEND_VERILOG_YYLTYPE_IS_TRIVIAL \
             && defined FRONTEND_VERILOG_YYSTYPE_IS_TRIVIAL && FRONTEND_VERILOG_YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   2898

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  174
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  308
/* YYNRULES -- Number of rules.  */
#define YYNRULES  695
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1341

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   402


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   173,     2,   164,     2,   153,   135,     2,
     161,   162,   151,   149,   158,   150,   160,   152,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   165,   163,
     141,   159,   144,   171,   170,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   166,     2,   167,   133,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   168,   131,   169,   172,     2,     2,     2,
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
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   132,   134,   136,   137,
     138,   139,   140,   142,   143,   145,   146,   147,   148,   154,
     155,   156,   157
};

#if FRONTEND_VERILOG_YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   396,   396,   396,   408,   409,   410,   411,   412,   413,
     414,   415,   416,   417,   420,   420,   436,   439,   442,   449,
     442,   460,   460,   463,   464,   467,   473,   481,   484,   492,
     502,   503,   504,   508,   510,   508,   535,   535,   535,   535,
     538,   538,   541,   542,   542,   548,   548,   554,   557,   557,
     557,   560,   560,   563,   563,   566,   584,   587,   587,   601,
     606,   601,   616,   616,   632,   637,   639,   637,   656,   656,
     659,   659,   659,   659,   662,   664,   662,   685,   685,   688,
     688,   688,   688,   688,   688,   688,   689,   689,   692,   697,
     701,   692,   729,   734,   735,   738,   739,   744,   752,   753,
     756,   757,   758,   760,   761,   762,   763,   764,   767,   767,
     770,   770,   775,   775,   780,   781,   784,   787,   790,   796,
     797,   798,   801,   805,   808,   811,   814,   814,   818,   820,
     823,   826,   830,   833,   836,   839,   848,   851,   854,   857,
     859,   864,   869,   870,   871,   872,   873,   876,   877,   880,
     885,   891,   897,   903,   906,   912,   915,   920,   921,   924,
     926,   927,   928,   929,   932,   932,   932,   932,   932,   932,
     932,   932,   932,   932,   933,   933,   933,   934,   934,   934,
     934,   934,   934,   937,   937,   948,   948,   958,   958,   969,
     969,   981,   981,   993,   993,  1010,  1010,  1035,  1040,  1043,
    1047,  1052,  1053,  1054,  1058,  1059,  1060,  1063,  1064,  1065,
    1069,  1074,  1080,  1081,  1084,  1085,  1086,  1087,  1090,  1091,
    1094,  1094,  1094,  1098,  1094,  1106,  1106,  1109,  1109,  1130,
    1130,  1144,  1145,  1150,  1153,  1154,  1157,  1241,  1315,  1318,
    1323,  1326,  1331,  1334,  1339,  1345,  1351,  1357,  1365,  1366,
    1367,  1370,  1378,  1385,  1394,  1406,  1426,  1432,  1442,  1443,
    1446,  1447,  1450,  1453,  1454,  1458,  1459,  1465,  1468,  1468,
    1471,  1474,  1474,  1477,  1483,  1484,  1488,  1489,  1490,  1494,
    1495,  1499,  1499,  1502,  1503,  1504,  1507,  1508,  1509,  1513,
    1514,  1517,  1518,  1521,  1522,  1525,  1525,  1525,  1529,  1533,
    1536,  1539,  1540,  1541,  1544,  1545,  1546,  1553,  1556,  1557,
    1564,  1564,  1570,  1572,  1574,  1577,  1582,  1587,  1593,  1595,
    1598,  1601,  1604,  1604,  1604,  1604,  1605,  1610,  1610,  1619,
    1619,  1628,  1628,  1631,  1637,  1650,  1667,  1670,  1670,  1673,
    1686,  1686,  1709,  1710,  1711,  1715,  1721,  1722,  1726,  1727,
    1728,  1731,  1732,  1736,  1751,  1752,  1756,  1757,  1760,  1772,
    1780,  1780,  1787,  1787,  1791,  1792,  1795,  1799,  1800,  1803,
    1804,  1805,  1808,  1809,  1812,  1816,  1817,  1820,  1820,  1829,
    1829,  1833,  1834,  1837,  1839,  1837,  1851,  1852,  1855,  1856,
    1859,  1872,  1876,  1872,  1882,  1882,  1890,  1890,  1900,  1901,
    1912,  1912,  1915,  1957,  1985,  2031,  2034,  2034,  2037,  2043,
    2044,  2048,  2061,  2065,  2070,  2079,  2088,  2089,  2093,  2093,
    2102,  2102,  2112,  2115,  2120,  2121,  2124,  2124,  2127,  2127,
    2138,  2138,  2149,  2150,  2153,  2154,  2157,  2158,  2158,  2166,
    2166,  2169,  2169,  2172,  2173,  2178,  2181,  2190,  2217,  2217,
    2220,  2225,  2231,  2239,  2246,  2255,  2263,  2266,  2271,  2274,
    2279,  2286,  2279,  2299,  2299,  2315,  2315,  2329,  2330,  2331,
    2332,  2333,  2334,  2337,  2338,  2339,  2342,  2348,  2354,  2361,
    2364,  2369,  2372,  2377,  2380,  2383,  2388,  2394,  2388,  2400,
    2400,  2403,  2403,  2406,  2407,  2410,  2420,  2420,  2423,  2436,
    2449,  2462,  2475,  2484,  2493,  2502,  2517,  2534,  2543,  2552,
    2561,  2570,  2579,  2592,  2607,  2613,  2619,  2625,  2631,  2647,
    2648,  2649,  2650,  2651,  2652,  2653,  2654,  2655,  2656,  2657,
    2658,  2661,  2669,  2672,  2710,  2710,  2710,  2710,  2710,  2710,
    2711,  2712,  2713,  2716,  2716,  2727,  2727,  2738,  2740,  2738,
    2765,  2770,  2772,  2765,  2783,  2783,  2797,  2797,  2811,  2822,
    2811,  2829,  2829,  2842,  2845,  2849,  2853,  2860,  2863,  2866,
    2871,  2875,  2879,  2882,  2883,  2886,  2886,  2897,  2900,  2901,
    2904,  2910,  2904,  2923,  2924,  2927,  2933,  2927,  2942,  2943,
    2946,  2951,  2958,  2961,  2966,  2972,  2982,  2990,  2993,  2998,
    3002,  3008,  3009,  3012,  3013,  3016,  3017,  3020,  3025,  3026,
    3027,  3030,  3030,  3031,  3036,  3043,  3046,  3062,  3070,  3074,
    3070,  3081,  3081,  3090,  3090,  3099,  3099,  3111,  3113,  3111,
    3129,  3129,  3136,  3139,  3139,  3142,  3145,  3155,  3158,  3168,
    3182,  3189,  3201,  3206,  3206,  3217,  3221,  3225,  3228,  3233,
    3236,  3239,  3244,  3249,  3254,  3259,  3264,  3269,  3274,  3279,
    3285,  3290,  3297,  3302,  3307,  3312,  3317,  3322,  3327,  3332,
    3337,  3342,  3347,  3352,  3357,  3362,  3367,  3372,  3377,  3382,
    3387,  3392,  3397,  3402,  3407,  3412,  3417,  3422,  3428,  3434,
    3442,  3445,  3451,  3452,  3453,  3458
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if 1
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "TOK_STRING", "TOK_ID",
  "TOK_CONSTVAL", "TOK_REALVAL", "TOK_PRIMITIVE", "TOK_SVA_LABEL",
  "TOK_SPECIFY_OPER", "TOK_MSG_TASKS", "TOK_BASE", "TOK_BASED_CONSTVAL",
  "TOK_UNBASED_UNSIZED_CONSTVAL", "TOK_USER_TYPE", "TOK_PKG_USER_TYPE",
  "TOK_ASSERT", "TOK_ASSUME", "TOK_RESTRICT", "TOK_COVER", "TOK_FINAL",
  "ATTR_BEGIN", "ATTR_END", "DEFATTR_BEGIN", "DEFATTR_END", "TOK_MODULE",
  "TOK_ENDMODULE", "TOK_PARAMETER", "TOK_LOCALPARAM", "TOK_DEFPARAM",
  "TOK_PACKAGE", "TOK_ENDPACKAGE", "TOK_PACKAGESEP", "TOK_INTERFACE",
  "TOK_ENDINTERFACE", "TOK_MODPORT", "TOK_VAR", "TOK_WILDCARD_CONNECT",
  "TOK_INPUT", "TOK_OUTPUT", "TOK_INOUT", "TOK_WIRE", "TOK_WAND",
  "TOK_WOR", "TOK_REG", "TOK_LOGIC", "TOK_INTEGER", "TOK_SIGNED",
  "TOK_ASSIGN", "TOK_ALWAYS", "TOK_INITIAL", "TOK_ALWAYS_FF",
  "TOK_ALWAYS_COMB", "TOK_ALWAYS_LATCH", "TOK_BEGIN", "TOK_END", "TOK_IF",
  "TOK_ELSE", "TOK_FOR", "TOK_WHILE", "TOK_REPEAT", "TOK_DPI_FUNCTION",
  "TOK_POSEDGE", "TOK_NEGEDGE", "TOK_OR", "TOK_AUTOMATIC", "TOK_CASE",
  "TOK_CASEX", "TOK_CASEZ", "TOK_ENDCASE", "TOK_DEFAULT", "TOK_FUNCTION",
  "TOK_ENDFUNCTION", "TOK_TASK", "TOK_ENDTASK", "TOK_SPECIFY",
  "TOK_IGNORED_SPECIFY", "TOK_ENDSPECIFY", "TOK_SPECPARAM",
  "TOK_SPECIFY_AND", "TOK_IGNORED_SPECIFY_AND", "TOK_GENERATE",
  "TOK_ENDGENERATE", "TOK_GENVAR", "TOK_REAL", "TOK_SYNOPSYS_FULL_CASE",
  "TOK_SYNOPSYS_PARALLEL_CASE", "TOK_SUPPLY0", "TOK_SUPPLY1",
  "TOK_TO_SIGNED", "TOK_TO_UNSIGNED", "TOK_POS_INDEXED", "TOK_NEG_INDEXED",
  "TOK_PROPERTY", "TOK_ENUM", "TOK_TYPEDEF", "TOK_RAND", "TOK_CONST",
  "TOK_CHECKER", "TOK_ENDCHECKER", "TOK_EVENTUALLY", "TOK_INCREMENT",
  "TOK_DECREMENT", "TOK_UNIQUE", "TOK_UNIQUE0", "TOK_PRIORITY",
  "TOK_STRUCT", "TOK_PACKED", "TOK_UNSIGNED", "TOK_INT", "TOK_BYTE",
  "TOK_SHORTINT", "TOK_LONGINT", "TOK_VOID", "TOK_UNION",
  "TOK_BIT_OR_ASSIGN", "TOK_BIT_AND_ASSIGN", "TOK_BIT_XOR_ASSIGN",
  "TOK_ADD_ASSIGN", "TOK_SUB_ASSIGN", "TOK_DIV_ASSIGN", "TOK_MOD_ASSIGN",
  "TOK_MUL_ASSIGN", "TOK_SHL_ASSIGN", "TOK_SHR_ASSIGN", "TOK_SSHL_ASSIGN",
  "TOK_SSHR_ASSIGN", "TOK_BIND", "TOK_TIME_SCALE", "OP_LOR", "OP_LAND",
  "'|'", "OP_NOR", "'^'", "OP_XNOR", "'&'", "OP_NAND", "OP_EQ", "OP_NE",
  "OP_EQX", "OP_NEX", "'<'", "OP_LE", "OP_GE", "'>'", "OP_SHL", "OP_SHR",
  "OP_SSHL", "OP_SSHR", "'+'", "'-'", "'*'", "'/'", "'%'", "OP_POW",
  "OP_CAST", "UNARY_OPS", "FAKE_THEN", "','", "'='", "'.'", "'('", "')'",
  "';'", "'#'", "':'", "'['", "']'", "'{'", "'}'", "'@'", "'?'", "'~'",
  "'!'", "$accept", "input", "$@1", "design", "attr", "$@2", "attr_opt",
  "defattr", "$@3", "$@4", "opt_attr_list", "attr_list", "attr_assign",
  "hierarchical_id", "hierarchical_type_id", "module", "$@5", "$@6",
  "module_para_opt", "$@7", "$@8", "module_para_list",
  "single_module_para", "$@9", "$@10", "module_args_opt", "module_args",
  "optional_comma", "module_arg_opt_assignment", "module_arg", "$@11",
  "$@12", "$@13", "$@14", "package", "$@15", "$@16", "package_body",
  "package_body_stmt", "interface", "$@17", "$@18", "interface_body",
  "interface_body_stmt", "bind_directive", "$@19", "$@20", "$@21",
  "bind_target", "opt_bind_target_instance_list",
  "bind_target_instance_list", "bind_target_instance", "mintypmax_expr",
  "non_opt_delay", "delay", "io_wire_type", "$@22", "non_io_wire_type",
  "$@23", "wire_type", "wire_type_token_io", "wire_type_signedness",
  "wire_type_const_rand", "opt_wire_type_token", "wire_type_token",
  "net_type", "logic_type", "integer_atom_type", "integer_vector_type",
  "non_opt_range", "non_opt_multirange", "range", "range_or_multirange",
  "module_body", "module_body_stmt", "checker_decl", "$@24",
  "task_func_decl", "$@25", "$@26", "$@27", "$@28", "$@29", "$@30",
  "func_return_type", "opt_type_vec", "opt_signedness_default_signed",
  "opt_signedness_default_unsigned", "dpi_function_arg",
  "opt_dpi_function_args", "dpi_function_args", "opt_automatic",
  "task_func_args_opt", "$@31", "$@32", "task_func_args", "task_func_port",
  "$@33", "$@34", "task_func_body", "specify_block", "specify_item_list",
  "specify_item", "specify_opt_triple", "specify_if", "specify_condition",
  "specify_target", "specify_edge", "specify_rise_fall", "specify_triple",
  "ignored_specify_block", "ignored_specify_item_opt",
  "ignored_specify_item", "specparam_declaration", "specparam_range",
  "list_of_specparam_assignments", "specparam_assignment",
  "ignspec_opt_cond", "path_declaration", "simple_path_declaration",
  "path_delay_value", "list_of_path_delay_extra_expressions",
  "specify_edge_identifier", "parallel_path_description",
  "full_path_description", "list_of_path_inputs", "more_path_inputs",
  "list_of_path_outputs", "opt_polarity_operator",
  "specify_input_terminal_descriptor",
  "specify_output_terminal_descriptor", "system_timing_declaration",
  "system_timing_arg", "system_timing_args", "ignspec_constant_expression",
  "ignspec_expr", "ignspec_id", "$@35", "param_signed", "param_integer",
  "param_real", "param_range", "param_integer_type", "param_range_type",
  "param_implicit_type", "param_type", "param_decl", "$@36",
  "localparam_decl", "$@37", "param_decl_list", "single_param_decl",
  "single_param_decl_ident", "defparam_decl", "defparam_decl_list",
  "single_defparam_decl", "enum_type", "$@38", "enum_base_type",
  "type_atom", "type_vec", "type_signing", "enum_name_list",
  "enum_name_decl", "opt_enum_init", "enum_var_list", "enum_var",
  "enum_decl", "struct_decl", "$@39", "struct_type", "$@40",
  "struct_union", "struct_body", "opt_packed", "opt_signed_struct",
  "struct_member_list", "struct_member", "member_name_list", "member_name",
  "$@41", "struct_member_type", "$@42", "member_type_token", "$@43",
  "$@44", "member_type", "struct_var_list", "struct_var", "wire_decl",
  "$@45", "$@46", "$@47", "$@48", "opt_supply_wires", "wire_name_list",
  "wire_name_and_opt_assign", "wire_name", "assign_stmt",
  "assign_expr_list", "assign_expr", "type_name", "typedef_decl",
  "typedef_base_type", "enum_struct_type", "cell_stmt", "$@49", "$@50",
  "tok_prim_wrapper", "cell_list", "single_cell", "single_cell_no_array",
  "$@51", "single_cell_arraylist", "$@52", "cell_list_no_array",
  "prim_list", "single_prim", "$@53", "cell_parameter_list_opt",
  "cell_parameter_list", "cell_parameter", "cell_port_list",
  "cell_port_list_rules", "cell_port", "always_comb_or_latch",
  "always_or_always_ff", "always_stmt", "$@54", "$@55", "$@56", "$@57",
  "always_cond", "always_events", "always_event", "opt_label",
  "opt_sva_label", "opt_property", "modport_stmt", "$@58", "$@59",
  "modport_args_opt", "modport_args", "modport_arg", "modport_member",
  "modport_type_token", "assert", "assert_property",
  "simple_behavioral_stmt", "asgn_binop", "for_initialization",
  "behavioral_stmt", "$@60", "$@61", "$@62", "$@63", "$@64", "$@65",
  "$@66", "$@67", "$@68", "$@69", "$@70", "$@71", "case_attr", "case_type",
  "opt_synopsys_attr", "behavioral_stmt_list", "optional_else", "$@72",
  "case_body", "case_item", "$@73", "$@74", "gen_case_body",
  "gen_case_item", "$@75", "$@76", "case_select", "case_expr_list",
  "rvalue", "lvalue", "lvalue_concat_list", "opt_arg_list", "arg_list",
  "arg_list2", "single_arg", "module_gen_body",
  "gen_stmt_or_module_body_stmt", "genvar_identifier",
  "genvar_initialization", "gen_stmt", "$@77", "$@78", "$@79", "$@80",
  "$@81", "gen_block", "$@82", "$@83", "gen_stmt_block", "$@84",
  "opt_gen_else", "expr", "basic_expr", "$@85", "concat_list",
  "integral_number", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-1069)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-591)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
   -1069,    57,   300, -1069, -1069, -1069,   660, -1069, -1069,   503,
   -1069,   300,   300,   300,   300,   300,   300,   300,   300,   300,
      77,   151, -1069, -1069, -1069, -1069, -1069, -1069, -1069, -1069,
   -1069, -1069, -1069, -1069,   173, -1069,    30,    50, -1069, -1069,
   -1069,   136,   108,    77, -1069, -1069, -1069, -1069,   240,   264,
     264,   325, -1069, -1069, -1069, -1069, -1069, -1069, -1069, -1069,
   -1069, -1069, -1069,   201, -1069,   134, -1069,   674,   209, -1069,
   -1069, -1069, -1069, -1069, -1069,   286,  2031, -1069,   108, -1069,
   -1069,   255,    18, -1069,   259,   442,   578,   578,   459,    67,
   -1069,   935,   468,    77,   450,    77,   487,  2031,   498,   318,
   -1069, -1069, -1069,   342,    90,    90, -1069,   216, -1069,   350,
   -1069,   510, -1069,   517, -1069,   384, -1069, -1069,   396, -1069,
   -1069, -1069, -1069, -1069, -1069, -1069, -1069,  2031,  2031, -1069,
   -1069,   188, -1069,   169,  2586, -1069,   136, -1069,   554,    77,
   -1069, -1069, -1069, -1069, -1069, -1069,   136,   260, -1069, -1069,
   -1069, -1069,   565, -1069,   260,   565, -1069, -1069,   596,   607,
   -1069, -1069,   610, -1069,    30,   625,    50, -1069,   609, -1069,
   -1069, -1069, -1069, -1069,   490,   496,   639, -1069, -1069, -1069,
     136, -1069, -1069, -1069, -1069,   637, -1069,   520,   537,   540,
     551,  2031,  2031,  2031,  2031,  2031,  2031,  2031,  2031,   269,
       1,   545,  2031,  2031,  2031, -1069,   136,   136, -1069, -1069,
    2031,  2031,  2031, -1069, -1069, -1069, -1069, -1069, -1069, -1069,
   -1069, -1069, -1069, -1069, -1069, -1069, -1069, -1069, -1069, -1069,
   -1069, -1069, -1069, -1069, -1069, -1069, -1069, -1069, -1069, -1069,
     561, -1069,   136, -1069,   580, -1069,   501, -1069,   318, -1069,
   -1069, -1069, -1069,   224, -1069,   591,   136,   226,   619,   612,
     788,   634, -1069, -1069, -1069,   136,   633, -1069, -1069,    68,
     632,   640,   638, -1069, -1069,   629, -1069,   796,   988, -1069,
    2031,  2031,  2031,  2031, -1069, -1069, -1069, -1069, -1069, -1069,
   -1069, -1069,   454,  2031,  2031,  2031, -1069, -1069, -1069,   234,
     645, -1069, -1069,   635,   642,   643,  2031,  2031,  2031,  2031,
    2031,  2031,  2031,  2031,  2031,  2031,  2031,  2031,  2031,  2031,
    2031,  2031,  2031,  2031,  2031,  2031,  2031,  2031,  2031,  2031,
    2031,  2031,  2031,  2031, -1069,   648,    77,   496,   565, -1069,
    2031, -1069, -1069, -1069,   801,   659, -1069,   803,   633,   633,
   -1069,   646,   661,    69,   819,   679, -1069,   654,   683, -1069,
   -1069,  2031, -1069,   639,   671, -1069, -1069, -1069,   250, -1069,
   -1069,    90,    90, -1069,   131, -1069,   680,   681,   690,   692,
   -1069,   704,   712, -1069,   702,   713,  2031, -1069, -1069, -1069,
    2655,  2680,  2703,  2703,  2724,  2724,  2743,  2743,  1282,  1282,
    1282,  1282,   682,   682,   682,   682,   696,   696,   696,   696,
     575,   575,   -56,   -56,   -56,   719,   714,   715,   716,   871,
   -1069,   718, -1069, -1069,   204,   874, -1069,    97, -1069,   612,
     875,   725,   727, -1069,   887, -1069,   641,   734, -1069, -1069,
     741,   889,   742, -1069,   694, -1069,   644,   136,    78,   739,
     308,  2629, -1069, -1069,   136,   796, -1069, -1069,   136, -1069,
   -1069, -1069, -1069, -1069,  2031,   736,    77,   745, -1069, -1069,
   -1069, -1069,  2031,  1446, -1069, -1069,   337, -1069,   744,   592,
   -1069, -1069, -1069, -1069, -1069, -1069,   801, -1069,   751, -1069,
   -1069, -1069,   654,   749, -1069,   911,   356, -1069, -1069,    69,
     754,  2031, -1069, -1069, -1069, -1069, -1069, -1069,   644,   820,
   -1069,   853,   915, -1069, -1069,   136, -1069,   917,   758,  1320,
   -1069, -1069, -1069, -1069, -1069, -1069, -1069, -1069, -1069, -1069,
   -1069, -1069, -1069,   286,   761, -1069,    63, -1069,  2031,   762,
   -1069,   923,   432, -1069, -1069,   767,   871, -1069,  2271,   926,
   -1069, -1069, -1069,   612,   322,   471,   136,    92, -1069,   136,
   -1069,   766, -1069,   186,   657, -1069,   403, -1069, -1069, -1069,
   -1069,   691, -1069,   770, -1069,   710,   578,   578, -1069, -1069,
   -1069,   741,   853, -1069,  1071, -1069, -1069, -1069, -1069, -1069,
   -1069, -1069,   268,   425, -1069, -1069, -1069,    77,   369, -1069,
   -1069, -1069,    45, -1069, -1069, -1069, -1069, -1069,   930,   931,
     136, -1069, -1069, -1069, -1069, -1069, -1069,   778,  1446, -1069,
   -1069, -1069, -1069,   744, -1069,   780,   781, -1069, -1069, -1069,
     190,   351,    42, -1069,   940, -1069,  2600, -1069, -1069, -1069,
   -1069, -1069, -1069, -1069, -1069, -1069, -1069, -1069,   941, -1069,
   -1069, -1069, -1069, -1069, -1069, -1069,   730, -1069,   785, -1069,
   -1069, -1069,   784, -1069, -1069, -1069, -1069,   786, -1069, -1069,
   -1069, -1069, -1069, -1069,  1504, -1069, -1069,   790,   792,   794,
     795, -1069, -1069, -1069, -1069,  2031,    87, -1069,  1937, -1069,
      34,    34,    34,    39, -1069,   799,   565,   565, -1069,   268,
   -1069, -1069, -1069, -1069, -1069, -1069, -1069,   741,   798,   136,
   -1069,   800,   391, -1069,   807,   403, -1069, -1069, -1069,   403,
     797,  1552, -1069,  1132,   806,   813, -1069,   811, -1069,   744,
    2031, -1069,   812,   814,   900,   190,   822, -1069,   824, -1069,
     359, -1069, -1069,   827, -1069,   828, -1069,   829,  2031,   989,
     394, -1069,   833,  2404,   831, -1069, -1069, -1069, -1069,   758,
   -1069,   412, -1069,   902,   905,   906,   908,  2031, -1069,   911,
   -1069,   838,   472,   839,   811,   744,  2031, -1069,  2031,  2031,
     836,   849,   811, -1069, -1069, -1069, -1069, -1069, -1069, -1069,
   -1069, -1069, -1069, -1069, -1069, -1069, -1069,   758,   758,   758,
   -1069, -1069,   847,   848,   851, -1069,   852,  2031, -1069, -1069,
   -1069, -1069,  2031, -1069,   146, -1069,    45, -1069,  2031, -1069,
   -1069, -1069,   758, -1069,    35, -1069, -1069,   854, -1069,  1013,
   -1069, -1069, -1069,  2031,   855, -1069,   857,   117,   390,  2031,
   -1069, -1069,   390,   136,  2031, -1069, -1069,   315,   862,   863,
   -1069,  1330,   861, -1069,   419,   989, -1069,  2031, -1069,   405,
   -1069, -1069, -1069, -1069, -1069,   648,  1023, -1069,   941, -1069,
     867,   868,   869,   870,   873, -1069, -1069,  2031, -1069,  2031,
     886, -1069,   888,  1032,   891,   892, -1069,  2031,   896,  2031,
    2031,  2031,  1249,  1644,  1692,  1742,   893, -1069, -1069, -1069,
   -1069, -1069,   904, -1069, -1069,  1047,   897, -1069, -1069,   426,
     439,   911,   901, -1069,   907,   403, -1069,   903, -1069,   745,
   -1069, -1069, -1069, -1069,  1061,   910,   909, -1069, -1069,  2031,
     912,  2031, -1069,   914,   913, -1069, -1069,   989,   916,   397,
   -1069,  2082,  2082,   989,   989, -1069,   -33, -1069,  2031, -1069,
   -1069, -1069, -1069, -1069,  1062, -1069,   449, -1069,  1062, -1069,
    1793,  1841,  1933,  2031, -1069,   482,   919, -1069, -1069, -1069,
     918,   136,   924, -1069, -1069, -1069, -1069, -1069, -1069, -1069,
    2031,   920,  2031,   928,  2031,   933,   925,   942, -1069,   410,
     943, -1069, -1069,  1066, -1069, -1069,   934, -1069,   922, -1069,
   -1069,  2031,  2031,   944,   170, -1069, -1069, -1069,  1981,   945,
    2493,  1017,   949,  2031,  2031,   994, -1069,  1084, -1069,  2031,
     951,   422,   966, -1069, -1069,   989,   478,   953,  2031, -1069,
     955, -1069, -1069, -1069,  1330,  1330,   956,   947,  2370,   136,
     455, -1069, -1069, -1069,  1023, -1069, -1069,   474, -1069,   959,
    2031,   961,  2031,   962,  2031,   963,   964, -1069,  2031, -1069,
    2031,   587,   403,  2031,  1117,  2031,   403,   403,   965,   967,
     969,   970,   972,   977, -1069,   978, -1069, -1069, -1069, -1069,
     911,   979,  2031, -1069, -1069, -1069,  1350,  1350, -1069, -1069,
     982, -1069,   744, -1069,  1090,  2527,  2031, -1069, -1069,  2031,
     990,  2172,   985,   984,  1007, -1069,   989, -1069,   989, -1069,
    1008,   955,   476,  2031, -1069, -1069, -1069, -1069, -1069, -1069,
   -1069,  1062, -1069, -1069,  1062, -1069, -1069,   991,   992,   995,
     993,   996,   998,   999,  1085,   997, -1069,   744, -1069, -1069,
   -1069,  1004, -1069, -1069, -1069,  1010, -1069,  1011, -1069,  1015,
   -1069, -1069,   652, -1069, -1069, -1069, -1069, -1069, -1069, -1069,
    1017, -1069, -1069, -1069,  1021, -1069,   390,  2031,  1009, -1069,
    2031,  1026,  1027,   488, -1069, -1069, -1069,   989,  1014,  2031,
     955,  1029, -1069, -1069,  1038,  1039, -1069,  1042, -1069,  1043,
   -1069, -1069, -1069, -1069,  1398, -1069, -1069,  1113,  2031,  1044,
   -1069, -1069, -1069, -1069, -1069,  1140, -1069, -1069,  2031,    71,
    1052, -1069,   989,   989,   989, -1069,  1051, -1069,  1036, -1069,
   -1069, -1069, -1069, -1069, -1069, -1069,   192, -1069,   195, -1069,
   -1069, -1069, -1069, -1069, -1069, -1069,  1398,    45,  1053,   994,
    2031,  2031,  2031,  2222,   292,   311, -1069, -1069,  2031,  1054,
    1017,  2031, -1069,   403,  1057, -1069,  1017,  1056,  1058,  1063,
     514,  2031,  1060, -1069,  1064,  2031,  1065,  2031,  1068,   761,
   -1069, -1069, -1069, -1069, -1069,   403, -1069,  2031, -1069, -1069,
   -1069,  1069,   518, -1069,  2031,  1072,  2031,  1073,  2031,   403,
   -1069,  1078,  2031,  2031,  1074,  1076,  1079,  1080,  1081, -1069,
    2031,  1082,   494,  1083,  2031, -1069,  1087, -1069,  1088, -1069,
    1093,  2031, -1069,  2031, -1069, -1069, -1069, -1069,   502,   761,
    2031, -1069,  1089,  2031,  1099,  2031,   508,  2031, -1069,  1101,
    2031,  1103,  2031,  1111,  2031,  1112,  2031,  1114,  2031,  1115,
   -1069
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,     0,    14,     1,    18,    74,     0,    88,     3,     0,
      17,    14,    14,    14,    14,    14,    14,    14,    14,    14,
      22,     0,    30,    31,   148,   147,   142,   340,   364,   143,
     146,   144,   145,   365,     0,   413,   204,   207,   416,   417,
     362,   156,     0,     0,    33,   327,   329,    65,     0,   219,
     219,    15,     5,     4,    10,    11,    12,     6,     7,     8,
       9,    27,    19,    21,    23,    25,    75,   344,     0,   205,
     206,   415,   208,   209,   414,   368,     0,   155,     0,   409,
     410,     0,    97,    89,    94,     0,   314,   314,     0,     0,
     218,   201,     0,    22,     0,     0,     0,     0,     0,    39,
     346,   347,   345,     0,   350,   350,    32,   371,   363,     0,
     642,   692,   641,     0,   693,     0,    14,    14,     0,    14,
      14,    14,    14,    14,    14,    14,    14,     0,     0,    14,
      14,   156,   637,     0,   635,   640,   156,   412,     0,     0,
      92,    34,   312,   316,   313,   326,   156,   314,   323,   322,
     324,   325,     0,   315,   314,     0,    66,   185,     0,     0,
     202,   203,     0,   197,   204,     0,   207,   191,     0,    20,
      24,    28,    26,    29,     0,    49,     0,   348,   349,   342,
     156,   369,   370,   367,   379,     0,   694,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     690,     0,     0,     0,     0,   643,   155,   596,   595,   639,
       0,     0,     0,   152,    14,    14,    14,    14,    14,    14,
      14,    14,    14,    14,    14,    14,    14,    14,    14,    14,
      14,    14,    14,    14,    14,    14,    14,    14,    14,    14,
       0,    14,   158,   157,     0,    90,    93,    95,    39,   317,
     321,   318,   335,     0,   331,   334,   319,     0,     0,   213,
       0,     0,   193,   200,   195,   198,   221,    16,    36,    14,
       0,   355,    54,   351,   343,   379,   372,     0,   383,   695,
       0,     0,     0,     0,   660,   661,   662,   663,   658,   659,
     682,   683,   647,     0,     0,     0,   649,   651,   686,     0,
       0,   153,   154,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   411,   440,     0,    49,     0,   328,
       0,   320,   330,    69,   217,     0,   187,     0,   221,   221,
     199,   222,     0,    14,    57,     0,    48,   112,    54,    51,
      78,     0,   353,    53,     0,   366,   373,   377,     0,   375,
     382,   350,   350,   380,     0,   381,     0,     0,     0,     0,
     638,     0,   690,   691,     0,   152,   604,   150,   151,   149,
     685,   684,   654,   655,   656,   657,   652,   653,   670,   671,
     672,   673,   668,   669,   674,   675,   664,   665,   666,   667,
     676,   677,   678,   679,   680,   681,     0,     0,     0,     0,
      96,     0,   332,   333,    14,   211,   216,     0,   186,   213,
       0,     0,     0,   220,    14,   232,     0,    37,    40,    47,
      56,     0,     0,   114,     0,   115,   125,   156,    14,     0,
      14,   354,   352,   341,   156,     0,   374,   386,   156,   384,
     687,   645,   646,   688,     0,     0,     0,    54,   603,   605,
     607,   689,     0,   443,   428,   432,     0,   163,   480,     0,
      68,    73,    72,    71,    70,   210,   215,   212,     0,   189,
     232,   232,   112,    54,   225,     0,    14,    43,    45,    14,
       0,     0,    58,    60,    64,   116,   117,   118,   125,   124,
     123,     0,     0,    52,    50,   156,    76,     0,   109,   112,
      77,    87,    79,    80,    82,    83,    85,    81,    84,    86,
     378,   376,   387,   368,     0,   650,   156,   594,    53,     0,
     636,     0,     0,   441,   444,     0,     0,    91,    14,     0,
      67,   214,   188,   213,    14,    14,   156,    14,   223,   156,
     230,     0,   192,     0,   112,   534,    14,   537,   538,   536,
     539,     0,   535,     0,   231,     0,   314,   314,    41,    38,
      55,    56,   127,   122,   132,   138,   137,   136,   130,   139,
     135,   141,   121,   128,   134,   140,    62,     0,     0,   337,
     486,   108,     0,   458,   465,   459,   456,   457,     0,     0,
     156,   463,   460,   385,   648,   606,   644,     0,   443,   439,
      14,   433,   625,   480,   627,     0,     0,   567,   568,   569,
     235,   272,     0,   610,     0,   162,   112,   179,   176,   159,
     181,   164,   165,   182,   170,   166,   167,   169,     0,   174,
     175,   171,   172,   168,   173,   177,     0,   180,     0,   160,
     161,   479,     0,   194,   196,   227,   226,     0,   404,   481,
     100,   101,   102,   103,     0,   545,   547,     0,     0,     0,
       0,   566,   564,   565,   542,     0,   156,   597,     0,   540,
     485,   485,   485,   485,   541,     0,     0,     0,    61,   121,
     126,   131,   133,   119,   120,   113,   129,    56,     0,   156,
     336,     0,     0,   406,     0,    14,   394,   396,   391,    14,
     472,     0,   442,   450,     0,   447,   448,   602,    35,   480,
       0,   618,     0,     0,     0,   235,     0,   310,     0,   259,
     272,   261,   262,     0,   263,     0,   264,     0,     0,     0,
       0,   268,     0,    14,     0,   418,   422,   423,   360,   109,
     358,     0,   356,     0,     0,     0,     0,     0,   190,     0,
     224,     0,     0,    98,   602,   480,     0,   550,     0,     0,
       0,   599,   602,   515,   516,   519,   520,   521,   522,   523,
     524,   525,   526,   527,   528,   529,   530,   109,   109,   109,
     484,   483,     0,     0,     0,   504,     0,     0,    44,    46,
     111,    63,     0,   338,     0,   487,     0,   405,     0,   466,
     398,   398,   109,   464,     0,   461,   445,     0,   455,     0,
     451,   429,    14,   604,     0,   628,     0,     0,   250,     0,
     233,   234,   250,   156,     0,   258,   260,     0,     0,     0,
     273,     0,     0,   307,     0,     0,   265,     0,   178,   112,
     612,   608,   611,   609,   183,   440,     0,   420,     0,   359,
       0,     0,     0,     0,     0,   228,   104,     0,   105,     0,
       0,   548,     0,   112,     0,     0,   598,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   339,   495,   496,
     497,   489,    54,   491,   494,     0,     0,   407,   408,     0,
       0,     0,     0,   471,     0,    14,   446,   454,   449,    54,
     626,   610,   621,   614,     0,     0,     0,   248,   249,     0,
       0,     0,   311,     0,   308,   281,   282,     0,     0,   297,
     298,     0,     0,     0,     0,   304,     0,   303,     0,   266,
     269,   270,   613,   163,     0,   390,     0,   388,   437,   357,
       0,     0,     0,     0,   623,     0,     0,   546,   574,   558,
       0,   156,     0,   554,   556,   600,   544,   517,   514,   518,
       0,     0,     0,     0,     0,     0,     0,     0,   561,    53,
       0,   493,   488,     0,   395,   397,   392,   400,   402,   469,
     470,     0,     0,     0,     0,   473,   478,   462,     0,     0,
      14,   630,   615,     0,     0,   243,   240,     0,   271,     0,
       0,   297,     0,   295,   296,     0,   297,   289,     0,   274,
     277,   275,   301,   302,     0,     0,     0,     0,    14,   428,
       0,   424,   426,   427,     0,   361,   436,     0,   434,     0,
       0,     0,     0,     0,     0,     0,     0,   584,     0,   106,
       0,    14,    14,     0,     0,     0,    14,    14,     0,     0,
       0,     0,     0,     0,   503,     0,   572,   492,   490,   399,
       0,     0,     0,   476,   477,   468,     0,     0,   467,   453,
       0,   601,   480,   632,   634,    14,     0,   617,   619,     0,
       0,     0,     0,     0,     0,   289,     0,   291,     0,   290,
       0,     0,   308,     0,   278,   305,   306,   300,   267,   184,
     430,     0,   419,   389,   437,   421,    14,     0,     0,     0,
       0,     0,     0,     0,   585,     0,    99,   480,   573,   559,
     531,   532,   551,   555,   557,     0,   498,     0,   499,     0,
     505,   502,   579,   401,   393,   403,   474,   475,   452,   629,
     630,   622,   631,   616,     0,   242,   250,     0,     0,   244,
       0,     0,     0,     0,   293,   299,   292,     0,     0,     0,
     279,     0,   425,   435,     0,     0,   507,     0,   508,     0,
     512,   511,   624,   583,     0,   107,   549,   577,     0,     0,
     500,   501,   506,   570,   571,   580,   633,    14,     0,     0,
       0,   309,     0,     0,     0,   286,     0,   276,     0,   280,
      14,   438,   509,   510,   513,   591,   589,   586,     0,   592,
     575,   560,   533,    14,   562,   578,     0,     0,     0,   243,
       0,     0,     0,     0,   297,   297,   294,   283,     0,     0,
     630,     0,   588,    14,     0,   581,   630,     0,     0,     0,
       0,     0,     0,   251,   256,     0,     0,     0,     0,   309,
     431,   587,   593,   576,   552,    14,   620,     0,   247,   246,
     245,     0,   256,   236,     0,     0,     0,     0,     0,    14,
     582,   239,     0,     0,     0,     0,     0,     0,     0,   553,
       0,     0,     0,     0,     0,   288,     0,   285,     0,   238,
       0,     0,   252,     0,   257,   287,   284,   237,     0,   257,
       0,   253,     0,     0,     0,     0,     0,     0,   254,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     255
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1069, -1069, -1069,   845,    51, -1069, -1069,   129, -1069, -1069,
    1133, -1069,  1136,   -19,    33, -1069, -1069, -1069,  1003, -1069,
   -1069, -1069,   746, -1069, -1069,   936, -1069,  -354,  -458,   826,
   -1069, -1069, -1069, -1069, -1069, -1069, -1069, -1069, -1069, -1069,
   -1069, -1069, -1069, -1069,  -447, -1069, -1069, -1069, -1069, -1069,
   -1069,   -97,  -833,  -501,  -432, -1069, -1069,   363, -1069,  -297,
   -1069,   576,   768, -1069,   697, -1069,  -271,     3, -1069,   -53,
    -126,   -31,  -443,   327,  -514, -1069, -1069,  -422, -1069, -1069,
   -1069, -1069, -1069, -1069, -1069, -1069,  1119,  1120,   802,  -374,
   -1069,  1228,   404, -1069, -1069, -1069,   728, -1069, -1069,   265,
   -1069,   552, -1069, -1069, -1069,    52, -1069,  -831, -1069,   431,
   -1069, -1069,   549,  -583, -1069,   541,   440, -1069, -1069, -1069,
     352,  -868, -1069, -1069, -1069,   360, -1069,    86, -1003,  -892,
    -697, -1069,  -276, -1069,   353,  -727,  -552, -1069,   215, -1069,
   -1069, -1069, -1069, -1069, -1069,   -73,    14, -1069,    73, -1069,
    1144,  -316, -1069,   856, -1069,   593,  1297, -1069, -1069,   -27,
     -26,   -67, -1069,   974, -1069, -1069,   477, -1069, -1069, -1069,
    1334, -1069,   968,   817, -1069, -1069, -1069,  1077, -1069,   921,
   -1069, -1069, -1069, -1069, -1069, -1069, -1069, -1069,   302,  -437,
   -1069, -1069, -1069, -1069,   523, -1069,   271,  -460,   929, -1069,
     548,  1287,    91, -1069, -1069, -1069, -1069, -1069, -1069, -1069,
    -911,  -255, -1069, -1069, -1069, -1069, -1069,   242, -1069,   509,
   -1069,   750, -1068, -1069,   535, -1069, -1069,   937, -1069, -1069,
   -1069, -1069, -1069, -1069,  -322,  -615,  -542,    96, -1069, -1069,
   -1069, -1069, -1069,   386,   473, -1069, -1069, -1069,  -889, -1069,
   -1069,  -554, -1069, -1069, -1069, -1069, -1069, -1069, -1069, -1069,
   -1069, -1069, -1069, -1069, -1069,   815, -1069, -1069, -1069, -1069,
   -1069, -1069, -1069, -1069, -1069, -1069, -1069, -1069,   150, -1069,
    -446,  -595,   504,  -376,   555, -1069,   858,   479,   294,   470,
   -1069,  -512, -1069, -1069, -1069, -1069, -1069,  -529, -1069, -1069,
    -898, -1069, -1069,   -76,  1001, -1069,   485,   -70
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     1,     2,     8,   564,    10,    51,   565,    20,    94,
      62,    63,    64,   131,   145,    12,    85,   248,   175,   353,
     500,   437,   438,   576,   577,   270,   358,   364,   502,   359,
     440,   441,   581,   707,    13,    88,   258,   424,   480,    14,
      21,    99,   450,   520,    15,    43,   138,   335,    83,   140,
     246,    84,   772,   566,   602,   443,   444,   445,   446,   610,
     508,   705,   511,   699,   592,   593,   594,   102,    37,    77,
     242,   243,   244,   548,   860,   640,   953,    16,   259,   429,
     553,   266,   348,   349,   165,   166,    71,    74,   426,   345,
     427,    91,   352,   434,   667,   493,   494,   769,   495,   496,
     642,   734,   735,  1301,   736,  1100,  1168,   929,  1262,  1263,
     643,   740,   741,   644,   749,   750,   751,   743,   744,   745,
    1029,  1114,   937,   848,   849,   938,  1026,  1173,  1027,   939,
    1174,   746,   945,   946,   852,   947,  1175,   843,   146,   147,
     148,   250,   149,   150,   151,   152,   567,    86,   568,    87,
     253,   254,   255,   647,   598,   599,   648,    67,   103,   153,
     154,   179,   272,   273,   362,   761,   762,   649,   650,   866,
     758,    75,    40,   108,   109,   183,   275,   276,   368,   369,
     454,   277,   278,   373,   374,   533,   375,   956,   957,   569,
     822,  1081,   820,   821,   909,   996,   997,   998,   652,   712,
     713,    81,   570,    41,    42,   654,   865,   958,   759,  1040,
    1046,  1042,   545,  1043,  1181,   476,  1047,  1048,  1049,   419,
     542,   543,   724,   725,   726,   611,   612,   655,   720,   915,
     719,   715,   825,  1004,  1005,   550,   571,   802,   529,   711,
     906,   815,   902,   903,   904,   905,   572,   657,   573,   799,
     972,   574,   782,   774,   775,   968,   883,  1199,  1289,  1066,
    1067,  1062,  1197,  1076,   575,   658,  1152,  1061,  1231,  1253,
    1205,  1235,  1236,  1275,  1134,  1193,  1194,  1250,  1227,  1228,
     132,   688,   780,   834,   467,   468,   469,   753,   861,   925,
     926,   862,   837,  1164,  1011,  1057,   727,  1093,   729,   921,
    1094,  1095,  1161,  1264,   134,   300,   201,   135
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     133,    65,   481,   521,   449,   207,   656,   714,   728,    36,
      78,   931,   689,   525,   155,   532,    17,   601,  1105,   660,
     537,   172,   422,  1109,    82,    17,    17,    17,    17,    17,
      17,    17,    17,    17,   639,   560,   659,   439,   180,    35,
     104,   105,   247,  1041,   965,  1021,   737,  1034,   742,    61,
      96,   199,   200,     9,   800,   488,   912,     3,  1184,   800,
     447,   209,     9,     9,     9,     9,     9,     9,     9,     9,
       9,   157,   354,   252,    65,    18,    65,    69,   206,   747,
     752,    61,   354,   206,    18,    18,    18,    18,    18,    18,
      18,    18,    18,    19,   164,    96,  -229,    72,   239,   240,
     208,   638,    19,    19,    19,    19,    19,    19,    19,    19,
      19,   651,    79,   539,   835,   249,   668,   933,   687,    96,
      82,   923,    80,   698,   163,  1035,   641,   801,   299,  1036,
     951,    11,   801,  1107,   303,   304,   305,   177,    70,   558,
      11,    11,    11,    11,    11,    11,    11,    11,    11,   274,
     898,   708,  1249,   301,   302,    66,   687,   742,    73,   294,
     881,   819,  1240,  1241,   475,   823,    96,   188,   189,   295,
     191,   192,   193,   194,   195,   196,   197,   198,    98,   662,
     202,   203,   205,   439,   899,   900,   913,    68,   747,   302,
     670,   671,   672,   111,   732,   556,   914,   752,   178,   113,
     924,   114,   805,   341,   376,   377,   378,   379,   748,   -14,
    1182,   656,   350,   685,  1030,  1030,  1176,   381,   382,   382,
      96,   714,   380,    98,   863,  1135,   158,   -42,   355,   204,
     356,   -42,   159,   292,  1086,   478,  1242,    28,   355,   420,
     -53,  1266,  1268,  1178,    89,    33,   733,    98,  -543,   811,
    -543,   371,   372,   204,   -53,   486,   416,   417,   601,   487,
     210,   211,  1206,   181,   423,   306,   307,   308,   309,   310,
     311,   312,   313,   314,   315,   316,   317,   318,   319,   320,
     321,   322,   323,   324,   325,   326,   327,   328,   329,   330,
     331,   621,   333,    97,    98,   940,   601,   601,   601,     6,
     -13,  1111,    76,   752,   457,   458,   638,   142,   901,   875,
     470,   370,  1219,   702,   673,   703,   651,    82,  1238,   737,
     357,   601,   706,     4,   182,   210,   211,   867,  1087,    90,
     561,   641,  1088,     5,   212,  1208,   213,   515,  -482,  -482,
    -482,  -482,   516,   517,  1254,     4,    93,   674,    98,   -14,
    -590,  -241,  1271,  1251,   204,   737,   518,  -590,  1276,    95,
    1252,  1007,   251,   737,   561,   889,   890,   891,   144,   256,
     687,   106,  -482,  -482,  -482,  -482,   704,   935,   936,     4,
     808,   809,   338,  1265,   338,   940,  1180,   339,   534,   342,
     911,  1032,  1033,   107,   663,     6,   540,   544,   880,   212,
     932,   385,  1267,     6,   436,   206,   888,   738,   455,   755,
     207,   561,   756,   456,   898,   738,   512,     6,   137,  -482,
    -482,  -482,  -482,   530,   139,   580,     4,     7,   739,   632,
     562,   292,    45,    46,   293,     7,   845,   632,   482,    22,
      23,  1023,  1024,  -110,  -110,  -110,   141,   536,   899,   900,
    1214,     6,   927,   928,   603,   604,   605,   606,   607,   111,
    1023,  1024,   470,   156,   522,   113,    48,   114,   656,   757,
     589,    26,   167,   940,   169,   479,    49,  1159,    50,   561,
    1216,   863,   174,   206,   597,   492,   563,  -482,  -482,  -482,
    -482,   171,   608,   609,     4,   546,   656,   483,     6,   357,
     547,   519,   173,   696,   697,   208,   206,  1138,  1139,   660,
     176,    28,  1143,  1144,   595,   484,  1245,  1246,   184,    33,
     563,   185,  1196,   523,   639,   665,   659,   709,    44,   186,
      45,    46,   710,    47,    29,    30,    31,    32,  1285,   187,
    1287,   527,   544,   664,   591,   686,  1023,  1024,   990,   816,
     436,   190,   855,   656,   817,  1025,   940,   856,   245,  1296,
     207,  1298,   645,   638,    48,  1009,     6,   563,   952,   252,
     868,  1023,  1024,   651,    49,   869,    50,   855,   536,   718,
    1025,  1104,   949,   536,   993,   595,    34,   595,   641,   994,
     618,   638,    22,    23,   619,   561,   595,   993,   773,   636,
     260,   651,   995,  -482,  -482,  -482,  -482,  1044,   492,   781,
       4,   261,  1045,  1121,   262,   591,   641,   591,  1122,    45,
      46,   646,   100,   101,    26,   142,   591,  1023,  1024,   264,
     877,   267,  1124,   206,   878,   563,  1108,  1125,   292,   653,
    1058,  1179,  1137,   271,  1059,   827,  1214,   830,   638,   279,
    1215,   268,  1311,    48,   836,   208,  1312,   269,   651,   336,
    1320,    61,   143,    49,  1321,    50,  1327,   675,   497,   498,
    1328,   723,   853,   641,    22,    23,  1280,   637,   597,   464,
     292,   280,     6,  1293,    45,    46,   144,    29,    30,    31,
      32,   874,  -110,  -110,  -110,  -110,  -110,  -110,   281,  1273,
     882,   282,   884,   885,    24,    25,    26,   690,   691,   692,
     693,   676,   283,   677,   296,   678,   679,   680,   100,   101,
      26,  1290,   332,  -563,  -563,  -563,   236,   237,   238,   239,
     240,   896,   505,   506,   507,  1299,   897,  1203,  1204,    34,
     509,   510,   908,   334,   608,   609,   763,   764,   765,   766,
     340,   563,   431,   432,    27,   554,   555,   470,  1115,  1116,
     681,   682,   683,   930,  1156,  1157,    28,   645,   934,    29,
      30,    31,    32,   344,    33,   934,   627,   628,   629,   383,
     384,   934,   343,    29,    30,    31,    32,   803,   804,   806,
     206,   687,   346,   347,   351,   360,   363,   536,   365,   361,
     367,   773,   387,   966,   859,   425,   386,   430,   433,   388,
     389,   781,   418,   977,   978,   979,   981,   983,   985,   987,
     684,    34,   428,   -59,   435,   685,   646,   230,   231,   232,
     233,   234,   235,   236,   237,   238,   239,   240,  1006,   442,
     453,   448,   460,   461,   653,   234,   235,   236,   237,   238,
     239,   240,   462,  1015,   463,  1017,    52,    53,    54,    55,
      56,    57,    58,    59,    60,   934,   934,    22,    23,   464,
     294,   465,   853,   466,   240,   474,   471,   473,   485,   489,
     472,   477,   637,   723,  1051,  1053,  1055,  1056,   490,   584,
     491,  -229,   499,   503,   585,   586,   587,   588,   589,    26,
     501,   514,   504,   538,  1068,   535,  1070,   557,  1072,   549,
     110,    61,   111,   112,   552,   559,   579,   583,   113,   596,
     114,   600,   563,   614,   616,  1083,  1084,   617,   620,  1000,
     661,   669,  1090,   694,   716,   717,   590,  1097,  1098,   721,
    1064,   730,   731,  1102,   754,   760,   767,   768,   770,    22,
      23,   776,  1112,   777,   115,   778,   779,   812,   934,   934,
     807,   814,    29,    30,    31,    32,   818,   824,   831,  1001,
    1002,   832,   833,   838,  1127,   839,  1129,   840,  1131,   160,
     161,    26,   773,   842,  1136,   844,  1120,  1140,   847,  1142,
     851,   850,   857,   737,   864,   870,   116,   117,   871,   872,
     876,   873,    22,    23,   879,   886,  1155,   887,   892,   893,
    1006,  1006,   894,   895,    34,   118,   916,   917,   920,   922,
    1163,   941,   942,  1165,   645,  1169,   948,   955,   960,   961,
     962,   963,   100,   101,    26,   964,   970,   934,   119,   120,
     121,   122,   123,   124,    29,    30,    31,    32,   162,   967,
     969,   898,   645,   973,   974,   988,   125,   126,  1003,   976,
     992,   859,   989,   999,  1008,   923,  1039,  1022,   127,  1013,
    1079,   624,  1014,  1099,  1016,   128,  1018,  1063,  1019,   129,
     130,  1082,  1069,   646,  1060,    22,    23,  1065,  1074,   636,
    1071,  1209,  1080,  1101,  1211,  1073,    34,    29,    30,    31,
      32,   653,  1103,  1218,  1075,  1078,  1085,  1091,  1096,   645,
    1106,   646,  1110,  1113,  1118,   701,   589,    26,  1229,  1117,
    1126,  1141,  1232,  1128,  1130,  1132,  1133,  1145,  1171,   653,
    1146,  1147,  1239,  1148,  1149,   110,    61,   111,   112,   637,
    1150,  1151,  1154,   113,  1158,   114,   859,  1160,  1166,    34,
    1170,  1172,  1177,  1185,  1192,  1186,  1188,  1187,  1189,  1195,
    1229,  1190,  1191,  1198,  1258,  1259,  1260,   637,   646,   828,
    1230,  1210,  1269,  1200,  1201,  1272,  1217,   723,  1202,   115,
      29,    30,    31,    32,  1207,  1282,   653,  1212,  1213,   934,
    1220,   934,   284,   285,   286,   287,   288,   289,   290,   291,
    1221,  1248,  1222,   297,   298,  1223,  1224,  1233,  1294,  1234,
     934,  1243,   934,  1247,  1277,  1256,  1270,  1303,   536,  1274,
    1278,   116,   117,  1283,   637,  1279,   168,  1292,  1314,  1284,
    1286,   170,    34,  1288,  1295,  1297,  1300,  1319,  1305,  1304,
     118,  1306,  1307,  1308,  1310,   578,   971,  1323,  1313,  1315,
    1316,   337,   110,    61,   111,   112,  1317,  1325,  1237,  1330,
     113,  1332,   114,   119,   120,   121,   122,   123,   124,  1334,
    1336,   723,  1338,   421,   513,   810,   582,  1340,    92,   700,
    1038,   125,   126,   263,  1237,   666,   265,   841,   551,   846,
     854,  1257,   829,   127,  1031,   950,   115,  1020,  1244,   257,
     128,  1037,   813,    38,   129,   130,   524,   390,   391,   392,
     393,   394,   395,   396,   397,   398,   399,   400,   401,   402,
     403,   404,   405,   406,   407,   408,   409,   410,   411,   412,
     413,   414,   415,   110,    61,   111,   112,   452,   116,   117,
      39,   113,   459,   114,   910,   959,  1123,    45,    46,   980,
     613,  1153,   366,   110,    61,   111,   112,   118,  -110,  -110,
    -110,   113,   451,   114,   907,   136,  1183,   918,   722,   603,
     604,   605,   606,   607,   954,  1077,   531,   115,   991,   526,
     119,   120,   121,   122,   123,   124,  1255,   528,   919,  1162,
     695,   975,   943,   944,  1012,     0,   615,   115,   125,   126,
    1010,   110,    61,   111,   112,     0,  1225,   608,   609,   113,
     127,   114,  1001,  1002,     0,     0,     0,   128,     0,   116,
     117,   129,   130,   226,   227,   228,   229,   230,   231,   232,
     233,   234,   235,   236,   237,   238,   239,   240,   118,   116,
     117,     0,     0,     0,     0,   115,     0,     0,     0,   110,
      61,   111,   112,     0,     0,     0,     0,   113,   118,   114,
       0,   119,   120,   121,   122,   123,   124,     0,  1226,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   125,
     126,   119,   120,   121,   122,   123,   124,   116,   117,     0,
       0,   127,     0,   115,     0,     0,     0,     0,   128,   125,
     126,     0,   129,   130,     0,     0,   118,   110,    61,   111,
     112,   127,     0,     0,     0,   113,     0,   114,   128,     0,
       0,     0,   129,   130,     0,     0,     0,     0,     0,   119,
     120,   121,   122,   123,   124,   116,   117,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   125,   126,     0,
       0,   115,     0,     0,   118,   110,    61,   111,   112,   127,
       0,     0,     0,   113,     0,   114,   128,     0,     0,     0,
     129,   130,     0,     0,     0,     0,     0,   119,   120,   121,
     122,   123,   124,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   116,   117,   125,   126,     0,     0,   115,
       0,     0,     0,     0,     0,     0,   541,   127,     0,     0,
       0,     0,   118,     0,   128,     0,     0,     0,   129,   130,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   771,     0,     0,   119,   120,   121,   122,   123,
     124,   116,   117,     0,     0,     0,     0,   110,    61,   111,
     112,     0,     0,   125,   126,   113,     0,   114,     0,     0,
     118,     0,     0,     0,     0,   127,     0,     0,     0,     0,
       0,     0,   128,     0,     0,     0,   129,   130,     0,     0,
       0,     0,     0,   119,   120,   121,   122,   123,   124,     0,
       0,   115,  1281,     0,     0,   110,    61,   111,   112,     0,
       0,   125,   126,   113,     0,   114,     0,     0,  1291,     0,
       0,     0,     0,   127,   826,     0,     0,     0,     0,     0,
     128,     0,     0,  1302,   129,   130,     0,     0,     0,     0,
       0,  1309,     0,   116,   117,     0,     0,     0,     0,   115,
       0,     0,  1318,     0,   982,   110,    61,   111,   112,     0,
       0,  1322,   118,   113,  1324,   114,  1326,     0,  1329,     0,
       0,  1331,     0,  1333,     0,  1335,     0,  1337,     0,  1339,
       0,     0,     0,     0,     0,   119,   120,   121,   122,   123,
     124,   116,   117,     0,     0,     0,     0,     0,     0,   115,
       0,     0,   984,   125,   126,     0,   110,    61,   111,   112,
     118,     0,     0,     0,   113,   127,   114,     0,     0,     0,
       0,     0,   128,     0,     0,     0,   129,   130,     0,     0,
       0,     0,     0,   119,   120,   121,   122,   123,   124,     0,
       0,   116,   117,     0,     0,     0,     0,     0,     0,     0,
     115,   125,   126,     0,   110,    61,   111,   112,     0,     0,
     118,     0,   113,   127,   114,     0,     0,     0,     0,     0,
     128,     0,     0,     0,   129,   130,     0,     0,     0,     0,
       0,     0,     0,   119,   120,   121,   122,   123,   124,     0,
       0,     0,   116,   117,     0,     0,     0,     0,   115,     0,
       0,   125,   126,  1050,     0,     0,     0,     0,     0,     0,
       0,   118,     0,   127,   986,     0,     0,     0,     0,     0,
     128,     0,     0,     0,   129,   130,     0,     0,     0,     0,
       0,     0,     0,     0,   119,   120,   121,   122,   123,   124,
     116,   117,     0,     0,     0,     0,   110,    61,   111,   112,
       0,  1052,   125,   126,   113,     0,   114,     0,     0,   118,
       0,     0,     0,     0,   127,     0,     0,     0,     0,     0,
       0,   128,     0,     0,     0,   129,   130,     0,     0,     0,
       0,     0,   119,   120,   121,   122,   123,   124,     0,     0,
     115,     0,     0,     0,   110,    61,   111,   112,     0,     0,
     125,   126,   113,     0,   114,     0,     0,     0,     0,     0,
       0,     0,   127,     0,     0,     0,     0,     0,     0,   128,
       0,     0,     0,   129,   130,     0,     0,     0,     0,     0,
       0,     0,   116,   117,     0,     0,     0,     0,   115,     0,
       0,     0,     0,  1054,   110,    61,   111,   112,   783,   784,
       0,   118,   113,     0,   114,     0,     0,     0,     0,     0,
       0,     0,   785,   786,   787,   788,   789,   790,   791,   792,
     793,   794,   795,   796,   119,   120,   121,   122,   123,   124,
     116,   117,     0,     0,     0,     0,     0,     0,   115,   797,
       0,     0,   125,   126,     0,   110,    61,   111,   112,   118,
       0,     0,     0,   113,   127,   114,   798,     0,     0,     0,
       0,   128,     0,     0,     0,   129,   130,     0,     0,     0,
       0,     0,   119,   120,   121,   122,   123,   124,     0,     0,
     116,   117,     0,     0,     0,     0,     0,     0,     0,   115,
     125,   126,     0,     0,     0,     0,     0,     0,     0,   118,
       0,     0,   127,  1089,     0,     0,     0,     0,     0,   128,
       0,     0,     0,   129,   130,     0,     0,     0,     0,     0,
       0,     0,   119,   120,   121,   122,   123,   124,     0,     0,
       0,   116,   117,     0,     0,   110,    61,   111,   112,     0,
     125,   126,     0,   113,     0,   114,     0,     0,     0,     0,
     118,     0,   127,     0,     0,     0,     0,     0,     0,   128,
       0,     0,     0,   129,   130,     0,     0,     0,     0,     0,
       0,     0,     0,   119,   120,   121,   122,   123,   124,   115,
       0,     0,     0,     0,     0,   110,    61,   111,   112,     0,
       0,   125,   126,   113,     0,   114,     0,     0,     0,     0,
       0,     0,     0,  1028,     0,     0,     0,     0,     0,     0,
     128,     0,     0,     0,   129,   130,     0,     0,     0,     0,
       0,   116,   117,     0,     0,     0,     0,     0,     0,   115,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   561,
     118,   622,     0,     0,     0,     0,     0,  -482,  -482,  -482,
    -482,     0,     0,     0,     4,     0,     0,   623,     0,     0,
     515,     0,     0,   119,   120,   121,   122,   123,   124,     0,
       0,   116,   117,     0,     0,     0,     0,     0,     0,   518,
       0,   125,   126,     0,     0,   624,     0,   625,     0,   626,
     118,     0,     0,  1167,     0,     0,     0,   627,   628,   629,
     128,     0,     0,     0,   129,   130,   630,   631,     0,   632,
       0,     0,   633,   119,   120,   121,   122,   123,   124,     0,
       0,     0,     0,     0,     0,    27,     6,     0,     0,   634,
       0,   125,   126,     0,     0,     0,     0,     0,   561,     0,
     622,     0,     0,  1261,     0,     0,  -482,  -482,  -482,  -482,
     128,     0,     0,     4,   129,   130,     0,     0,     7,   515,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   561,     0,   622,     0,     0,     0,   518,     0,
    -482,  -482,  -482,  -482,   624,     0,   625,     4,   626,     0,
       0,     0,     0,   515,   635,     0,   627,   628,   629,     0,
       0,     0,     0,     0,     0,   630,   631,     0,   632,     0,
       0,   633,   518,     0,     0,     0,     0,     0,   624,     0,
     625,     0,   626,     0,    27,     6,     0,     0,   634,  1119,
     627,   628,   629,     0,     0,     0,     0,     0,     0,   630,
     631,     0,   632,     0,     0,   633,   858,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     7,    27,     6,
       0,   561,   634,   622,     0,     0,     0,     0,     0,  -482,
    -482,  -482,  -482,     0,     0,     0,     4,     0,     0,     0,
       0,     0,   515,     0,     0,     0,     0,     0,     0,     0,
       0,     7,     0,   635,     0,   561,     0,   622,     0,     0,
       0,   518,     0,  -482,  -482,  -482,  -482,   624,  1092,   625,
       4,   626,     0,     0,     0,     0,   515,     0,     0,   627,
     628,   629,     0,     0,     0,     0,     0,     0,   630,   631,
       0,   632,     0,     0,   633,   518,     0,     0,     0,     0,
       0,     0,     0,   625,     0,   626,     0,    27,     6,     0,
       0,   634,     0,   627,   628,   629,     0,     0,     0,     0,
       0,     0,   630,   631,   755,   632,     0,   756,   633,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       7,    27,     6,     0,     0,   634,     0,    45,    46,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -110,  -110,
    -110,     0,     0,     0,     0,     0,     0,     0,     0,   603,
     604,   605,   606,   607,     7,     0,     0,     0,     0,     0,
       0,    48,     0,     0,   757,     0,     0,     0,     0,     0,
       0,    49,     0,    50,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   608,   609,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    28,     0,     0,     0,
       0,     0,     0,     0,    33,   214,   215,   216,   217,   218,
     219,   220,   221,   222,   223,   224,   225,   226,   227,   228,
     229,   230,   231,   232,   233,   234,   235,   236,   237,   238,
     239,   240,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   241,   214,   215,
     216,   217,   218,   219,   220,   221,   222,   223,   224,   225,
     226,   227,   228,   229,   230,   231,   232,   233,   234,   235,
     236,   237,   238,   239,   240,   215,   216,   217,   218,   219,
     220,   221,   222,   223,   224,   225,   226,   227,   228,   229,
     230,   231,   232,   233,   234,   235,   236,   237,   238,   239,
     240,   216,   217,   218,   219,   220,   221,   222,   223,   224,
     225,   226,   227,   228,   229,   230,   231,   232,   233,   234,
     235,   236,   237,   238,   239,   240,   218,   219,   220,   221,
     222,   223,   224,   225,   226,   227,   228,   229,   230,   231,
     232,   233,   234,   235,   236,   237,   238,   239,   240,   220,
     221,   222,   223,   224,   225,   226,   227,   228,   229,   230,
     231,   232,   233,   234,   235,   236,   237,   238,   239,   240,
     222,   223,   224,   225,   226,   227,   228,   229,   230,   231,
     232,   233,   234,   235,   236,   237,   238,   239,   240
};

static const yytype_int16 yycheck[] =
{
      76,    20,   424,   450,   358,   131,   548,   602,   623,     6,
      41,   842,   566,   450,    87,   458,     2,   518,  1021,   548,
     466,    97,   338,  1026,    43,    11,    12,    13,    14,    15,
      16,    17,    18,    19,   548,   495,   548,   353,   105,     6,
      67,    67,   139,   954,   877,   937,     4,    80,   631,     4,
      32,   127,   128,     2,    20,   429,    21,     0,  1126,    20,
     357,   131,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     4,     4,     4,    93,     2,    95,    47,   131,   631,
     632,     4,     4,   136,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     2,    91,    32,     4,    47,   154,   155,
     131,   548,    11,    12,    13,    14,    15,    16,    17,    18,
      19,   548,     4,   467,   729,   146,   559,   844,   564,    32,
     139,     4,    14,   581,    91,   158,   548,    93,   204,   162,
     857,     2,    93,  1025,   210,   211,   212,    47,   108,   493,
      11,    12,    13,    14,    15,    16,    17,    18,    19,   180,
       4,   597,  1220,   206,   207,     4,   602,   740,   108,   158,
     775,   715,    91,    92,   419,   719,    32,   116,   117,   168,
     119,   120,   121,   122,   123,   124,   125,   126,   160,   553,
     129,   130,   131,   499,    38,    39,   151,    14,   740,   242,
       4,     5,     6,     5,     4,   492,   161,   749,   108,    11,
      83,    13,   163,   256,   280,   281,   282,   283,   166,    21,
    1121,   753,   265,   168,   941,   942,  1108,   293,   294,   295,
      32,   816,   292,   160,   753,  1058,   159,   158,   160,   166,
     162,   162,   165,   162,    64,    31,   165,   106,   160,   336,
     162,  1244,  1245,  1111,     4,   114,    56,   160,   161,   707,
     163,   278,   278,   166,   162,   158,   332,   333,   759,   162,
      91,    92,  1160,    47,   340,   214,   215,   216,   217,   218,
     219,   220,   221,   222,   223,   224,   225,   226,   227,   228,
     229,   230,   231,   232,   233,   234,   235,   236,   237,   238,
     239,   546,   241,   159,   160,   847,   797,   798,   799,    95,
       0,  1028,   166,   855,   371,   372,   753,    47,   162,   769,
     386,   278,  1180,   584,   128,    47,   753,   336,  1207,     4,
     269,   822,   593,    23,   108,    91,    92,   759,   158,    65,
       8,   753,   162,    33,   165,  1166,   167,    29,    16,    17,
      18,    19,    34,    35,  1233,    23,    21,   161,   160,   161,
     158,   161,  1250,   158,   166,     4,    48,   165,  1256,   158,
     165,   915,   147,     4,     8,   797,   798,   799,   108,   154,
     816,   162,    16,    17,    18,    19,   108,    62,    63,    23,
     696,   697,   158,    91,   158,   937,  1113,   163,   464,   163,
     822,   943,   944,   107,    72,    95,   472,   473,   774,   165,
     843,   167,    91,    95,   353,   458,   782,    56,   158,     4,
     536,     8,     7,   163,     4,    56,   447,    95,   163,    16,
      17,    18,    19,   454,   165,   501,    23,   127,    77,    78,
      74,   162,    27,    28,   165,   127,    77,    78,   424,    14,
      15,   149,   150,    38,    39,    40,     4,   466,    38,    39,
     158,    95,    62,    63,    49,    50,    51,    52,    53,     5,
     149,   150,   538,     4,   450,    11,    61,    13,  1010,    64,
      45,    46,     4,  1025,    24,   424,    71,  1092,    73,     8,
    1177,  1010,   164,   536,   515,   434,   164,    16,    17,    18,
      19,     4,    87,    88,    23,   158,  1038,   424,    95,   448,
     163,   450,     4,   576,   577,   536,   559,  1061,  1062,  1038,
     168,   106,  1066,  1067,   511,   424,  1213,  1214,   168,   114,
     164,    11,  1137,   450,  1038,   556,  1038,   158,    25,    12,
      27,    28,   163,    30,   109,   110,   111,   112,  1265,   155,
    1267,   450,   618,    72,   511,   564,   149,   150,   902,   158,
     499,   155,   158,  1095,   163,   158,  1108,   163,     4,  1286,
     686,  1288,   548,  1010,    61,   919,    95,   164,   163,     4,
     158,   149,   150,  1010,    71,   163,    73,   158,   597,   610,
     158,   159,   163,   602,   158,   582,   161,   584,  1010,   163,
     158,  1038,    14,    15,   162,     8,   593,   158,   674,   548,
       4,  1038,   163,    16,    17,    18,    19,   158,   557,   685,
      23,     4,   163,   158,     4,   582,  1038,   584,   163,    27,
      28,   548,    44,    45,    46,    47,   593,   149,   150,     4,
     158,    22,   158,   686,   162,   164,   158,   163,   162,   548,
     158,   165,    55,     4,   162,   721,   158,   723,  1095,    12,
     162,   161,   158,    61,   730,   686,   162,   161,  1095,   158,
     158,     4,    84,    71,   162,    73,   158,    10,    27,    28,
     162,   620,   748,  1095,    14,    15,   162,   548,   709,   165,
     162,   161,    95,   165,    27,    28,   108,   109,   110,   111,
     112,   767,    38,    39,    40,    38,    39,    40,   161,  1253,
     776,   161,   778,   779,    44,    45,    46,    16,    17,    18,
      19,    54,   161,    56,   169,    58,    59,    60,    44,    45,
      46,  1275,   161,    66,    67,    68,   151,   152,   153,   154,
     155,   807,    38,    39,    40,  1289,   812,    85,    86,   161,
      96,    97,   818,   163,    87,    88,    16,    17,    18,    19,
     159,   164,   348,   349,    94,   490,   491,   833,  1034,  1035,
     103,   104,   105,   839,  1086,  1087,   106,   753,   844,   109,
     110,   111,   112,   161,   114,   851,    66,    67,    68,   294,
     295,   857,   163,   109,   110,   111,   112,   691,   692,   693,
     843,  1237,     4,   159,   161,   163,   158,   816,   169,   159,
       4,   877,   167,   879,   753,     4,   161,     4,   162,   167,
     167,   887,   164,   889,   890,   891,   892,   893,   894,   895,
     163,   161,   163,     4,   163,   168,   753,   145,   146,   147,
     148,   149,   150,   151,   152,   153,   154,   155,   914,   160,
     169,   158,   162,   162,   753,   149,   150,   151,   152,   153,
     154,   155,   162,   929,   162,   931,    11,    12,    13,    14,
      15,    16,    17,    18,    19,   941,   942,    14,    15,   165,
     158,   169,   948,   160,   155,     4,   162,   161,     4,     4,
     165,   163,   753,   832,   960,   961,   962,   963,   163,    36,
     163,     4,   158,     4,    41,    42,    43,    44,    45,    46,
     159,   162,   160,   158,   980,   169,   982,   158,   984,   165,
       3,     4,     5,     6,   163,     4,   162,    97,    11,     4,
      13,     4,   164,   162,   162,  1001,  1002,     4,   161,    22,
       4,   165,  1008,   163,     4,     4,    83,  1013,  1014,   161,
     971,   161,   161,  1019,     4,     4,   161,   163,   162,    14,
      15,   161,  1028,   161,    47,   161,   161,   159,  1034,  1035,
     161,   161,   109,   110,   111,   112,   159,   170,   162,    62,
      63,   158,   161,   161,  1050,   161,  1052,    77,  1054,    44,
      45,    46,  1058,   161,  1060,   161,  1039,  1063,   161,  1065,
     161,   163,   159,     4,   163,    93,    89,    90,    93,    93,
     162,    93,    14,    15,   165,   169,  1082,   158,   161,   161,
    1086,  1087,   161,   161,   161,   108,   162,     4,   163,   162,
    1096,   159,   159,  1099,  1010,  1101,   165,     4,   161,   161,
     161,   161,    44,    45,    46,   162,     4,  1113,   131,   132,
     133,   134,   135,   136,   109,   110,   111,   112,   113,   163,
     162,     4,  1038,   162,   162,   162,   149,   150,   151,   163,
     163,  1010,   158,   162,   161,     4,     4,   151,   161,   159,
       4,    54,   163,    79,   162,   168,   162,   159,   165,   172,
     173,   159,   162,  1010,   165,    14,    15,   163,   163,  1038,
     162,  1167,   158,     9,  1170,   162,   161,   109,   110,   111,
     112,  1010,   151,  1179,   162,   162,   162,   162,   159,  1095,
     144,  1038,   159,   158,   167,    44,    45,    46,  1194,   163,
     161,     4,  1198,   162,   162,   162,   162,   162,   144,  1038,
     163,   162,  1208,   163,   162,     3,     4,     5,     6,  1010,
     163,   163,   163,    11,   162,    13,  1095,    57,   158,   161,
     165,   144,   144,   162,    69,   163,   163,   162,   162,   162,
    1236,   163,   163,   159,  1240,  1241,  1242,  1038,  1095,    37,
      57,   162,  1248,   163,   163,  1251,   162,  1126,   163,    47,
     109,   110,   111,   112,   163,  1261,  1095,   161,   161,  1265,
     161,  1267,   191,   192,   193,   194,   195,   196,   197,   198,
     162,   165,   163,   202,   203,   163,   163,   163,  1284,    69,
    1286,   159,  1288,   162,   158,   162,   162,  1293,  1237,   162,
     162,    89,    90,   163,  1095,   162,    93,   158,  1304,   165,
     165,    95,   161,   165,   162,   162,   158,  1313,   162,   165,
     108,   162,   162,   162,   162,   499,   883,   158,   165,   162,
     162,   248,     3,     4,     5,     6,   163,   158,  1207,   158,
      11,   158,    13,   131,   132,   133,   134,   135,   136,   158,
     158,  1220,   158,   337,   448,   699,   508,   162,    50,   582,
     953,   149,   150,   164,  1233,   557,   166,   735,   486,   740,
     749,  1239,   160,   161,   942,   855,    47,   937,  1212,   155,
     168,   948,   709,     6,   172,   173,   450,   306,   307,   308,
     309,   310,   311,   312,   313,   314,   315,   316,   317,   318,
     319,   320,   321,   322,   323,   324,   325,   326,   327,   328,
     329,   330,   331,     3,     4,     5,     6,   363,    89,    90,
       6,    11,   374,    13,   821,   868,  1044,    27,    28,   100,
     533,  1080,   275,     3,     4,     5,     6,   108,    38,    39,
      40,    11,   361,    13,   816,    78,  1124,   832,   618,    49,
      50,    51,    52,    53,   865,   989,   455,    47,   905,   450,
     131,   132,   133,   134,   135,   136,  1236,   450,   833,  1095,
     575,   887,    62,    63,   924,    -1,   538,    47,   149,   150,
     921,     3,     4,     5,     6,    -1,     8,    87,    88,    11,
     161,    13,    62,    63,    -1,    -1,    -1,   168,    -1,    89,
      90,   172,   173,   141,   142,   143,   144,   145,   146,   147,
     148,   149,   150,   151,   152,   153,   154,   155,   108,    89,
      90,    -1,    -1,    -1,    -1,    47,    -1,    -1,    -1,     3,
       4,     5,     6,    -1,    -1,    -1,    -1,    11,   108,    13,
      -1,   131,   132,   133,   134,   135,   136,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
     150,   131,   132,   133,   134,   135,   136,    89,    90,    -1,
      -1,   161,    -1,    47,    -1,    -1,    -1,    -1,   168,   149,
     150,    -1,   172,   173,    -1,    -1,   108,     3,     4,     5,
       6,   161,    -1,    -1,    -1,    11,    -1,    13,   168,    -1,
      -1,    -1,   172,   173,    -1,    -1,    -1,    -1,    -1,   131,
     132,   133,   134,   135,   136,    89,    90,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,
      -1,    47,    -1,    -1,   108,     3,     4,     5,     6,   161,
      -1,    -1,    -1,    11,    -1,    13,   168,    -1,    -1,    -1,
     172,   173,    -1,    -1,    -1,    -1,    -1,   131,   132,   133,
     134,   135,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    90,   149,   150,    -1,    -1,    47,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,   108,    -1,   168,    -1,    -1,    -1,   172,   173,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,    -1,    -1,   131,   132,   133,   134,   135,
     136,    89,    90,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,    -1,    -1,   149,   150,    11,    -1,    13,    -1,    -1,
     108,    -1,    -1,    -1,    -1,   161,    -1,    -1,    -1,    -1,
      -1,    -1,   168,    -1,    -1,    -1,   172,   173,    -1,    -1,
      -1,    -1,    -1,   131,   132,   133,   134,   135,   136,    -1,
      -1,    47,  1261,    -1,    -1,     3,     4,     5,     6,    -1,
      -1,   149,   150,    11,    -1,    13,    -1,    -1,  1277,    -1,
      -1,    -1,    -1,   161,   162,    -1,    -1,    -1,    -1,    -1,
     168,    -1,    -1,  1292,   172,   173,    -1,    -1,    -1,    -1,
      -1,  1300,    -1,    89,    90,    -1,    -1,    -1,    -1,    47,
      -1,    -1,  1311,    -1,   100,     3,     4,     5,     6,    -1,
      -1,  1320,   108,    11,  1323,    13,  1325,    -1,  1327,    -1,
      -1,  1330,    -1,  1332,    -1,  1334,    -1,  1336,    -1,  1338,
      -1,    -1,    -1,    -1,    -1,   131,   132,   133,   134,   135,
     136,    89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    47,
      -1,    -1,   100,   149,   150,    -1,     3,     4,     5,     6,
     108,    -1,    -1,    -1,    11,   161,    13,    -1,    -1,    -1,
      -1,    -1,   168,    -1,    -1,    -1,   172,   173,    -1,    -1,
      -1,    -1,    -1,   131,   132,   133,   134,   135,   136,    -1,
      -1,    89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      47,   149,   150,    -1,     3,     4,     5,     6,    -1,    -1,
     108,    -1,    11,   161,    13,    -1,    -1,    -1,    -1,    -1,
     168,    -1,    -1,    -1,   172,   173,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   131,   132,   133,   134,   135,   136,    -1,
      -1,    -1,    89,    90,    -1,    -1,    -1,    -1,    47,    -1,
      -1,   149,   150,   100,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   108,    -1,   161,   162,    -1,    -1,    -1,    -1,    -1,
     168,    -1,    -1,    -1,   172,   173,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   131,   132,   133,   134,   135,   136,
      89,    90,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
      -1,   100,   149,   150,    11,    -1,    13,    -1,    -1,   108,
      -1,    -1,    -1,    -1,   161,    -1,    -1,    -1,    -1,    -1,
      -1,   168,    -1,    -1,    -1,   172,   173,    -1,    -1,    -1,
      -1,    -1,   131,   132,   133,   134,   135,   136,    -1,    -1,
      47,    -1,    -1,    -1,     3,     4,     5,     6,    -1,    -1,
     149,   150,    11,    -1,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   161,    -1,    -1,    -1,    -1,    -1,    -1,   168,
      -1,    -1,    -1,   172,   173,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    90,    -1,    -1,    -1,    -1,    47,    -1,
      -1,    -1,    -1,   100,     3,     4,     5,     6,   101,   102,
      -1,   108,    11,    -1,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   115,   116,   117,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   131,   132,   133,   134,   135,   136,
      89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    47,   142,
      -1,    -1,   149,   150,    -1,     3,     4,     5,     6,   108,
      -1,    -1,    -1,    11,   161,    13,   159,    -1,    -1,    -1,
      -1,   168,    -1,    -1,    -1,   172,   173,    -1,    -1,    -1,
      -1,    -1,   131,   132,   133,   134,   135,   136,    -1,    -1,
      89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    47,
     149,   150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,
      -1,    -1,   161,   162,    -1,    -1,    -1,    -1,    -1,   168,
      -1,    -1,    -1,   172,   173,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   131,   132,   133,   134,   135,   136,    -1,    -1,
      -1,    89,    90,    -1,    -1,     3,     4,     5,     6,    -1,
     149,   150,    -1,    11,    -1,    13,    -1,    -1,    -1,    -1,
     108,    -1,   161,    -1,    -1,    -1,    -1,    -1,    -1,   168,
      -1,    -1,    -1,   172,   173,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   131,   132,   133,   134,   135,   136,    47,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,    -1,
      -1,   149,   150,    11,    -1,    13,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   161,    -1,    -1,    -1,    -1,    -1,    -1,
     168,    -1,    -1,    -1,   172,   173,    -1,    -1,    -1,    -1,
      -1,    89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    47,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     8,
     108,    10,    -1,    -1,    -1,    -1,    -1,    16,    17,    18,
      19,    -1,    -1,    -1,    23,    -1,    -1,    26,    -1,    -1,
      29,    -1,    -1,   131,   132,   133,   134,   135,   136,    -1,
      -1,    89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      -1,   149,   150,    -1,    -1,    54,    -1,    56,    -1,    58,
     108,    -1,    -1,   161,    -1,    -1,    -1,    66,    67,    68,
     168,    -1,    -1,    -1,   172,   173,    75,    76,    -1,    78,
      -1,    -1,    81,   131,   132,   133,   134,   135,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    94,    95,    -1,    -1,    98,
      -1,   149,   150,    -1,    -1,    -1,    -1,    -1,     8,    -1,
      10,    -1,    -1,   161,    -1,    -1,    16,    17,    18,    19,
     168,    -1,    -1,    23,   172,   173,    -1,    -1,   127,    29,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     8,    -1,    10,    -1,    -1,    -1,    48,    -1,
      16,    17,    18,    19,    54,    -1,    56,    23,    58,    -1,
      -1,    -1,    -1,    29,   163,    -1,    66,    67,    68,    -1,
      -1,    -1,    -1,    -1,    -1,    75,    76,    -1,    78,    -1,
      -1,    81,    48,    -1,    -1,    -1,    -1,    -1,    54,    -1,
      56,    -1,    58,    -1,    94,    95,    -1,    -1,    98,    99,
      66,    67,    68,    -1,    -1,    -1,    -1,    -1,    -1,    75,
      76,    -1,    78,    -1,    -1,    81,    82,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   127,    94,    95,
      -1,     8,    98,    10,    -1,    -1,    -1,    -1,    -1,    16,
      17,    18,    19,    -1,    -1,    -1,    23,    -1,    -1,    -1,
      -1,    -1,    29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   127,    -1,   163,    -1,     8,    -1,    10,    -1,    -1,
      -1,    48,    -1,    16,    17,    18,    19,    54,    55,    56,
      23,    58,    -1,    -1,    -1,    -1,    29,    -1,    -1,    66,
      67,    68,    -1,    -1,    -1,    -1,    -1,    -1,    75,    76,
      -1,    78,    -1,    -1,    81,    48,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    56,    -1,    58,    -1,    94,    95,    -1,
      -1,    98,    -1,    66,    67,    68,    -1,    -1,    -1,    -1,
      -1,    -1,    75,    76,     4,    78,    -1,     7,    81,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     127,    94,    95,    -1,    -1,    98,    -1,    27,    28,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    38,    39,
      40,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,
      50,    51,    52,    53,   127,    -1,    -1,    -1,    -1,    -1,
      -1,    61,    -1,    -1,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    88,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   106,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   114,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   147,   148,   149,   150,   151,   152,   153,
     154,   155,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   171,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   143,   144,   145,   146,   147,   148,   149,   150,
     151,   152,   153,   154,   155,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,   141,   142,   143,   144,   145,   146,   147,   148,   149,
     150,   151,   152,   153,   154,   155,   133,   134,   135,   136,
     137,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     147,   148,   149,   150,   151,   152,   153,   154,   155,   135,
     136,   137,   138,   139,   140,   141,   142,   143,   144,   145,
     146,   147,   148,   149,   150,   151,   152,   153,   154,   155,
     137,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     147,   148,   149,   150,   151,   152,   153,   154,   155
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   175,   176,     0,    23,    33,    95,   127,   177,   178,
     179,   181,   189,   208,   213,   218,   251,   320,   322,   376,
     182,   214,    14,    15,    44,    45,    46,    94,   106,   109,
     110,   111,   112,   114,   161,   188,   241,   242,   330,   344,
     346,   377,   378,   219,    25,    27,    28,    30,    61,    71,
      73,   180,   177,   177,   177,   177,   177,   177,   177,   177,
     177,     4,   184,   185,   186,   187,     4,   331,    14,    47,
     108,   260,    47,   108,   261,   345,   166,   243,   245,     4,
      14,   375,   187,   222,   225,   190,   321,   323,   209,     4,
      65,   265,   265,    21,   183,   158,    32,   159,   160,   215,
      44,    45,   241,   332,   333,   334,   162,   107,   347,   348,
       3,     5,     6,    11,    13,    47,    89,    90,   108,   131,
     132,   133,   134,   135,   136,   149,   150,   161,   168,   172,
     173,   187,   454,   477,   478,   481,   375,   163,   220,   165,
     223,     4,    47,    84,   108,   188,   312,   313,   314,   316,
     317,   318,   319,   333,   334,   319,     4,     4,   159,   165,
      44,    45,   113,   188,   241,   258,   259,     4,   184,    24,
     186,     4,   477,     4,   164,   192,   168,    47,   108,   335,
     335,    47,   108,   349,   168,    11,    12,   155,   178,   178,
     155,   178,   178,   178,   178,   178,   178,   178,   178,   477,
     477,   480,   178,   178,   166,   178,   243,   244,   245,   481,
      91,    92,   165,   167,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155,   171,   244,   245,   246,     4,   224,   225,   191,   245,
     315,   312,     4,   324,   325,   326,   312,   324,   210,   252,
       4,     4,     4,   260,     4,   261,   255,    22,   161,   161,
     199,     4,   336,   337,   245,   350,   351,   355,   356,    12,
     161,   161,   161,   161,   478,   478,   478,   478,   478,   478,
     478,   478,   162,   165,   158,   168,   169,   478,   478,   477,
     479,   243,   243,   477,   477,   477,   178,   178,   178,   178,
     178,   178,   178,   178,   178,   178,   178,   178,   178,   178,
     178,   178,   178,   178,   178,   178,   178,   178,   178,   178,
     178,   178,   161,   178,   163,   221,   158,   192,   158,   163,
     159,   243,   163,   163,   161,   263,     4,   159,   256,   257,
     243,   161,   266,   193,     4,   160,   162,   178,   200,   203,
     163,   159,   338,   158,   201,   169,   351,     4,   352,   353,
     188,   333,   334,   357,   358,   360,   477,   477,   477,   477,
     481,   477,   477,   480,   480,   167,   161,   167,   167,   167,
     478,   478,   478,   478,   478,   478,   478,   478,   478,   478,
     478,   478,   478,   478,   478,   478,   478,   478,   478,   478,
     478,   478,   478,   478,   478,   478,   477,   477,   164,   393,
     225,   199,   325,   477,   211,     4,   262,   264,   163,   253,
       4,   266,   266,   162,   267,   163,   178,   195,   196,   325,
     204,   205,   160,   229,   230,   231,   232,   233,   158,   201,
     216,   478,   337,   169,   354,   158,   163,   335,   335,   346,
     162,   162,   162,   162,   165,   169,   160,   458,   459,   460,
     477,   162,   165,   161,     4,   385,   389,   163,    31,   178,
     212,   251,   320,   322,   376,     4,   158,   162,   263,     4,
     163,   163,   178,   269,   270,   272,   273,    27,    28,   158,
     194,   159,   202,     4,   160,    38,    39,    40,   234,    96,
      97,   236,   245,   203,   162,    29,    34,    35,    48,   178,
     217,   218,   320,   322,   327,   363,   372,   376,   401,   412,
     245,   353,   246,   359,   477,   169,   187,   454,   158,   201,
     477,   160,   394,   395,   477,   386,   158,   163,   247,   165,
     409,   262,   163,   254,   273,   273,   233,   158,   201,     4,
     371,     8,    74,   164,   178,   181,   227,   320,   322,   363,
     376,   410,   420,   422,   425,   438,   197,   198,   196,   162,
     477,   206,   236,    97,    36,    41,    42,    43,    44,    45,
      83,   188,   238,   239,   240,   241,     4,   245,   328,   329,
       4,   227,   228,    49,    50,    51,    52,    53,    87,    88,
     233,   399,   400,   347,   162,   460,   162,     4,   158,   162,
     161,   385,    10,    26,    54,    56,    58,    66,    67,    68,
      75,    76,    78,    81,    98,   163,   178,   181,   218,   248,
     249,   251,   274,   284,   287,   320,   322,   327,   330,   341,
     342,   363,   372,   376,   379,   401,   410,   421,   439,   465,
     471,     4,   263,    72,    72,   245,   270,   268,   246,   165,
       4,     5,     6,   128,   161,    10,    54,    56,    58,    59,
      60,   103,   104,   105,   163,   168,   187,   454,   455,   425,
      16,    17,    18,    19,   163,   439,   319,   319,   202,   237,
     238,    44,   240,    47,   108,   235,   240,   207,   454,   158,
     163,   413,   373,   374,   455,   405,     4,     4,   245,   404,
     402,   161,   395,   178,   396,   397,   398,   470,   409,   472,
     161,   161,     4,    56,   275,   276,   278,     4,    56,    77,
     285,   286,   287,   291,   292,   293,   305,   310,   166,   288,
     289,   290,   310,   461,     4,     4,     7,    64,   344,   382,
       4,   339,   340,    16,    17,    18,    19,   161,   163,   271,
     162,   128,   226,   477,   427,   428,   161,   161,   161,   161,
     456,   477,   426,   101,   102,   115,   116,   117,   118,   119,
     120,   121,   122,   123,   124,   125,   126,   142,   159,   423,
      20,    93,   411,   411,   411,   163,   411,   161,   325,   325,
     235,   202,   159,   329,   161,   415,   158,   163,   159,   425,
     366,   367,   364,   425,   170,   406,   162,   477,    37,   160,
     477,   162,   158,   161,   457,   409,   477,   466,   161,   161,
      77,   275,   161,   311,   161,    77,   286,   161,   297,   298,
     163,   161,   308,   477,   289,   158,   163,   159,    82,   178,
     248,   462,   465,   471,   163,   380,   343,   228,   158,   163,
      93,    93,    93,    93,   477,   371,   162,   158,   162,   165,
     457,   409,   477,   430,   477,   477,   169,   158,   457,   228,
     228,   228,   161,   161,   161,   161,   477,   477,     4,    38,
      39,   162,   416,   417,   418,   419,   414,   374,   477,   368,
     368,   228,    21,   151,   161,   403,   162,     4,   398,   458,
     163,   473,   162,     4,    83,   463,   464,    62,    63,   281,
     477,   281,   246,   309,   477,    62,    63,   296,   299,   303,
     310,   159,   159,    62,    63,   306,   307,   309,   165,   163,
     290,   309,   163,   250,   393,     4,   361,   362,   381,   340,
     161,   161,   161,   161,   162,   226,   477,   163,   429,   162,
       4,   231,   424,   162,   162,   456,   163,   477,   477,   477,
     100,   477,   100,   477,   100,   477,   162,   477,   162,   158,
     201,   418,   163,   158,   163,   163,   369,   370,   371,   162,
      22,    62,    63,   151,   407,   408,   477,   425,   161,   201,
     461,   468,   463,   159,   163,   477,   162,   477,   162,   165,
     299,   303,   151,   149,   150,   158,   300,   302,   161,   294,
     309,   294,   310,   310,    80,   158,   162,   308,   247,     4,
     383,   384,   385,   387,   158,   163,   384,   390,   391,   392,
     100,   477,   100,   477,   100,   477,   477,   469,   158,   162,
     165,   441,   435,   159,   245,   163,   433,   434,   477,   162,
     477,   162,   477,   162,   163,   162,   437,   417,   162,     4,
     158,   365,   159,   477,   477,   162,    64,   158,   162,   162,
     477,   162,    55,   471,   474,   475,   159,   477,   477,    79,
     279,     9,   477,   151,   159,   302,   144,   303,   158,   302,
     159,   309,   477,   158,   295,   306,   306,   163,   167,    99,
     243,   158,   163,   362,   158,   163,   161,   477,   162,   477,
     162,   477,   162,   162,   448,   226,   477,    55,   425,   425,
     477,     4,   477,   425,   425,   162,   163,   162,   163,   162,
     163,   163,   440,   370,   163,   477,   408,   408,   162,   409,
      57,   476,   462,   477,   467,   477,   158,   161,   280,   477,
     165,   144,   144,   301,   304,   310,   303,   144,   295,   165,
     309,   388,   384,   391,   396,   162,   163,   162,   163,   162,
     163,   163,    69,   449,   450,   162,   409,   436,   159,   431,
     163,   163,   163,    85,    86,   444,   474,   163,   281,   477,
     162,   477,   161,   161,   158,   162,   304,   162,   477,   295,
     161,   162,   163,   163,   163,     8,    70,   452,   453,   477,
      57,   442,   477,   163,    69,   445,   446,   178,   422,   477,
      91,    92,   165,   159,   301,   304,   304,   162,   165,   396,
     451,   158,   165,   443,   422,   452,   162,   279,   477,   477,
     477,   161,   282,   283,   477,    91,   302,    91,   302,   477,
     162,   474,   477,   425,   162,   447,   474,   158,   162,   162,
     162,   283,   477,   163,   165,   309,   165,   309,   165,   432,
     425,   283,   158,   165,   477,   162,   309,   162,   309,   425,
     158,   277,   283,   477,   165,   162,   162,   162,   162,   283,
     162,   158,   162,   165,   477,   162,   162,   163,   283,   477,
     158,   162,   283,   158,   283,   158,   283,   158,   162,   283,
     158,   283,   158,   283,   158,   283,   158,   283,   158,   283,
     162
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int16 yyr1[] =
{
       0,   174,   176,   175,   177,   177,   177,   177,   177,   177,
     177,   177,   177,   177,   179,   178,   180,   180,   182,   183,
     181,   184,   184,   185,   185,   186,   186,   187,   187,   187,
     188,   188,   188,   190,   191,   189,   193,   194,   192,   192,
     195,   195,   196,   197,   196,   198,   196,   196,   199,   199,
     199,   200,   200,   201,   201,   202,   202,   204,   203,   205,
     206,   203,   207,   203,   203,   209,   210,   208,   211,   211,
     212,   212,   212,   212,   214,   215,   213,   216,   216,   217,
     217,   217,   217,   217,   217,   217,   217,   217,   219,   220,
     221,   218,   222,   223,   223,   224,   224,   225,   226,   226,
     227,   227,   227,   227,   227,   227,   227,   227,   228,   228,
     230,   229,   232,   231,   233,   233,   234,   234,   234,   235,
     235,   235,   236,   236,   236,   236,   237,   237,   238,   238,
     238,   238,   238,   238,   238,   238,   239,   239,   239,   240,
     240,   240,   241,   241,   241,   241,   241,   242,   242,   243,
     243,   243,   243,   244,   244,   245,   245,   246,   246,   247,
     247,   247,   247,   247,   248,   248,   248,   248,   248,   248,
     248,   248,   248,   248,   248,   248,   248,   248,   248,   248,
     248,   248,   248,   250,   249,   252,   251,   253,   251,   254,
     251,   255,   251,   256,   251,   257,   251,   258,   258,   258,
     258,   259,   259,   259,   260,   260,   260,   261,   261,   261,
     262,   262,   263,   263,   264,   264,   264,   264,   265,   265,
     266,   266,   267,   268,   266,   269,   269,   271,   270,   272,
     270,   273,   273,   274,   275,   275,   276,   276,   277,   277,
     278,   278,   279,   279,   280,   280,   280,   280,   281,   281,
     281,   282,   282,   282,   282,   282,   283,   283,   284,   284,
     285,   285,   286,   286,   286,   287,   287,   288,   289,   289,
     290,   291,   291,   292,   293,   293,   294,   294,   294,   295,
     295,   296,   296,   297,   297,   297,   298,   298,   298,   299,
     299,   300,   300,   301,   301,   302,   302,   302,   303,   304,
     305,   306,   306,   306,   307,   307,   307,   308,   309,   309,
     311,   310,   312,   312,   312,   313,   314,   315,   316,   317,
     317,   318,   319,   319,   319,   319,   319,   321,   320,   323,
     322,   324,   324,   325,   325,   326,   327,   328,   328,   329,
     331,   330,   332,   332,   332,   333,   334,   334,   335,   335,
     335,   336,   336,   337,   338,   338,   339,   339,   340,   341,
     343,   342,   345,   344,   346,   346,   347,   348,   348,   349,
     349,   349,   350,   350,   351,   352,   352,   354,   353,   356,
     355,   357,   357,   358,   359,   357,   360,   360,   361,   361,
     362,   364,   365,   363,   366,   363,   367,   363,   368,   368,
     369,   369,   370,   370,   371,   372,   373,   373,   374,   375,
     375,   376,   376,   377,   377,   377,   378,   378,   380,   379,
     381,   379,   382,   382,   383,   383,   384,   384,   386,   385,
     388,   387,   389,   389,   390,   390,   391,   392,   391,   393,
     393,   394,   394,   395,   395,   395,   395,   396,   397,   397,
     398,   398,   398,   398,   398,   398,   399,   399,   400,   400,
     402,   403,   401,   404,   401,   405,   401,   406,   406,   406,
     406,   406,   406,   407,   407,   407,   408,   408,   408,   409,
     409,   410,   410,   411,   411,   411,   413,   414,   412,   415,
     415,   416,   416,   417,   417,   418,   419,   419,   420,   420,
     420,   420,   420,   420,   420,   420,   420,   421,   421,   421,
     421,   421,   421,   421,   422,   422,   422,   422,   422,   423,
     423,   423,   423,   423,   423,   423,   423,   423,   423,   423,
     423,   424,   424,   424,   425,   425,   425,   425,   425,   425,
     425,   425,   425,   426,   425,   427,   425,   428,   429,   425,
     430,   431,   432,   425,   433,   425,   434,   425,   435,   436,
     425,   437,   425,   438,   438,   438,   438,   439,   439,   439,
     440,   440,   440,   441,   441,   443,   442,   442,   444,   444,
     446,   447,   445,   448,   448,   450,   451,   449,   452,   452,
     453,   453,   453,   453,   454,   454,   454,   455,   455,   456,
     456,   457,   457,   458,   458,   459,   459,   460,   461,   461,
     461,   462,   462,   462,   463,   464,   464,   464,   466,   467,
     465,   468,   465,   469,   465,   470,   465,   472,   473,   471,
     475,   474,   474,   476,   476,   477,   477,   478,   478,   478,
     478,   478,   478,   479,   478,   478,   478,   478,   478,   478,
     478,   478,   478,   478,   478,   478,   478,   478,   478,   478,
     478,   478,   478,   478,   478,   478,   478,   478,   478,   478,
     478,   478,   478,   478,   478,   478,   478,   478,   478,   478,
     478,   478,   478,   478,   478,   478,   478,   478,   478,   478,
     480,   480,   481,   481,   481,   481
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     0,     0,     2,     4,     0,     0,     0,
       5,     1,     0,     1,     3,     1,     3,     1,     3,     3,
       1,     1,     3,     0,     0,    11,     0,     0,     6,     0,
       1,     3,     0,     0,     5,     0,     5,     1,     2,     0,
       4,     1,     3,     1,     0,     2,     0,     0,     3,     0,
       0,     5,     0,     6,     3,     0,     0,     9,     2,     0,
       1,     1,     1,     1,     0,     0,     9,     2,     0,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     0,
       0,     9,     2,     2,     0,     1,     3,     1,     1,     5,
       2,     2,     2,     2,     4,     4,     6,     8,     1,     0,
       0,     5,     0,     4,     1,     1,     1,     1,     1,     1,
       1,     0,     2,     1,     1,     0,     1,     0,     1,     2,
       1,     2,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     5,
       5,     5,     3,     2,     2,     1,     0,     1,     1,     2,
       2,     2,     2,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     1,
       1,     1,     1,     0,     6,     0,     7,     0,     9,     0,
      11,     0,     9,     0,    10,     0,    10,     1,     2,     3,
       2,     0,     1,     1,     0,     1,     1,     0,     1,     1,
       2,     1,     3,     0,     3,     2,     1,     0,     1,     0,
       2,     0,     0,     0,     6,     1,     3,     0,     5,     0,
       2,     2,     0,     3,     2,     0,    10,    14,     2,     0,
       4,     0,     2,     0,     1,     5,     5,     5,     1,     1,
       0,     1,     5,     7,    13,    25,     1,     5,     3,     2,
       2,     1,     1,     1,     1,     3,     4,     5,     1,     3,
       3,     4,     0,     2,     4,     4,     4,     1,     2,     2,
       3,     1,     1,     7,    12,    11,     6,    12,    11,     2,
       3,     2,     3,     1,     3,     1,     1,     0,     1,     1,
       5,     2,     2,     1,     1,     3,     3,     1,     1,     5,
       0,     3,     1,     1,     0,     1,     1,     1,     2,     2,
       3,     2,     1,     1,     1,     1,     1,     0,     6,     0,
       6,     1,     3,     3,     1,     1,     3,     1,     3,     4,
       0,     7,     2,     3,     0,     1,     1,     1,     1,     1,
       0,     1,     3,     2,     2,     0,     1,     3,     1,     3,
       0,     5,     0,     3,     1,     1,     4,     2,     0,     1,
       1,     0,     1,     2,     3,     1,     3,     0,     3,     0,
       2,     1,     1,     0,     0,     4,     2,     3,     1,     3,
       1,     0,     0,     8,     0,     6,     0,     6,     0,     3,
       1,     3,     1,     3,     2,     4,     1,     3,     3,     1,
       1,     6,     4,     1,     2,     2,     1,     1,     0,     6,
       0,     6,     1,     1,     1,     3,     1,     1,     0,     5,
       0,     6,     1,     3,     1,     3,     1,     0,     4,     4,
       0,     1,     3,     0,     1,     4,     5,     1,     1,     3,
       1,     2,     6,     5,     3,     2,     1,     1,     1,     1,
       0,     0,     6,     0,     4,     0,     4,     4,     4,     3,
       3,     2,     0,     1,     3,     3,     2,     2,     1,     2,
       0,     2,     0,     1,     1,     0,     0,     0,     6,     2,
       4,     1,     3,     2,     1,     1,     1,     1,     7,     7,
       8,     8,     7,     6,     3,     7,     8,     7,     7,     8,
       8,     7,     7,     8,     5,     3,     3,     5,     5,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     3,     5,     1,     1,     1,     1,     1,     1,
       2,     2,     2,     0,     5,     0,     5,     0,     0,     8,
       0,     0,     0,    13,     0,     7,     0,     7,     0,     0,
       9,     0,     9,     1,     2,     2,     2,     1,     1,     1,
       2,     2,     0,     2,     0,     0,     3,     0,     2,     0,
       0,     0,     4,     2,     0,     0,     0,     4,     2,     1,
       1,     1,     1,     3,     6,     2,     2,     1,     3,     1,
       3,     4,     0,     1,     0,     1,     3,     1,     2,     2,
       0,     1,     1,     2,     1,     2,     4,     3,     0,     0,
      11,     0,     7,     0,     7,     0,     4,     0,     0,     7,
       0,     2,     1,     2,     0,     1,     6,     1,     4,     2,
       1,     1,     1,     0,     7,     5,     5,     3,     7,     3,
       6,     3,     4,     4,     4,     4,     4,     4,     3,     3,
       3,     3,     3,     3,     4,     4,     4,     4,     4,     4,
       4,     4,     4,     4,     4,     4,     4,     4,     4,     4,
       4,     4,     3,     3,     4,     4,     3,     5,     5,     5,
       1,     3,     1,     1,     2,     3
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = FRONTEND_VERILOG_YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == FRONTEND_VERILOG_YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        YY_LAC_DISCARD ("YYBACKUP");                              \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use FRONTEND_VERILOG_YYerror or FRONTEND_VERILOG_YYUNDEF. */
#define YYERRCODE FRONTEND_VERILOG_YYUNDEF

/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if FRONTEND_VERILOG_YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YYLOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

# ifndef YYLOCATION_PRINT

#  if defined YY_LOCATION_PRINT

   /* Temporary convenience wrapper in case some people defined the
      undocumented and private YY_LOCATION_PRINT macros.  */
#   define YYLOCATION_PRINT(File, Loc)  YY_LOCATION_PRINT(File, *(Loc))

#  elif defined FRONTEND_VERILOG_YYLTYPE_IS_TRIVIAL && FRONTEND_VERILOG_YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static int
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  int res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
}

#   define YYLOCATION_PRINT  yy_location_print_

    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT(File, Loc)  YYLOCATION_PRINT(File, &(Loc))

#  else

#   define YYLOCATION_PRINT(File, Loc) ((void) 0)
    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT  YYLOCATION_PRINT

#  endif
# endif /* !defined YYLOCATION_PRINT */


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, Location); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  YY_USE (yylocationp);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  YYLOCATION_PRINT (yyo, yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yykind, yyvaluep, yylocationp);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)],
                       &(yylsp[(yyi + 1) - (yynrhs)]));
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !FRONTEND_VERILOG_YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !FRONTEND_VERILOG_YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
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


/* Given a state stack such that *YYBOTTOM is its bottom, such that
   *YYTOP is either its top or is YYTOP_EMPTY to indicate an empty
   stack, and such that *YYCAPACITY is the maximum number of elements it
   can hold without a reallocation, make sure there is enough room to
   store YYADD more elements.  If not, allocate a new stack using
   YYSTACK_ALLOC, copy the existing elements, and adjust *YYBOTTOM,
   *YYTOP, and *YYCAPACITY to reflect the new capacity and memory
   location.  If *YYBOTTOM != YYBOTTOM_NO_FREE, then free the old stack
   using YYSTACK_FREE.  Return 0 if successful or if no reallocation is
   required.  Return YYENOMEM if memory is exhausted.  */
static int
yy_lac_stack_realloc (YYPTRDIFF_T *yycapacity, YYPTRDIFF_T yyadd,
#if FRONTEND_VERILOG_YYDEBUG
                      char const *yydebug_prefix,
                      char const *yydebug_suffix,
#endif
                      yy_state_t **yybottom,
                      yy_state_t *yybottom_no_free,
                      yy_state_t **yytop, yy_state_t *yytop_empty)
{
  YYPTRDIFF_T yysize_old =
    *yytop == yytop_empty ? 0 : *yytop - *yybottom + 1;
  YYPTRDIFF_T yysize_new = yysize_old + yyadd;
  if (*yycapacity < yysize_new)
    {
      YYPTRDIFF_T yyalloc = 2 * yysize_new;
      yy_state_t *yybottom_new;
      /* Use YYMAXDEPTH for maximum stack size given that the stack
         should never need to grow larger than the main state stack
         needs to grow without LAC.  */
      if (YYMAXDEPTH < yysize_new)
        {
          YYDPRINTF ((stderr, "%smax size exceeded%s", yydebug_prefix,
                      yydebug_suffix));
          return YYENOMEM;
        }
      if (YYMAXDEPTH < yyalloc)
        yyalloc = YYMAXDEPTH;
      yybottom_new =
        YY_CAST (yy_state_t *,
                 YYSTACK_ALLOC (YY_CAST (YYSIZE_T,
                                         yyalloc * YYSIZEOF (*yybottom_new))));
      if (!yybottom_new)
        {
          YYDPRINTF ((stderr, "%srealloc failed%s", yydebug_prefix,
                      yydebug_suffix));
          return YYENOMEM;
        }
      if (*yytop != yytop_empty)
        {
          YYCOPY (yybottom_new, *yybottom, yysize_old);
          *yytop = yybottom_new + (yysize_old - 1);
        }
      if (*yybottom != yybottom_no_free)
        YYSTACK_FREE (*yybottom);
      *yybottom = yybottom_new;
      *yycapacity = yyalloc;
    }
  return 0;
}

/* Establish the initial context for the current lookahead if no initial
   context is currently established.

   We define a context as a snapshot of the parser stacks.  We define
   the initial context for a lookahead as the context in which the
   parser initially examines that lookahead in order to select a
   syntactic action.  Thus, if the lookahead eventually proves
   syntactically unacceptable (possibly in a later context reached via a
   series of reductions), the initial context can be used to determine
   the exact set of tokens that would be syntactically acceptable in the
   lookahead's place.  Moreover, it is the context after which any
   further semantic actions would be erroneous because they would be
   determined by a syntactically unacceptable token.

   YY_LAC_ESTABLISH should be invoked when a reduction is about to be
   performed in an inconsistent state (which, for the purposes of LAC,
   includes consistent states that don't know they're consistent because
   their default reductions have been disabled).  Iff there is a
   lookahead token, it should also be invoked before reporting a syntax
   error.  This latter case is for the sake of the debugging output.

   For parse.lac=full, the implementation of YY_LAC_ESTABLISH is as
   follows.  If no initial context is currently established for the
   current lookahead, then check if that lookahead can eventually be
   shifted if syntactic actions continue from the current context.
   Report a syntax error if it cannot.  */
#define YY_LAC_ESTABLISH                                                \
do {                                                                    \
  if (!yy_lac_established)                                              \
    {                                                                   \
      YYDPRINTF ((stderr,                                               \
                  "LAC: initial context established for %s\n",          \
                  yysymbol_name (yytoken)));                            \
      yy_lac_established = 1;                                           \
      switch (yy_lac (yyesa, &yyes, &yyes_capacity, yyssp, yytoken))    \
        {                                                               \
        case YYENOMEM:                                                  \
          YYNOMEM;                                                      \
        case 1:                                                         \
          goto yyerrlab;                                                \
        }                                                               \
    }                                                                   \
} while (0)

/* Discard any previous initial lookahead context because of Event,
   which may be a lookahead change or an invalidation of the currently
   established initial context for the current lookahead.

   The most common example of a lookahead change is a shift.  An example
   of both cases is syntax error recovery.  That is, a syntax error
   occurs when the lookahead is syntactically erroneous for the
   currently established initial context, so error recovery manipulates
   the parser stacks to try to find a new initial context in which the
   current lookahead is syntactically acceptable.  If it fails to find
   such a context, it discards the lookahead.  */
#if FRONTEND_VERILOG_YYDEBUG
# define YY_LAC_DISCARD(Event)                                           \
do {                                                                     \
  if (yy_lac_established)                                                \
    {                                                                    \
      YYDPRINTF ((stderr, "LAC: initial context discarded due to "       \
                  Event "\n"));                                          \
      yy_lac_established = 0;                                            \
    }                                                                    \
} while (0)
#else
# define YY_LAC_DISCARD(Event) yy_lac_established = 0
#endif

/* Given the stack whose top is *YYSSP, return 0 iff YYTOKEN can
   eventually (after perhaps some reductions) be shifted, return 1 if
   not, or return YYENOMEM if memory is exhausted.  As preconditions and
   postconditions: *YYES_CAPACITY is the allocated size of the array to
   which *YYES points, and either *YYES = YYESA or *YYES points to an
   array allocated with YYSTACK_ALLOC.  yy_lac may overwrite the
   contents of either array, alter *YYES and *YYES_CAPACITY, and free
   any old *YYES other than YYESA.  */
static int
yy_lac (yy_state_t *yyesa, yy_state_t **yyes,
        YYPTRDIFF_T *yyes_capacity, yy_state_t *yyssp, yysymbol_kind_t yytoken)
{
  yy_state_t *yyes_prev = yyssp;
  yy_state_t *yyesp = yyes_prev;
  /* Reduce until we encounter a shift and thereby accept the token.  */
  YYDPRINTF ((stderr, "LAC: checking lookahead %s:", yysymbol_name (yytoken)));
  if (yytoken == YYSYMBOL_YYUNDEF)
    {
      YYDPRINTF ((stderr, " Always Err\n"));
      return 1;
    }
  while (1)
    {
      int yyrule = yypact[+*yyesp];
      if (yypact_value_is_default (yyrule)
          || (yyrule += yytoken) < 0 || YYLAST < yyrule
          || yycheck[yyrule] != yytoken)
        {
          /* Use the default action.  */
          yyrule = yydefact[+*yyesp];
          if (yyrule == 0)
            {
              YYDPRINTF ((stderr, " Err\n"));
              return 1;
            }
        }
      else
        {
          /* Use the action from yytable.  */
          yyrule = yytable[yyrule];
          if (yytable_value_is_error (yyrule))
            {
              YYDPRINTF ((stderr, " Err\n"));
              return 1;
            }
          if (0 < yyrule)
            {
              YYDPRINTF ((stderr, " S%d\n", yyrule));
              return 0;
            }
          yyrule = -yyrule;
        }
      /* By now we know we have to simulate a reduce.  */
      YYDPRINTF ((stderr, " R%d", yyrule - 1));
      {
        /* Pop the corresponding number of values from the stack.  */
        YYPTRDIFF_T yylen = yyr2[yyrule];
        /* First pop from the LAC stack as many tokens as possible.  */
        if (yyesp != yyes_prev)
          {
            YYPTRDIFF_T yysize = yyesp - *yyes + 1;
            if (yylen < yysize)
              {
                yyesp -= yylen;
                yylen = 0;
              }
            else
              {
                yyesp = yyes_prev;
                yylen -= yysize;
              }
          }
        /* Only afterwards look at the main stack.  */
        if (yylen)
          yyesp = yyes_prev -= yylen;
      }
      /* Push the resulting state of the reduction.  */
      {
        yy_state_fast_t yystate;
        {
          const int yylhs = yyr1[yyrule] - YYNTOKENS;
          const int yyi = yypgoto[yylhs] + *yyesp;
          yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyesp
                     ? yytable[yyi]
                     : yydefgoto[yylhs]);
        }
        if (yyesp == yyes_prev)
          {
            yyesp = *yyes;
            YY_IGNORE_USELESS_CAST_BEGIN
            *yyesp = YY_CAST (yy_state_t, yystate);
            YY_IGNORE_USELESS_CAST_END
          }
        else
          {
            if (yy_lac_stack_realloc (yyes_capacity, 1,
#if FRONTEND_VERILOG_YYDEBUG
                                      " (", ")",
#endif
                                      yyes, yyesa, &yyesp, yyes_prev))
              {
                YYDPRINTF ((stderr, "\n"));
                return YYENOMEM;
              }
            YY_IGNORE_USELESS_CAST_BEGIN
            *++yyesp = YY_CAST (yy_state_t, yystate);
            YY_IGNORE_USELESS_CAST_END
          }
        YYDPRINTF ((stderr, " G%d", yystate));
      }
    }
}

/* Context of a parse error.  */
typedef struct
{
  yy_state_t *yyssp;
  yy_state_t *yyesa;
  yy_state_t **yyes;
  YYPTRDIFF_T *yyes_capacity;
  yysymbol_kind_t yytoken;
  YYLTYPE *yylloc;
} yypcontext_t;

/* Put in YYARG at most YYARGN of the expected tokens given the
   current YYCTX, and return the number of tokens stored in YYARG.  If
   YYARG is null, return the number of expected tokens (guaranteed to
   be less than YYNTOKENS).  Return YYENOMEM on memory exhaustion.
   Return 0 if there are more than YYARGN expected tokens, yet fill
   YYARG up to YYARGN. */
static int
yypcontext_expected_tokens (const yypcontext_t *yyctx,
                            yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;

  int yyx;
  for (yyx = 0; yyx < YYNTOKENS; ++yyx)
    {
      yysymbol_kind_t yysym = YY_CAST (yysymbol_kind_t, yyx);
      if (yysym != YYSYMBOL_YYerror && yysym != YYSYMBOL_YYUNDEF)
        switch (yy_lac (yyctx->yyesa, yyctx->yyes, yyctx->yyes_capacity, yyctx->yyssp, yysym))
          {
          case YYENOMEM:
            return YYENOMEM;
          case 1:
            continue;
          default:
            if (!yyarg)
              ++yycount;
            else if (yycount == yyargn)
              return 0;
            else
              yyarg[yycount++] = yysym;
          }
    }
  if (yyarg && yycount == 0 && 0 < yyargn)
    yyarg[0] = YYSYMBOL_YYEMPTY;
  return yycount;
}




#ifndef yystrlen
# if defined __GLIBC__ && defined _STRING_H
#  define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
# else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
# endif
#endif

#ifndef yystpcpy
# if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#  define yystpcpy stpcpy
# else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
# endif
#endif

#ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
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
            else
              goto append;

          append:
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

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
#endif


static int
yy_syntax_error_arguments (const yypcontext_t *yyctx,
                           yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
       In the first two cases, it might appear that the current syntax
       error should have been detected in the previous state when yy_lac
       was invoked.  However, at that time, there might have been a
       different syntax error that discarded a different initial context
       during error recovery, leaving behind the current lookahead.
  */
  if (yyctx->yytoken != YYSYMBOL_YYEMPTY)
    {
      int yyn;
      YYDPRINTF ((stderr, "Constructing syntax error message\n"));
      if (yyarg)
        yyarg[yycount] = yyctx->yytoken;
      ++yycount;
      yyn = yypcontext_expected_tokens (yyctx,
                                        yyarg ? yyarg + 1 : yyarg, yyargn - 1);
      if (yyn == YYENOMEM)
        return YYENOMEM;
      else if (yyn == 0)
        YYDPRINTF ((stderr, "No expected tokens.\n"));
      else
        yycount += yyn;
    }
  return yycount;
}

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.  In order to see if a particular token T is a
   valid looakhead, invoke yy_lac (YYESA, YYES, YYES_CAPACITY, YYSSP, T).

   Return 0 if *YYMSG was successfully written.  Return -1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return YYENOMEM if the
   required number of bytes is too large to store or if
   yy_lac returned YYENOMEM.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                const yypcontext_t *yyctx)
{
  enum { YYARGS_MAX = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  yysymbol_kind_t yyarg[YYARGS_MAX];
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* Actual size of YYARG. */
  int yycount = yy_syntax_error_arguments (yyctx, yyarg, YYARGS_MAX);
  if (yycount == YYENOMEM)
    return YYENOMEM;

  switch (yycount)
    {
#define YYCASE_(N, S)                       \
      case N:                               \
        yyformat = S;                       \
        break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
    }

  /* Compute error message size.  Don't count the "%s"s, but reserve
     room for the terminator.  */
  yysize = yystrlen (yyformat) - 2 * yycount + 1;
  {
    int yyi;
    for (yyi = 0; yyi < yycount; ++yyi)
      {
        YYPTRDIFF_T yysize1
          = yysize + yytnamerr (YY_NULLPTR, yytname[yyarg[yyi]]);
        if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
          yysize = yysize1;
        else
          return YYENOMEM;
      }
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return -1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yytname[yyarg[yyi++]]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, YYLTYPE *yylocationp)
{
  YY_USE (yyvaluep);
  YY_USE (yylocationp);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}






/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
/* Lookahead token kind.  */
int yychar;


/* The semantic value of the lookahead symbol.  */
/* Default value used for initialization, for pacifying older GCCs
   or non-GCC compilers.  */
YY_INITIAL_VALUE (static YYSTYPE yyval_default;)
YYSTYPE yylval YY_INITIAL_VALUE (= yyval_default);

/* Location data for the lookahead symbol.  */
static YYLTYPE yyloc_default
# if defined FRONTEND_VERILOG_YYLTYPE_IS_TRIVIAL && FRONTEND_VERILOG_YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
YYLTYPE yylloc = yyloc_default;

    /* Number of syntax errors so far.  */
    int yynerrs = 0;

    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

    /* The location stack: array, bottom, top.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls = yylsa;
    YYLTYPE *yylsp = yyls;

    yy_state_t yyesa[20];
    yy_state_t *yyes = yyesa;
    YYPTRDIFF_T yyes_capacity = 20 < YYMAXDEPTH ? 20 : YYMAXDEPTH;

  /* Whether LAC context is established.  A Boolean.  */
  int yy_lac_established = 0;
  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[3];

  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = FRONTEND_VERILOG_YYEMPTY; /* Cause a token to be read.  */

  yylsp[0] = yylloc;
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yyls1, yysize * YYSIZEOF (*yylsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
        yyls = yyls1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == FRONTEND_VERILOG_YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex (&yylval, &yylloc);
    }

  if (yychar <= FRONTEND_VERILOG_YYEOF)
    {
      yychar = FRONTEND_VERILOG_YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == FRONTEND_VERILOG_YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = FRONTEND_VERILOG_YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      yyerror_range[1] = yylloc;
      goto yyerrlab1;
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
    {
      YY_LAC_ESTABLISH;
      goto yydefault;
    }
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      YY_LAC_ESTABLISH;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;

  /* Discard the shifted token.  */
  yychar = FRONTEND_VERILOG_YYEMPTY;
  YY_LAC_DISCARD ("shift");
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
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn);
  {
    int yychar_backup = yychar;
    switch (yyn)
      {
  case 2: /* $@1: %empty  */
#line 396 "verilog_parser.y"
       {
	ast_stack.clear();
	ast_stack.push_back(current_ast);
}
#line 3798 "verilog_parser.tab.cc"
    break;

  case 3: /* input: $@1 design  */
#line 399 "verilog_parser.y"
         {
	ast_stack.pop_back();
	log_assert(GetSize(ast_stack) == 0);
	for (auto &it : default_attr_list)
		delete it.second;
	default_attr_list.clear();
}
#line 3810 "verilog_parser.tab.cc"
    break;

  case 14: /* $@2: %empty  */
#line 420 "verilog_parser.y"
        {
		if (attr_list != nullptr)
			attr_list_stack.push(attr_list);
		attr_list = new dict<IdString, AstNode*>;
		for (auto &it : default_attr_list)
			(*attr_list)[it.first] = it.second->clone();
	}
#line 3822 "verilog_parser.tab.cc"
    break;

  case 15: /* attr: $@2 attr_opt  */
#line 426 "verilog_parser.y"
                   {
		(yyval.al) = attr_list;
		if (!attr_list_stack.empty()) {
			attr_list = attr_list_stack.top();
			attr_list_stack.pop();
		} else
			attr_list = nullptr;
	}
#line 3835 "verilog_parser.tab.cc"
    break;

  case 16: /* attr_opt: attr_opt ATTR_BEGIN opt_attr_list ATTR_END  */
#line 436 "verilog_parser.y"
                                                   {
		SET_RULE_LOC((yyloc), (yylsp[-2]), (yyloc));
	}
#line 3843 "verilog_parser.tab.cc"
    break;

  case 18: /* $@3: %empty  */
#line 442 "verilog_parser.y"
                      {
		if (attr_list != nullptr)
			attr_list_stack.push(attr_list);
		attr_list = new dict<IdString, AstNode*>;
		for (auto &it : default_attr_list)
			delete it.second;
		default_attr_list.clear();
	}
#line 3856 "verilog_parser.tab.cc"
    break;

  case 19: /* $@4: %empty  */
#line 449 "verilog_parser.y"
                        {
		attr_list->swap(default_attr_list);
		delete attr_list;
		if (!attr_list_stack.empty()) {
			attr_list = attr_list_stack.top();
			attr_list_stack.pop();
		} else
			attr_list = nullptr;
	}
#line 3870 "verilog_parser.tab.cc"
    break;

  case 25: /* attr_assign: hierarchical_id  */
#line 467 "verilog_parser.y"
                        {
		if (attr_list->count(*(yyvsp[0].string)) != 0)
			delete (*attr_list)[*(yyvsp[0].string)];
		(*attr_list)[*(yyvsp[0].string)] = AstNode::mkconst_int(1, false);
		delete (yyvsp[0].string);
	}
#line 3881 "verilog_parser.tab.cc"
    break;

  case 26: /* attr_assign: hierarchical_id '=' expr  */
#line 473 "verilog_parser.y"
                                 {
		if (attr_list->count(*(yyvsp[-2].string)) != 0)
			delete (*attr_list)[*(yyvsp[-2].string)];
		(*attr_list)[*(yyvsp[-2].string)] = (yyvsp[0].ast);
		delete (yyvsp[-2].string);
	}
#line 3892 "verilog_parser.tab.cc"
    break;

  case 27: /* hierarchical_id: TOK_ID  */
#line 481 "verilog_parser.y"
               {
		(yyval.string) = (yyvsp[0].string);
	}
#line 3900 "verilog_parser.tab.cc"
    break;

  case 28: /* hierarchical_id: hierarchical_id TOK_PACKAGESEP TOK_ID  */
#line 484 "verilog_parser.y"
                                              {
		if ((yyvsp[0].string)->compare(0, 1, "\\") == 0)
			*(yyvsp[-2].string) += "::" + (yyvsp[0].string)->substr(1);
		else
			*(yyvsp[-2].string) += "::" + *(yyvsp[0].string);
		delete (yyvsp[0].string);
		(yyval.string) = (yyvsp[-2].string);
	}
#line 3913 "verilog_parser.tab.cc"
    break;

  case 29: /* hierarchical_id: hierarchical_id '.' TOK_ID  */
#line 492 "verilog_parser.y"
                                   {
		if ((yyvsp[0].string)->compare(0, 1, "\\") == 0)
			*(yyvsp[-2].string) += "." + (yyvsp[0].string)->substr(1);
		else
			*(yyvsp[-2].string) += "." + *(yyvsp[0].string);
		delete (yyvsp[0].string);
		(yyval.string) = (yyvsp[-2].string);
	}
#line 3926 "verilog_parser.tab.cc"
    break;

  case 32: /* hierarchical_type_id: '(' TOK_USER_TYPE ')'  */
#line 504 "verilog_parser.y"
                                { (yyval.string) = (yyvsp[-1].string); }
#line 3932 "verilog_parser.tab.cc"
    break;

  case 33: /* $@5: %empty  */
#line 508 "verilog_parser.y"
                        {
		enterTypeScope();
	}
#line 3940 "verilog_parser.tab.cc"
    break;

  case 34: /* $@6: %empty  */
#line 510 "verilog_parser.y"
                 {
		do_not_require_port_stubs = false;
		AstNode *mod = new AstNode(AST_MODULE);
		ast_stack.back()->children.push_back(mod);
		ast_stack.push_back(mod);
		current_ast_mod = mod;
		port_stubs.clear();
		port_counter = 0;
		mod->str = *(yyvsp[0].string);
		append_attr(mod, (yyvsp[-3].al));
	}
#line 3956 "verilog_parser.tab.cc"
    break;

  case 35: /* module: attr TOK_MODULE $@5 TOK_ID $@6 module_para_opt module_args_opt ';' module_body TOK_ENDMODULE opt_label  */
#line 520 "verilog_parser.y"
                                                                                  {
		if (port_stubs.size() != 0)
			frontend_verilog_yyerror("Missing details for module port `%s'.",
					port_stubs.begin()->first.c_str());
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-9]), (yyloc));
		ast_stack.pop_back();
		log_assert(ast_stack.size() == 1);
		checkLabelsMatch("Module name", (yyvsp[-7].string), (yyvsp[0].string));
		current_ast_mod = NULL;
		delete (yyvsp[-7].string);
		delete (yyvsp[0].string);
		exitTypeScope();
	}
#line 3974 "verilog_parser.tab.cc"
    break;

  case 36: /* $@7: %empty  */
#line 535 "verilog_parser.y"
                { astbuf1 = nullptr; }
#line 3980 "verilog_parser.tab.cc"
    break;

  case 37: /* $@8: %empty  */
#line 535 "verilog_parser.y"
                                                        { if (astbuf1) delete astbuf1; }
#line 3986 "verilog_parser.tab.cc"
    break;

  case 43: /* $@9: %empty  */
#line 542 "verilog_parser.y"
                           {
		if (astbuf1) delete astbuf1;
		astbuf1 = new AstNode(AST_PARAMETER);
		astbuf1->children.push_back(AstNode::mkconst_int(0, true));
		append_attr(astbuf1, (yyvsp[-1].al));
	}
#line 3997 "verilog_parser.tab.cc"
    break;

  case 45: /* $@10: %empty  */
#line 548 "verilog_parser.y"
                            {
		if (astbuf1) delete astbuf1;
		astbuf1 = new AstNode(AST_LOCALPARAM);
		astbuf1->children.push_back(AstNode::mkconst_int(0, true));
		append_attr(astbuf1, (yyvsp[-1].al));
	}
#line 4008 "verilog_parser.tab.cc"
    break;

  case 55: /* module_arg_opt_assignment: '=' expr  */
#line 566 "verilog_parser.y"
                 {
		if (ast_stack.back()->children.size() > 0 && ast_stack.back()->children.back()->type == AST_WIRE) {
			if (ast_stack.back()->children.back()->is_input) {
				AstNode *n = ast_stack.back()->children.back();
				if (n->attributes.count(ID::defaultvalue))
					delete n->attributes.at(ID::defaultvalue);
				n->attributes[ID::defaultvalue] = (yyvsp[0].ast);
			} else {
				AstNode *wire = new AstNode(AST_IDENTIFIER);
				wire->str = ast_stack.back()->children.back()->str;
				if (ast_stack.back()->children.back()->is_reg || ast_stack.back()->children.back()->is_logic)
					ast_stack.back()->children.push_back(new AstNode(AST_INITIAL, new AstNode(AST_BLOCK, new AstNode(AST_ASSIGN_LE, wire, (yyvsp[0].ast)))));
				else
					ast_stack.back()->children.push_back(new AstNode(AST_ASSIGN, wire, (yyvsp[0].ast)));
			}
		} else
			frontend_verilog_yyerror("SystemVerilog interface in module port list cannot have a default value.");
	}
#line 4031 "verilog_parser.tab.cc"
    break;

  case 57: /* $@11: %empty  */
#line 587 "verilog_parser.y"
               {
		if (ast_stack.back()->children.size() > 0 && ast_stack.back()->children.back()->type == AST_WIRE) {
			AstNode *node = ast_stack.back()->children.back()->clone();
			node->str = *(yyvsp[0].string);
			node->port_id = ++port_counter;
			ast_stack.back()->children.push_back(node);
			SET_AST_NODE_LOC(node, (yylsp[0]), (yylsp[0]));
		} else {
			if (port_stubs.count(*(yyvsp[0].string)) != 0)
				frontend_verilog_yyerror("Duplicate module port `%s'.", (yyvsp[0].string)->c_str());
			port_stubs[*(yyvsp[0].string)] = ++port_counter;
		}
		delete (yyvsp[0].string);
	}
#line 4050 "verilog_parser.tab.cc"
    break;

  case 59: /* $@12: %empty  */
#line 601 "verilog_parser.y"
               {
		astbuf1 = new AstNode(AST_INTERFACEPORT);
		astbuf1->children.push_back(new AstNode(AST_INTERFACEPORTTYPE));
		astbuf1->children[0]->str = *(yyvsp[0].string);
		delete (yyvsp[0].string);
	}
#line 4061 "verilog_parser.tab.cc"
    break;

  case 60: /* $@13: %empty  */
#line 606 "verilog_parser.y"
                 {  /* SV interfaces */
		if (!sv_mode)
			frontend_verilog_yyerror("Interface found in port list (%s). This is not supported unless read_verilog is called with -sv!", (yyvsp[0].string)->c_str());
		astbuf2 = astbuf1->clone(); // really only needed if multiple instances of same type.
		astbuf2->str = *(yyvsp[0].string);
		delete (yyvsp[0].string);
		astbuf2->port_id = ++port_counter;
		ast_stack.back()->children.push_back(astbuf2);
		delete astbuf1; // really only needed if multiple instances of same type.
	}
#line 4076 "verilog_parser.tab.cc"
    break;

  case 62: /* $@14: %empty  */
#line 616 "verilog_parser.y"
                                    {
		AstNode *node = (yyvsp[-2].ast);
		node->str = *(yyvsp[0].string);
		SET_AST_NODE_LOC(node, (yylsp[0]), (yylsp[0]));
		node->port_id = ++port_counter;
		AstNode *range = checkRange(node, (yyvsp[-1].ast));
		if (range != NULL)
			node->children.push_back(range);
		if (!node->is_input && !node->is_output)
			frontend_verilog_yyerror("Module port `%s' is neither input nor output.", (yyvsp[0].string)->c_str());
		if (node->is_reg && node->is_input && !node->is_output && !sv_mode)
			frontend_verilog_yyerror("Input port `%s' is declared as register.", (yyvsp[0].string)->c_str());
		ast_stack.back()->children.push_back(node);
		append_attr(node, (yyvsp[-3].al));
		delete (yyvsp[0].string);
	}
#line 4097 "verilog_parser.tab.cc"
    break;

  case 64: /* module_arg: '.' '.' '.'  */
#line 632 "verilog_parser.y"
                    {
		do_not_require_port_stubs = true;
	}
#line 4105 "verilog_parser.tab.cc"
    break;

  case 65: /* $@15: %empty  */
#line 637 "verilog_parser.y"
                         {
		enterTypeScope();
	}
#line 4113 "verilog_parser.tab.cc"
    break;

  case 66: /* $@16: %empty  */
#line 639 "verilog_parser.y"
                 {
		AstNode *mod = new AstNode(AST_PACKAGE);
		ast_stack.back()->children.push_back(mod);
		ast_stack.push_back(mod);
		current_ast_mod = mod;
		mod->str = *(yyvsp[0].string);
		append_attr(mod, (yyvsp[-3].al));
	}
#line 4126 "verilog_parser.tab.cc"
    break;

  case 67: /* package: attr TOK_PACKAGE $@15 TOK_ID $@16 ';' package_body TOK_ENDPACKAGE opt_label  */
#line 646 "verilog_parser.y"
                                                    {
		ast_stack.pop_back();
		checkLabelsMatch("Package name", (yyvsp[-5].string), (yyvsp[0].string));
		current_ast_mod = NULL;
		delete (yyvsp[-5].string);
		delete (yyvsp[0].string);
		exitTypeScope();
	}
#line 4139 "verilog_parser.tab.cc"
    break;

  case 74: /* $@17: %empty  */
#line 662 "verilog_parser.y"
                      {
		enterTypeScope();
	}
#line 4147 "verilog_parser.tab.cc"
    break;

  case 75: /* $@18: %empty  */
#line 664 "verilog_parser.y"
                 {
		do_not_require_port_stubs = false;
		AstNode *intf = new AstNode(AST_INTERFACE);
		ast_stack.back()->children.push_back(intf);
		ast_stack.push_back(intf);
		current_ast_mod = intf;
		port_stubs.clear();
		port_counter = 0;
		intf->str = *(yyvsp[0].string);
		delete (yyvsp[0].string);
	}
#line 4163 "verilog_parser.tab.cc"
    break;

  case 76: /* interface: TOK_INTERFACE $@17 TOK_ID $@18 module_para_opt module_args_opt ';' interface_body TOK_ENDINTERFACE  */
#line 674 "verilog_parser.y"
                                                                              {
		if (port_stubs.size() != 0)
			frontend_verilog_yyerror("Missing details for module port `%s'.",
				port_stubs.begin()->first.c_str());
		ast_stack.pop_back();
		log_assert(ast_stack.size() == 1);
		current_ast_mod = NULL;
		exitTypeScope();
	}
#line 4177 "verilog_parser.tab.cc"
    break;

  case 88: /* $@19: %empty  */
#line 692 "verilog_parser.y"
                 {
		AstNode *bnode = new AstNode(AST_BIND);
		ast_stack.back()->children.push_back(bnode);
		ast_stack.push_back(bnode);
	}
#line 4187 "verilog_parser.tab.cc"
    break;

  case 89: /* $@20: %empty  */
#line 697 "verilog_parser.y"
                    {
		// bind_target should have added at least one child
		log_assert(ast_stack.back()->children.size() >= 1);
	}
#line 4196 "verilog_parser.tab.cc"
    break;

  case 90: /* $@21: %empty  */
#line 701 "verilog_parser.y"
               {
		// The single_cell parser in cell_list_no_array uses astbuf1 as
		// a sort of template for constructing cells.
		astbuf1 = new AstNode(AST_CELL);
		astbuf1->children.push_back(new AstNode(AST_CELLTYPE));
		astbuf1->children[0]->str = *(yyvsp[0].string);
		delete (yyvsp[0].string);
	}
#line 4209 "verilog_parser.tab.cc"
    break;

  case 91: /* bind_directive: TOK_BIND $@19 bind_target $@20 TOK_ID $@21 cell_parameter_list_opt cell_list_no_array ';'  */
#line 709 "verilog_parser.y"
                                                       {
		// cell_list should have added at least one more child
		log_assert(ast_stack.back()->children.size() >= 2);
		delete astbuf1;
		ast_stack.pop_back();
	}
#line 4220 "verilog_parser.tab.cc"
    break;

  case 97: /* bind_target_instance: hierarchical_id  */
#line 744 "verilog_parser.y"
                        {
		auto *node = new AstNode(AST_IDENTIFIER);
		node->str = *(yyvsp[0].string);
		delete (yyvsp[0].string);
		ast_stack.back()->children.push_back(node);
	}
#line 4231 "verilog_parser.tab.cc"
    break;

  case 98: /* mintypmax_expr: expr  */
#line 752 "verilog_parser.y"
             { delete (yyvsp[0].ast); }
#line 4237 "verilog_parser.tab.cc"
    break;

  case 99: /* mintypmax_expr: expr ':' expr ':' expr  */
#line 753 "verilog_parser.y"
                               { delete (yyvsp[-4].ast); delete (yyvsp[-2].ast); delete (yyvsp[0].ast); }
#line 4243 "verilog_parser.tab.cc"
    break;

  case 100: /* non_opt_delay: '#' TOK_ID  */
#line 756 "verilog_parser.y"
                   { delete (yyvsp[0].string); }
#line 4249 "verilog_parser.tab.cc"
    break;

  case 101: /* non_opt_delay: '#' TOK_CONSTVAL  */
#line 757 "verilog_parser.y"
                         { delete (yyvsp[0].string); }
#line 4255 "verilog_parser.tab.cc"
    break;

  case 102: /* non_opt_delay: '#' TOK_REALVAL  */
#line 758 "verilog_parser.y"
                        { delete (yyvsp[0].string); }
#line 4261 "verilog_parser.tab.cc"
    break;

  case 110: /* $@22: %empty  */
#line 770 "verilog_parser.y"
        { astbuf3 = new AstNode(AST_WIRE); current_wire_rand = false; current_wire_const = false; }
#line 4267 "verilog_parser.tab.cc"
    break;

  case 111: /* io_wire_type: $@22 wire_type_token_io wire_type_const_rand opt_wire_type_token wire_type_signedness  */
#line 772 "verilog_parser.y"
        { (yyval.ast) = astbuf3; SET_RULE_LOC((yyloc), (yylsp[-3]), (yyloc)); }
#line 4273 "verilog_parser.tab.cc"
    break;

  case 112: /* $@23: %empty  */
#line 775 "verilog_parser.y"
        { astbuf3 = new AstNode(AST_WIRE); current_wire_rand = false; current_wire_const = false; }
#line 4279 "verilog_parser.tab.cc"
    break;

  case 113: /* non_io_wire_type: $@23 wire_type_const_rand wire_type_token wire_type_signedness  */
#line 777 "verilog_parser.y"
        { (yyval.ast) = astbuf3; SET_RULE_LOC((yyloc), (yylsp[-2]), (yyloc)); }
#line 4285 "verilog_parser.tab.cc"
    break;

  case 116: /* wire_type_token_io: TOK_INPUT  */
#line 784 "verilog_parser.y"
                  {
		astbuf3->is_input = true;
	}
#line 4293 "verilog_parser.tab.cc"
    break;

  case 117: /* wire_type_token_io: TOK_OUTPUT  */
#line 787 "verilog_parser.y"
                   {
		astbuf3->is_output = true;
	}
#line 4301 "verilog_parser.tab.cc"
    break;

  case 118: /* wire_type_token_io: TOK_INOUT  */
#line 790 "verilog_parser.y"
                  {
		astbuf3->is_input = true;
		astbuf3->is_output = true;
	}
#line 4310 "verilog_parser.tab.cc"
    break;

  case 119: /* wire_type_signedness: TOK_SIGNED  */
#line 796 "verilog_parser.y"
                     { astbuf3->is_signed = true;  }
#line 4316 "verilog_parser.tab.cc"
    break;

  case 120: /* wire_type_signedness: TOK_UNSIGNED  */
#line 797 "verilog_parser.y"
                     { astbuf3->is_signed = false; }
#line 4322 "verilog_parser.tab.cc"
    break;

  case 122: /* wire_type_const_rand: TOK_RAND TOK_CONST  */
#line 801 "verilog_parser.y"
                           {
	    current_wire_rand = true;
	    current_wire_const = true;
	}
#line 4331 "verilog_parser.tab.cc"
    break;

  case 123: /* wire_type_const_rand: TOK_CONST  */
#line 805 "verilog_parser.y"
                  {
	    current_wire_const = true;
	}
#line 4339 "verilog_parser.tab.cc"
    break;

  case 124: /* wire_type_const_rand: TOK_RAND  */
#line 808 "verilog_parser.y"
                 {
	    current_wire_rand = true;
	}
#line 4347 "verilog_parser.tab.cc"
    break;

  case 128: /* wire_type_token: net_type  */
#line 818 "verilog_parser.y"
                 {
	}
#line 4354 "verilog_parser.tab.cc"
    break;

  case 129: /* wire_type_token: net_type logic_type  */
#line 820 "verilog_parser.y"
                            {
	}
#line 4361 "verilog_parser.tab.cc"
    break;

  case 130: /* wire_type_token: TOK_REG  */
#line 823 "verilog_parser.y"
                {
		astbuf3->is_reg = true;
	}
#line 4369 "verilog_parser.tab.cc"
    break;

  case 131: /* wire_type_token: TOK_VAR TOK_REG  */
#line 826 "verilog_parser.y"
                        {
		astbuf3->is_reg = true;
	}
#line 4377 "verilog_parser.tab.cc"
    break;

  case 132: /* wire_type_token: TOK_VAR  */
#line 830 "verilog_parser.y"
                {
		astbuf3->is_logic = true;
	}
#line 4385 "verilog_parser.tab.cc"
    break;

  case 133: /* wire_type_token: TOK_VAR logic_type  */
#line 833 "verilog_parser.y"
                           {
		astbuf3->is_logic = true;
	}
#line 4393 "verilog_parser.tab.cc"
    break;

  case 134: /* wire_type_token: logic_type  */
#line 836 "verilog_parser.y"
                   {
		astbuf3->is_logic = true;
	}
#line 4401 "verilog_parser.tab.cc"
    break;

  case 135: /* wire_type_token: TOK_GENVAR  */
#line 839 "verilog_parser.y"
                   {
		astbuf3->type = AST_GENVAR;
		astbuf3->is_reg = true;
		astbuf3->is_signed = true;
		astbuf3->range_left = 31;
		astbuf3->range_right = 0;
	}
#line 4413 "verilog_parser.tab.cc"
    break;

  case 136: /* net_type: TOK_WOR  */
#line 848 "verilog_parser.y"
                {
		astbuf3->is_wor = true;
	}
#line 4421 "verilog_parser.tab.cc"
    break;

  case 137: /* net_type: TOK_WAND  */
#line 851 "verilog_parser.y"
                 {
		astbuf3->is_wand = true;
	}
#line 4429 "verilog_parser.tab.cc"
    break;

  case 139: /* logic_type: TOK_LOGIC  */
#line 857 "verilog_parser.y"
                  {
	}
#line 4436 "verilog_parser.tab.cc"
    break;

  case 140: /* logic_type: integer_atom_type  */
#line 859 "verilog_parser.y"
                          {
		astbuf3->range_left = (yyvsp[0].integer) - 1;
		astbuf3->range_right = 0;
		astbuf3->is_signed = true;
	}
#line 4446 "verilog_parser.tab.cc"
    break;

  case 141: /* logic_type: hierarchical_type_id  */
#line 864 "verilog_parser.y"
                             {
		addWiretypeNode((yyvsp[0].string), astbuf3);
	}
#line 4454 "verilog_parser.tab.cc"
    break;

  case 142: /* integer_atom_type: TOK_INTEGER  */
#line 869 "verilog_parser.y"
                        { (yyval.integer) = 32; }
#line 4460 "verilog_parser.tab.cc"
    break;

  case 143: /* integer_atom_type: TOK_INT  */
#line 870 "verilog_parser.y"
                        { (yyval.integer) = 32; }
#line 4466 "verilog_parser.tab.cc"
    break;

  case 144: /* integer_atom_type: TOK_SHORTINT  */
#line 871 "verilog_parser.y"
                        { (yyval.integer) = 16; }
#line 4472 "verilog_parser.tab.cc"
    break;

  case 145: /* integer_atom_type: TOK_LONGINT  */
#line 872 "verilog_parser.y"
                        { (yyval.integer) = 64; }
#line 4478 "verilog_parser.tab.cc"
    break;

  case 146: /* integer_atom_type: TOK_BYTE  */
#line 873 "verilog_parser.y"
                        { (yyval.integer) =  8; }
#line 4484 "verilog_parser.tab.cc"
    break;

  case 147: /* integer_vector_type: TOK_LOGIC  */
#line 876 "verilog_parser.y"
                  { (yyval.integer) = TOK_LOGIC; }
#line 4490 "verilog_parser.tab.cc"
    break;

  case 148: /* integer_vector_type: TOK_REG  */
#line 877 "verilog_parser.y"
                  { (yyval.integer) = TOK_REG; }
#line 4496 "verilog_parser.tab.cc"
    break;

  case 149: /* non_opt_range: '[' expr ':' expr ']'  */
#line 880 "verilog_parser.y"
                              {
		(yyval.ast) = new AstNode(AST_RANGE);
		(yyval.ast)->children.push_back((yyvsp[-3].ast));
		(yyval.ast)->children.push_back((yyvsp[-1].ast));
	}
#line 4506 "verilog_parser.tab.cc"
    break;

  case 150: /* non_opt_range: '[' expr TOK_POS_INDEXED expr ']'  */
#line 885 "verilog_parser.y"
                                          {
		(yyval.ast) = new AstNode(AST_RANGE);
		AstNode *expr = new AstNode(AST_SELFSZ, (yyvsp[-3].ast));
		(yyval.ast)->children.push_back(new AstNode(AST_SUB, new AstNode(AST_ADD, expr->clone(), (yyvsp[-1].ast)), AstNode::mkconst_int(1, true)));
		(yyval.ast)->children.push_back(new AstNode(AST_ADD, expr, AstNode::mkconst_int(0, true)));
	}
#line 4517 "verilog_parser.tab.cc"
    break;

  case 151: /* non_opt_range: '[' expr TOK_NEG_INDEXED expr ']'  */
#line 891 "verilog_parser.y"
                                          {
		(yyval.ast) = new AstNode(AST_RANGE);
		AstNode *expr = new AstNode(AST_SELFSZ, (yyvsp[-3].ast));
		(yyval.ast)->children.push_back(new AstNode(AST_ADD, expr, AstNode::mkconst_int(0, true)));
		(yyval.ast)->children.push_back(new AstNode(AST_SUB, new AstNode(AST_ADD, expr->clone(), AstNode::mkconst_int(1, true)), (yyvsp[-1].ast)));
	}
#line 4528 "verilog_parser.tab.cc"
    break;

  case 152: /* non_opt_range: '[' expr ']'  */
#line 897 "verilog_parser.y"
                     {
		(yyval.ast) = new AstNode(AST_RANGE);
		(yyval.ast)->children.push_back((yyvsp[-1].ast));
	}
#line 4537 "verilog_parser.tab.cc"
    break;

  case 153: /* non_opt_multirange: non_opt_range non_opt_range  */
#line 903 "verilog_parser.y"
                                    {
		(yyval.ast) = new AstNode(AST_MULTIRANGE, (yyvsp[-1].ast), (yyvsp[0].ast));
	}
#line 4545 "verilog_parser.tab.cc"
    break;

  case 154: /* non_opt_multirange: non_opt_multirange non_opt_range  */
#line 906 "verilog_parser.y"
                                         {
		(yyval.ast) = (yyvsp[-1].ast);
		(yyval.ast)->children.push_back((yyvsp[0].ast));
	}
#line 4554 "verilog_parser.tab.cc"
    break;

  case 155: /* range: non_opt_range  */
#line 912 "verilog_parser.y"
                      {
		(yyval.ast) = (yyvsp[0].ast);
	}
#line 4562 "verilog_parser.tab.cc"
    break;

  case 156: /* range: %empty  */
#line 915 "verilog_parser.y"
               {
		(yyval.ast) = NULL;
	}
#line 4570 "verilog_parser.tab.cc"
    break;

  case 157: /* range_or_multirange: range  */
#line 920 "verilog_parser.y"
              { (yyval.ast) = (yyvsp[0].ast); }
#line 4576 "verilog_parser.tab.cc"
    break;

  case 158: /* range_or_multirange: non_opt_multirange  */
#line 921 "verilog_parser.y"
                           { (yyval.ast) = (yyvsp[0].ast); }
#line 4582 "verilog_parser.tab.cc"
    break;

  case 183: /* $@24: %empty  */
#line 937 "verilog_parser.y"
                               {
		AstNode *node = new AstNode(AST_GENBLOCK);
		node->str = *(yyvsp[-1].string);
		ast_stack.back()->children.push_back(node);
		ast_stack.push_back(node);
	}
#line 4593 "verilog_parser.tab.cc"
    break;

  case 184: /* checker_decl: TOK_CHECKER TOK_ID ';' $@24 module_body TOK_ENDCHECKER  */
#line 942 "verilog_parser.y"
                                     {
		delete (yyvsp[-4].string);
		ast_stack.pop_back();
	}
#line 4602 "verilog_parser.tab.cc"
    break;

  case 185: /* $@25: %empty  */
#line 948 "verilog_parser.y"
                                            {
		current_function_or_task = new AstNode(AST_DPI_FUNCTION, AstNode::mkconst_str(*(yyvsp[-1].string)), AstNode::mkconst_str(*(yyvsp[0].string)));
		current_function_or_task->str = *(yyvsp[0].string);
		append_attr(current_function_or_task, (yyvsp[-3].al));
		ast_stack.back()->children.push_back(current_function_or_task);
		delete (yyvsp[-1].string);
		delete (yyvsp[0].string);
	}
#line 4615 "verilog_parser.tab.cc"
    break;

  case 186: /* task_func_decl: attr TOK_DPI_FUNCTION TOK_ID TOK_ID $@25 opt_dpi_function_args ';'  */
#line 955 "verilog_parser.y"
                                    {
		current_function_or_task = NULL;
	}
#line 4623 "verilog_parser.tab.cc"
    break;

  case 187: /* $@26: %empty  */
#line 958 "verilog_parser.y"
                                                       {
		current_function_or_task = new AstNode(AST_DPI_FUNCTION, AstNode::mkconst_str(*(yyvsp[-1].string)), AstNode::mkconst_str(*(yyvsp[-3].string)));
		current_function_or_task->str = *(yyvsp[0].string);
		append_attr(current_function_or_task, (yyvsp[-5].al));
		ast_stack.back()->children.push_back(current_function_or_task);
		delete (yyvsp[-3].string);
		delete (yyvsp[-1].string);
		delete (yyvsp[0].string);
	}
#line 4637 "verilog_parser.tab.cc"
    break;

  case 188: /* task_func_decl: attr TOK_DPI_FUNCTION TOK_ID '=' TOK_ID TOK_ID $@26 opt_dpi_function_args ';'  */
#line 966 "verilog_parser.y"
                                    {
		current_function_or_task = NULL;
	}
#line 4645 "verilog_parser.tab.cc"
    break;

  case 189: /* $@27: %empty  */
#line 969 "verilog_parser.y"
                                                                  {
		current_function_or_task = new AstNode(AST_DPI_FUNCTION, AstNode::mkconst_str(*(yyvsp[-1].string)), AstNode::mkconst_str(*(yyvsp[-5].string) + ":" + RTLIL::unescape_id(*(yyvsp[-3].string))));
		current_function_or_task->str = *(yyvsp[0].string);
		append_attr(current_function_or_task, (yyvsp[-7].al));
		ast_stack.back()->children.push_back(current_function_or_task);
		delete (yyvsp[-5].string);
		delete (yyvsp[-3].string);
		delete (yyvsp[-1].string);
		delete (yyvsp[0].string);
	}
#line 4660 "verilog_parser.tab.cc"
    break;

  case 190: /* task_func_decl: attr TOK_DPI_FUNCTION TOK_ID ':' TOK_ID '=' TOK_ID TOK_ID $@27 opt_dpi_function_args ';'  */
#line 978 "verilog_parser.y"
                                    {
		current_function_or_task = NULL;
	}
#line 4668 "verilog_parser.tab.cc"
    break;

  case 191: /* $@28: %empty  */
#line 981 "verilog_parser.y"
                                           {
		current_function_or_task = new AstNode(AST_TASK);
		current_function_or_task->str = *(yyvsp[0].string);
		append_attr(current_function_or_task, (yyvsp[-3].al));
		ast_stack.back()->children.push_back(current_function_or_task);
		ast_stack.push_back(current_function_or_task);
		current_function_or_task_port_id = 1;
		delete (yyvsp[0].string);
	}
#line 4682 "verilog_parser.tab.cc"
    break;

  case 192: /* task_func_decl: attr TOK_TASK opt_automatic TOK_ID $@28 task_func_args_opt ';' task_func_body TOK_ENDTASK  */
#line 989 "verilog_parser.y"
                                                            {
		current_function_or_task = NULL;
		ast_stack.pop_back();
	}
#line 4691 "verilog_parser.tab.cc"
    break;

  case 193: /* $@29: %empty  */
#line 993 "verilog_parser.y"
                                                        {
		// The difference between void functions and tasks is that
		// always_comb's implicit sensitivity list behaves as if functions were
		// inlined, but ignores signals read only in tasks. This only matters
		// for event based simulation, and for synthesis we can treat a void
		// function like a task.
		current_function_or_task = new AstNode(AST_TASK);
		current_function_or_task->str = *(yyvsp[0].string);
		append_attr(current_function_or_task, (yyvsp[-4].al));
		ast_stack.back()->children.push_back(current_function_or_task);
		ast_stack.push_back(current_function_or_task);
		current_function_or_task_port_id = 1;
		delete (yyvsp[0].string);
	}
#line 4710 "verilog_parser.tab.cc"
    break;

  case 194: /* task_func_decl: attr TOK_FUNCTION opt_automatic TOK_VOID TOK_ID $@29 task_func_args_opt ';' task_func_body TOK_ENDFUNCTION  */
#line 1006 "verilog_parser.y"
                                                                {
		current_function_or_task = NULL;
		ast_stack.pop_back();
	}
#line 4719 "verilog_parser.tab.cc"
    break;

  case 195: /* $@30: %empty  */
#line 1010 "verilog_parser.y"
                                                                {
		current_function_or_task = new AstNode(AST_FUNCTION);
		current_function_or_task->str = *(yyvsp[0].string);
		append_attr(current_function_or_task, (yyvsp[-4].al));
		ast_stack.back()->children.push_back(current_function_or_task);
		ast_stack.push_back(current_function_or_task);
		AstNode *outreg = new AstNode(AST_WIRE);
		outreg->str = *(yyvsp[0].string);
		outreg->is_signed = false;
		outreg->is_reg = true;
		if ((yyvsp[-1].ast) != NULL) {
			outreg->children.push_back((yyvsp[-1].ast));
			outreg->is_signed = (yyvsp[-1].ast)->is_signed;
			(yyvsp[-1].ast)->is_signed = false;
			outreg->is_custom_type = (yyvsp[-1].ast)->type == AST_WIRETYPE;
		}
		current_function_or_task->children.push_back(outreg);
		current_function_or_task_port_id = 1;
		delete (yyvsp[0].string);
	}
#line 4744 "verilog_parser.tab.cc"
    break;

  case 196: /* task_func_decl: attr TOK_FUNCTION opt_automatic func_return_type TOK_ID $@30 task_func_args_opt ';' task_func_body TOK_ENDFUNCTION  */
#line 1029 "verilog_parser.y"
                                                                {
		current_function_or_task = NULL;
		ast_stack.pop_back();
	}
#line 4753 "verilog_parser.tab.cc"
    break;

  case 197: /* func_return_type: hierarchical_type_id  */
#line 1035 "verilog_parser.y"
                             {
		(yyval.ast) = new AstNode(AST_WIRETYPE);
		(yyval.ast)->str = *(yyvsp[0].string);
		delete (yyvsp[0].string);
	}
#line 4763 "verilog_parser.tab.cc"
    break;

  case 198: /* func_return_type: opt_type_vec opt_signedness_default_unsigned  */
#line 1040 "verilog_parser.y"
                                                     {
		(yyval.ast) = makeRange(0, 0, (yyvsp[0].boolean));
	}
#line 4771 "verilog_parser.tab.cc"
    break;

  case 199: /* func_return_type: opt_type_vec opt_signedness_default_unsigned non_opt_range  */
#line 1043 "verilog_parser.y"
                                                                   {
		(yyval.ast) = (yyvsp[0].ast);
		(yyval.ast)->is_signed = (yyvsp[-1].boolean);
	}
#line 4780 "verilog_parser.tab.cc"
    break;

  case 200: /* func_return_type: integer_atom_type opt_signedness_default_signed  */
#line 1047 "verilog_parser.y"
                                                        {
		(yyval.ast) = makeRange((yyvsp[-1].integer) - 1, 0, (yyvsp[0].boolean));
	}
#line 4788 "verilog_parser.tab.cc"
    break;

  case 204: /* opt_signedness_default_signed: %empty  */
#line 1058 "verilog_parser.y"
                        { (yyval.boolean) = true; }
#line 4794 "verilog_parser.tab.cc"
    break;

  case 205: /* opt_signedness_default_signed: TOK_SIGNED  */
#line 1059 "verilog_parser.y"
                        { (yyval.boolean) = true; }
#line 4800 "verilog_parser.tab.cc"
    break;

  case 206: /* opt_signedness_default_signed: TOK_UNSIGNED  */
#line 1060 "verilog_parser.y"
                        { (yyval.boolean) = false; }
#line 4806 "verilog_parser.tab.cc"
    break;

  case 207: /* opt_signedness_default_unsigned: %empty  */
#line 1063 "verilog_parser.y"
                        { (yyval.boolean) = false; }
#line 4812 "verilog_parser.tab.cc"
    break;

  case 208: /* opt_signedness_default_unsigned: TOK_SIGNED  */
#line 1064 "verilog_parser.y"
                        { (yyval.boolean) = true; }
#line 4818 "verilog_parser.tab.cc"
    break;

  case 209: /* opt_signedness_default_unsigned: TOK_UNSIGNED  */
#line 1065 "verilog_parser.y"
                        { (yyval.boolean) = false; }
#line 4824 "verilog_parser.tab.cc"
    break;

  case 210: /* dpi_function_arg: TOK_ID TOK_ID  */
#line 1069 "verilog_parser.y"
                      {
		current_function_or_task->children.push_back(AstNode::mkconst_str(*(yyvsp[-1].string)));
		delete (yyvsp[-1].string);
		delete (yyvsp[0].string);
	}
#line 4834 "verilog_parser.tab.cc"
    break;

  case 211: /* dpi_function_arg: TOK_ID  */
#line 1074 "verilog_parser.y"
               {
		current_function_or_task->children.push_back(AstNode::mkconst_str(*(yyvsp[0].string)));
		delete (yyvsp[0].string);
	}
#line 4843 "verilog_parser.tab.cc"
    break;

  case 222: /* $@31: %empty  */
#line 1094 "verilog_parser.y"
                               {
		albuf = nullptr;
		astbuf1 = nullptr;
		astbuf2 = nullptr;
	}
#line 4853 "verilog_parser.tab.cc"
    break;

  case 223: /* $@32: %empty  */
#line 1098 "verilog_parser.y"
                                        {
		delete astbuf1;
		if (astbuf2 != NULL)
			delete astbuf2;
		free_attr(albuf);
	}
#line 4864 "verilog_parser.tab.cc"
    break;

  case 227: /* $@33: %empty  */
#line 1109 "verilog_parser.y"
                             {
		bool prev_was_input = true;
		bool prev_was_output = false;
		if (albuf) {
			prev_was_input = astbuf1->is_input;
			prev_was_output = astbuf1->is_output;
			delete astbuf1;
			if (astbuf2 != NULL)
				delete astbuf2;
			free_attr(albuf);
		}
		albuf = (yyvsp[-2].al);
		astbuf1 = (yyvsp[-1].ast);
		astbuf2 = checkRange(astbuf1, (yyvsp[0].ast));
		if (!astbuf1->is_input && !astbuf1->is_output) {
			if (!sv_mode)
				frontend_verilog_yyerror("task/function argument direction missing");
			astbuf1->is_input = prev_was_input;
			astbuf1->is_output = prev_was_output;
		}
	}
#line 4890 "verilog_parser.tab.cc"
    break;

  case 229: /* $@34: %empty  */
#line 1130 "verilog_parser.y"
        {
		if (!astbuf1) {
			if (!sv_mode)
				frontend_verilog_yyerror("task/function argument direction missing");
			albuf = new dict<IdString, AstNode*>;
			astbuf1 = new AstNode(AST_WIRE);
			current_wire_rand = false;
			current_wire_const = false;
			astbuf1->is_input = true;
			astbuf2 = NULL;
		}
	}
#line 4907 "verilog_parser.tab.cc"
    break;

  case 236: /* specify_item: specify_if '(' specify_edge expr TOK_SPECIFY_OPER specify_target ')' '=' specify_rise_fall ';'  */
#line 1157 "verilog_parser.y"
                                                                                                       {
		AstNode *en_expr = (yyvsp[-9].ast);
		char specify_edge = (yyvsp[-7].ch);
		AstNode *src_expr = (yyvsp[-6].ast);
		string *oper = (yyvsp[-5].string);
		specify_target *target = (yyvsp[-4].specify_target_ptr);
		specify_rise_fall *timing = (yyvsp[-1].specify_rise_fall_ptr);

		if (specify_edge != 0 && target->dat == nullptr)
			frontend_verilog_yyerror("Found specify edge but no data spec.\n");

		AstNode *cell = new AstNode(AST_CELL);
		ast_stack.back()->children.push_back(cell);
		cell->str = stringf("$specify$%d", autoidx++);
		cell->children.push_back(new AstNode(AST_CELLTYPE));
		cell->children.back()->str = target->dat ? "$specify3" : "$specify2";
		SET_AST_NODE_LOC(cell, en_expr ? (yylsp[-9]) : (yylsp[-8]), (yylsp[0]));

		char oper_polarity = 0;
		char oper_type = oper->at(0);

		if (oper->size() == 3) {
			oper_polarity = oper->at(0);
			oper_type = oper->at(1);
		}

		cell->children.push_back(new AstNode(AST_PARASET, AstNode::mkconst_int(oper_type == '*', false, 1)));
		cell->children.back()->str = "\\FULL";

		cell->children.push_back(new AstNode(AST_PARASET, AstNode::mkconst_int(oper_polarity != 0, false, 1)));
		cell->children.back()->str = "\\SRC_DST_PEN";

		cell->children.push_back(new AstNode(AST_PARASET, AstNode::mkconst_int(oper_polarity == '+', false, 1)));
		cell->children.back()->str = "\\SRC_DST_POL";

		cell->children.push_back(new AstNode(AST_PARASET, timing->rise.t_min));
		cell->children.back()->str = "\\T_RISE_MIN";

		cell->children.push_back(new AstNode(AST_PARASET, timing->rise.t_avg));
		cell->children.back()->str = "\\T_RISE_TYP";

		cell->children.push_back(new AstNode(AST_PARASET, timing->rise.t_max));
		cell->children.back()->str = "\\T_RISE_MAX";

		cell->children.push_back(new AstNode(AST_PARASET, timing->fall.t_min));
		cell->children.back()->str = "\\T_FALL_MIN";

		cell->children.push_back(new AstNode(AST_PARASET, timing->fall.t_avg));
		cell->children.back()->str = "\\T_FALL_TYP";

		cell->children.push_back(new AstNode(AST_PARASET, timing->fall.t_max));
		cell->children.back()->str = "\\T_FALL_MAX";

		cell->children.push_back(new AstNode(AST_ARGUMENT, en_expr ? en_expr : AstNode::mkconst_int(1, false, 1)));
		cell->children.back()->str = "\\EN";

		cell->children.push_back(new AstNode(AST_ARGUMENT, src_expr));
		cell->children.back()->str = "\\SRC";

		cell->children.push_back(new AstNode(AST_ARGUMENT, target->dst));
		cell->children.back()->str = "\\DST";

		if (target->dat)
		{
			cell->children.push_back(new AstNode(AST_PARASET, AstNode::mkconst_int(specify_edge != 0, false, 1)));
			cell->children.back()->str = "\\EDGE_EN";

			cell->children.push_back(new AstNode(AST_PARASET, AstNode::mkconst_int(specify_edge == 'p', false, 1)));
			cell->children.back()->str = "\\EDGE_POL";

			cell->children.push_back(new AstNode(AST_PARASET, AstNode::mkconst_int(target->polarity_op != 0, false, 1)));
			cell->children.back()->str = "\\DAT_DST_PEN";

			cell->children.push_back(new AstNode(AST_PARASET, AstNode::mkconst_int(target->polarity_op == '+', false, 1)));
			cell->children.back()->str = "\\DAT_DST_POL";

			cell->children.push_back(new AstNode(AST_ARGUMENT, target->dat));
			cell->children.back()->str = "\\DAT";
		}

		delete oper;
		delete target;
		delete timing;
	}
#line 4996 "verilog_parser.tab.cc"
    break;

  case 237: /* specify_item: TOK_ID '(' specify_edge expr specify_condition ',' specify_edge expr specify_condition ',' specify_triple specify_opt_triple ')' ';'  */
#line 1241 "verilog_parser.y"
                                                                                                                                             {
		if (*(yyvsp[-13].string) != "$setup" && *(yyvsp[-13].string) != "$hold" && *(yyvsp[-13].string) != "$setuphold" && *(yyvsp[-13].string) != "$removal" && *(yyvsp[-13].string) != "$recovery" &&
				*(yyvsp[-13].string) != "$recrem" && *(yyvsp[-13].string) != "$skew" && *(yyvsp[-13].string) != "$timeskew" && *(yyvsp[-13].string) != "$fullskew" && *(yyvsp[-13].string) != "$nochange")
			frontend_verilog_yyerror("Unsupported specify rule type: %s\n", (yyvsp[-13].string)->c_str());

		AstNode *src_pen = AstNode::mkconst_int((yyvsp[-11].ch) != 0, false, 1);
		AstNode *src_pol = AstNode::mkconst_int((yyvsp[-11].ch) == 'p', false, 1);
		AstNode *src_expr = (yyvsp[-10].ast), *src_en = (yyvsp[-9].ast) ? (yyvsp[-9].ast) : AstNode::mkconst_int(1, false, 1);

		AstNode *dst_pen = AstNode::mkconst_int((yyvsp[-7].ch) != 0, false, 1);
		AstNode *dst_pol = AstNode::mkconst_int((yyvsp[-7].ch) == 'p', false, 1);
		AstNode *dst_expr = (yyvsp[-6].ast), *dst_en = (yyvsp[-5].ast) ? (yyvsp[-5].ast) : AstNode::mkconst_int(1, false, 1);

		specify_triple *limit = (yyvsp[-3].specify_triple_ptr);
		specify_triple *limit2 = (yyvsp[-2].specify_triple_ptr);

		AstNode *cell = new AstNode(AST_CELL);
		ast_stack.back()->children.push_back(cell);
		cell->str = stringf("$specify$%d", autoidx++);
		cell->children.push_back(new AstNode(AST_CELLTYPE));
		cell->children.back()->str = "$specrule";
		SET_AST_NODE_LOC(cell, (yylsp[-13]), (yylsp[0]));

		cell->children.push_back(new AstNode(AST_PARASET, AstNode::mkconst_str(*(yyvsp[-13].string))));
		cell->children.back()->str = "\\TYPE";

		cell->children.push_back(new AstNode(AST_PARASET, limit->t_min));
		cell->children.back()->str = "\\T_LIMIT_MIN";

		cell->children.push_back(new AstNode(AST_PARASET, limit->t_avg));
		cell->children.back()->str = "\\T_LIMIT_TYP";

		cell->children.push_back(new AstNode(AST_PARASET, limit->t_max));
		cell->children.back()->str = "\\T_LIMIT_MAX";

		cell->children.push_back(new AstNode(AST_PARASET, limit2 ? limit2->t_min : AstNode::mkconst_int(0, true)));
		cell->children.back()->str = "\\T_LIMIT2_MIN";

		cell->children.push_back(new AstNode(AST_PARASET, limit2 ? limit2->t_avg : AstNode::mkconst_int(0, true)));
		cell->children.back()->str = "\\T_LIMIT2_TYP";

		cell->children.push_back(new AstNode(AST_PARASET, limit2 ? limit2->t_max : AstNode::mkconst_int(0, true)));
		cell->children.back()->str = "\\T_LIMIT2_MAX";

		cell->children.push_back(new AstNode(AST_PARASET, src_pen));
		cell->children.back()->str = "\\SRC_PEN";

		cell->children.push_back(new AstNode(AST_PARASET, src_pol));
		cell->children.back()->str = "\\SRC_POL";

		cell->children.push_back(new AstNode(AST_PARASET, dst_pen));
		cell->children.back()->str = "\\DST_PEN";

		cell->children.push_back(new AstNode(AST_PARASET, dst_pol));
		cell->children.back()->str = "\\DST_POL";

		cell->children.push_back(new AstNode(AST_ARGUMENT, src_en));
		cell->children.back()->str = "\\SRC_EN";

		cell->children.push_back(new AstNode(AST_ARGUMENT, src_expr));
		cell->children.back()->str = "\\SRC";

		cell->children.push_back(new AstNode(AST_ARGUMENT, dst_en));
		cell->children.back()->str = "\\DST_EN";

		cell->children.push_back(new AstNode(AST_ARGUMENT, dst_expr));
		cell->children.back()->str = "\\DST";

		delete (yyvsp[-13].string);
		delete limit;
		delete limit2;
	}
#line 5073 "verilog_parser.tab.cc"
    break;

  case 238: /* specify_opt_triple: ',' specify_triple  */
#line 1315 "verilog_parser.y"
                           {
		(yyval.specify_triple_ptr) = (yyvsp[0].specify_triple_ptr);
	}
#line 5081 "verilog_parser.tab.cc"
    break;

  case 239: /* specify_opt_triple: %empty  */
#line 1318 "verilog_parser.y"
               {
		(yyval.specify_triple_ptr) = nullptr;
	}
#line 5089 "verilog_parser.tab.cc"
    break;

  case 240: /* specify_if: TOK_IF '(' expr ')'  */
#line 1323 "verilog_parser.y"
                            {
		(yyval.ast) = (yyvsp[-1].ast);
	}
#line 5097 "verilog_parser.tab.cc"
    break;

  case 241: /* specify_if: %empty  */
#line 1326 "verilog_parser.y"
               {
		(yyval.ast) = nullptr;
	}
#line 5105 "verilog_parser.tab.cc"
    break;

  case 242: /* specify_condition: TOK_SPECIFY_AND expr  */
#line 1331 "verilog_parser.y"
                             {
		(yyval.ast) = (yyvsp[0].ast);
	}
#line 5113 "verilog_parser.tab.cc"
    break;

  case 243: /* specify_condition: %empty  */
#line 1334 "verilog_parser.y"
               {
		(yyval.ast) = nullptr;
	}
#line 5121 "verilog_parser.tab.cc"
    break;

  case 244: /* specify_target: expr  */
#line 1339 "verilog_parser.y"
             {
		(yyval.specify_target_ptr) = new specify_target;
		(yyval.specify_target_ptr)->polarity_op = 0;
		(yyval.specify_target_ptr)->dst = (yyvsp[0].ast);
		(yyval.specify_target_ptr)->dat = nullptr;
	}
#line 5132 "verilog_parser.tab.cc"
    break;

  case 245: /* specify_target: '(' expr ':' expr ')'  */
#line 1345 "verilog_parser.y"
                             {
		(yyval.specify_target_ptr) = new specify_target;
		(yyval.specify_target_ptr)->polarity_op = 0;
		(yyval.specify_target_ptr)->dst = (yyvsp[-3].ast);
		(yyval.specify_target_ptr)->dat = (yyvsp[-1].ast);
	}
#line 5143 "verilog_parser.tab.cc"
    break;

  case 246: /* specify_target: '(' expr TOK_NEG_INDEXED expr ')'  */
#line 1351 "verilog_parser.y"
                                         {
		(yyval.specify_target_ptr) = new specify_target;
		(yyval.specify_target_ptr)->polarity_op = '-';
		(yyval.specify_target_ptr)->dst = (yyvsp[-3].ast);
		(yyval.specify_target_ptr)->dat = (yyvsp[-1].ast);
	}
#line 5154 "verilog_parser.tab.cc"
    break;

  case 247: /* specify_target: '(' expr TOK_POS_INDEXED expr ')'  */
#line 1357 "verilog_parser.y"
                                         {
		(yyval.specify_target_ptr) = new specify_target;
		(yyval.specify_target_ptr)->polarity_op = '+';
		(yyval.specify_target_ptr)->dst = (yyvsp[-3].ast);
		(yyval.specify_target_ptr)->dat = (yyvsp[-1].ast);
	}
#line 5165 "verilog_parser.tab.cc"
    break;

  case 248: /* specify_edge: TOK_POSEDGE  */
#line 1365 "verilog_parser.y"
                    { (yyval.ch) = 'p'; }
#line 5171 "verilog_parser.tab.cc"
    break;

  case 249: /* specify_edge: TOK_NEGEDGE  */
#line 1366 "verilog_parser.y"
                    { (yyval.ch) = 'n'; }
#line 5177 "verilog_parser.tab.cc"
    break;

  case 250: /* specify_edge: %empty  */
#line 1367 "verilog_parser.y"
               { (yyval.ch) = 0; }
#line 5183 "verilog_parser.tab.cc"
    break;

  case 251: /* specify_rise_fall: specify_triple  */
#line 1370 "verilog_parser.y"
                       {
		(yyval.specify_rise_fall_ptr) = new specify_rise_fall;
		(yyval.specify_rise_fall_ptr)->rise = *(yyvsp[0].specify_triple_ptr);
		(yyval.specify_rise_fall_ptr)->fall.t_min = (yyvsp[0].specify_triple_ptr)->t_min->clone();
		(yyval.specify_rise_fall_ptr)->fall.t_avg = (yyvsp[0].specify_triple_ptr)->t_avg->clone();
		(yyval.specify_rise_fall_ptr)->fall.t_max = (yyvsp[0].specify_triple_ptr)->t_max->clone();
		delete (yyvsp[0].specify_triple_ptr);
	}
#line 5196 "verilog_parser.tab.cc"
    break;

  case 252: /* specify_rise_fall: '(' specify_triple ',' specify_triple ')'  */
#line 1378 "verilog_parser.y"
                                                  {
		(yyval.specify_rise_fall_ptr) = new specify_rise_fall;
		(yyval.specify_rise_fall_ptr)->rise = *(yyvsp[-3].specify_triple_ptr);
		(yyval.specify_rise_fall_ptr)->fall = *(yyvsp[-1].specify_triple_ptr);
		delete (yyvsp[-3].specify_triple_ptr);
		delete (yyvsp[-1].specify_triple_ptr);
	}
#line 5208 "verilog_parser.tab.cc"
    break;

  case 253: /* specify_rise_fall: '(' specify_triple ',' specify_triple ',' specify_triple ')'  */
#line 1385 "verilog_parser.y"
                                                                     {
		(yyval.specify_rise_fall_ptr) = new specify_rise_fall;
		(yyval.specify_rise_fall_ptr)->rise = *(yyvsp[-5].specify_triple_ptr);
		(yyval.specify_rise_fall_ptr)->fall = *(yyvsp[-3].specify_triple_ptr);
		delete (yyvsp[-5].specify_triple_ptr);
		delete (yyvsp[-3].specify_triple_ptr);
		delete (yyvsp[-1].specify_triple_ptr);
		log_file_warning(current_filename, get_line_num(), "Path delay expressions beyond rise/fall not currently supported. Ignoring.\n");
	}
#line 5222 "verilog_parser.tab.cc"
    break;

  case 254: /* specify_rise_fall: '(' specify_triple ',' specify_triple ',' specify_triple ',' specify_triple ',' specify_triple ',' specify_triple ')'  */
#line 1394 "verilog_parser.y"
                                                                                                                              {
		(yyval.specify_rise_fall_ptr) = new specify_rise_fall;
		(yyval.specify_rise_fall_ptr)->rise = *(yyvsp[-11].specify_triple_ptr);
		(yyval.specify_rise_fall_ptr)->fall = *(yyvsp[-9].specify_triple_ptr);
		delete (yyvsp[-11].specify_triple_ptr);
		delete (yyvsp[-9].specify_triple_ptr);
		delete (yyvsp[-7].specify_triple_ptr);
		delete (yyvsp[-5].specify_triple_ptr);
		delete (yyvsp[-3].specify_triple_ptr);
		delete (yyvsp[-1].specify_triple_ptr);
		log_file_warning(current_filename, get_line_num(), "Path delay expressions beyond rise/fall not currently supported. Ignoring.\n");
	}
#line 5239 "verilog_parser.tab.cc"
    break;

  case 255: /* specify_rise_fall: '(' specify_triple ',' specify_triple ',' specify_triple ',' specify_triple ',' specify_triple ',' specify_triple ',' specify_triple ',' specify_triple ',' specify_triple ',' specify_triple ',' specify_triple ',' specify_triple ')'  */
#line 1406 "verilog_parser.y"
                                                                                                                                                                                                                                                {
		(yyval.specify_rise_fall_ptr) = new specify_rise_fall;
		(yyval.specify_rise_fall_ptr)->rise = *(yyvsp[-23].specify_triple_ptr);
		(yyval.specify_rise_fall_ptr)->fall = *(yyvsp[-21].specify_triple_ptr);
		delete (yyvsp[-23].specify_triple_ptr);
		delete (yyvsp[-21].specify_triple_ptr);
		delete (yyvsp[-19].specify_triple_ptr);
		delete (yyvsp[-17].specify_triple_ptr);
		delete (yyvsp[-15].specify_triple_ptr);
		delete (yyvsp[-13].specify_triple_ptr);
		delete (yyvsp[-11].specify_triple_ptr);
		delete (yyvsp[-9].specify_triple_ptr);
		delete (yyvsp[-7].specify_triple_ptr);
		delete (yyvsp[-5].specify_triple_ptr);
		delete (yyvsp[-3].specify_triple_ptr);
		delete (yyvsp[-1].specify_triple_ptr);
		log_file_warning(current_filename, get_line_num(), "Path delay expressions beyond rise/fall not currently supported. Ignoring.\n");
	}
#line 5262 "verilog_parser.tab.cc"
    break;

  case 256: /* specify_triple: expr  */
#line 1426 "verilog_parser.y"
             {
		(yyval.specify_triple_ptr) = new specify_triple;
		(yyval.specify_triple_ptr)->t_min = (yyvsp[0].ast);
		(yyval.specify_triple_ptr)->t_avg = (yyvsp[0].ast)->clone();
		(yyval.specify_triple_ptr)->t_max = (yyvsp[0].ast)->clone();
	}
#line 5273 "verilog_parser.tab.cc"
    break;

  case 257: /* specify_triple: expr ':' expr ':' expr  */
#line 1432 "verilog_parser.y"
                               {
		(yyval.specify_triple_ptr) = new specify_triple;
		(yyval.specify_triple_ptr)->t_min = (yyvsp[-4].ast);
		(yyval.specify_triple_ptr)->t_avg = (yyvsp[-2].ast);
		(yyval.specify_triple_ptr)->t_max = (yyvsp[0].ast);
	}
#line 5284 "verilog_parser.tab.cc"
    break;

  case 307: /* ignspec_constant_expression: expr  */
#line 1553 "verilog_parser.y"
             { delete (yyvsp[0].ast); }
#line 5290 "verilog_parser.tab.cc"
    break;

  case 308: /* ignspec_expr: expr  */
#line 1556 "verilog_parser.y"
             { delete (yyvsp[0].ast); }
#line 5296 "verilog_parser.tab.cc"
    break;

  case 309: /* ignspec_expr: expr ':' expr ':' expr  */
#line 1557 "verilog_parser.y"
                               {
		delete (yyvsp[-4].ast);
		delete (yyvsp[-2].ast);
		delete (yyvsp[0].ast);
	}
#line 5306 "verilog_parser.tab.cc"
    break;

  case 310: /* $@35: %empty  */
#line 1564 "verilog_parser.y"
               { delete (yyvsp[0].string); }
#line 5312 "verilog_parser.tab.cc"
    break;

  case 311: /* ignspec_id: TOK_ID $@35 range_or_multirange  */
#line 1565 "verilog_parser.y"
                            { delete (yyvsp[0].ast); }
#line 5318 "verilog_parser.tab.cc"
    break;

  case 312: /* param_signed: TOK_SIGNED  */
#line 1570 "verilog_parser.y"
                   {
		astbuf1->is_signed = true;
	}
#line 5326 "verilog_parser.tab.cc"
    break;

  case 313: /* param_signed: TOK_UNSIGNED  */
#line 1572 "verilog_parser.y"
                         {
		astbuf1->is_signed = false;
	}
#line 5334 "verilog_parser.tab.cc"
    break;

  case 315: /* param_integer: type_atom  */
#line 1577 "verilog_parser.y"
                  {
		astbuf1->is_reg = false;
	}
#line 5342 "verilog_parser.tab.cc"
    break;

  case 316: /* param_real: TOK_REAL  */
#line 1582 "verilog_parser.y"
                 {
		astbuf1->children.push_back(new AstNode(AST_REALVALUE));
	}
#line 5350 "verilog_parser.tab.cc"
    break;

  case 317: /* param_range: range  */
#line 1587 "verilog_parser.y"
              {
		if ((yyvsp[0].ast) != NULL) {
			astbuf1->children.push_back((yyvsp[0].ast));
		}
	}
#line 5360 "verilog_parser.tab.cc"
    break;

  case 319: /* param_range_type: type_vec param_signed  */
#line 1595 "verilog_parser.y"
                              {
		addRange(astbuf1, 0, 0);
	}
#line 5368 "verilog_parser.tab.cc"
    break;

  case 320: /* param_range_type: type_vec param_signed non_opt_range  */
#line 1598 "verilog_parser.y"
                                            {
		astbuf1->children.push_back((yyvsp[0].ast));
	}
#line 5376 "verilog_parser.tab.cc"
    break;

  case 326: /* param_type: hierarchical_type_id  */
#line 1605 "verilog_parser.y"
                             {
		addWiretypeNode((yyvsp[0].string), astbuf1);
	}
#line 5384 "verilog_parser.tab.cc"
    break;

  case 327: /* $@36: %empty  */
#line 1610 "verilog_parser.y"
                           {
		astbuf1 = new AstNode(AST_PARAMETER);
		astbuf1->children.push_back(AstNode::mkconst_int(0, true));
		append_attr(astbuf1, (yyvsp[-1].al));
	}
#line 5394 "verilog_parser.tab.cc"
    break;

  case 328: /* param_decl: attr TOK_PARAMETER $@36 param_type param_decl_list ';'  */
#line 1614 "verilog_parser.y"
                                         {
		delete astbuf1;
	}
#line 5402 "verilog_parser.tab.cc"
    break;

  case 329: /* $@37: %empty  */
#line 1619 "verilog_parser.y"
                            {
		astbuf1 = new AstNode(AST_LOCALPARAM);
		astbuf1->children.push_back(AstNode::mkconst_int(0, true));
		append_attr(astbuf1, (yyvsp[-1].al));
	}
#line 5412 "verilog_parser.tab.cc"
    break;

  case 330: /* localparam_decl: attr TOK_LOCALPARAM $@37 param_type param_decl_list ';'  */
#line 1623 "verilog_parser.y"
                                         {
		delete astbuf1;
	}
#line 5420 "verilog_parser.tab.cc"
    break;

  case 333: /* single_param_decl: single_param_decl_ident '=' expr  */
#line 1631 "verilog_parser.y"
                                         {
		AstNode *decl = ast_stack.back()->children.back();
		log_assert(decl->type == AST_PARAMETER || decl->type == AST_LOCALPARAM);
		delete decl->children[0];
		decl->children[0] = (yyvsp[0].ast);
	}
#line 5431 "verilog_parser.tab.cc"
    break;

  case 334: /* single_param_decl: single_param_decl_ident  */
#line 1637 "verilog_parser.y"
                                {
		AstNode *decl = ast_stack.back()->children.back();
		if (decl->type != AST_PARAMETER) {
			log_assert(decl->type == AST_LOCALPARAM);
			frontend_verilog_yyerror("localparam initialization is missing!");
		}
		if (!sv_mode)
			frontend_verilog_yyerror("Parameter defaults can only be omitted in SystemVerilog mode!");
		delete decl->children[0];
		decl->children.erase(decl->children.begin());
	}
#line 5447 "verilog_parser.tab.cc"
    break;

  case 335: /* single_param_decl_ident: TOK_ID  */
#line 1650 "verilog_parser.y"
               {
		AstNode *node;
		if (astbuf1 == nullptr) {
			if (!sv_mode)
				frontend_verilog_yyerror("In pure Verilog (not SystemVerilog), parameter/localparam with an initializer must use the parameter/localparam keyword");
			node = new AstNode(AST_PARAMETER);
			node->children.push_back(AstNode::mkconst_int(0, true));
		} else {
			node = astbuf1->clone();
		}
		node->str = *(yyvsp[0].string);
		ast_stack.back()->children.push_back(node);
		delete (yyvsp[0].string);
		SET_AST_NODE_LOC(node, (yylsp[0]), (yylsp[0]));
	}
#line 5467 "verilog_parser.tab.cc"
    break;

  case 339: /* single_defparam_decl: range rvalue '=' expr  */
#line 1673 "verilog_parser.y"
                              {
		AstNode *node = new AstNode(AST_DEFPARAM);
		node->children.push_back((yyvsp[-2].ast));
		node->children.push_back((yyvsp[0].ast));
		if ((yyvsp[-3].ast) != NULL)
			node->children.push_back((yyvsp[-3].ast));
		ast_stack.back()->children.push_back(node);
	}
#line 5480 "verilog_parser.tab.cc"
    break;

  case 340: /* $@38: %empty  */
#line 1686 "verilog_parser.y"
                    {
		static int enum_count;
		// create parent node for the enum
		astbuf2 = new AstNode(AST_ENUM);
		ast_stack.back()->children.push_back(astbuf2);
		astbuf2->str = std::string("$enum");
		astbuf2->str += std::to_string(enum_count++);
		// create the template for the names
		astbuf1 = new AstNode(AST_ENUM_ITEM);
		astbuf1->children.push_back(AstNode::mkconst_int(0, true));
	}
#line 5496 "verilog_parser.tab.cc"
    break;

  case 341: /* enum_type: TOK_ENUM $@38 enum_base_type '{' enum_name_list optional_comma '}'  */
#line 1696 "verilog_parser.y"
                                                               {
		// create template for the enum vars
		auto tnode = astbuf1->clone();
		delete astbuf1;
		astbuf1 = tnode;
		tnode->type = AST_WIRE;
		tnode->attributes[ID::enum_type] = AstNode::mkconst_str(astbuf2->str);
		// drop constant but keep any range
		delete tnode->children[0];
		tnode->children.erase(tnode->children.begin());
		(yyval.ast) = astbuf1;
	}
#line 5513 "verilog_parser.tab.cc"
    break;

  case 343: /* enum_base_type: type_vec type_signing range  */
#line 1710 "verilog_parser.y"
                                        { if ((yyvsp[0].ast)) astbuf1->children.push_back((yyvsp[0].ast)); }
#line 5519 "verilog_parser.tab.cc"
    break;

  case 344: /* enum_base_type: %empty  */
#line 1711 "verilog_parser.y"
                                        { astbuf1->is_reg = true; addRange(astbuf1); }
#line 5525 "verilog_parser.tab.cc"
    break;

  case 345: /* type_atom: integer_atom_type  */
#line 1715 "verilog_parser.y"
                          {
		astbuf1->is_reg = true;
		astbuf1->is_signed = true;
		addRange(astbuf1, (yyvsp[0].integer) - 1, 0);
	}
#line 5535 "verilog_parser.tab.cc"
    break;

  case 346: /* type_vec: TOK_REG  */
#line 1721 "verilog_parser.y"
                                { astbuf1->is_reg   = true; }
#line 5541 "verilog_parser.tab.cc"
    break;

  case 347: /* type_vec: TOK_LOGIC  */
#line 1722 "verilog_parser.y"
                                { astbuf1->is_logic = true; }
#line 5547 "verilog_parser.tab.cc"
    break;

  case 348: /* type_signing: TOK_SIGNED  */
#line 1726 "verilog_parser.y"
                                { astbuf1->is_signed = true; }
#line 5553 "verilog_parser.tab.cc"
    break;

  case 349: /* type_signing: TOK_UNSIGNED  */
#line 1727 "verilog_parser.y"
                                { astbuf1->is_signed = false; }
#line 5559 "verilog_parser.tab.cc"
    break;

  case 353: /* enum_name_decl: TOK_ID opt_enum_init  */
#line 1736 "verilog_parser.y"
                             {
		// put in fn
		log_assert(astbuf1);
		log_assert(astbuf2);
		auto node = astbuf1->clone();
		node->str = *(yyvsp[-1].string);
		delete (yyvsp[-1].string);
		SET_AST_NODE_LOC(node, (yylsp[-1]), (yylsp[-1]));
		delete node->children[0];
		node->children[0] = (yyvsp[0].ast) ? (yyvsp[0].ast) : new AstNode(AST_NONE);
		astbuf2->children.push_back(node);
	}
#line 5576 "verilog_parser.tab.cc"
    break;

  case 354: /* opt_enum_init: '=' basic_expr  */
#line 1751 "verilog_parser.y"
                                { (yyval.ast) = (yyvsp[0].ast); }
#line 5582 "verilog_parser.tab.cc"
    break;

  case 355: /* opt_enum_init: %empty  */
#line 1752 "verilog_parser.y"
                                { (yyval.ast) = NULL; }
#line 5588 "verilog_parser.tab.cc"
    break;

  case 358: /* enum_var: TOK_ID  */
#line 1760 "verilog_parser.y"
                 {
		log_assert(astbuf1);
		log_assert(astbuf2);
		auto node = astbuf1->clone();
		ast_stack.back()->children.push_back(node);
		node->str = *(yyvsp[0].string);
		delete (yyvsp[0].string);
		SET_AST_NODE_LOC(node, (yylsp[0]), (yylsp[0]));
		node->is_enum = true;
	}
#line 5603 "verilog_parser.tab.cc"
    break;

  case 359: /* enum_decl: enum_type enum_var_list ';'  */
#line 1772 "verilog_parser.y"
                                                { delete (yyvsp[-2].ast); }
#line 5609 "verilog_parser.tab.cc"
    break;

  case 360: /* $@39: %empty  */
#line 1780 "verilog_parser.y"
                         {
		append_attr((yyvsp[0].ast), (yyvsp[-1].al));
	}
#line 5617 "verilog_parser.tab.cc"
    break;

  case 361: /* struct_decl: attr struct_type $@39 struct_var_list ';'  */
#line 1782 "verilog_parser.y"
                              {
		delete astbuf2;
	}
#line 5625 "verilog_parser.tab.cc"
    break;

  case 362: /* $@40: %empty  */
#line 1787 "verilog_parser.y"
                          { astbuf2 = (yyvsp[0].ast); }
#line 5631 "verilog_parser.tab.cc"
    break;

  case 363: /* struct_type: struct_union $@40 struct_body  */
#line 1787 "verilog_parser.y"
                                                        { (yyval.ast) = astbuf2; }
#line 5637 "verilog_parser.tab.cc"
    break;

  case 364: /* struct_union: TOK_STRUCT  */
#line 1791 "verilog_parser.y"
                                { (yyval.ast) = new AstNode(AST_STRUCT); }
#line 5643 "verilog_parser.tab.cc"
    break;

  case 365: /* struct_union: TOK_UNION  */
#line 1792 "verilog_parser.y"
                                { (yyval.ast) = new AstNode(AST_UNION); }
#line 5649 "verilog_parser.tab.cc"
    break;

  case 368: /* opt_packed: %empty  */
#line 1800 "verilog_parser.y"
               { frontend_verilog_yyerror("Only PACKED supported at this time"); }
#line 5655 "verilog_parser.tab.cc"
    break;

  case 369: /* opt_signed_struct: TOK_SIGNED  */
#line 1803 "verilog_parser.y"
                                { astbuf2->is_signed = true; }
#line 5661 "verilog_parser.tab.cc"
    break;

  case 370: /* opt_signed_struct: TOK_UNSIGNED  */
#line 1804 "verilog_parser.y"
                                { astbuf2->is_signed = false; }
#line 5667 "verilog_parser.tab.cc"
    break;

  case 374: /* struct_member: struct_member_type member_name_list ';'  */
#line 1812 "verilog_parser.y"
                                                                { delete astbuf1; }
#line 5673 "verilog_parser.tab.cc"
    break;

  case 377: /* $@41: %empty  */
#line 1820 "verilog_parser.y"
                    {
			astbuf1->str = (yyvsp[0].string)->substr(1);
			delete (yyvsp[0].string);
			astbuf3 = astbuf1->clone();
			SET_AST_NODE_LOC(astbuf3, (yylsp[0]), (yylsp[0]));
			astbuf2->children.push_back(astbuf3);
		}
#line 5685 "verilog_parser.tab.cc"
    break;

  case 378: /* member_name: TOK_ID $@41 range  */
#line 1826 "verilog_parser.y"
                        { if ((yyvsp[0].ast)) astbuf3->children.push_back((yyvsp[0].ast)); }
#line 5691 "verilog_parser.tab.cc"
    break;

  case 379: /* $@42: %empty  */
#line 1829 "verilog_parser.y"
                    { astbuf1 = new AstNode(AST_STRUCT_ITEM); }
#line 5697 "verilog_parser.tab.cc"
    break;

  case 382: /* member_type_token: hierarchical_type_id  */
#line 1834 "verilog_parser.y"
                               {
			addWiretypeNode((yyvsp[0].string), astbuf1);
		}
#line 5705 "verilog_parser.tab.cc"
    break;

  case 383: /* $@43: %empty  */
#line 1837 "verilog_parser.y"
          {
		delete astbuf1;
	}
#line 5713 "verilog_parser.tab.cc"
    break;

  case 384: /* $@44: %empty  */
#line 1839 "verilog_parser.y"
                       {
			// stash state on ast_stack
			ast_stack.push_back(astbuf2);
			astbuf2 = (yyvsp[0].ast);
		}
#line 5723 "verilog_parser.tab.cc"
    break;

  case 385: /* member_type_token: $@43 struct_union $@44 struct_body  */
#line 1843 "verilog_parser.y"
                               {
		        astbuf1 = astbuf2;
			// recover state
			astbuf2 = ast_stack.back();
			ast_stack.pop_back();
		}
#line 5734 "verilog_parser.tab.cc"
    break;

  case 387: /* member_type: type_vec type_signing range_or_multirange  */
#line 1852 "verilog_parser.y"
                                                        { if ((yyvsp[0].ast)) astbuf1->children.push_back((yyvsp[0].ast)); }
#line 5740 "verilog_parser.tab.cc"
    break;

  case 390: /* struct_var: TOK_ID  */
#line 1859 "verilog_parser.y"
                        {	auto *var_node = astbuf2->clone();
				var_node->str = *(yyvsp[0].string);
				delete (yyvsp[0].string);
				SET_AST_NODE_LOC(var_node, (yylsp[0]), (yylsp[0]));
				ast_stack.back()->children.push_back(var_node);
			}
#line 5751 "verilog_parser.tab.cc"
    break;

  case 391: /* $@45: %empty  */
#line 1872 "verilog_parser.y"
                             {
		albuf = (yyvsp[-2].al);
		astbuf1 = (yyvsp[-1].ast);
		astbuf2 = checkRange(astbuf1, (yyvsp[0].ast));
	}
#line 5761 "verilog_parser.tab.cc"
    break;

  case 392: /* $@46: %empty  */
#line 1876 "verilog_parser.y"
                               {
		delete astbuf1;
		if (astbuf2 != NULL)
			delete astbuf2;
		free_attr(albuf);
	}
#line 5772 "verilog_parser.tab.cc"
    break;

  case 394: /* $@47: %empty  */
#line 1882 "verilog_parser.y"
                                {
		ast_stack.back()->children.push_back(new AstNode(AST_WIRE));
		ast_stack.back()->children.back()->str = *(yyvsp[0].string);
		append_attr(ast_stack.back()->children.back(), (yyvsp[-2].al));
		ast_stack.back()->children.push_back(new AstNode(AST_ASSIGN, new AstNode(AST_IDENTIFIER), AstNode::mkconst_int(0, false, 1)));
		ast_stack.back()->children.back()->children[0]->str = *(yyvsp[0].string);
		delete (yyvsp[0].string);
	}
#line 5785 "verilog_parser.tab.cc"
    break;

  case 396: /* $@48: %empty  */
#line 1890 "verilog_parser.y"
                                {
		ast_stack.back()->children.push_back(new AstNode(AST_WIRE));
		ast_stack.back()->children.back()->str = *(yyvsp[0].string);
		append_attr(ast_stack.back()->children.back(), (yyvsp[-2].al));
		ast_stack.back()->children.push_back(new AstNode(AST_ASSIGN, new AstNode(AST_IDENTIFIER), AstNode::mkconst_int(1, false, 1)));
		ast_stack.back()->children.back()->children[0]->str = *(yyvsp[0].string);
		delete (yyvsp[0].string);
	}
#line 5798 "verilog_parser.tab.cc"
    break;

  case 399: /* opt_supply_wires: opt_supply_wires ',' TOK_ID  */
#line 1901 "verilog_parser.y"
                                    {
		AstNode *wire_node = ast_stack.back()->children.at(GetSize(ast_stack.back()->children)-2)->clone();
		AstNode *assign_node = ast_stack.back()->children.at(GetSize(ast_stack.back()->children)-1)->clone();
		wire_node->str = *(yyvsp[0].string);
		assign_node->children[0]->str = *(yyvsp[0].string);
		ast_stack.back()->children.push_back(wire_node);
		ast_stack.back()->children.push_back(assign_node);
		delete (yyvsp[0].string);
	}
#line 5812 "verilog_parser.tab.cc"
    break;

  case 402: /* wire_name_and_opt_assign: wire_name  */
#line 1915 "verilog_parser.y"
                  {
		bool attr_anyconst = false;
		bool attr_anyseq = false;
		bool attr_allconst = false;
		bool attr_allseq = false;
		if (ast_stack.back()->children.back()->get_bool_attribute(ID::anyconst)) {
			delete ast_stack.back()->children.back()->attributes.at(ID::anyconst);
			ast_stack.back()->children.back()->attributes.erase(ID::anyconst);
			attr_anyconst = true;
		}
		if (ast_stack.back()->children.back()->get_bool_attribute(ID::anyseq)) {
			delete ast_stack.back()->children.back()->attributes.at(ID::anyseq);
			ast_stack.back()->children.back()->attributes.erase(ID::anyseq);
			attr_anyseq = true;
		}
		if (ast_stack.back()->children.back()->get_bool_attribute(ID::allconst)) {
			delete ast_stack.back()->children.back()->attributes.at(ID::allconst);
			ast_stack.back()->children.back()->attributes.erase(ID::allconst);
			attr_allconst = true;
		}
		if (ast_stack.back()->children.back()->get_bool_attribute(ID::allseq)) {
			delete ast_stack.back()->children.back()->attributes.at(ID::allseq);
			ast_stack.back()->children.back()->attributes.erase(ID::allseq);
			attr_allseq = true;
		}
		if (current_wire_rand || attr_anyconst || attr_anyseq || attr_allconst || attr_allseq) {
			AstNode *wire = new AstNode(AST_IDENTIFIER);
			AstNode *fcall = new AstNode(AST_FCALL);
			wire->str = ast_stack.back()->children.back()->str;
			fcall->str = current_wire_const ? "\\$anyconst" : "\\$anyseq";
			if (attr_anyconst)
				fcall->str = "\\$anyconst";
			if (attr_anyseq)
				fcall->str = "\\$anyseq";
			if (attr_allconst)
				fcall->str = "\\$allconst";
			if (attr_allseq)
				fcall->str = "\\$allseq";
			fcall->attributes[ID::reg] = AstNode::mkconst_str(RTLIL::unescape_id(wire->str));
			ast_stack.back()->children.push_back(new AstNode(AST_ASSIGN, wire, fcall));
		}
	}
#line 5859 "verilog_parser.tab.cc"
    break;

  case 403: /* wire_name_and_opt_assign: wire_name '=' expr  */
#line 1957 "verilog_parser.y"
                           {
		AstNode *wire = new AstNode(AST_IDENTIFIER);
		wire->str = ast_stack.back()->children.back()->str;
		if (astbuf1->is_input) {
			if (astbuf1->attributes.count(ID::defaultvalue))
				delete astbuf1->attributes.at(ID::defaultvalue);
			astbuf1->attributes[ID::defaultvalue] = (yyvsp[0].ast);
		}
		else if (astbuf1->is_reg || astbuf1->is_logic){
			AstNode *assign = new AstNode(AST_ASSIGN_LE, wire, (yyvsp[0].ast));
			AstNode *block = new AstNode(AST_BLOCK, assign);
			AstNode *init = new AstNode(AST_INITIAL, block);

			SET_AST_NODE_LOC(assign, (yylsp[-2]), (yylsp[0]));
			SET_AST_NODE_LOC(block, (yylsp[-2]), (yylsp[0]));
			SET_AST_NODE_LOC(init, (yylsp[-2]), (yylsp[0]));

			ast_stack.back()->children.push_back(init);
		}
		else {
			AstNode *assign = new AstNode(AST_ASSIGN, wire, (yyvsp[0].ast));
			SET_AST_NODE_LOC(assign, (yylsp[-2]), (yylsp[0]));
			ast_stack.back()->children.push_back(assign);
		}

	}
#line 5890 "verilog_parser.tab.cc"
    break;

  case 404: /* wire_name: TOK_ID range_or_multirange  */
#line 1985 "verilog_parser.y"
                                   {
		if (astbuf1 == nullptr)
			frontend_verilog_yyerror("Internal error - should not happen - no AST_WIRE node.");
		AstNode *node = astbuf1->clone();
		node->str = *(yyvsp[-1].string);
		append_attr_clone(node, albuf);
		if (astbuf2 != NULL)
			node->children.push_back(astbuf2->clone());
		if ((yyvsp[0].ast) != NULL) {
			if (node->is_input || node->is_output)
				frontend_verilog_yyerror("input/output/inout ports cannot have unpacked dimensions.");
			if (!astbuf2 && !node->is_custom_type) {
				addRange(node, 0, 0, false);
			}
			rewriteAsMemoryNode(node, (yyvsp[0].ast));
		}
		if (current_function_or_task) {
			if (node->is_input || node->is_output)
				node->port_id = current_function_or_task_port_id++;
		} else if (ast_stack.back()->type == AST_GENBLOCK) {
			if (node->is_input || node->is_output)
				frontend_verilog_yyerror("Cannot declare module port `%s' within a generate block.", (yyvsp[-1].string)->c_str());
		} else {
			if (do_not_require_port_stubs && (node->is_input || node->is_output) && port_stubs.count(*(yyvsp[-1].string)) == 0) {
				port_stubs[*(yyvsp[-1].string)] = ++port_counter;
			}
			if (port_stubs.count(*(yyvsp[-1].string)) != 0) {
				if (!node->is_input && !node->is_output)
					frontend_verilog_yyerror("Module port `%s' is neither input nor output.", (yyvsp[-1].string)->c_str());
				if (node->is_reg && node->is_input && !node->is_output && !sv_mode)
					frontend_verilog_yyerror("Input port `%s' is declared as register.", (yyvsp[-1].string)->c_str());
				node->port_id = port_stubs[*(yyvsp[-1].string)];
				port_stubs.erase(*(yyvsp[-1].string));
			} else {
				if (node->is_input || node->is_output)
					frontend_verilog_yyerror("Module port `%s' is not declared in module header.", (yyvsp[-1].string)->c_str());
			}
		}
		//FIXME: for some reason, TOK_ID has a location which always points to one column *after* the real last column...
		SET_AST_NODE_LOC(node, (yylsp[-1]), (yylsp[-1]));
		ast_stack.back()->children.push_back(node);

		delete (yyvsp[-1].string);
	}
#line 5939 "verilog_parser.tab.cc"
    break;

  case 408: /* assign_expr: lvalue '=' expr  */
#line 2037 "verilog_parser.y"
                        {
		AstNode *node = new AstNode(AST_ASSIGN, (yyvsp[-2].ast), (yyvsp[0].ast));
		SET_AST_NODE_LOC(node, (yyloc), (yyloc));
		ast_stack.back()->children.push_back(node);
	}
#line 5949 "verilog_parser.tab.cc"
    break;

  case 410: /* type_name: TOK_USER_TYPE  */
#line 2044 "verilog_parser.y"
                                { if (isInLocalScope((yyvsp[0].string))) frontend_verilog_yyerror("Duplicate declaration of TYPEDEF '%s'", (yyvsp[0].string)->c_str()+1); }
#line 5955 "verilog_parser.tab.cc"
    break;

  case 411: /* typedef_decl: TOK_TYPEDEF typedef_base_type range type_name range_or_multirange ';'  */
#line 2048 "verilog_parser.y"
                                                                              {
		astbuf1 = (yyvsp[-4].ast);
		astbuf2 = checkRange(astbuf1, (yyvsp[-3].ast));
		if (astbuf2)
			astbuf1->children.push_back(astbuf2);

		if ((yyvsp[-1].ast) != NULL) {
			if (!astbuf2) {
				addRange(astbuf1, 0, 0, false);
			}
			rewriteAsMemoryNode(astbuf1, (yyvsp[-1].ast));
		}
		addTypedefNode((yyvsp[-2].string), astbuf1); }
#line 5973 "verilog_parser.tab.cc"
    break;

  case 412: /* typedef_decl: TOK_TYPEDEF enum_struct_type type_name ';'  */
#line 2061 "verilog_parser.y"
                                                       { addTypedefNode((yyvsp[-1].string), (yyvsp[-2].ast)); }
#line 5979 "verilog_parser.tab.cc"
    break;

  case 413: /* typedef_base_type: hierarchical_type_id  */
#line 2065 "verilog_parser.y"
                             {
		(yyval.ast) = new AstNode(AST_WIRE);
		(yyval.ast)->is_logic = true;
		addWiretypeNode((yyvsp[0].string), (yyval.ast));
	}
#line 5989 "verilog_parser.tab.cc"
    break;

  case 414: /* typedef_base_type: integer_vector_type opt_signedness_default_unsigned  */
#line 2070 "verilog_parser.y"
                                                            {
		(yyval.ast) = new AstNode(AST_WIRE);
		if ((yyvsp[-1].integer) == TOK_REG) {
			(yyval.ast)->is_reg = true;
		} else {
			(yyval.ast)->is_logic = true;
		}
		(yyval.ast)->is_signed = (yyvsp[0].boolean);
	}
#line 6003 "verilog_parser.tab.cc"
    break;

  case 415: /* typedef_base_type: integer_atom_type opt_signedness_default_signed  */
#line 2079 "verilog_parser.y"
                                                        {
		(yyval.ast) = new AstNode(AST_WIRE);
		(yyval.ast)->is_logic = true;
		(yyval.ast)->is_signed = (yyvsp[0].boolean);
		(yyval.ast)->range_left = (yyvsp[-1].integer) - 1;
		(yyval.ast)->range_right = 0;
	}
#line 6015 "verilog_parser.tab.cc"
    break;

  case 418: /* $@49: %empty  */
#line 2093 "verilog_parser.y"
                    {
		astbuf1 = new AstNode(AST_CELL);
		append_attr(astbuf1, (yyvsp[-1].al));
		astbuf1->children.push_back(new AstNode(AST_CELLTYPE));
		astbuf1->children[0]->str = *(yyvsp[0].string);
		delete (yyvsp[0].string);
	}
#line 6027 "verilog_parser.tab.cc"
    break;

  case 419: /* cell_stmt: attr TOK_ID $@49 cell_parameter_list_opt cell_list ';'  */
#line 2099 "verilog_parser.y"
                                                {
		delete astbuf1;
	}
#line 6035 "verilog_parser.tab.cc"
    break;

  case 420: /* $@50: %empty  */
#line 2102 "verilog_parser.y"
                                    {
		astbuf1 = new AstNode(AST_PRIMITIVE);
		astbuf1->str = *(yyvsp[-1].string);
		append_attr(astbuf1, (yyvsp[-2].al));
		delete (yyvsp[-1].string);
	}
#line 6046 "verilog_parser.tab.cc"
    break;

  case 421: /* cell_stmt: attr tok_prim_wrapper delay $@50 prim_list ';'  */
#line 2107 "verilog_parser.y"
                        {
		delete astbuf1;
	}
#line 6054 "verilog_parser.tab.cc"
    break;

  case 422: /* tok_prim_wrapper: TOK_PRIMITIVE  */
#line 2112 "verilog_parser.y"
                      {
		(yyval.string) = (yyvsp[0].string);
	}
#line 6062 "verilog_parser.tab.cc"
    break;

  case 423: /* tok_prim_wrapper: TOK_OR  */
#line 2115 "verilog_parser.y"
               {
		(yyval.string) = new std::string("or");
	}
#line 6070 "verilog_parser.tab.cc"
    break;

  case 428: /* $@51: %empty  */
#line 2127 "verilog_parser.y"
               {
		astbuf2 = astbuf1->clone();
		if (astbuf2->type != AST_PRIMITIVE)
			astbuf2->str = *(yyvsp[0].string);
		delete (yyvsp[0].string);
		ast_stack.back()->children.push_back(astbuf2);
	}
#line 6082 "verilog_parser.tab.cc"
    break;

  case 429: /* single_cell_no_array: TOK_ID $@51 '(' cell_port_list ')'  */
#line 2133 "verilog_parser.y"
                                 {
		SET_AST_NODE_LOC(astbuf2, (yylsp[-4]), (yyloc));
	}
#line 6090 "verilog_parser.tab.cc"
    break;

  case 430: /* $@52: %empty  */
#line 2138 "verilog_parser.y"
                             {
		astbuf2 = astbuf1->clone();
		if (astbuf2->type != AST_PRIMITIVE)
			astbuf2->str = *(yyvsp[-1].string);
		delete (yyvsp[-1].string);
		ast_stack.back()->children.push_back(new AstNode(AST_CELLARRAY, (yyvsp[0].ast), astbuf2));
	}
#line 6102 "verilog_parser.tab.cc"
    break;

  case 431: /* single_cell_arraylist: TOK_ID non_opt_range $@52 '(' cell_port_list ')'  */
#line 2144 "verilog_parser.y"
                                {
		SET_AST_NODE_LOC(astbuf2, (yylsp[-5]), (yyloc));
	}
#line 6110 "verilog_parser.tab.cc"
    break;

  case 437: /* $@53: %empty  */
#line 2158 "verilog_parser.y"
                      {
		astbuf2 = astbuf1->clone();
		ast_stack.back()->children.push_back(astbuf2);
	}
#line 6119 "verilog_parser.tab.cc"
    break;

  case 438: /* single_prim: $@53 '(' cell_port_list ')'  */
#line 2161 "verilog_parser.y"
                                 {
		SET_AST_NODE_LOC(astbuf2, (yylsp[-3]), (yyloc));
	}
#line 6127 "verilog_parser.tab.cc"
    break;

  case 444: /* cell_parameter: expr  */
#line 2173 "verilog_parser.y"
             {
		AstNode *node = new AstNode(AST_PARASET);
		astbuf1->children.push_back(node);
		node->children.push_back((yyvsp[0].ast));
	}
#line 6137 "verilog_parser.tab.cc"
    break;

  case 445: /* cell_parameter: '.' TOK_ID '(' ')'  */
#line 2178 "verilog_parser.y"
                           {
		// just ignore empty parameters
	}
#line 6145 "verilog_parser.tab.cc"
    break;

  case 446: /* cell_parameter: '.' TOK_ID '(' expr ')'  */
#line 2181 "verilog_parser.y"
                                {
		AstNode *node = new AstNode(AST_PARASET);
		node->str = *(yyvsp[-3].string);
		astbuf1->children.push_back(node);
		node->children.push_back((yyvsp[-1].ast));
		delete (yyvsp[-3].string);
	}
#line 6157 "verilog_parser.tab.cc"
    break;

  case 447: /* cell_port_list: cell_port_list_rules  */
#line 2190 "verilog_parser.y"
                             {
		// remove empty args from end of list
		while (!astbuf2->children.empty()) {
			AstNode *node = astbuf2->children.back();
			if (node->type != AST_ARGUMENT) break;
			if (!node->children.empty()) break;
			if (!node->str.empty()) break;
			astbuf2->children.pop_back();
			delete node;
		}

		// check port types
		bool has_positional_args = false;
		bool has_named_args = false;
		for (auto node : astbuf2->children) {
			if (node->type != AST_ARGUMENT) continue;
			if (node->str.empty())
				has_positional_args = true;
			else
				has_named_args = true;
		}

		if (has_positional_args && has_named_args)
			frontend_verilog_yyerror("Mix of positional and named cell ports.");
	}
#line 6187 "verilog_parser.tab.cc"
    break;

  case 450: /* cell_port: attr  */
#line 2220 "verilog_parser.y"
             {
		AstNode *node = new AstNode(AST_ARGUMENT);
		astbuf2->children.push_back(node);
		free_attr((yyvsp[0].al));
	}
#line 6197 "verilog_parser.tab.cc"
    break;

  case 451: /* cell_port: attr expr  */
#line 2225 "verilog_parser.y"
                  {
		AstNode *node = new AstNode(AST_ARGUMENT);
		astbuf2->children.push_back(node);
		node->children.push_back((yyvsp[0].ast));
		free_attr((yyvsp[-1].al));
	}
#line 6208 "verilog_parser.tab.cc"
    break;

  case 452: /* cell_port: attr '.' TOK_ID '(' expr ')'  */
#line 2231 "verilog_parser.y"
                                     {
		AstNode *node = new AstNode(AST_ARGUMENT);
		node->str = *(yyvsp[-3].string);
		astbuf2->children.push_back(node);
		node->children.push_back((yyvsp[-1].ast));
		delete (yyvsp[-3].string);
		free_attr((yyvsp[-5].al));
	}
#line 6221 "verilog_parser.tab.cc"
    break;

  case 453: /* cell_port: attr '.' TOK_ID '(' ')'  */
#line 2239 "verilog_parser.y"
                                {
		AstNode *node = new AstNode(AST_ARGUMENT);
		node->str = *(yyvsp[-2].string);
		astbuf2->children.push_back(node);
		delete (yyvsp[-2].string);
		free_attr((yyvsp[-4].al));
	}
#line 6233 "verilog_parser.tab.cc"
    break;

  case 454: /* cell_port: attr '.' TOK_ID  */
#line 2246 "verilog_parser.y"
                        {
		AstNode *node = new AstNode(AST_ARGUMENT);
		node->str = *(yyvsp[0].string);
		astbuf2->children.push_back(node);
		node->children.push_back(new AstNode(AST_IDENTIFIER));
		node->children.back()->str = *(yyvsp[0].string);
		delete (yyvsp[0].string);
		free_attr((yyvsp[-2].al));
	}
#line 6247 "verilog_parser.tab.cc"
    break;

  case 455: /* cell_port: attr TOK_WILDCARD_CONNECT  */
#line 2255 "verilog_parser.y"
                                  {
		if (!sv_mode)
			frontend_verilog_yyerror("Wildcard port connections are only supported in SystemVerilog mode.");
		astbuf2->attributes[ID::wildcard_port_conns] = AstNode::mkconst_int(1, false);
		free_attr((yyvsp[-1].al));
	}
#line 6258 "verilog_parser.tab.cc"
    break;

  case 456: /* always_comb_or_latch: TOK_ALWAYS_COMB  */
#line 2263 "verilog_parser.y"
                        {
		(yyval.boolean) = false;
	}
#line 6266 "verilog_parser.tab.cc"
    break;

  case 457: /* always_comb_or_latch: TOK_ALWAYS_LATCH  */
#line 2266 "verilog_parser.y"
                         {
		(yyval.boolean) = true;
	}
#line 6274 "verilog_parser.tab.cc"
    break;

  case 458: /* always_or_always_ff: TOK_ALWAYS  */
#line 2271 "verilog_parser.y"
                   {
		(yyval.boolean) = false;
	}
#line 6282 "verilog_parser.tab.cc"
    break;

  case 459: /* always_or_always_ff: TOK_ALWAYS_FF  */
#line 2274 "verilog_parser.y"
                      {
		(yyval.boolean) = true;
	}
#line 6290 "verilog_parser.tab.cc"
    break;

  case 460: /* $@54: %empty  */
#line 2279 "verilog_parser.y"
                                 {
		AstNode *node = new AstNode(AST_ALWAYS);
		append_attr(node, (yyvsp[-1].al));
		if ((yyvsp[0].boolean))
			node->attributes[ID::always_ff] = AstNode::mkconst_int(1, false);
		ast_stack.back()->children.push_back(node);
		ast_stack.push_back(node);
	}
#line 6303 "verilog_parser.tab.cc"
    break;

  case 461: /* $@55: %empty  */
#line 2286 "verilog_parser.y"
                      {
		AstNode *block = new AstNode(AST_BLOCK);
		ast_stack.back()->children.push_back(block);
		ast_stack.push_back(block);
	}
#line 6313 "verilog_parser.tab.cc"
    break;

  case 462: /* always_stmt: attr always_or_always_ff $@54 always_cond $@55 behavioral_stmt  */
#line 2290 "verilog_parser.y"
                          {
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[0]), (yylsp[0]));
		ast_stack.pop_back();

		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-4]), (yyloc));
		ast_stack.pop_back();

		SET_RULE_LOC((yyloc), (yylsp[-4]), (yyloc));
	}
#line 6327 "verilog_parser.tab.cc"
    break;

  case 463: /* $@56: %empty  */
#line 2299 "verilog_parser.y"
                                  {
		AstNode *node = new AstNode(AST_ALWAYS);
		append_attr(node, (yyvsp[-1].al));
		if ((yyvsp[0].boolean))
			node->attributes[ID::always_latch] = AstNode::mkconst_int(1, false);
		else
			node->attributes[ID::always_comb] = AstNode::mkconst_int(1, false);
		ast_stack.back()->children.push_back(node);
		ast_stack.push_back(node);
		AstNode *block = new AstNode(AST_BLOCK);
		ast_stack.back()->children.push_back(block);
		ast_stack.push_back(block);
	}
#line 6345 "verilog_parser.tab.cc"
    break;

  case 464: /* always_stmt: attr always_comb_or_latch $@56 behavioral_stmt  */
#line 2311 "verilog_parser.y"
                          {
		ast_stack.pop_back();
		ast_stack.pop_back();
	}
#line 6354 "verilog_parser.tab.cc"
    break;

  case 465: /* $@57: %empty  */
#line 2315 "verilog_parser.y"
                         {
		AstNode *node = new AstNode(AST_INITIAL);
		append_attr(node, (yyvsp[-1].al));
		ast_stack.back()->children.push_back(node);
		ast_stack.push_back(node);
		AstNode *block = new AstNode(AST_BLOCK);
		ast_stack.back()->children.push_back(block);
		ast_stack.push_back(block);
	}
#line 6368 "verilog_parser.tab.cc"
    break;

  case 466: /* always_stmt: attr TOK_INITIAL $@57 behavioral_stmt  */
#line 2323 "verilog_parser.y"
                          {
		ast_stack.pop_back();
		ast_stack.pop_back();
	}
#line 6377 "verilog_parser.tab.cc"
    break;

  case 476: /* always_event: TOK_POSEDGE expr  */
#line 2342 "verilog_parser.y"
                         {
		AstNode *node = new AstNode(AST_POSEDGE);
		SET_AST_NODE_LOC(node, (yylsp[-1]), (yylsp[-1]));
		ast_stack.back()->children.push_back(node);
		node->children.push_back((yyvsp[0].ast));
	}
#line 6388 "verilog_parser.tab.cc"
    break;

  case 477: /* always_event: TOK_NEGEDGE expr  */
#line 2348 "verilog_parser.y"
                         {
		AstNode *node = new AstNode(AST_NEGEDGE);
		SET_AST_NODE_LOC(node, (yylsp[-1]), (yylsp[-1]));
		ast_stack.back()->children.push_back(node);
		node->children.push_back((yyvsp[0].ast));
	}
#line 6399 "verilog_parser.tab.cc"
    break;

  case 478: /* always_event: expr  */
#line 2354 "verilog_parser.y"
             {
		AstNode *node = new AstNode(AST_EDGE);
		ast_stack.back()->children.push_back(node);
		node->children.push_back((yyvsp[0].ast));
	}
#line 6409 "verilog_parser.tab.cc"
    break;

  case 479: /* opt_label: ':' TOK_ID  */
#line 2361 "verilog_parser.y"
                   {
		(yyval.string) = (yyvsp[0].string);
	}
#line 6417 "verilog_parser.tab.cc"
    break;

  case 480: /* opt_label: %empty  */
#line 2364 "verilog_parser.y"
               {
		(yyval.string) = NULL;
	}
#line 6425 "verilog_parser.tab.cc"
    break;

  case 481: /* opt_sva_label: TOK_SVA_LABEL ':'  */
#line 2369 "verilog_parser.y"
                          {
		(yyval.string) = (yyvsp[-1].string);
	}
#line 6433 "verilog_parser.tab.cc"
    break;

  case 482: /* opt_sva_label: %empty  */
#line 2372 "verilog_parser.y"
               {
		(yyval.string) = NULL;
	}
#line 6441 "verilog_parser.tab.cc"
    break;

  case 483: /* opt_property: TOK_PROPERTY  */
#line 2377 "verilog_parser.y"
                     {
		(yyval.boolean) = true;
	}
#line 6449 "verilog_parser.tab.cc"
    break;

  case 484: /* opt_property: TOK_FINAL  */
#line 2380 "verilog_parser.y"
                  {
		(yyval.boolean) = false;
	}
#line 6457 "verilog_parser.tab.cc"
    break;

  case 485: /* opt_property: %empty  */
#line 2383 "verilog_parser.y"
               {
		(yyval.boolean) = false;
	}
#line 6465 "verilog_parser.tab.cc"
    break;

  case 486: /* $@58: %empty  */
#line 2388 "verilog_parser.y"
                       {
        AstNode *modport = new AstNode(AST_MODPORT);
        ast_stack.back()->children.push_back(modport);
        ast_stack.push_back(modport);
        modport->str = *(yyvsp[0].string);
        delete (yyvsp[0].string);
    }
#line 6477 "verilog_parser.tab.cc"
    break;

  case 487: /* $@59: %empty  */
#line 2394 "verilog_parser.y"
                        {
        ast_stack.pop_back();
        log_assert(ast_stack.size() == 2);
    }
#line 6486 "verilog_parser.tab.cc"
    break;

  case 495: /* modport_member: TOK_ID  */
#line 2410 "verilog_parser.y"
           {
        AstNode *modport_member = new AstNode(AST_MODPORTMEMBER);
        ast_stack.back()->children.push_back(modport_member);
        modport_member->str = *(yyvsp[0].string);
        modport_member->is_input = current_modport_input;
        modport_member->is_output = current_modport_output;
        delete (yyvsp[0].string);
    }
#line 6499 "verilog_parser.tab.cc"
    break;

  case 496: /* modport_type_token: TOK_INPUT  */
#line 2420 "verilog_parser.y"
              {current_modport_input = 1; current_modport_output = 0;}
#line 6505 "verilog_parser.tab.cc"
    break;

  case 497: /* modport_type_token: TOK_OUTPUT  */
#line 2420 "verilog_parser.y"
                                                                                    {current_modport_input = 0; current_modport_output = 1;}
#line 6511 "verilog_parser.tab.cc"
    break;

  case 498: /* assert: opt_sva_label TOK_ASSERT opt_property '(' expr ')' ';'  */
#line 2423 "verilog_parser.y"
                                                               {
		if (noassert_mode) {
			delete (yyvsp[-2].ast);
		} else {
			AstNode *node = new AstNode(assume_asserts_mode ? AST_ASSUME : AST_ASSERT, (yyvsp[-2].ast));
			SET_AST_NODE_LOC(node, (yylsp[-6]), (yylsp[-1]));
			if ((yyvsp[-6].string) != nullptr)
				node->str = *(yyvsp[-6].string);
			ast_stack.back()->children.push_back(node);
		}
		if ((yyvsp[-6].string) != nullptr)
			delete (yyvsp[-6].string);
	}
#line 6529 "verilog_parser.tab.cc"
    break;

  case 499: /* assert: opt_sva_label TOK_ASSUME opt_property '(' expr ')' ';'  */
#line 2436 "verilog_parser.y"
                                                               {
		if (noassume_mode) {
			delete (yyvsp[-2].ast);
		} else {
			AstNode *node = new AstNode(assert_assumes_mode ? AST_ASSERT : AST_ASSUME, (yyvsp[-2].ast));
			SET_AST_NODE_LOC(node, (yylsp[-6]), (yylsp[-1]));
			if ((yyvsp[-6].string) != nullptr)
				node->str = *(yyvsp[-6].string);
			ast_stack.back()->children.push_back(node);
		}
		if ((yyvsp[-6].string) != nullptr)
			delete (yyvsp[-6].string);
	}
#line 6547 "verilog_parser.tab.cc"
    break;

  case 500: /* assert: opt_sva_label TOK_ASSERT opt_property '(' TOK_EVENTUALLY expr ')' ';'  */
#line 2449 "verilog_parser.y"
                                                                              {
		if (noassert_mode) {
			delete (yyvsp[-2].ast);
		} else {
			AstNode *node = new AstNode(assume_asserts_mode ? AST_FAIR : AST_LIVE, (yyvsp[-2].ast));
			SET_AST_NODE_LOC(node, (yylsp[-7]), (yylsp[-1]));
			if ((yyvsp[-7].string) != nullptr)
				node->str = *(yyvsp[-7].string);
			ast_stack.back()->children.push_back(node);
		}
		if ((yyvsp[-7].string) != nullptr)
			delete (yyvsp[-7].string);
	}
#line 6565 "verilog_parser.tab.cc"
    break;

  case 501: /* assert: opt_sva_label TOK_ASSUME opt_property '(' TOK_EVENTUALLY expr ')' ';'  */
#line 2462 "verilog_parser.y"
                                                                              {
		if (noassume_mode) {
			delete (yyvsp[-2].ast);
		} else {
			AstNode *node = new AstNode(assert_assumes_mode ? AST_LIVE : AST_FAIR, (yyvsp[-2].ast));
			SET_AST_NODE_LOC(node, (yylsp[-7]), (yylsp[-1]));
			if ((yyvsp[-7].string) != nullptr)
				node->str = *(yyvsp[-7].string);
			ast_stack.back()->children.push_back(node);
		}
		if ((yyvsp[-7].string) != nullptr)
			delete (yyvsp[-7].string);
	}
#line 6583 "verilog_parser.tab.cc"
    break;

  case 502: /* assert: opt_sva_label TOK_COVER opt_property '(' expr ')' ';'  */
#line 2475 "verilog_parser.y"
                                                              {
		AstNode *node = new AstNode(AST_COVER, (yyvsp[-2].ast));
		SET_AST_NODE_LOC(node, (yylsp[-6]), (yylsp[-1]));
		if ((yyvsp[-6].string) != nullptr) {
			node->str = *(yyvsp[-6].string);
			delete (yyvsp[-6].string);
		}
		ast_stack.back()->children.push_back(node);
	}
#line 6597 "verilog_parser.tab.cc"
    break;

  case 503: /* assert: opt_sva_label TOK_COVER opt_property '(' ')' ';'  */
#line 2484 "verilog_parser.y"
                                                         {
		AstNode *node = new AstNode(AST_COVER, AstNode::mkconst_int(1, false));
		SET_AST_NODE_LOC(node, (yylsp[-5]), (yylsp[-1]));
		if ((yyvsp[-5].string) != nullptr) {
			node->str = *(yyvsp[-5].string);
			delete (yyvsp[-5].string);
		}
		ast_stack.back()->children.push_back(node);
	}
#line 6611 "verilog_parser.tab.cc"
    break;

  case 504: /* assert: opt_sva_label TOK_COVER ';'  */
#line 2493 "verilog_parser.y"
                                    {
		AstNode *node = new AstNode(AST_COVER, AstNode::mkconst_int(1, false));
		SET_AST_NODE_LOC(node, (yylsp[-2]), (yylsp[-1]));
		if ((yyvsp[-2].string) != nullptr) {
			node->str = *(yyvsp[-2].string);
			delete (yyvsp[-2].string);
		}
		ast_stack.back()->children.push_back(node);
	}
#line 6625 "verilog_parser.tab.cc"
    break;

  case 505: /* assert: opt_sva_label TOK_RESTRICT opt_property '(' expr ')' ';'  */
#line 2502 "verilog_parser.y"
                                                                 {
		if (norestrict_mode) {
			delete (yyvsp[-2].ast);
		} else {
			AstNode *node = new AstNode(AST_ASSUME, (yyvsp[-2].ast));
			SET_AST_NODE_LOC(node, (yylsp[-6]), (yylsp[-1]));
			if ((yyvsp[-6].string) != nullptr)
				node->str = *(yyvsp[-6].string);
			ast_stack.back()->children.push_back(node);
		}
		if (!(yyvsp[-4].boolean))
			log_file_warning(current_filename, get_line_num(), "SystemVerilog does not allow \"restrict\" without \"property\".\n");
		if ((yyvsp[-6].string) != nullptr)
			delete (yyvsp[-6].string);
	}
#line 6645 "verilog_parser.tab.cc"
    break;

  case 506: /* assert: opt_sva_label TOK_RESTRICT opt_property '(' TOK_EVENTUALLY expr ')' ';'  */
#line 2517 "verilog_parser.y"
                                                                                {
		if (norestrict_mode) {
			delete (yyvsp[-2].ast);
		} else {
			AstNode *node = new AstNode(AST_FAIR, (yyvsp[-2].ast));
			SET_AST_NODE_LOC(node, (yylsp[-7]), (yylsp[-1]));
			if ((yyvsp[-7].string) != nullptr)
				node->str = *(yyvsp[-7].string);
			ast_stack.back()->children.push_back(node);
		}
		if (!(yyvsp[-5].boolean))
			log_file_warning(current_filename, get_line_num(), "SystemVerilog does not allow \"restrict\" without \"property\".\n");
		if ((yyvsp[-7].string) != nullptr)
			delete (yyvsp[-7].string);
	}
#line 6665 "verilog_parser.tab.cc"
    break;

  case 507: /* assert_property: opt_sva_label TOK_ASSERT TOK_PROPERTY '(' expr ')' ';'  */
#line 2534 "verilog_parser.y"
                                                               {
		AstNode *node = new AstNode(assume_asserts_mode ? AST_ASSUME : AST_ASSERT, (yyvsp[-2].ast));
		SET_AST_NODE_LOC(node, (yylsp[-6]), (yylsp[-1]));
		ast_stack.back()->children.push_back(node);
		if ((yyvsp[-6].string) != nullptr) {
			ast_stack.back()->children.back()->str = *(yyvsp[-6].string);
			delete (yyvsp[-6].string);
		}
	}
#line 6679 "verilog_parser.tab.cc"
    break;

  case 508: /* assert_property: opt_sva_label TOK_ASSUME TOK_PROPERTY '(' expr ')' ';'  */
#line 2543 "verilog_parser.y"
                                                               {
		AstNode *node = new AstNode(AST_ASSUME, (yyvsp[-2].ast));
		SET_AST_NODE_LOC(node, (yylsp[-6]), (yylsp[-1]));
		ast_stack.back()->children.push_back(node);
		if ((yyvsp[-6].string) != nullptr) {
			ast_stack.back()->children.back()->str = *(yyvsp[-6].string);
			delete (yyvsp[-6].string);
		}
	}
#line 6693 "verilog_parser.tab.cc"
    break;

  case 509: /* assert_property: opt_sva_label TOK_ASSERT TOK_PROPERTY '(' TOK_EVENTUALLY expr ')' ';'  */
#line 2552 "verilog_parser.y"
                                                                              {
		AstNode *node = new AstNode(assume_asserts_mode ? AST_FAIR : AST_LIVE, (yyvsp[-2].ast));
		SET_AST_NODE_LOC(node, (yylsp[-7]), (yylsp[-1]));
		ast_stack.back()->children.push_back(node);
		if ((yyvsp[-7].string) != nullptr) {
			ast_stack.back()->children.back()->str = *(yyvsp[-7].string);
			delete (yyvsp[-7].string);
		}
	}
#line 6707 "verilog_parser.tab.cc"
    break;

  case 510: /* assert_property: opt_sva_label TOK_ASSUME TOK_PROPERTY '(' TOK_EVENTUALLY expr ')' ';'  */
#line 2561 "verilog_parser.y"
                                                                              {
		AstNode *node = new AstNode(AST_FAIR, (yyvsp[-2].ast));
		SET_AST_NODE_LOC(node, (yylsp[-7]), (yylsp[-1]));
		ast_stack.back()->children.push_back(node);
		if ((yyvsp[-7].string) != nullptr) {
			ast_stack.back()->children.back()->str = *(yyvsp[-7].string);
			delete (yyvsp[-7].string);
		}
	}
#line 6721 "verilog_parser.tab.cc"
    break;

  case 511: /* assert_property: opt_sva_label TOK_COVER TOK_PROPERTY '(' expr ')' ';'  */
#line 2570 "verilog_parser.y"
                                                              {
		AstNode *node = new AstNode(AST_COVER, (yyvsp[-2].ast));
		SET_AST_NODE_LOC(node, (yylsp[-6]), (yylsp[-1]));
		ast_stack.back()->children.push_back(node);
		if ((yyvsp[-6].string) != nullptr) {
			ast_stack.back()->children.back()->str = *(yyvsp[-6].string);
			delete (yyvsp[-6].string);
		}
	}
#line 6735 "verilog_parser.tab.cc"
    break;

  case 512: /* assert_property: opt_sva_label TOK_RESTRICT TOK_PROPERTY '(' expr ')' ';'  */
#line 2579 "verilog_parser.y"
                                                                 {
		if (norestrict_mode) {
			delete (yyvsp[-2].ast);
		} else {
			AstNode *node = new AstNode(AST_ASSUME, (yyvsp[-2].ast));
			SET_AST_NODE_LOC(node, (yylsp[-6]), (yylsp[-1]));
			ast_stack.back()->children.push_back(node);
			if ((yyvsp[-6].string) != nullptr) {
				ast_stack.back()->children.back()->str = *(yyvsp[-6].string);
				delete (yyvsp[-6].string);
			}
		}
	}
#line 6753 "verilog_parser.tab.cc"
    break;

  case 513: /* assert_property: opt_sva_label TOK_RESTRICT TOK_PROPERTY '(' TOK_EVENTUALLY expr ')' ';'  */
#line 2592 "verilog_parser.y"
                                                                                {
		if (norestrict_mode) {
			delete (yyvsp[-2].ast);
		} else {
			AstNode *node = new AstNode(AST_FAIR, (yyvsp[-2].ast));
			SET_AST_NODE_LOC(node, (yylsp[-7]), (yylsp[-1]));
			ast_stack.back()->children.push_back(node);
			if ((yyvsp[-7].string) != nullptr) {
				ast_stack.back()->children.back()->str = *(yyvsp[-7].string);
				delete (yyvsp[-7].string);
			}
		}
	}
#line 6771 "verilog_parser.tab.cc"
    break;

  case 514: /* simple_behavioral_stmt: attr lvalue '=' delay expr  */
#line 2607 "verilog_parser.y"
                                   {
		AstNode *node = new AstNode(AST_ASSIGN_EQ, (yyvsp[-3].ast), (yyvsp[0].ast));
		ast_stack.back()->children.push_back(node);
		SET_AST_NODE_LOC(node, (yylsp[-3]), (yylsp[0]));
		append_attr(node, (yyvsp[-4].al));
	}
#line 6782 "verilog_parser.tab.cc"
    break;

  case 515: /* simple_behavioral_stmt: attr lvalue TOK_INCREMENT  */
#line 2613 "verilog_parser.y"
                                  {
		AstNode *node = new AstNode(AST_ASSIGN_EQ, (yyvsp[-1].ast), new AstNode(AST_ADD, (yyvsp[-1].ast)->clone(), AstNode::mkconst_int(1, true)));
		ast_stack.back()->children.push_back(node);
		SET_AST_NODE_LOC(node, (yylsp[-1]), (yylsp[0]));
		append_attr(node, (yyvsp[-2].al));
	}
#line 6793 "verilog_parser.tab.cc"
    break;

  case 516: /* simple_behavioral_stmt: attr lvalue TOK_DECREMENT  */
#line 2619 "verilog_parser.y"
                                  {
		AstNode *node = new AstNode(AST_ASSIGN_EQ, (yyvsp[-1].ast), new AstNode(AST_SUB, (yyvsp[-1].ast)->clone(), AstNode::mkconst_int(1, true)));
		ast_stack.back()->children.push_back(node);
		SET_AST_NODE_LOC(node, (yylsp[-1]), (yylsp[0]));
		append_attr(node, (yyvsp[-2].al));
	}
#line 6804 "verilog_parser.tab.cc"
    break;

  case 517: /* simple_behavioral_stmt: attr lvalue OP_LE delay expr  */
#line 2625 "verilog_parser.y"
                                     {
		AstNode *node = new AstNode(AST_ASSIGN_LE, (yyvsp[-3].ast), (yyvsp[0].ast));
		ast_stack.back()->children.push_back(node);
		SET_AST_NODE_LOC(node, (yylsp[-3]), (yylsp[0]));
		append_attr(node, (yyvsp[-4].al));
	}
#line 6815 "verilog_parser.tab.cc"
    break;

  case 518: /* simple_behavioral_stmt: attr lvalue asgn_binop delay expr  */
#line 2631 "verilog_parser.y"
                                          {
		AstNode *expr_node = (yyvsp[0].ast);
		if ((yyvsp[-2].ast_node_type) == AST_SHIFT_LEFT || (yyvsp[-2].ast_node_type) == AST_SHIFT_RIGHT ||
			(yyvsp[-2].ast_node_type) == AST_SHIFT_SLEFT || (yyvsp[-2].ast_node_type) == AST_SHIFT_SRIGHT) {
			expr_node = new AstNode(AST_TO_UNSIGNED, expr_node);
			SET_AST_NODE_LOC(expr_node, (yylsp[0]), (yylsp[0]));
		}
		AstNode *op_node = new AstNode((yyvsp[-2].ast_node_type), (yyvsp[-3].ast)->clone(), expr_node);
		AstNode *node = new AstNode(AST_ASSIGN_EQ, (yyvsp[-3].ast), op_node);
		SET_AST_NODE_LOC(op_node, (yylsp[-3]), (yylsp[0]));
		SET_AST_NODE_LOC(node, (yylsp[-3]), (yylsp[0]));
		ast_stack.back()->children.push_back(node);
		append_attr(node, (yyvsp[-4].al));
	}
#line 6834 "verilog_parser.tab.cc"
    break;

  case 519: /* asgn_binop: TOK_BIT_OR_ASSIGN  */
#line 2647 "verilog_parser.y"
                          { (yyval.ast_node_type) = AST_BIT_OR; }
#line 6840 "verilog_parser.tab.cc"
    break;

  case 520: /* asgn_binop: TOK_BIT_AND_ASSIGN  */
#line 2648 "verilog_parser.y"
                           { (yyval.ast_node_type) = AST_BIT_AND; }
#line 6846 "verilog_parser.tab.cc"
    break;

  case 521: /* asgn_binop: TOK_BIT_XOR_ASSIGN  */
#line 2649 "verilog_parser.y"
                           { (yyval.ast_node_type) = AST_BIT_XOR; }
#line 6852 "verilog_parser.tab.cc"
    break;

  case 522: /* asgn_binop: TOK_ADD_ASSIGN  */
#line 2650 "verilog_parser.y"
                       { (yyval.ast_node_type) = AST_ADD; }
#line 6858 "verilog_parser.tab.cc"
    break;

  case 523: /* asgn_binop: TOK_SUB_ASSIGN  */
#line 2651 "verilog_parser.y"
                       { (yyval.ast_node_type) = AST_SUB; }
#line 6864 "verilog_parser.tab.cc"
    break;

  case 524: /* asgn_binop: TOK_DIV_ASSIGN  */
#line 2652 "verilog_parser.y"
                       { (yyval.ast_node_type) = AST_DIV; }
#line 6870 "verilog_parser.tab.cc"
    break;

  case 525: /* asgn_binop: TOK_MOD_ASSIGN  */
#line 2653 "verilog_parser.y"
                       { (yyval.ast_node_type) = AST_MOD; }
#line 6876 "verilog_parser.tab.cc"
    break;

  case 526: /* asgn_binop: TOK_MUL_ASSIGN  */
#line 2654 "verilog_parser.y"
                       { (yyval.ast_node_type) = AST_MUL; }
#line 6882 "verilog_parser.tab.cc"
    break;

  case 527: /* asgn_binop: TOK_SHL_ASSIGN  */
#line 2655 "verilog_parser.y"
                       { (yyval.ast_node_type) = AST_SHIFT_LEFT; }
#line 6888 "verilog_parser.tab.cc"
    break;

  case 528: /* asgn_binop: TOK_SHR_ASSIGN  */
#line 2656 "verilog_parser.y"
                       { (yyval.ast_node_type) = AST_SHIFT_RIGHT; }
#line 6894 "verilog_parser.tab.cc"
    break;

  case 529: /* asgn_binop: TOK_SSHL_ASSIGN  */
#line 2657 "verilog_parser.y"
                        { (yyval.ast_node_type) = AST_SHIFT_SLEFT; }
#line 6900 "verilog_parser.tab.cc"
    break;

  case 530: /* asgn_binop: TOK_SSHR_ASSIGN  */
#line 2658 "verilog_parser.y"
                        { (yyval.ast_node_type) = AST_SHIFT_SRIGHT; }
#line 6906 "verilog_parser.tab.cc"
    break;

  case 531: /* for_initialization: TOK_ID '=' expr  */
#line 2661 "verilog_parser.y"
                        {
		AstNode *ident = new AstNode(AST_IDENTIFIER);
		ident->str = *(yyvsp[-2].string);
		AstNode *node = new AstNode(AST_ASSIGN_EQ, ident, (yyvsp[0].ast));
		ast_stack.back()->children.push_back(node);
		SET_AST_NODE_LOC(node, (yylsp[-2]), (yylsp[0]));
		delete (yyvsp[-2].string);
	}
#line 6919 "verilog_parser.tab.cc"
    break;

  case 532: /* for_initialization: non_io_wire_type range TOK_ID  */
#line 2669 "verilog_parser.y"
                                      {
		frontend_verilog_yyerror("For loop variable declaration is missing initialization!");
	}
#line 6927 "verilog_parser.tab.cc"
    break;

  case 533: /* for_initialization: non_io_wire_type range TOK_ID '=' expr  */
#line 2672 "verilog_parser.y"
                                               {
		if (!sv_mode)
			frontend_verilog_yyerror("For loop inline variable declaration is only supported in SystemVerilog mode!");

		// loop variable declaration
		AstNode *wire = (yyvsp[-4].ast);
		AstNode *range = checkRange(wire, (yyvsp[-3].ast));
		if (range != nullptr)
			wire->children.push_back(range);
		SET_AST_NODE_LOC(wire, (yylsp[-4]), (yylsp[-2]));
		SET_AST_NODE_LOC(range, (yylsp[-3]), (yylsp[-3]));

		AstNode *ident = new AstNode(AST_IDENTIFIER);
		ident->str = *(yyvsp[-2].string);
		wire->str = *(yyvsp[-2].string);
		delete (yyvsp[-2].string);

		AstNode *loop = ast_stack.back();
		AstNode *parent = ast_stack.at(ast_stack.size() - 2);
		log_assert(parent->children.back() == loop);

		// loop variable initialization
		AstNode *asgn = new AstNode(AST_ASSIGN_EQ, ident, (yyvsp[0].ast));
		loop->children.push_back(asgn);
		SET_AST_NODE_LOC(asgn, (yylsp[-2]), (yylsp[0]));
		SET_AST_NODE_LOC(ident, (yylsp[-2]), (yylsp[-2]));

		// inject a wrapping block to declare the loop variable and
		// contain the current loop
		AstNode *wrapper = new AstNode(AST_BLOCK);
		wrapper->str = "$fordecl_block$" + std::to_string(autoidx++);
		wrapper->children.push_back(wire);
		wrapper->children.push_back(loop);
		parent->children.back() = wrapper; // replaces `loop`
	}
#line 6967 "verilog_parser.tab.cc"
    break;

  case 542: /* behavioral_stmt: attr ';'  */
#line 2713 "verilog_parser.y"
                 {
		free_attr((yyvsp[-1].al));
	}
#line 6975 "verilog_parser.tab.cc"
    break;

  case 543: /* $@60: %empty  */
#line 2716 "verilog_parser.y"
                             {
		AstNode *node = new AstNode(AST_TCALL);
		node->str = *(yyvsp[0].string);
		delete (yyvsp[0].string);
		ast_stack.back()->children.push_back(node);
		ast_stack.push_back(node);
		append_attr(node, (yyvsp[-1].al));
	}
#line 6988 "verilog_parser.tab.cc"
    break;

  case 544: /* behavioral_stmt: attr hierarchical_id $@60 opt_arg_list ';'  */
#line 2723 "verilog_parser.y"
                          {
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-3]), (yylsp[0]));
		ast_stack.pop_back();
	}
#line 6997 "verilog_parser.tab.cc"
    break;

  case 545: /* $@61: %empty  */
#line 2727 "verilog_parser.y"
                           {
		AstNode *node = new AstNode(AST_TCALL);
		node->str = *(yyvsp[0].string);
		delete (yyvsp[0].string);
		ast_stack.back()->children.push_back(node);
		ast_stack.push_back(node);
		append_attr(node, (yyvsp[-1].al));
	}
#line 7010 "verilog_parser.tab.cc"
    break;

  case 546: /* behavioral_stmt: attr TOK_MSG_TASKS $@61 opt_arg_list ';'  */
#line 2734 "verilog_parser.y"
                          {
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-3]), (yylsp[0]));
		ast_stack.pop_back();
	}
#line 7019 "verilog_parser.tab.cc"
    break;

  case 547: /* $@62: %empty  */
#line 2738 "verilog_parser.y"
                       {
		enterTypeScope();
	}
#line 7027 "verilog_parser.tab.cc"
    break;

  case 548: /* $@63: %empty  */
#line 2740 "verilog_parser.y"
                    {
		AstNode *node = new AstNode(AST_BLOCK);
		ast_stack.back()->children.push_back(node);
		ast_stack.push_back(node);
		append_attr(node, (yyvsp[-3].al));
		if ((yyvsp[0].string) != NULL)
			node->str = *(yyvsp[0].string);
	}
#line 7040 "verilog_parser.tab.cc"
    break;

  case 549: /* behavioral_stmt: attr TOK_BEGIN $@62 opt_label $@63 behavioral_stmt_list TOK_END opt_label  */
#line 2747 "verilog_parser.y"
                                                 {
		exitTypeScope();
		checkLabelsMatch("Begin label", (yyvsp[-4].string), (yyvsp[0].string));
		AstNode *node = ast_stack.back();
		// In SystemVerilog, unnamed blocks with block item declarations
		// create an implicit hierarchy scope
		if (sv_mode && node->str.empty())
		    for (const AstNode* child : node->children)
			if (child->type == AST_WIRE || child->type == AST_MEMORY || child->type == AST_PARAMETER
				|| child->type == AST_LOCALPARAM || child->type == AST_TYPEDEF) {
			    node->str = "$unnamed_block$" + std::to_string(autoidx++);
			    break;
			}
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-6]), (yylsp[0]));
		delete (yyvsp[-4].string);
		delete (yyvsp[0].string);
		ast_stack.pop_back();
	}
#line 7063 "verilog_parser.tab.cc"
    break;

  case 550: /* $@64: %empty  */
#line 2765 "verilog_parser.y"
                         {
		AstNode *node = new AstNode(AST_FOR);
		ast_stack.back()->children.push_back(node);
		ast_stack.push_back(node);
		append_attr(node, (yyvsp[-2].al));
	}
#line 7074 "verilog_parser.tab.cc"
    break;

  case 551: /* $@65: %empty  */
#line 2770 "verilog_parser.y"
                                      {
		ast_stack.back()->children.push_back((yyvsp[0].ast));
	}
#line 7082 "verilog_parser.tab.cc"
    break;

  case 552: /* $@66: %empty  */
#line 2772 "verilog_parser.y"
                                         {
		AstNode *block = new AstNode(AST_BLOCK);
		block->str = "$for_loop$" + std::to_string(autoidx++);
		ast_stack.back()->children.push_back(block);
		ast_stack.push_back(block);
	}
#line 7093 "verilog_parser.tab.cc"
    break;

  case 553: /* behavioral_stmt: attr TOK_FOR '(' $@64 for_initialization ';' expr $@65 ';' simple_behavioral_stmt ')' $@66 behavioral_stmt  */
#line 2777 "verilog_parser.y"
                          {
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[0]), (yylsp[0]));
		ast_stack.pop_back();
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-11]), (yylsp[0]));
		ast_stack.pop_back();
	}
#line 7104 "verilog_parser.tab.cc"
    break;

  case 554: /* $@67: %empty  */
#line 2783 "verilog_parser.y"
                                    {
		AstNode *node = new AstNode(AST_WHILE);
		ast_stack.back()->children.push_back(node);
		ast_stack.push_back(node);
		append_attr(node, (yyvsp[-4].al));
		AstNode *block = new AstNode(AST_BLOCK);
		ast_stack.back()->children.push_back((yyvsp[-1].ast));
		ast_stack.back()->children.push_back(block);
		ast_stack.push_back(block);
	}
#line 7119 "verilog_parser.tab.cc"
    break;

  case 555: /* behavioral_stmt: attr TOK_WHILE '(' expr ')' $@67 behavioral_stmt  */
#line 2792 "verilog_parser.y"
                          {
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[0]), (yylsp[0]));
		ast_stack.pop_back();
		ast_stack.pop_back();
	}
#line 7129 "verilog_parser.tab.cc"
    break;

  case 556: /* $@68: %empty  */
#line 2797 "verilog_parser.y"
                                     {
		AstNode *node = new AstNode(AST_REPEAT);
		ast_stack.back()->children.push_back(node);
		ast_stack.push_back(node);
		append_attr(node, (yyvsp[-4].al));
		AstNode *block = new AstNode(AST_BLOCK);
		ast_stack.back()->children.push_back((yyvsp[-1].ast));
		ast_stack.back()->children.push_back(block);
		ast_stack.push_back(block);
	}
#line 7144 "verilog_parser.tab.cc"
    break;

  case 557: /* behavioral_stmt: attr TOK_REPEAT '(' expr ')' $@68 behavioral_stmt  */
#line 2806 "verilog_parser.y"
                          {
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[0]), (yylsp[0]));
		ast_stack.pop_back();
		ast_stack.pop_back();
	}
#line 7154 "verilog_parser.tab.cc"
    break;

  case 558: /* $@69: %empty  */
#line 2811 "verilog_parser.y"
                                 {
		AstNode *node = new AstNode(AST_CASE);
		AstNode *block = new AstNode(AST_BLOCK);
		AstNode *cond = new AstNode(AST_COND, AstNode::mkconst_int(1, false, 1), block);
		SET_AST_NODE_LOC(cond, (yylsp[-1]), (yylsp[-1]));
		ast_stack.back()->children.push_back(node);
		node->children.push_back(new AstNode(AST_REDUCE_BOOL, (yyvsp[-1].ast)));
		node->children.push_back(cond);
		ast_stack.push_back(node);
		ast_stack.push_back(block);
		append_attr(node, (yyvsp[-4].al));
	}
#line 7171 "verilog_parser.tab.cc"
    break;

  case 559: /* $@70: %empty  */
#line 2822 "verilog_parser.y"
                          {
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[0]), (yylsp[0]));
	}
#line 7179 "verilog_parser.tab.cc"
    break;

  case 560: /* behavioral_stmt: attr TOK_IF '(' expr ')' $@69 behavioral_stmt $@70 optional_else  */
#line 2824 "verilog_parser.y"
                        {
		ast_stack.pop_back();
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-7]), (yylsp[0]));
		ast_stack.pop_back();
	}
#line 7189 "verilog_parser.tab.cc"
    break;

  case 561: /* $@71: %empty  */
#line 2829 "verilog_parser.y"
                                         {
		AstNode *node = new AstNode(AST_CASE, (yyvsp[-1].ast));
		ast_stack.back()->children.push_back(node);
		ast_stack.push_back(node);
		append_attr(node, (yyvsp[-4].al));
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-1]), (yylsp[-1]));
	}
#line 7201 "verilog_parser.tab.cc"
    break;

  case 562: /* behavioral_stmt: case_attr case_type '(' expr ')' $@71 opt_synopsys_attr case_body TOK_ENDCASE  */
#line 2835 "verilog_parser.y"
                                                  {
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-7]), (yylsp[0]));
		case_type_stack.pop_back();
		ast_stack.pop_back();
	}
#line 7211 "verilog_parser.tab.cc"
    break;

  case 563: /* case_attr: attr  */
#line 2842 "verilog_parser.y"
             {
		(yyval.al) = (yyvsp[0].al);
	}
#line 7219 "verilog_parser.tab.cc"
    break;

  case 564: /* case_attr: attr TOK_UNIQUE0  */
#line 2845 "verilog_parser.y"
                         {
		(*(yyvsp[-1].al))[ID::parallel_case] = AstNode::mkconst_int(1, false);
		(yyval.al) = (yyvsp[-1].al);
	}
#line 7228 "verilog_parser.tab.cc"
    break;

  case 565: /* case_attr: attr TOK_PRIORITY  */
#line 2849 "verilog_parser.y"
                          {
		(*(yyvsp[-1].al))[ID::full_case] = AstNode::mkconst_int(1, false);
		(yyval.al) = (yyvsp[-1].al);
	}
#line 7237 "verilog_parser.tab.cc"
    break;

  case 566: /* case_attr: attr TOK_UNIQUE  */
#line 2853 "verilog_parser.y"
                        {
		(*(yyvsp[-1].al))[ID::full_case] = AstNode::mkconst_int(1, false);
		(*(yyvsp[-1].al))[ID::parallel_case] = AstNode::mkconst_int(1, false);
		(yyval.al) = (yyvsp[-1].al);
	}
#line 7247 "verilog_parser.tab.cc"
    break;

  case 567: /* case_type: TOK_CASE  */
#line 2860 "verilog_parser.y"
                 {
		case_type_stack.push_back(0);
	}
#line 7255 "verilog_parser.tab.cc"
    break;

  case 568: /* case_type: TOK_CASEX  */
#line 2863 "verilog_parser.y"
                  {
		case_type_stack.push_back('x');
	}
#line 7263 "verilog_parser.tab.cc"
    break;

  case 569: /* case_type: TOK_CASEZ  */
#line 2866 "verilog_parser.y"
                  {
		case_type_stack.push_back('z');
	}
#line 7271 "verilog_parser.tab.cc"
    break;

  case 570: /* opt_synopsys_attr: opt_synopsys_attr TOK_SYNOPSYS_FULL_CASE  */
#line 2871 "verilog_parser.y"
                                                 {
		if (ast_stack.back()->attributes.count(ID::full_case) == 0)
			ast_stack.back()->attributes[ID::full_case] = AstNode::mkconst_int(1, false);
	}
#line 7280 "verilog_parser.tab.cc"
    break;

  case 571: /* opt_synopsys_attr: opt_synopsys_attr TOK_SYNOPSYS_PARALLEL_CASE  */
#line 2875 "verilog_parser.y"
                                                     {
		if (ast_stack.back()->attributes.count(ID::parallel_case) == 0)
			ast_stack.back()->attributes[ID::parallel_case] = AstNode::mkconst_int(1, false);
	}
#line 7289 "verilog_parser.tab.cc"
    break;

  case 575: /* $@72: %empty  */
#line 2886 "verilog_parser.y"
                 {
		AstNode *block = new AstNode(AST_BLOCK);
		AstNode *cond = new AstNode(AST_COND, new AstNode(AST_DEFAULT), block);
		SET_AST_NODE_LOC(cond, (yylsp[0]), (yylsp[0]));

		ast_stack.pop_back();
		ast_stack.back()->children.push_back(cond);
		ast_stack.push_back(block);
	}
#line 7303 "verilog_parser.tab.cc"
    break;

  case 576: /* optional_else: TOK_ELSE $@72 behavioral_stmt  */
#line 2894 "verilog_parser.y"
                          {
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[0]), (yylsp[0]));
	}
#line 7311 "verilog_parser.tab.cc"
    break;

  case 580: /* $@73: %empty  */
#line 2904 "verilog_parser.y"
        {
		AstNode *node = new AstNode(
				case_type_stack.size() && case_type_stack.back() == 'x' ? AST_CONDX :
				case_type_stack.size() && case_type_stack.back() == 'z' ? AST_CONDZ : AST_COND);
		ast_stack.back()->children.push_back(node);
		ast_stack.push_back(node);
	}
#line 7323 "verilog_parser.tab.cc"
    break;

  case 581: /* $@74: %empty  */
#line 2910 "verilog_parser.y"
                      {
		AstNode *block = new AstNode(AST_BLOCK);
		ast_stack.back()->children.push_back(block);
		ast_stack.push_back(block);
		case_type_stack.push_back(0);
	}
#line 7334 "verilog_parser.tab.cc"
    break;

  case 582: /* case_item: $@73 case_select $@74 behavioral_stmt  */
#line 2915 "verilog_parser.y"
                          {
		case_type_stack.pop_back();
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[0]), (yylsp[0]));
		ast_stack.pop_back();
		ast_stack.pop_back();
	}
#line 7345 "verilog_parser.tab.cc"
    break;

  case 585: /* $@75: %empty  */
#line 2927 "verilog_parser.y"
        {
		AstNode *node = new AstNode(
				case_type_stack.size() && case_type_stack.back() == 'x' ? AST_CONDX :
				case_type_stack.size() && case_type_stack.back() == 'z' ? AST_CONDZ : AST_COND);
		ast_stack.back()->children.push_back(node);
		ast_stack.push_back(node);
	}
#line 7357 "verilog_parser.tab.cc"
    break;

  case 586: /* $@76: %empty  */
#line 2933 "verilog_parser.y"
                      {
		case_type_stack.push_back(0);
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[0]), (yylsp[0]));
	}
#line 7366 "verilog_parser.tab.cc"
    break;

  case 587: /* gen_case_item: $@75 case_select $@76 gen_stmt_block  */
#line 2936 "verilog_parser.y"
                         {
		case_type_stack.pop_back();
		ast_stack.pop_back();
	}
#line 7375 "verilog_parser.tab.cc"
    break;

  case 590: /* case_expr_list: TOK_DEFAULT  */
#line 2946 "verilog_parser.y"
                    {
		AstNode *node = new AstNode(AST_DEFAULT);
		SET_AST_NODE_LOC(node, (yylsp[0]), (yylsp[0]));
		ast_stack.back()->children.push_back(node);
	}
#line 7385 "verilog_parser.tab.cc"
    break;

  case 591: /* case_expr_list: TOK_SVA_LABEL  */
#line 2951 "verilog_parser.y"
                      {
		AstNode *node = new AstNode(AST_IDENTIFIER);
		SET_AST_NODE_LOC(node, (yylsp[0]), (yylsp[0]));
		ast_stack.back()->children.push_back(node);
		ast_stack.back()->children.back()->str = *(yyvsp[0].string);
		delete (yyvsp[0].string);
	}
#line 7397 "verilog_parser.tab.cc"
    break;

  case 592: /* case_expr_list: expr  */
#line 2958 "verilog_parser.y"
             {
		ast_stack.back()->children.push_back((yyvsp[0].ast));
	}
#line 7405 "verilog_parser.tab.cc"
    break;

  case 593: /* case_expr_list: case_expr_list ',' expr  */
#line 2961 "verilog_parser.y"
                                {
		ast_stack.back()->children.push_back((yyvsp[0].ast));
	}
#line 7413 "verilog_parser.tab.cc"
    break;

  case 594: /* rvalue: hierarchical_id '[' expr ']' '.' rvalue  */
#line 2966 "verilog_parser.y"
                                                {
		(yyval.ast) = new AstNode(AST_PREFIX, (yyvsp[-3].ast), (yyvsp[0].ast));
		(yyval.ast)->str = *(yyvsp[-5].string);
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-5]), (yylsp[0]));
		delete (yyvsp[-5].string);
	}
#line 7424 "verilog_parser.tab.cc"
    break;

  case 595: /* rvalue: hierarchical_id range  */
#line 2972 "verilog_parser.y"
                              {
		(yyval.ast) = new AstNode(AST_IDENTIFIER, (yyvsp[0].ast));
		(yyval.ast)->str = *(yyvsp[-1].string);
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-1]), (yylsp[-1]));
		delete (yyvsp[-1].string);
		if ((yyvsp[0].ast) == nullptr && ((yyval.ast)->str == "\\$initstate" ||
				(yyval.ast)->str == "\\$anyconst" || (yyval.ast)->str == "\\$anyseq" ||
				(yyval.ast)->str == "\\$allconst" || (yyval.ast)->str == "\\$allseq"))
			(yyval.ast)->type = AST_FCALL;
	}
#line 7439 "verilog_parser.tab.cc"
    break;

  case 596: /* rvalue: hierarchical_id non_opt_multirange  */
#line 2982 "verilog_parser.y"
                                           {
		(yyval.ast) = new AstNode(AST_IDENTIFIER, (yyvsp[0].ast));
		(yyval.ast)->str = *(yyvsp[-1].string);
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-1]), (yylsp[-1]));
		delete (yyvsp[-1].string);
	}
#line 7450 "verilog_parser.tab.cc"
    break;

  case 597: /* lvalue: rvalue  */
#line 2990 "verilog_parser.y"
               {
		(yyval.ast) = (yyvsp[0].ast);
	}
#line 7458 "verilog_parser.tab.cc"
    break;

  case 598: /* lvalue: '{' lvalue_concat_list '}'  */
#line 2993 "verilog_parser.y"
                                   {
		(yyval.ast) = (yyvsp[-1].ast);
	}
#line 7466 "verilog_parser.tab.cc"
    break;

  case 599: /* lvalue_concat_list: expr  */
#line 2998 "verilog_parser.y"
             {
		(yyval.ast) = new AstNode(AST_CONCAT);
		(yyval.ast)->children.push_back((yyvsp[0].ast));
	}
#line 7475 "verilog_parser.tab.cc"
    break;

  case 600: /* lvalue_concat_list: expr ',' lvalue_concat_list  */
#line 3002 "verilog_parser.y"
                                    {
		(yyval.ast) = (yyvsp[0].ast);
		(yyval.ast)->children.push_back((yyvsp[-2].ast));
	}
#line 7484 "verilog_parser.tab.cc"
    break;

  case 607: /* single_arg: expr  */
#line 3020 "verilog_parser.y"
             {
		ast_stack.back()->children.push_back((yyvsp[0].ast));
	}
#line 7492 "verilog_parser.tab.cc"
    break;

  case 613: /* gen_stmt_or_module_body_stmt: attr ';'  */
#line 3031 "verilog_parser.y"
                 {
		free_attr((yyvsp[-1].al));
	}
#line 7500 "verilog_parser.tab.cc"
    break;

  case 614: /* genvar_identifier: TOK_ID  */
#line 3036 "verilog_parser.y"
               {
		(yyval.ast) = new AstNode(AST_IDENTIFIER);
		(yyval.ast)->str = *(yyvsp[0].string);
		delete (yyvsp[0].string);
	}
#line 7510 "verilog_parser.tab.cc"
    break;

  case 615: /* genvar_initialization: TOK_GENVAR genvar_identifier  */
#line 3043 "verilog_parser.y"
                                     {
		frontend_verilog_yyerror("Generate for loop variable declaration is missing initialization!");
	}
#line 7518 "verilog_parser.tab.cc"
    break;

  case 616: /* genvar_initialization: TOK_GENVAR genvar_identifier '=' expr  */
#line 3046 "verilog_parser.y"
                                              {
		if (!sv_mode)
			frontend_verilog_yyerror("Generate for loop inline variable declaration is only supported in SystemVerilog mode!");
		AstNode *node = new AstNode(AST_GENVAR);
		node->is_reg = true;
		node->is_signed = true;
		node->range_left = 31;
		node->range_right = 0;
		node->str = (yyvsp[-2].ast)->str;
		node->children.push_back(checkRange(node, nullptr));
		ast_stack.back()->children.push_back(node);
		SET_AST_NODE_LOC(node, (yylsp[-3]), (yylsp[0]));
		node = new AstNode(AST_ASSIGN_EQ, (yyvsp[-2].ast), (yyvsp[0].ast));
		ast_stack.back()->children.push_back(node);
		SET_AST_NODE_LOC(node, (yylsp[-3]), (yylsp[0]));
	}
#line 7539 "verilog_parser.tab.cc"
    break;

  case 617: /* genvar_initialization: genvar_identifier '=' expr  */
#line 3062 "verilog_parser.y"
                                   {
		AstNode *node = new AstNode(AST_ASSIGN_EQ, (yyvsp[-2].ast), (yyvsp[0].ast));
		ast_stack.back()->children.push_back(node);
		SET_AST_NODE_LOC(node, (yylsp[-2]), (yylsp[0]));
	}
#line 7549 "verilog_parser.tab.cc"
    break;

  case 618: /* $@77: %empty  */
#line 3070 "verilog_parser.y"
                    {
		AstNode *node = new AstNode(AST_GENFOR);
		ast_stack.back()->children.push_back(node);
		ast_stack.push_back(node);
	}
#line 7559 "verilog_parser.tab.cc"
    break;

  case 619: /* $@78: %empty  */
#line 3074 "verilog_parser.y"
                                         {
		ast_stack.back()->children.push_back((yyvsp[0].ast));
	}
#line 7567 "verilog_parser.tab.cc"
    break;

  case 620: /* gen_stmt: TOK_FOR '(' $@77 genvar_initialization ';' expr $@78 ';' simple_behavioral_stmt ')' gen_stmt_block  */
#line 3076 "verilog_parser.y"
                                                        {
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-10]), (yylsp[0]));
		rewriteGenForDeclInit(ast_stack.back());
		ast_stack.pop_back();
	}
#line 7577 "verilog_parser.tab.cc"
    break;

  case 621: /* $@79: %empty  */
#line 3081 "verilog_parser.y"
                            {
		AstNode *node = new AstNode(AST_GENIF);
		ast_stack.back()->children.push_back(node);
		ast_stack.push_back(node);
		ast_stack.back()->children.push_back((yyvsp[-1].ast));
	}
#line 7588 "verilog_parser.tab.cc"
    break;

  case 622: /* gen_stmt: TOK_IF '(' expr ')' $@79 gen_stmt_block opt_gen_else  */
#line 3086 "verilog_parser.y"
                                      {
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-6]), (yylsp[0]));
		ast_stack.pop_back();
	}
#line 7597 "verilog_parser.tab.cc"
    break;

  case 623: /* $@80: %empty  */
#line 3090 "verilog_parser.y"
                               {
		AstNode *node = new AstNode(AST_GENCASE, (yyvsp[-1].ast));
		ast_stack.back()->children.push_back(node);
		ast_stack.push_back(node);
	}
#line 7607 "verilog_parser.tab.cc"
    break;

  case 624: /* gen_stmt: case_type '(' expr ')' $@80 gen_case_body TOK_ENDCASE  */
#line 3094 "verilog_parser.y"
                                    {
		case_type_stack.pop_back();
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-6]), (yylsp[0]));
		ast_stack.pop_back();
	}
#line 7617 "verilog_parser.tab.cc"
    break;

  case 625: /* $@81: %empty  */
#line 3099 "verilog_parser.y"
                      {
		AstNode *node = new AstNode(AST_TECALL);
		node->str = *(yyvsp[0].string);
		delete (yyvsp[0].string);
		ast_stack.back()->children.push_back(node);
		ast_stack.push_back(node);
	}
#line 7629 "verilog_parser.tab.cc"
    break;

  case 626: /* gen_stmt: TOK_MSG_TASKS $@81 opt_arg_list ';'  */
#line 3105 "verilog_parser.y"
                          {
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-3]), (yylsp[-1]));
		ast_stack.pop_back();
	}
#line 7638 "verilog_parser.tab.cc"
    break;

  case 627: /* $@82: %empty  */
#line 3111 "verilog_parser.y"
                  {
		enterTypeScope();
	}
#line 7646 "verilog_parser.tab.cc"
    break;

  case 628: /* $@83: %empty  */
#line 3113 "verilog_parser.y"
                    {
		AstNode *node = new AstNode(AST_GENBLOCK);
		node->str = (yyvsp[0].string) ? *(yyvsp[0].string) : std::string();
		ast_stack.back()->children.push_back(node);
		ast_stack.push_back(node);
	}
#line 7657 "verilog_parser.tab.cc"
    break;

  case 629: /* gen_block: TOK_BEGIN $@82 opt_label $@83 module_gen_body TOK_END opt_label  */
#line 3118 "verilog_parser.y"
                                            {
		exitTypeScope();
		checkLabelsMatch("Begin label", (yyvsp[-4].string), (yyvsp[0].string));
		delete (yyvsp[-4].string);
		delete (yyvsp[0].string);
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-6]), (yylsp[0]));
		ast_stack.pop_back();
	}
#line 7670 "verilog_parser.tab.cc"
    break;

  case 630: /* $@84: %empty  */
#line 3129 "verilog_parser.y"
        {
		AstNode *node = new AstNode(AST_GENBLOCK);
		ast_stack.back()->children.push_back(node);
		ast_stack.push_back(node);
	}
#line 7680 "verilog_parser.tab.cc"
    break;

  case 631: /* gen_stmt_block: $@84 gen_stmt_or_module_body_stmt  */
#line 3133 "verilog_parser.y"
                                       {
		SET_AST_NODE_LOC(ast_stack.back(), (yylsp[0]), (yylsp[0]));
		ast_stack.pop_back();
	}
#line 7689 "verilog_parser.tab.cc"
    break;

  case 635: /* expr: basic_expr  */
#line 3142 "verilog_parser.y"
                   {
		(yyval.ast) = (yyvsp[0].ast);
	}
#line 7697 "verilog_parser.tab.cc"
    break;

  case 636: /* expr: basic_expr '?' attr expr ':' expr  */
#line 3145 "verilog_parser.y"
                                          {
		(yyval.ast) = new AstNode(AST_TERNARY);
		(yyval.ast)->children.push_back((yyvsp[-5].ast));
		(yyval.ast)->children.push_back((yyvsp[-2].ast));
		(yyval.ast)->children.push_back((yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-5]), (yyloc));
		append_attr((yyval.ast), (yyvsp[-3].al));
	}
#line 7710 "verilog_parser.tab.cc"
    break;

  case 637: /* basic_expr: rvalue  */
#line 3155 "verilog_parser.y"
               {
		(yyval.ast) = (yyvsp[0].ast);
	}
#line 7718 "verilog_parser.tab.cc"
    break;

  case 638: /* basic_expr: '(' expr ')' integral_number  */
#line 3158 "verilog_parser.y"
                                     {
		if ((yyvsp[0].string)->compare(0, 1, "'") != 0)
			frontend_verilog_yyerror("Cast operation must be applied on sized constants e.g. (<expr>)<constval> , while %s is not a sized constant.", (yyvsp[0].string)->c_str());
		AstNode *bits = (yyvsp[-2].ast);
		AstNode *val = const2ast(*(yyvsp[0].string), case_type_stack.size() == 0 ? 0 : case_type_stack.back(), !lib_mode);
		if (val == NULL)
			log_error("Value conversion failed: `%s'\n", (yyvsp[0].string)->c_str());
		(yyval.ast) = new AstNode(AST_TO_BITS, bits, val);
		delete (yyvsp[0].string);
	}
#line 7733 "verilog_parser.tab.cc"
    break;

  case 639: /* basic_expr: hierarchical_id integral_number  */
#line 3168 "verilog_parser.y"
                                        {
		if ((yyvsp[0].string)->compare(0, 1, "'") != 0)
			frontend_verilog_yyerror("Cast operation must be applied on sized constants, e.g. <ID>\'d0, while %s is not a sized constant.", (yyvsp[0].string)->c_str());
		AstNode *bits = new AstNode(AST_IDENTIFIER);
		bits->str = *(yyvsp[-1].string);
		SET_AST_NODE_LOC(bits, (yylsp[-1]), (yylsp[-1]));
		AstNode *val = const2ast(*(yyvsp[0].string), case_type_stack.size() == 0 ? 0 : case_type_stack.back(), !lib_mode);
		SET_AST_NODE_LOC(val, (yylsp[0]), (yylsp[0]));
		if (val == NULL)
			log_error("Value conversion failed: `%s'\n", (yyvsp[0].string)->c_str());
		(yyval.ast) = new AstNode(AST_TO_BITS, bits, val);
		delete (yyvsp[-1].string);
		delete (yyvsp[0].string);
	}
#line 7752 "verilog_parser.tab.cc"
    break;

  case 640: /* basic_expr: integral_number  */
#line 3182 "verilog_parser.y"
                        {
		(yyval.ast) = const2ast(*(yyvsp[0].string), case_type_stack.size() == 0 ? 0 : case_type_stack.back(), !lib_mode);
		SET_AST_NODE_LOC((yyval.ast), (yylsp[0]), (yylsp[0]));
		if ((yyval.ast) == NULL)
			log_error("Value conversion failed: `%s'\n", (yyvsp[0].string)->c_str());
		delete (yyvsp[0].string);
	}
#line 7764 "verilog_parser.tab.cc"
    break;

  case 641: /* basic_expr: TOK_REALVAL  */
#line 3189 "verilog_parser.y"
                    {
		(yyval.ast) = new AstNode(AST_REALVALUE);
		char *p = (char*)malloc(GetSize(*(yyvsp[0].string)) + 1), *q;
		for (int i = 0, j = 0; j < GetSize(*(yyvsp[0].string)); j++)
			if ((*(yyvsp[0].string))[j] != '_')
				p[i++] = (*(yyvsp[0].string))[j], p[i] = 0;
		(yyval.ast)->realvalue = strtod(p, &q);
		SET_AST_NODE_LOC((yyval.ast), (yylsp[0]), (yylsp[0]));
		log_assert(*q == 0);
		delete (yyvsp[0].string);
		free(p);
	}
#line 7781 "verilog_parser.tab.cc"
    break;

  case 642: /* basic_expr: TOK_STRING  */
#line 3201 "verilog_parser.y"
                   {
		(yyval.ast) = AstNode::mkconst_str(*(yyvsp[0].string));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[0]), (yylsp[0]));
		delete (yyvsp[0].string);
	}
#line 7791 "verilog_parser.tab.cc"
    break;

  case 643: /* $@85: %empty  */
#line 3206 "verilog_parser.y"
                             {
		AstNode *node = new AstNode(AST_FCALL);
		node->str = *(yyvsp[-1].string);
		delete (yyvsp[-1].string);
		ast_stack.push_back(node);
		SET_AST_NODE_LOC(node, (yylsp[-1]), (yylsp[-1]));
		append_attr(node, (yyvsp[0].al));
	}
#line 7804 "verilog_parser.tab.cc"
    break;

  case 644: /* basic_expr: hierarchical_id attr $@85 '(' arg_list optional_comma ')'  */
#line 3213 "verilog_parser.y"
                                          {
		(yyval.ast) = ast_stack.back();
		ast_stack.pop_back();
	}
#line 7813 "verilog_parser.tab.cc"
    break;

  case 645: /* basic_expr: TOK_TO_SIGNED attr '(' expr ')'  */
#line 3217 "verilog_parser.y"
                                        {
		(yyval.ast) = new AstNode(AST_TO_SIGNED, (yyvsp[-1].ast));
		append_attr((yyval.ast), (yyvsp[-3].al));
	}
#line 7822 "verilog_parser.tab.cc"
    break;

  case 646: /* basic_expr: TOK_TO_UNSIGNED attr '(' expr ')'  */
#line 3221 "verilog_parser.y"
                                          {
		(yyval.ast) = new AstNode(AST_TO_UNSIGNED, (yyvsp[-1].ast));
		append_attr((yyval.ast), (yyvsp[-3].al));
	}
#line 7831 "verilog_parser.tab.cc"
    break;

  case 647: /* basic_expr: '(' expr ')'  */
#line 3225 "verilog_parser.y"
                     {
		(yyval.ast) = (yyvsp[-1].ast);
	}
#line 7839 "verilog_parser.tab.cc"
    break;

  case 648: /* basic_expr: '(' expr ':' expr ':' expr ')'  */
#line 3228 "verilog_parser.y"
                                       {
		delete (yyvsp[-5].ast);
		(yyval.ast) = (yyvsp[-3].ast);
		delete (yyvsp[-1].ast);
	}
#line 7849 "verilog_parser.tab.cc"
    break;

  case 649: /* basic_expr: '{' concat_list '}'  */
#line 3233 "verilog_parser.y"
                            {
		(yyval.ast) = (yyvsp[-1].ast);
	}
#line 7857 "verilog_parser.tab.cc"
    break;

  case 650: /* basic_expr: '{' expr '{' concat_list '}' '}'  */
#line 3236 "verilog_parser.y"
                                         {
		(yyval.ast) = new AstNode(AST_REPLICATE, (yyvsp[-4].ast), (yyvsp[-2].ast));
	}
#line 7865 "verilog_parser.tab.cc"
    break;

  case 651: /* basic_expr: '~' attr basic_expr  */
#line 3239 "verilog_parser.y"
                                            {
		(yyval.ast) = new AstNode(AST_BIT_NOT, (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 7875 "verilog_parser.tab.cc"
    break;

  case 652: /* basic_expr: basic_expr '&' attr basic_expr  */
#line 3244 "verilog_parser.y"
                                       {
		(yyval.ast) = new AstNode(AST_BIT_AND, (yyvsp[-3].ast), (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 7885 "verilog_parser.tab.cc"
    break;

  case 653: /* basic_expr: basic_expr OP_NAND attr basic_expr  */
#line 3249 "verilog_parser.y"
                                           {
		(yyval.ast) = new AstNode(AST_BIT_NOT, new AstNode(AST_BIT_AND, (yyvsp[-3].ast), (yyvsp[0].ast)));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 7895 "verilog_parser.tab.cc"
    break;

  case 654: /* basic_expr: basic_expr '|' attr basic_expr  */
#line 3254 "verilog_parser.y"
                                       {
		(yyval.ast) = new AstNode(AST_BIT_OR, (yyvsp[-3].ast), (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 7905 "verilog_parser.tab.cc"
    break;

  case 655: /* basic_expr: basic_expr OP_NOR attr basic_expr  */
#line 3259 "verilog_parser.y"
                                          {
		(yyval.ast) = new AstNode(AST_BIT_NOT, new AstNode(AST_BIT_OR, (yyvsp[-3].ast), (yyvsp[0].ast)));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 7915 "verilog_parser.tab.cc"
    break;

  case 656: /* basic_expr: basic_expr '^' attr basic_expr  */
#line 3264 "verilog_parser.y"
                                       {
		(yyval.ast) = new AstNode(AST_BIT_XOR, (yyvsp[-3].ast), (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 7925 "verilog_parser.tab.cc"
    break;

  case 657: /* basic_expr: basic_expr OP_XNOR attr basic_expr  */
#line 3269 "verilog_parser.y"
                                           {
		(yyval.ast) = new AstNode(AST_BIT_XNOR, (yyvsp[-3].ast), (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 7935 "verilog_parser.tab.cc"
    break;

  case 658: /* basic_expr: '&' attr basic_expr  */
#line 3274 "verilog_parser.y"
                                            {
		(yyval.ast) = new AstNode(AST_REDUCE_AND, (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 7945 "verilog_parser.tab.cc"
    break;

  case 659: /* basic_expr: OP_NAND attr basic_expr  */
#line 3279 "verilog_parser.y"
                                                {
		(yyval.ast) = new AstNode(AST_REDUCE_AND, (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
		(yyval.ast) = new AstNode(AST_LOGIC_NOT, (yyval.ast));
	}
#line 7956 "verilog_parser.tab.cc"
    break;

  case 660: /* basic_expr: '|' attr basic_expr  */
#line 3285 "verilog_parser.y"
                                            {
		(yyval.ast) = new AstNode(AST_REDUCE_OR, (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 7966 "verilog_parser.tab.cc"
    break;

  case 661: /* basic_expr: OP_NOR attr basic_expr  */
#line 3290 "verilog_parser.y"
                                               {
		(yyval.ast) = new AstNode(AST_REDUCE_OR, (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
		(yyval.ast) = new AstNode(AST_LOGIC_NOT, (yyval.ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
	}
#line 7978 "verilog_parser.tab.cc"
    break;

  case 662: /* basic_expr: '^' attr basic_expr  */
#line 3297 "verilog_parser.y"
                                            {
		(yyval.ast) = new AstNode(AST_REDUCE_XOR, (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 7988 "verilog_parser.tab.cc"
    break;

  case 663: /* basic_expr: OP_XNOR attr basic_expr  */
#line 3302 "verilog_parser.y"
                                                {
		(yyval.ast) = new AstNode(AST_REDUCE_XNOR, (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 7998 "verilog_parser.tab.cc"
    break;

  case 664: /* basic_expr: basic_expr OP_SHL attr basic_expr  */
#line 3307 "verilog_parser.y"
                                          {
		(yyval.ast) = new AstNode(AST_SHIFT_LEFT, (yyvsp[-3].ast), new AstNode(AST_TO_UNSIGNED, (yyvsp[0].ast)));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8008 "verilog_parser.tab.cc"
    break;

  case 665: /* basic_expr: basic_expr OP_SHR attr basic_expr  */
#line 3312 "verilog_parser.y"
                                          {
		(yyval.ast) = new AstNode(AST_SHIFT_RIGHT, (yyvsp[-3].ast), new AstNode(AST_TO_UNSIGNED, (yyvsp[0].ast)));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8018 "verilog_parser.tab.cc"
    break;

  case 666: /* basic_expr: basic_expr OP_SSHL attr basic_expr  */
#line 3317 "verilog_parser.y"
                                           {
		(yyval.ast) = new AstNode(AST_SHIFT_SLEFT, (yyvsp[-3].ast), new AstNode(AST_TO_UNSIGNED, (yyvsp[0].ast)));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8028 "verilog_parser.tab.cc"
    break;

  case 667: /* basic_expr: basic_expr OP_SSHR attr basic_expr  */
#line 3322 "verilog_parser.y"
                                           {
		(yyval.ast) = new AstNode(AST_SHIFT_SRIGHT, (yyvsp[-3].ast), new AstNode(AST_TO_UNSIGNED, (yyvsp[0].ast)));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8038 "verilog_parser.tab.cc"
    break;

  case 668: /* basic_expr: basic_expr '<' attr basic_expr  */
#line 3327 "verilog_parser.y"
                                       {
		(yyval.ast) = new AstNode(AST_LT, (yyvsp[-3].ast), (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8048 "verilog_parser.tab.cc"
    break;

  case 669: /* basic_expr: basic_expr OP_LE attr basic_expr  */
#line 3332 "verilog_parser.y"
                                         {
		(yyval.ast) = new AstNode(AST_LE, (yyvsp[-3].ast), (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8058 "verilog_parser.tab.cc"
    break;

  case 670: /* basic_expr: basic_expr OP_EQ attr basic_expr  */
#line 3337 "verilog_parser.y"
                                         {
		(yyval.ast) = new AstNode(AST_EQ, (yyvsp[-3].ast), (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8068 "verilog_parser.tab.cc"
    break;

  case 671: /* basic_expr: basic_expr OP_NE attr basic_expr  */
#line 3342 "verilog_parser.y"
                                         {
		(yyval.ast) = new AstNode(AST_NE, (yyvsp[-3].ast), (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8078 "verilog_parser.tab.cc"
    break;

  case 672: /* basic_expr: basic_expr OP_EQX attr basic_expr  */
#line 3347 "verilog_parser.y"
                                          {
		(yyval.ast) = new AstNode(AST_EQX, (yyvsp[-3].ast), (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8088 "verilog_parser.tab.cc"
    break;

  case 673: /* basic_expr: basic_expr OP_NEX attr basic_expr  */
#line 3352 "verilog_parser.y"
                                          {
		(yyval.ast) = new AstNode(AST_NEX, (yyvsp[-3].ast), (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8098 "verilog_parser.tab.cc"
    break;

  case 674: /* basic_expr: basic_expr OP_GE attr basic_expr  */
#line 3357 "verilog_parser.y"
                                         {
		(yyval.ast) = new AstNode(AST_GE, (yyvsp[-3].ast), (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8108 "verilog_parser.tab.cc"
    break;

  case 675: /* basic_expr: basic_expr '>' attr basic_expr  */
#line 3362 "verilog_parser.y"
                                       {
		(yyval.ast) = new AstNode(AST_GT, (yyvsp[-3].ast), (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8118 "verilog_parser.tab.cc"
    break;

  case 676: /* basic_expr: basic_expr '+' attr basic_expr  */
#line 3367 "verilog_parser.y"
                                       {
		(yyval.ast) = new AstNode(AST_ADD, (yyvsp[-3].ast), (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8128 "verilog_parser.tab.cc"
    break;

  case 677: /* basic_expr: basic_expr '-' attr basic_expr  */
#line 3372 "verilog_parser.y"
                                       {
		(yyval.ast) = new AstNode(AST_SUB, (yyvsp[-3].ast), (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8138 "verilog_parser.tab.cc"
    break;

  case 678: /* basic_expr: basic_expr '*' attr basic_expr  */
#line 3377 "verilog_parser.y"
                                       {
		(yyval.ast) = new AstNode(AST_MUL, (yyvsp[-3].ast), (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8148 "verilog_parser.tab.cc"
    break;

  case 679: /* basic_expr: basic_expr '/' attr basic_expr  */
#line 3382 "verilog_parser.y"
                                       {
		(yyval.ast) = new AstNode(AST_DIV, (yyvsp[-3].ast), (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8158 "verilog_parser.tab.cc"
    break;

  case 680: /* basic_expr: basic_expr '%' attr basic_expr  */
#line 3387 "verilog_parser.y"
                                       {
		(yyval.ast) = new AstNode(AST_MOD, (yyvsp[-3].ast), (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8168 "verilog_parser.tab.cc"
    break;

  case 681: /* basic_expr: basic_expr OP_POW attr basic_expr  */
#line 3392 "verilog_parser.y"
                                          {
		(yyval.ast) = new AstNode(AST_POW, (yyvsp[-3].ast), (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8178 "verilog_parser.tab.cc"
    break;

  case 682: /* basic_expr: '+' attr basic_expr  */
#line 3397 "verilog_parser.y"
                                            {
		(yyval.ast) = new AstNode(AST_POS, (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8188 "verilog_parser.tab.cc"
    break;

  case 683: /* basic_expr: '-' attr basic_expr  */
#line 3402 "verilog_parser.y"
                                            {
		(yyval.ast) = new AstNode(AST_NEG, (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8198 "verilog_parser.tab.cc"
    break;

  case 684: /* basic_expr: basic_expr OP_LAND attr basic_expr  */
#line 3407 "verilog_parser.y"
                                           {
		(yyval.ast) = new AstNode(AST_LOGIC_AND, (yyvsp[-3].ast), (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8208 "verilog_parser.tab.cc"
    break;

  case 685: /* basic_expr: basic_expr OP_LOR attr basic_expr  */
#line 3412 "verilog_parser.y"
                                          {
		(yyval.ast) = new AstNode(AST_LOGIC_OR, (yyvsp[-3].ast), (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8218 "verilog_parser.tab.cc"
    break;

  case 686: /* basic_expr: '!' attr basic_expr  */
#line 3417 "verilog_parser.y"
                                            {
		(yyval.ast) = new AstNode(AST_LOGIC_NOT, (yyvsp[0].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
		append_attr((yyval.ast), (yyvsp[-1].al));
	}
#line 8228 "verilog_parser.tab.cc"
    break;

  case 687: /* basic_expr: TOK_SIGNED OP_CAST '(' expr ')'  */
#line 3422 "verilog_parser.y"
                                        {
		if (!sv_mode)
			frontend_verilog_yyerror("Static cast is only supported in SystemVerilog mode.");
		(yyval.ast) = new AstNode(AST_TO_SIGNED, (yyvsp[-1].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-4]), (yylsp[-1]));
	}
#line 8239 "verilog_parser.tab.cc"
    break;

  case 688: /* basic_expr: TOK_UNSIGNED OP_CAST '(' expr ')'  */
#line 3428 "verilog_parser.y"
                                          {
		if (!sv_mode)
			frontend_verilog_yyerror("Static cast is only supported in SystemVerilog mode.");
		(yyval.ast) = new AstNode(AST_TO_UNSIGNED, (yyvsp[-1].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-4]), (yylsp[-1]));
	}
#line 8250 "verilog_parser.tab.cc"
    break;

  case 689: /* basic_expr: basic_expr OP_CAST '(' expr ')'  */
#line 3434 "verilog_parser.y"
                                        {
		if (!sv_mode)
			frontend_verilog_yyerror("Static cast is only supported in SystemVerilog mode.");
		(yyval.ast) = new AstNode(AST_CAST_SIZE, (yyvsp[-4].ast), (yyvsp[-1].ast));
		SET_AST_NODE_LOC((yyval.ast), (yylsp[-4]), (yylsp[-1]));
	}
#line 8261 "verilog_parser.tab.cc"
    break;

  case 690: /* concat_list: expr  */
#line 3442 "verilog_parser.y"
             {
		(yyval.ast) = new AstNode(AST_CONCAT, (yyvsp[0].ast));
	}
#line 8269 "verilog_parser.tab.cc"
    break;

  case 691: /* concat_list: expr ',' concat_list  */
#line 3445 "verilog_parser.y"
                             {
		(yyval.ast) = (yyvsp[0].ast);
		(yyval.ast)->children.push_back((yyvsp[-2].ast));
	}
#line 8278 "verilog_parser.tab.cc"
    break;

  case 692: /* integral_number: TOK_CONSTVAL  */
#line 3451 "verilog_parser.y"
                     { (yyval.string) = (yyvsp[0].string); }
#line 8284 "verilog_parser.tab.cc"
    break;

  case 693: /* integral_number: TOK_UNBASED_UNSIZED_CONSTVAL  */
#line 3452 "verilog_parser.y"
                                     { (yyval.string) = (yyvsp[0].string); }
#line 8290 "verilog_parser.tab.cc"
    break;

  case 694: /* integral_number: TOK_BASE TOK_BASED_CONSTVAL  */
#line 3453 "verilog_parser.y"
                                    {
		(yyvsp[-1].string)->append(*(yyvsp[0].string));
		(yyval.string) = (yyvsp[-1].string);
		delete (yyvsp[0].string);
	}
#line 8300 "verilog_parser.tab.cc"
    break;

  case 695: /* integral_number: TOK_CONSTVAL TOK_BASE TOK_BASED_CONSTVAL  */
#line 3458 "verilog_parser.y"
                                                 {
		(yyvsp[-2].string)->append(*(yyvsp[-1].string)).append(*(yyvsp[0].string));
		(yyval.string) = (yyvsp[-2].string);
		delete (yyvsp[-1].string);
		delete (yyvsp[0].string);
	}
#line 8311 "verilog_parser.tab.cc"
    break;


#line 8315 "verilog_parser.tab.cc"

        default: break;
      }
    if (yychar_backup != yychar)
      YY_LAC_DISCARD ("yychar change");
  }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == FRONTEND_VERILOG_YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      {
        yypcontext_t yyctx
          = {yyssp, yyesa, &yyes, &yyes_capacity, yytoken, &yylloc};
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        if (yychar != FRONTEND_VERILOG_YYEMPTY)
          YY_LAC_ESTABLISH;
        yysyntax_error_status = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == -1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *,
                             YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (yymsg)
              {
                yysyntax_error_status
                  = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
                yymsgp = yymsg;
              }
            else
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = YYENOMEM;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == YYENOMEM)
          YYNOMEM;
      }
    }

  yyerror_range[1] = yylloc;
  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= FRONTEND_VERILOG_YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == FRONTEND_VERILOG_YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, &yylloc);
          yychar = FRONTEND_VERILOG_YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
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
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  /* If the stack popping above didn't lose the initial context for the
     current lookahead token, the shift below will for sure.  */
  YY_LAC_DISCARD ("error recovery");

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  ++yylsp;
  YYLLOC_DEFAULT (*yylsp, yyerror_range, 2);

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != FRONTEND_VERILOG_YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, yylsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  if (yyes != yyesa)
    YYSTACK_FREE (yyes);
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
  return yyresult;
}

