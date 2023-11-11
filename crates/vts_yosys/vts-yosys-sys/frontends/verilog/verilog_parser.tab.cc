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
#define YYSTYPE FRONTEND_VERILOG_YYSTYPE
#define YYLTYPE FRONTEND_VERILOG_YYLTYPE
/* Substitute the variable and function names.  */
#define yyparse frontend_verilog_yyparse
#define yylex frontend_verilog_yylex
#define yyerror frontend_verilog_yyerror
#define yydebug frontend_verilog_yydebug
#define yynerrs frontend_verilog_yynerrs

/* First part of user prologue.  */
#line 38 "verilog_parser.y"

#include "frontends/verilog/verilog_parser.tab.hh"
#include "frontends/verilog/verilog_frontend.h"
// #include "kernel/log.h"
#include <list>
#include <stack>
#include <string.h>

#define YYLEX_PARAM &yylval, &yylloc

USING_YOSYS_NAMESPACE
using namespace AST;
using namespace VERILOG_FRONTEND;

YOSYS_NAMESPACE_BEGIN
namespace VERILOG_FRONTEND {
int port_counter;
dict<std::string, int> port_stubs;
dict<IdString, AstNode *> *attr_list, default_attr_list;
std::stack<dict<IdString, AstNode *> *> attr_list_stack;
dict<IdString, AstNode *> *albuf;
std::vector<UserTypeMap> user_type_stack;
dict<std::string, AstNode *> pkg_user_types;
std::vector<AstNode *> ast_stack;
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
} // namespace VERILOG_FRONTEND
YOSYS_NAMESPACE_END

#define SET_AST_NODE_LOC(WHICH, BEGIN, END)                                    \
  do {                                                                         \
    (WHICH)->location.first_line = (BEGIN).first_line;                         \
    (WHICH)->location.first_column = (BEGIN).first_column;                     \
    (WHICH)->location.last_line = (END).last_line;                             \
    (WHICH)->location.last_column = (END).last_column;                         \
  } while (0)

#define SET_RULE_LOC(LHS, BEGIN, END)                                          \
  do {                                                                         \
    (LHS).first_line = (BEGIN).first_line;                                     \
    (LHS).first_column = (BEGIN).first_column;                                 \
    (LHS).last_line = (END).last_line;                                         \
    (LHS).last_column = (END).last_column;                                     \
  } while (0)

int frontend_verilog_yylex(YYSTYPE *yylval_param, YYLTYPE *yyloc_param);

static void append_attr(AstNode *ast, dict<IdString, AstNode *> *al) {
  for (auto &it : *al) {
    if (ast->attributes.count(it.first) > 0)
      delete ast->attributes[it.first];
    ast->attributes[it.first] = it.second;
  }
  delete al;
}

static void append_attr_clone(AstNode *ast, dict<IdString, AstNode *> *al) {
  for (auto &it : *al) {
    if (ast->attributes.count(it.first) > 0)
      delete ast->attributes[it.first];
    ast->attributes[it.first] = it.second->clone();
  }
}

static void free_attr(dict<IdString, AstNode *> *al) {
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

static void addWiretypeNode(std::string *name, AstNode *node) {
  log_assert(node);
  node->is_custom_type = true;
  node->children.push_back(new AstNode(AST_WIRETYPE));
  node->children.back()->str = *name;
  delete name;
}

static void addTypedefNode(std::string *name, AstNode *node) {
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

static void enterTypeScope() { user_type_stack.push_back(UserTypeMap()); }

static void exitTypeScope() { user_type_stack.pop_back(); }

static bool isInLocalScope(const std::string *name) {
  // tests if a name was declared in the current block scope
  auto &user_types = user_type_stack.back();
  return (user_types.count(*name) > 0);
}

static AstNode *makeRange(int msb = 31, int lsb = 0, bool isSigned = true) {
  auto range = new AstNode(AST_RANGE);
  range->children.push_back(AstNode::mkconst_int(msb, true));
  range->children.push_back(AstNode::mkconst_int(lsb, true));
  range->is_signed = isSigned;
  return range;
}

static void addRange(AstNode *parent, int msb = 31, int lsb = 0,
                     bool isSigned = true) {
  auto range = makeRange(msb, lsb, isSigned);
  parent->children.push_back(range);
}

static AstNode *checkRange(AstNode *type_node, AstNode *range_node) {
  if (type_node->range_left >= 0 && type_node->range_right >= 0) {
    // type already restricts the range
    if (range_node) {
      frontend_verilog_yyerror(
          "integer/genvar types cannot have packed dimensions.");
    } else {
      range_node =
          makeRange(type_node->range_left, type_node->range_right, false);
    }
  }
  if (range_node && range_node->children.size() != 2) {
    frontend_verilog_yyerror(
        "wire/reg/logic packed dimension must be of the form: [<expr>:<expr>], "
        "[<expr>+:<expr>], or [<expr>-:<expr>]");
  }
  return range_node;
}

static void rewriteRange(AstNode *rangeNode) {
  if (rangeNode->type == AST_RANGE && rangeNode->children.size() == 1) {
    // SV array size [n], rewrite as [0:n-1]
    rangeNode->children.push_back(new AstNode(AST_SUB, rangeNode->children[0],
                                              AstNode::mkconst_int(1, true)));
    rangeNode->children[0] = AstNode::mkconst_int(0, false);
  }
}

static void rewriteAsMemoryNode(AstNode *node, AstNode *rangeNode) {
  node->type = AST_MEMORY;
  if (rangeNode->type == AST_MULTIRANGE) {
    for (auto *itr : rangeNode->children)
      rewriteRange(itr);
  } else
    rewriteRange(rangeNode);
  node->children.push_back(rangeNode);
}

static void checkLabelsMatch(const char *element, const std::string *before,
                             const std::string *after) {
  if (!before && after)
    frontend_verilog_yyerror("%s missing where end label (%s) was given.",
                             element, after->c_str() + 1);
  if (before && after && *before != *after)
    frontend_verilog_yyerror("%s (%s) and end label (%s) don't match.", element,
                             before->c_str() + 1, after->c_str() + 1);
}

// This transforms a loop like
//   for (genvar i = 0; i < 10; i++) begin : blk
// to
//   genvar _i;
//   for (_i = 0; _i < 10; _i++) begin : blk
//     localparam i = _i;
// where `_i` is actually some auto-generated name.
static void rewriteGenForDeclInit(AstNode *loop) {
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
  std::string new_str =
      stringf("$genfordecl$%d$%s", autoidx++, old_str.c_str());

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
  std::function<void(AstNode *)> substitute = [&](AstNode *node) {
    if (node->type == AST_IDENTIFIER && node->str == old_str)
      node->str = new_str;
    for (AstNode *child : node->children)
      substitute(child);
  };
  substitute(init);
  substitute(cond);
  substitute(incr);
}

static void ensureAsgnExprAllowed() {
  if (!sv_mode)
    frontend_verilog_yyerror("Assignments within expressions are only "
                             "supported in SystemVerilog mode.");
  if (ast_stack.back()->type != AST_BLOCK)
    frontend_verilog_yyerror(
        "Assignments within expressions are only permitted within procedures.");
}

// add a pre/post-increment/decrement statement
static const AstNode *addIncOrDecStmt(dict<IdString, AstNode *> *stmt_attr,
                                      AstNode *lhs,
                                      dict<IdString, AstNode *> *op_attr,
                                      AST::AstNodeType op, YYLTYPE begin,
                                      YYLTYPE end) {
  AstNode *one = AstNode::mkconst_int(1, true);
  AstNode *rhs = new AstNode(op, lhs->clone(), one);
  if (op_attr != nullptr)
    append_attr(rhs, op_attr);
  AstNode *stmt = new AstNode(AST_ASSIGN_EQ, lhs, rhs);
  SET_AST_NODE_LOC(stmt, begin, end);
  if (stmt_attr != nullptr)
    append_attr(stmt, stmt_attr);
  ast_stack.back()->children.push_back(stmt);
  return stmt;
}

// create a pre/post-increment/decrement expression, and add the corresponding
// statement
static AstNode *addIncOrDecExpr(AstNode *lhs, dict<IdString, AstNode *> *attr,
                                AST::AstNodeType op, YYLTYPE begin, YYLTYPE end,
                                bool undo) {
  ensureAsgnExprAllowed();
  const AstNode *stmt = addIncOrDecStmt(nullptr, lhs, attr, op, begin, end);
  log_assert(stmt->type == AST_ASSIGN_EQ);
  AstNode *expr = stmt->children[0]->clone();
  if (undo) {
    AstNode *minus_one = AstNode::mkconst_int(-1, true, 1);
    expr = new AstNode(op, expr, minus_one);
  }
  SET_AST_NODE_LOC(expr, begin, end);
  return expr;
}

// add a binary operator assignment statement, e.g., a += b
static const AstNode *addAsgnBinopStmt(dict<IdString, AstNode *> *attr,
                                       AstNode *lhs, AST::AstNodeType op,
                                       AstNode *rhs, YYLTYPE begin,
                                       YYLTYPE end) {
  SET_AST_NODE_LOC(rhs, end, end);
  if (op == AST_SHIFT_LEFT || op == AST_SHIFT_RIGHT || op == AST_SHIFT_SLEFT ||
      op == AST_SHIFT_SRIGHT) {
    rhs = new AstNode(AST_TO_UNSIGNED, rhs);
    SET_AST_NODE_LOC(rhs, end, end);
  }
  rhs = new AstNode(op, lhs->clone(), rhs);
  AstNode *stmt = new AstNode(AST_ASSIGN_EQ, lhs, rhs);
  SET_AST_NODE_LOC(rhs, begin, end);
  SET_AST_NODE_LOC(stmt, begin, end);
  ast_stack.back()->children.push_back(stmt);
  if (attr != nullptr)
    append_attr(stmt, attr);
  return lhs;
}

#line 396 "verilog_parser.tab.cc"

#ifndef YY_CAST
#ifdef __cplusplus
#define YY_CAST(Type, Val) static_cast<Type>(Val)
#define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type>(Val)
#else
#define YY_CAST(Type, Val) ((Type)(Val))
#define YY_REINTERPRET_CAST(Type, Val) ((Type)(Val))
#endif
#endif
#ifndef YY_NULLPTR
#if defined __cplusplus
#if 201103L <= __cplusplus
#define YY_NULLPTR nullptr
#else
#define YY_NULLPTR 0
#endif
#else
#define YY_NULLPTR ((void *)0)
#endif
#endif

#include "verilog_parser.tab.hh"
/* Symbol kind.  */
enum yysymbol_kind_t {
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                         /* "end of file"  */
  YYSYMBOL_YYerror = 1,                       /* error  */
  YYSYMBOL_YYUNDEF = 2,                       /* "invalid token"  */
  YYSYMBOL_TOK_STRING = 3,                    /* TOK_STRING  */
  YYSYMBOL_TOK_ID = 4,                        /* TOK_ID  */
  YYSYMBOL_TOK_CONSTVAL = 5,                  /* TOK_CONSTVAL  */
  YYSYMBOL_TOK_REALVAL = 6,                   /* TOK_REALVAL  */
  YYSYMBOL_TOK_PRIMITIVE = 7,                 /* TOK_PRIMITIVE  */
  YYSYMBOL_TOK_SVA_LABEL = 8,                 /* TOK_SVA_LABEL  */
  YYSYMBOL_TOK_SPECIFY_OPER = 9,              /* TOK_SPECIFY_OPER  */
  YYSYMBOL_TOK_MSG_TASKS = 10,                /* TOK_MSG_TASKS  */
  YYSYMBOL_TOK_BASE = 11,                     /* TOK_BASE  */
  YYSYMBOL_TOK_BASED_CONSTVAL = 12,           /* TOK_BASED_CONSTVAL  */
  YYSYMBOL_TOK_UNBASED_UNSIZED_CONSTVAL = 13, /* TOK_UNBASED_UNSIZED_CONSTVAL */
  YYSYMBOL_TOK_USER_TYPE = 14,                /* TOK_USER_TYPE  */
  YYSYMBOL_TOK_PKG_USER_TYPE = 15,            /* TOK_PKG_USER_TYPE  */
  YYSYMBOL_TOK_ASSERT = 16,                   /* TOK_ASSERT  */
  YYSYMBOL_TOK_ASSUME = 17,                   /* TOK_ASSUME  */
  YYSYMBOL_TOK_RESTRICT = 18,                 /* TOK_RESTRICT  */
  YYSYMBOL_TOK_COVER = 19,                    /* TOK_COVER  */
  YYSYMBOL_TOK_FINAL = 20,                    /* TOK_FINAL  */
  YYSYMBOL_ATTR_BEGIN = 21,                   /* ATTR_BEGIN  */
  YYSYMBOL_ATTR_END = 22,                     /* ATTR_END  */
  YYSYMBOL_DEFATTR_BEGIN = 23,                /* DEFATTR_BEGIN  */
  YYSYMBOL_DEFATTR_END = 24,                  /* DEFATTR_END  */
  YYSYMBOL_TOK_MODULE = 25,                   /* TOK_MODULE  */
  YYSYMBOL_TOK_ENDMODULE = 26,                /* TOK_ENDMODULE  */
  YYSYMBOL_TOK_PARAMETER = 27,                /* TOK_PARAMETER  */
  YYSYMBOL_TOK_LOCALPARAM = 28,               /* TOK_LOCALPARAM  */
  YYSYMBOL_TOK_DEFPARAM = 29,                 /* TOK_DEFPARAM  */
  YYSYMBOL_TOK_PACKAGE = 30,                  /* TOK_PACKAGE  */
  YYSYMBOL_TOK_ENDPACKAGE = 31,               /* TOK_ENDPACKAGE  */
  YYSYMBOL_TOK_PACKAGESEP = 32,               /* TOK_PACKAGESEP  */
  YYSYMBOL_TOK_INTERFACE = 33,                /* TOK_INTERFACE  */
  YYSYMBOL_TOK_ENDINTERFACE = 34,             /* TOK_ENDINTERFACE  */
  YYSYMBOL_TOK_MODPORT = 35,                  /* TOK_MODPORT  */
  YYSYMBOL_TOK_VAR = 36,                      /* TOK_VAR  */
  YYSYMBOL_TOK_WILDCARD_CONNECT = 37,         /* TOK_WILDCARD_CONNECT  */
  YYSYMBOL_TOK_INPUT = 38,                    /* TOK_INPUT  */
  YYSYMBOL_TOK_OUTPUT = 39,                   /* TOK_OUTPUT  */
  YYSYMBOL_TOK_INOUT = 40,                    /* TOK_INOUT  */
  YYSYMBOL_TOK_WIRE = 41,                     /* TOK_WIRE  */
  YYSYMBOL_TOK_WAND = 42,                     /* TOK_WAND  */
  YYSYMBOL_TOK_WOR = 43,                      /* TOK_WOR  */
  YYSYMBOL_TOK_REG = 44,                      /* TOK_REG  */
  YYSYMBOL_TOK_LOGIC = 45,                    /* TOK_LOGIC  */
  YYSYMBOL_TOK_INTEGER = 46,                  /* TOK_INTEGER  */
  YYSYMBOL_TOK_SIGNED = 47,                   /* TOK_SIGNED  */
  YYSYMBOL_TOK_ASSIGN = 48,                   /* TOK_ASSIGN  */
  YYSYMBOL_TOK_ALWAYS = 49,                   /* TOK_ALWAYS  */
  YYSYMBOL_TOK_INITIAL = 50,                  /* TOK_INITIAL  */
  YYSYMBOL_TOK_ALWAYS_FF = 51,                /* TOK_ALWAYS_FF  */
  YYSYMBOL_TOK_ALWAYS_COMB = 52,              /* TOK_ALWAYS_COMB  */
  YYSYMBOL_TOK_ALWAYS_LATCH = 53,             /* TOK_ALWAYS_LATCH  */
  YYSYMBOL_TOK_BEGIN = 54,                    /* TOK_BEGIN  */
  YYSYMBOL_TOK_END = 55,                      /* TOK_END  */
  YYSYMBOL_TOK_IF = 56,                       /* TOK_IF  */
  YYSYMBOL_TOK_ELSE = 57,                     /* TOK_ELSE  */
  YYSYMBOL_TOK_FOR = 58,                      /* TOK_FOR  */
  YYSYMBOL_TOK_WHILE = 59,                    /* TOK_WHILE  */
  YYSYMBOL_TOK_REPEAT = 60,                   /* TOK_REPEAT  */
  YYSYMBOL_TOK_DPI_FUNCTION = 61,             /* TOK_DPI_FUNCTION  */
  YYSYMBOL_TOK_POSEDGE = 62,                  /* TOK_POSEDGE  */
  YYSYMBOL_TOK_NEGEDGE = 63,                  /* TOK_NEGEDGE  */
  YYSYMBOL_TOK_OR = 64,                       /* TOK_OR  */
  YYSYMBOL_TOK_AUTOMATIC = 65,                /* TOK_AUTOMATIC  */
  YYSYMBOL_TOK_CASE = 66,                     /* TOK_CASE  */
  YYSYMBOL_TOK_CASEX = 67,                    /* TOK_CASEX  */
  YYSYMBOL_TOK_CASEZ = 68,                    /* TOK_CASEZ  */
  YYSYMBOL_TOK_ENDCASE = 69,                  /* TOK_ENDCASE  */
  YYSYMBOL_TOK_DEFAULT = 70,                  /* TOK_DEFAULT  */
  YYSYMBOL_TOK_FUNCTION = 71,                 /* TOK_FUNCTION  */
  YYSYMBOL_TOK_ENDFUNCTION = 72,              /* TOK_ENDFUNCTION  */
  YYSYMBOL_TOK_TASK = 73,                     /* TOK_TASK  */
  YYSYMBOL_TOK_ENDTASK = 74,                  /* TOK_ENDTASK  */
  YYSYMBOL_TOK_SPECIFY = 75,                  /* TOK_SPECIFY  */
  YYSYMBOL_TOK_IGNORED_SPECIFY = 76,          /* TOK_IGNORED_SPECIFY  */
  YYSYMBOL_TOK_ENDSPECIFY = 77,               /* TOK_ENDSPECIFY  */
  YYSYMBOL_TOK_SPECPARAM = 78,                /* TOK_SPECPARAM  */
  YYSYMBOL_TOK_SPECIFY_AND = 79,              /* TOK_SPECIFY_AND  */
  YYSYMBOL_TOK_IGNORED_SPECIFY_AND = 80,      /* TOK_IGNORED_SPECIFY_AND  */
  YYSYMBOL_TOK_GENERATE = 81,                 /* TOK_GENERATE  */
  YYSYMBOL_TOK_ENDGENERATE = 82,              /* TOK_ENDGENERATE  */
  YYSYMBOL_TOK_GENVAR = 83,                   /* TOK_GENVAR  */
  YYSYMBOL_TOK_REAL = 84,                     /* TOK_REAL  */
  YYSYMBOL_TOK_SYNOPSYS_FULL_CASE = 85,       /* TOK_SYNOPSYS_FULL_CASE  */
  YYSYMBOL_TOK_SYNOPSYS_PARALLEL_CASE = 86,   /* TOK_SYNOPSYS_PARALLEL_CASE  */
  YYSYMBOL_TOK_SUPPLY0 = 87,                  /* TOK_SUPPLY0  */
  YYSYMBOL_TOK_SUPPLY1 = 88,                  /* TOK_SUPPLY1  */
  YYSYMBOL_TOK_TO_SIGNED = 89,                /* TOK_TO_SIGNED  */
  YYSYMBOL_TOK_TO_UNSIGNED = 90,              /* TOK_TO_UNSIGNED  */
  YYSYMBOL_TOK_POS_INDEXED = 91,              /* TOK_POS_INDEXED  */
  YYSYMBOL_TOK_NEG_INDEXED = 92,              /* TOK_NEG_INDEXED  */
  YYSYMBOL_TOK_PROPERTY = 93,                 /* TOK_PROPERTY  */
  YYSYMBOL_TOK_ENUM = 94,                     /* TOK_ENUM  */
  YYSYMBOL_TOK_TYPEDEF = 95,                  /* TOK_TYPEDEF  */
  YYSYMBOL_TOK_RAND = 96,                     /* TOK_RAND  */
  YYSYMBOL_TOK_CONST = 97,                    /* TOK_CONST  */
  YYSYMBOL_TOK_CHECKER = 98,                  /* TOK_CHECKER  */
  YYSYMBOL_TOK_ENDCHECKER = 99,               /* TOK_ENDCHECKER  */
  YYSYMBOL_TOK_EVENTUALLY = 100,              /* TOK_EVENTUALLY  */
  YYSYMBOL_TOK_INCREMENT = 101,               /* TOK_INCREMENT  */
  YYSYMBOL_TOK_DECREMENT = 102,               /* TOK_DECREMENT  */
  YYSYMBOL_TOK_UNIQUE = 103,                  /* TOK_UNIQUE  */
  YYSYMBOL_TOK_UNIQUE0 = 104,                 /* TOK_UNIQUE0  */
  YYSYMBOL_TOK_PRIORITY = 105,                /* TOK_PRIORITY  */
  YYSYMBOL_TOK_STRUCT = 106,                  /* TOK_STRUCT  */
  YYSYMBOL_TOK_PACKED = 107,                  /* TOK_PACKED  */
  YYSYMBOL_TOK_UNSIGNED = 108,                /* TOK_UNSIGNED  */
  YYSYMBOL_TOK_INT = 109,                     /* TOK_INT  */
  YYSYMBOL_TOK_BYTE = 110,                    /* TOK_BYTE  */
  YYSYMBOL_TOK_SHORTINT = 111,                /* TOK_SHORTINT  */
  YYSYMBOL_TOK_LONGINT = 112,                 /* TOK_LONGINT  */
  YYSYMBOL_TOK_VOID = 113,                    /* TOK_VOID  */
  YYSYMBOL_TOK_UNION = 114,                   /* TOK_UNION  */
  YYSYMBOL_TOK_BIT_OR_ASSIGN = 115,           /* TOK_BIT_OR_ASSIGN  */
  YYSYMBOL_TOK_BIT_AND_ASSIGN = 116,          /* TOK_BIT_AND_ASSIGN  */
  YYSYMBOL_TOK_BIT_XOR_ASSIGN = 117,          /* TOK_BIT_XOR_ASSIGN  */
  YYSYMBOL_TOK_ADD_ASSIGN = 118,              /* TOK_ADD_ASSIGN  */
  YYSYMBOL_TOK_SUB_ASSIGN = 119,              /* TOK_SUB_ASSIGN  */
  YYSYMBOL_TOK_DIV_ASSIGN = 120,              /* TOK_DIV_ASSIGN  */
  YYSYMBOL_TOK_MOD_ASSIGN = 121,              /* TOK_MOD_ASSIGN  */
  YYSYMBOL_TOK_MUL_ASSIGN = 122,              /* TOK_MUL_ASSIGN  */
  YYSYMBOL_TOK_SHL_ASSIGN = 123,              /* TOK_SHL_ASSIGN  */
  YYSYMBOL_TOK_SHR_ASSIGN = 124,              /* TOK_SHR_ASSIGN  */
  YYSYMBOL_TOK_SSHL_ASSIGN = 125,             /* TOK_SSHL_ASSIGN  */
  YYSYMBOL_TOK_SSHR_ASSIGN = 126,             /* TOK_SSHR_ASSIGN  */
  YYSYMBOL_TOK_BIND = 127,                    /* TOK_BIND  */
  YYSYMBOL_TOK_TIME_SCALE = 128,              /* TOK_TIME_SCALE  */
  YYSYMBOL_OP_LOR = 129,                      /* OP_LOR  */
  YYSYMBOL_OP_LAND = 130,                     /* OP_LAND  */
  YYSYMBOL_131_ = 131,                        /* '|'  */
  YYSYMBOL_OP_NOR = 132,                      /* OP_NOR  */
  YYSYMBOL_133_ = 133,                        /* '^'  */
  YYSYMBOL_OP_XNOR = 134,                     /* OP_XNOR  */
  YYSYMBOL_135_ = 135,                        /* '&'  */
  YYSYMBOL_OP_NAND = 136,                     /* OP_NAND  */
  YYSYMBOL_OP_EQ = 137,                       /* OP_EQ  */
  YYSYMBOL_OP_NE = 138,                       /* OP_NE  */
  YYSYMBOL_OP_EQX = 139,                      /* OP_EQX  */
  YYSYMBOL_OP_NEX = 140,                      /* OP_NEX  */
  YYSYMBOL_141_ = 141,                        /* '<'  */
  YYSYMBOL_OP_LE = 142,                       /* OP_LE  */
  YYSYMBOL_OP_GE = 143,                       /* OP_GE  */
  YYSYMBOL_144_ = 144,                        /* '>'  */
  YYSYMBOL_OP_SHL = 145,                      /* OP_SHL  */
  YYSYMBOL_OP_SHR = 146,                      /* OP_SHR  */
  YYSYMBOL_OP_SSHL = 147,                     /* OP_SSHL  */
  YYSYMBOL_OP_SSHR = 148,                     /* OP_SSHR  */
  YYSYMBOL_149_ = 149,                        /* '+'  */
  YYSYMBOL_150_ = 150,                        /* '-'  */
  YYSYMBOL_151_ = 151,                        /* '*'  */
  YYSYMBOL_152_ = 152,                        /* '/'  */
  YYSYMBOL_153_ = 153,                        /* '%'  */
  YYSYMBOL_OP_POW = 154,                      /* OP_POW  */
  YYSYMBOL_OP_CAST = 155,                     /* OP_CAST  */
  YYSYMBOL_UNARY_OPS = 156,                   /* UNARY_OPS  */
  YYSYMBOL_FAKE_THEN = 157,                   /* FAKE_THEN  */
  YYSYMBOL_158_ = 158,                        /* ','  */
  YYSYMBOL_159_ = 159,                        /* '='  */
  YYSYMBOL_160_ = 160,                        /* '.'  */
  YYSYMBOL_161_ = 161,                        /* '('  */
  YYSYMBOL_162_ = 162,                        /* ')'  */
  YYSYMBOL_163_ = 163,                        /* ';'  */
  YYSYMBOL_164_ = 164,                        /* '#'  */
  YYSYMBOL_165_ = 165,                        /* ':'  */
  YYSYMBOL_166_ = 166,                        /* '['  */
  YYSYMBOL_167_ = 167,                        /* ']'  */
  YYSYMBOL_168_ = 168,                        /* '{'  */
  YYSYMBOL_169_ = 169,                        /* '}'  */
  YYSYMBOL_170_ = 170,                        /* '@'  */
  YYSYMBOL_171_ = 171,                        /* '?'  */
  YYSYMBOL_172_ = 172,                        /* '~'  */
  YYSYMBOL_173_ = 173,                        /* '!'  */
  YYSYMBOL_YYACCEPT = 174,                    /* $accept  */
  YYSYMBOL_input = 175,                       /* input  */
  YYSYMBOL_176_1 = 176,                       /* $@1  */
  YYSYMBOL_design = 177,                      /* design  */
  YYSYMBOL_attr = 178,                        /* attr  */
  YYSYMBOL_179_2 = 179,                       /* $@2  */
  YYSYMBOL_attr_opt = 180,                    /* attr_opt  */
  YYSYMBOL_defattr = 181,                     /* defattr  */
  YYSYMBOL_182_3 = 182,                       /* $@3  */
  YYSYMBOL_183_4 = 183,                       /* $@4  */
  YYSYMBOL_opt_attr_list = 184,               /* opt_attr_list  */
  YYSYMBOL_attr_list = 185,                   /* attr_list  */
  YYSYMBOL_attr_assign = 186,                 /* attr_assign  */
  YYSYMBOL_hierarchical_id = 187,             /* hierarchical_id  */
  YYSYMBOL_hierarchical_type_id = 188,        /* hierarchical_type_id  */
  YYSYMBOL_module = 189,                      /* module  */
  YYSYMBOL_190_5 = 190,                       /* $@5  */
  YYSYMBOL_191_6 = 191,                       /* $@6  */
  YYSYMBOL_module_para_opt = 192,             /* module_para_opt  */
  YYSYMBOL_193_7 = 193,                       /* $@7  */
  YYSYMBOL_194_8 = 194,                       /* $@8  */
  YYSYMBOL_module_para_list = 195,            /* module_para_list  */
  YYSYMBOL_single_module_para = 196,          /* single_module_para  */
  YYSYMBOL_197_9 = 197,                       /* $@9  */
  YYSYMBOL_198_10 = 198,                      /* $@10  */
  YYSYMBOL_module_args_opt = 199,             /* module_args_opt  */
  YYSYMBOL_module_args = 200,                 /* module_args  */
  YYSYMBOL_optional_comma = 201,              /* optional_comma  */
  YYSYMBOL_module_arg_opt_assignment = 202,   /* module_arg_opt_assignment  */
  YYSYMBOL_module_arg = 203,                  /* module_arg  */
  YYSYMBOL_204_11 = 204,                      /* $@11  */
  YYSYMBOL_205_12 = 205,                      /* $@12  */
  YYSYMBOL_206_13 = 206,                      /* $@13  */
  YYSYMBOL_207_14 = 207,                      /* $@14  */
  YYSYMBOL_package = 208,                     /* package  */
  YYSYMBOL_209_15 = 209,                      /* $@15  */
  YYSYMBOL_210_16 = 210,                      /* $@16  */
  YYSYMBOL_package_body = 211,                /* package_body  */
  YYSYMBOL_package_body_stmt = 212,           /* package_body_stmt  */
  YYSYMBOL_interface = 213,                   /* interface  */
  YYSYMBOL_214_17 = 214,                      /* $@17  */
  YYSYMBOL_215_18 = 215,                      /* $@18  */
  YYSYMBOL_interface_body = 216,              /* interface_body  */
  YYSYMBOL_interface_body_stmt = 217,         /* interface_body_stmt  */
  YYSYMBOL_bind_directive = 218,              /* bind_directive  */
  YYSYMBOL_219_19 = 219,                      /* $@19  */
  YYSYMBOL_220_20 = 220,                      /* $@20  */
  YYSYMBOL_221_21 = 221,                      /* $@21  */
  YYSYMBOL_bind_target = 222,                 /* bind_target  */
  YYSYMBOL_opt_bind_target_instance_list =
      223,                                  /* opt_bind_target_instance_list  */
  YYSYMBOL_bind_target_instance_list = 224, /* bind_target_instance_list  */
  YYSYMBOL_bind_target_instance = 225,      /* bind_target_instance  */
  YYSYMBOL_mintypmax_expr = 226,            /* mintypmax_expr  */
  YYSYMBOL_non_opt_delay = 227,             /* non_opt_delay  */
  YYSYMBOL_delay = 228,                     /* delay  */
  YYSYMBOL_io_wire_type = 229,              /* io_wire_type  */
  YYSYMBOL_230_22 = 230,                    /* $@22  */
  YYSYMBOL_non_io_wire_type = 231,          /* non_io_wire_type  */
  YYSYMBOL_232_23 = 232,                    /* $@23  */
  YYSYMBOL_wire_type = 233,                 /* wire_type  */
  YYSYMBOL_wire_type_token_io = 234,        /* wire_type_token_io  */
  YYSYMBOL_wire_type_signedness = 235,      /* wire_type_signedness  */
  YYSYMBOL_wire_type_const_rand = 236,      /* wire_type_const_rand  */
  YYSYMBOL_opt_wire_type_token = 237,       /* opt_wire_type_token  */
  YYSYMBOL_wire_type_token = 238,           /* wire_type_token  */
  YYSYMBOL_net_type = 239,                  /* net_type  */
  YYSYMBOL_logic_type = 240,                /* logic_type  */
  YYSYMBOL_integer_atom_type = 241,         /* integer_atom_type  */
  YYSYMBOL_integer_vector_type = 242,       /* integer_vector_type  */
  YYSYMBOL_non_opt_range = 243,             /* non_opt_range  */
  YYSYMBOL_non_opt_multirange = 244,        /* non_opt_multirange  */
  YYSYMBOL_range = 245,                     /* range  */
  YYSYMBOL_range_or_multirange = 246,       /* range_or_multirange  */
  YYSYMBOL_module_body = 247,               /* module_body  */
  YYSYMBOL_module_body_stmt = 248,          /* module_body_stmt  */
  YYSYMBOL_checker_decl = 249,              /* checker_decl  */
  YYSYMBOL_250_24 = 250,                    /* $@24  */
  YYSYMBOL_task_func_decl = 251,            /* task_func_decl  */
  YYSYMBOL_252_25 = 252,                    /* $@25  */
  YYSYMBOL_253_26 = 253,                    /* $@26  */
  YYSYMBOL_254_27 = 254,                    /* $@27  */
  YYSYMBOL_255_28 = 255,                    /* $@28  */
  YYSYMBOL_256_29 = 256,                    /* $@29  */
  YYSYMBOL_257_30 = 257,                    /* $@30  */
  YYSYMBOL_func_return_type = 258,          /* func_return_type  */
  YYSYMBOL_opt_type_vec = 259,              /* opt_type_vec  */
  YYSYMBOL_opt_signedness_default_signed =
      260, /* opt_signedness_default_signed  */
  YYSYMBOL_opt_signedness_default_unsigned =
      261,                              /* opt_signedness_default_unsigned  */
  YYSYMBOL_dpi_function_arg = 262,      /* dpi_function_arg  */
  YYSYMBOL_opt_dpi_function_args = 263, /* opt_dpi_function_args  */
  YYSYMBOL_dpi_function_args = 264,     /* dpi_function_args  */
  YYSYMBOL_opt_automatic = 265,         /* opt_automatic  */
  YYSYMBOL_task_func_args_opt = 266,    /* task_func_args_opt  */
  YYSYMBOL_267_31 = 267,                /* $@31  */
  YYSYMBOL_268_32 = 268,                /* $@32  */
  YYSYMBOL_task_func_args = 269,        /* task_func_args  */
  YYSYMBOL_task_func_port = 270,        /* task_func_port  */
  YYSYMBOL_271_33 = 271,                /* $@33  */
  YYSYMBOL_272_34 = 272,                /* $@34  */
  YYSYMBOL_task_func_body = 273,        /* task_func_body  */
  YYSYMBOL_specify_block = 274,         /* specify_block  */
  YYSYMBOL_specify_item_list = 275,     /* specify_item_list  */
  YYSYMBOL_specify_item = 276,          /* specify_item  */
  YYSYMBOL_specify_opt_triple = 277,    /* specify_opt_triple  */
  YYSYMBOL_specify_if = 278,            /* specify_if  */
  YYSYMBOL_specify_condition = 279,     /* specify_condition  */
  YYSYMBOL_specify_target = 280,        /* specify_target  */
  YYSYMBOL_specify_edge = 281,          /* specify_edge  */
  YYSYMBOL_specify_rise_fall = 282,     /* specify_rise_fall  */
  YYSYMBOL_specify_triple = 283,        /* specify_triple  */
  YYSYMBOL_ignored_specify_block = 284, /* ignored_specify_block  */
  YYSYMBOL_ignored_specify_item_opt = 285, /* ignored_specify_item_opt  */
  YYSYMBOL_ignored_specify_item = 286,     /* ignored_specify_item  */
  YYSYMBOL_specparam_declaration = 287,    /* specparam_declaration  */
  YYSYMBOL_specparam_range = 288,          /* specparam_range  */
  YYSYMBOL_list_of_specparam_assignments =
      289,                                /* list_of_specparam_assignments  */
  YYSYMBOL_specparam_assignment = 290,    /* specparam_assignment  */
  YYSYMBOL_ignspec_opt_cond = 291,        /* ignspec_opt_cond  */
  YYSYMBOL_path_declaration = 292,        /* path_declaration  */
  YYSYMBOL_simple_path_declaration = 293, /* simple_path_declaration  */
  YYSYMBOL_path_delay_value = 294,        /* path_delay_value  */
  YYSYMBOL_list_of_path_delay_extra_expressions =
      295, /* list_of_path_delay_extra_expressions  */
  YYSYMBOL_specify_edge_identifier = 296,   /* specify_edge_identifier  */
  YYSYMBOL_parallel_path_description = 297, /* parallel_path_description  */
  YYSYMBOL_full_path_description = 298,     /* full_path_description  */
  YYSYMBOL_list_of_path_inputs = 299,       /* list_of_path_inputs  */
  YYSYMBOL_more_path_inputs = 300,          /* more_path_inputs  */
  YYSYMBOL_list_of_path_outputs = 301,      /* list_of_path_outputs  */
  YYSYMBOL_opt_polarity_operator = 302,     /* opt_polarity_operator  */
  YYSYMBOL_specify_input_terminal_descriptor =
      303, /* specify_input_terminal_descriptor  */
  YYSYMBOL_specify_output_terminal_descriptor =
      304, /* specify_output_terminal_descriptor  */
  YYSYMBOL_system_timing_declaration = 305,   /* system_timing_declaration  */
  YYSYMBOL_system_timing_arg = 306,           /* system_timing_arg  */
  YYSYMBOL_system_timing_args = 307,          /* system_timing_args  */
  YYSYMBOL_ignspec_constant_expression = 308, /* ignspec_constant_expression  */
  YYSYMBOL_ignspec_expr = 309,                /* ignspec_expr  */
  YYSYMBOL_ignspec_id = 310,                  /* ignspec_id  */
  YYSYMBOL_311_35 = 311,                      /* $@35  */
  YYSYMBOL_param_signed = 312,                /* param_signed  */
  YYSYMBOL_param_integer = 313,               /* param_integer  */
  YYSYMBOL_param_real = 314,                  /* param_real  */
  YYSYMBOL_param_range = 315,                 /* param_range  */
  YYSYMBOL_param_integer_type = 316,          /* param_integer_type  */
  YYSYMBOL_param_range_type = 317,            /* param_range_type  */
  YYSYMBOL_param_implicit_type = 318,         /* param_implicit_type  */
  YYSYMBOL_param_type = 319,                  /* param_type  */
  YYSYMBOL_param_decl = 320,                  /* param_decl  */
  YYSYMBOL_321_36 = 321,                      /* $@36  */
  YYSYMBOL_localparam_decl = 322,             /* localparam_decl  */
  YYSYMBOL_323_37 = 323,                      /* $@37  */
  YYSYMBOL_param_decl_list = 324,             /* param_decl_list  */
  YYSYMBOL_single_param_decl = 325,           /* single_param_decl  */
  YYSYMBOL_single_param_decl_ident = 326,     /* single_param_decl_ident  */
  YYSYMBOL_defparam_decl = 327,               /* defparam_decl  */
  YYSYMBOL_defparam_decl_list = 328,          /* defparam_decl_list  */
  YYSYMBOL_single_defparam_decl = 329,        /* single_defparam_decl  */
  YYSYMBOL_enum_type = 330,                   /* enum_type  */
  YYSYMBOL_331_38 = 331,                      /* $@38  */
  YYSYMBOL_enum_base_type = 332,              /* enum_base_type  */
  YYSYMBOL_type_atom = 333,                   /* type_atom  */
  YYSYMBOL_type_vec = 334,                    /* type_vec  */
  YYSYMBOL_type_signing = 335,                /* type_signing  */
  YYSYMBOL_enum_name_list = 336,              /* enum_name_list  */
  YYSYMBOL_enum_name_decl = 337,              /* enum_name_decl  */
  YYSYMBOL_opt_enum_init = 338,               /* opt_enum_init  */
  YYSYMBOL_enum_var_list = 339,               /* enum_var_list  */
  YYSYMBOL_enum_var = 340,                    /* enum_var  */
  YYSYMBOL_enum_decl = 341,                   /* enum_decl  */
  YYSYMBOL_struct_decl = 342,                 /* struct_decl  */
  YYSYMBOL_343_39 = 343,                      /* $@39  */
  YYSYMBOL_struct_type = 344,                 /* struct_type  */
  YYSYMBOL_345_40 = 345,                      /* $@40  */
  YYSYMBOL_struct_union = 346,                /* struct_union  */
  YYSYMBOL_struct_body = 347,                 /* struct_body  */
  YYSYMBOL_opt_packed = 348,                  /* opt_packed  */
  YYSYMBOL_opt_signed_struct = 349,           /* opt_signed_struct  */
  YYSYMBOL_struct_member_list = 350,          /* struct_member_list  */
  YYSYMBOL_struct_member = 351,               /* struct_member  */
  YYSYMBOL_member_name_list = 352,            /* member_name_list  */
  YYSYMBOL_member_name = 353,                 /* member_name  */
  YYSYMBOL_354_41 = 354,                      /* $@41  */
  YYSYMBOL_struct_member_type = 355,          /* struct_member_type  */
  YYSYMBOL_356_42 = 356,                      /* $@42  */
  YYSYMBOL_member_type_token = 357,           /* member_type_token  */
  YYSYMBOL_358_43 = 358,                      /* $@43  */
  YYSYMBOL_359_44 = 359,                      /* $@44  */
  YYSYMBOL_member_type = 360,                 /* member_type  */
  YYSYMBOL_struct_var_list = 361,             /* struct_var_list  */
  YYSYMBOL_struct_var = 362,                  /* struct_var  */
  YYSYMBOL_wire_decl = 363,                   /* wire_decl  */
  YYSYMBOL_364_45 = 364,                      /* $@45  */
  YYSYMBOL_365_46 = 365,                      /* $@46  */
  YYSYMBOL_366_47 = 366,                      /* $@47  */
  YYSYMBOL_367_48 = 367,                      /* $@48  */
  YYSYMBOL_opt_supply_wires = 368,            /* opt_supply_wires  */
  YYSYMBOL_wire_name_list = 369,              /* wire_name_list  */
  YYSYMBOL_wire_name_and_opt_assign = 370,    /* wire_name_and_opt_assign  */
  YYSYMBOL_wire_name = 371,                   /* wire_name  */
  YYSYMBOL_assign_stmt = 372,                 /* assign_stmt  */
  YYSYMBOL_assign_expr_list = 373,            /* assign_expr_list  */
  YYSYMBOL_assign_expr = 374,                 /* assign_expr  */
  YYSYMBOL_type_name = 375,                   /* type_name  */
  YYSYMBOL_typedef_decl = 376,                /* typedef_decl  */
  YYSYMBOL_typedef_base_type = 377,           /* typedef_base_type  */
  YYSYMBOL_enum_struct_type = 378,            /* enum_struct_type  */
  YYSYMBOL_cell_stmt = 379,                   /* cell_stmt  */
  YYSYMBOL_380_49 = 380,                      /* $@49  */
  YYSYMBOL_381_50 = 381,                      /* $@50  */
  YYSYMBOL_tok_prim_wrapper = 382,            /* tok_prim_wrapper  */
  YYSYMBOL_cell_list = 383,                   /* cell_list  */
  YYSYMBOL_single_cell = 384,                 /* single_cell  */
  YYSYMBOL_single_cell_no_array = 385,        /* single_cell_no_array  */
  YYSYMBOL_386_51 = 386,                      /* $@51  */
  YYSYMBOL_single_cell_arraylist = 387,       /* single_cell_arraylist  */
  YYSYMBOL_388_52 = 388,                      /* $@52  */
  YYSYMBOL_cell_list_no_array = 389,          /* cell_list_no_array  */
  YYSYMBOL_prim_list = 390,                   /* prim_list  */
  YYSYMBOL_single_prim = 391,                 /* single_prim  */
  YYSYMBOL_392_53 = 392,                      /* $@53  */
  YYSYMBOL_cell_parameter_list_opt = 393,     /* cell_parameter_list_opt  */
  YYSYMBOL_cell_parameter_list = 394,         /* cell_parameter_list  */
  YYSYMBOL_cell_parameter = 395,              /* cell_parameter  */
  YYSYMBOL_cell_port_list = 396,              /* cell_port_list  */
  YYSYMBOL_cell_port_list_rules = 397,        /* cell_port_list_rules  */
  YYSYMBOL_cell_port = 398,                   /* cell_port  */
  YYSYMBOL_always_comb_or_latch = 399,        /* always_comb_or_latch  */
  YYSYMBOL_always_or_always_ff = 400,         /* always_or_always_ff  */
  YYSYMBOL_always_stmt = 401,                 /* always_stmt  */
  YYSYMBOL_402_54 = 402,                      /* $@54  */
  YYSYMBOL_403_55 = 403,                      /* $@55  */
  YYSYMBOL_404_56 = 404,                      /* $@56  */
  YYSYMBOL_405_57 = 405,                      /* $@57  */
  YYSYMBOL_always_cond = 406,                 /* always_cond  */
  YYSYMBOL_always_events = 407,               /* always_events  */
  YYSYMBOL_always_event = 408,                /* always_event  */
  YYSYMBOL_opt_label = 409,                   /* opt_label  */
  YYSYMBOL_opt_sva_label = 410,               /* opt_sva_label  */
  YYSYMBOL_opt_property = 411,                /* opt_property  */
  YYSYMBOL_modport_stmt = 412,                /* modport_stmt  */
  YYSYMBOL_413_58 = 413,                      /* $@58  */
  YYSYMBOL_414_59 = 414,                      /* $@59  */
  YYSYMBOL_modport_args_opt = 415,            /* modport_args_opt  */
  YYSYMBOL_modport_args = 416,                /* modport_args  */
  YYSYMBOL_modport_arg = 417,                 /* modport_arg  */
  YYSYMBOL_modport_member = 418,              /* modport_member  */
  YYSYMBOL_modport_type_token = 419,          /* modport_type_token  */
  YYSYMBOL_assert = 420,                      /* assert  */
  YYSYMBOL_assert_property = 421,             /* assert_property  */
  YYSYMBOL_simple_behavioral_stmt = 422,      /* simple_behavioral_stmt  */
  YYSYMBOL_asgn_binop = 423,                  /* asgn_binop  */
  YYSYMBOL_inc_or_dec_op = 424,               /* inc_or_dec_op  */
  YYSYMBOL_for_initialization = 425,          /* for_initialization  */
  YYSYMBOL_behavioral_stmt = 426,             /* behavioral_stmt  */
  YYSYMBOL_427_60 = 427,                      /* $@60  */
  YYSYMBOL_428_61 = 428,                      /* $@61  */
  YYSYMBOL_429_62 = 429,                      /* $@62  */
  YYSYMBOL_430_63 = 430,                      /* $@63  */
  YYSYMBOL_431_64 = 431,                      /* $@64  */
  YYSYMBOL_432_65 = 432,                      /* $@65  */
  YYSYMBOL_433_66 = 433,                      /* $@66  */
  YYSYMBOL_434_67 = 434,                      /* $@67  */
  YYSYMBOL_435_68 = 435,                      /* $@68  */
  YYSYMBOL_436_69 = 436,                      /* $@69  */
  YYSYMBOL_437_70 = 437,                      /* $@70  */
  YYSYMBOL_438_71 = 438,                      /* $@71  */
  YYSYMBOL_case_attr = 439,                   /* case_attr  */
  YYSYMBOL_case_type = 440,                   /* case_type  */
  YYSYMBOL_opt_synopsys_attr = 441,           /* opt_synopsys_attr  */
  YYSYMBOL_behavioral_stmt_list = 442,        /* behavioral_stmt_list  */
  YYSYMBOL_optional_else = 443,               /* optional_else  */
  YYSYMBOL_444_72 = 444,                      /* $@72  */
  YYSYMBOL_case_body = 445,                   /* case_body  */
  YYSYMBOL_case_item = 446,                   /* case_item  */
  YYSYMBOL_447_73 = 447,                      /* $@73  */
  YYSYMBOL_448_74 = 448,                      /* $@74  */
  YYSYMBOL_gen_case_body = 449,               /* gen_case_body  */
  YYSYMBOL_gen_case_item = 450,               /* gen_case_item  */
  YYSYMBOL_451_75 = 451,                      /* $@75  */
  YYSYMBOL_452_76 = 452,                      /* $@76  */
  YYSYMBOL_case_select = 453,                 /* case_select  */
  YYSYMBOL_case_expr_list = 454,              /* case_expr_list  */
  YYSYMBOL_rvalue = 455,                      /* rvalue  */
  YYSYMBOL_lvalue = 456,                      /* lvalue  */
  YYSYMBOL_lvalue_concat_list = 457,          /* lvalue_concat_list  */
  YYSYMBOL_opt_arg_list = 458,                /* opt_arg_list  */
  YYSYMBOL_arg_list = 459,                    /* arg_list  */
  YYSYMBOL_arg_list2 = 460,                   /* arg_list2  */
  YYSYMBOL_single_arg = 461,                  /* single_arg  */
  YYSYMBOL_module_gen_body = 462,             /* module_gen_body  */
  YYSYMBOL_gen_stmt_or_module_body_stmt =
      463,                              /* gen_stmt_or_module_body_stmt  */
  YYSYMBOL_genvar_identifier = 464,     /* genvar_identifier  */
  YYSYMBOL_genvar_initialization = 465, /* genvar_initialization  */
  YYSYMBOL_gen_stmt = 466,              /* gen_stmt  */
  YYSYMBOL_467_77 = 467,                /* $@77  */
  YYSYMBOL_468_78 = 468,                /* $@78  */
  YYSYMBOL_469_79 = 469,                /* $@79  */
  YYSYMBOL_470_80 = 470,                /* $@80  */
  YYSYMBOL_471_81 = 471,                /* $@81  */
  YYSYMBOL_gen_block = 472,             /* gen_block  */
  YYSYMBOL_473_82 = 473,                /* $@82  */
  YYSYMBOL_474_83 = 474,                /* $@83  */
  YYSYMBOL_gen_stmt_block = 475,        /* gen_stmt_block  */
  YYSYMBOL_476_84 = 476,                /* $@84  */
  YYSYMBOL_opt_gen_else = 477,          /* opt_gen_else  */
  YYSYMBOL_expr = 478,                  /* expr  */
  YYSYMBOL_basic_expr = 479,            /* basic_expr  */
  YYSYMBOL_480_85 = 480,                /* $@85  */
  YYSYMBOL_concat_list = 481,           /* concat_list  */
  YYSYMBOL_integral_number = 482        /* integral_number  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;

#ifdef short
#undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
#include <limits.h> /* INFRINGES ON USER NAME SPACE */
#if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#define YY_STDINT_H
#endif
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
#undef UINT_LEAST8_MAX
#undef UINT_LEAST16_MAX
#define UINT_LEAST8_MAX 255
#define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H &&                  \
       UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H &&                 \
       UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
#if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#define YYPTRDIFF_T __PTRDIFF_TYPE__
#define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
#elif defined PTRDIFF_MAX
#ifndef ptrdiff_t
#include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#endif
#define YYPTRDIFF_T ptrdiff_t
#define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
#else
#define YYPTRDIFF_T long
#define YYPTRDIFF_MAXIMUM LONG_MAX
#endif
#endif

#ifndef YYSIZE_T
#ifdef __SIZE_TYPE__
#define YYSIZE_T __SIZE_TYPE__
#elif defined size_t
#define YYSIZE_T size_t
#elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#define YYSIZE_T size_t
#else
#define YYSIZE_T unsigned
#endif
#endif

#define YYSIZE_MAXIMUM                                                         \
  YY_CAST(YYPTRDIFF_T,                                                         \
          (YYPTRDIFF_MAXIMUM < YY_CAST(YYSIZE_T, -1) ? YYPTRDIFF_MAXIMUM       \
                                                     : YY_CAST(YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST(YYPTRDIFF_T, sizeof(X))

/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
#if defined YYENABLE_NLS && YYENABLE_NLS
#if ENABLE_NLS
#include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#define YY_(Msgid) dgettext("bison-runtime", Msgid)
#endif
#endif
#ifndef YY_
#define YY_(Msgid) Msgid
#endif
#endif

#ifndef YY_ATTRIBUTE_PURE
#if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#define YY_ATTRIBUTE_PURE __attribute__((__pure__))
#else
#define YY_ATTRIBUTE_PURE
#endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
#if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#define YY_ATTRIBUTE_UNUSED __attribute__((__unused__))
#else
#define YY_ATTRIBUTE_UNUSED
#endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if !defined lint || defined __GNUC__
#define YY_USE(E) ((void)(E))
#else
#define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && !defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
#if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                                    \
  _Pragma("GCC diagnostic push")                                               \
      _Pragma("GCC diagnostic ignored \"-Wuninitialized\"")
#else
#define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                                    \
  _Pragma("GCC diagnostic push")                                               \
      _Pragma("GCC diagnostic ignored \"-Wuninitialized\"")                    \
          _Pragma("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
#endif
#define YY_IGNORE_MAYBE_UNINITIALIZED_END _Pragma("GCC diagnostic pop")
#else
#define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
#define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
#define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
#define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && !defined __ICC && 6 <= __GNUC__
#define YY_IGNORE_USELESS_CAST_BEGIN                                           \
  _Pragma("GCC diagnostic push")                                               \
      _Pragma("GCC diagnostic ignored \"-Wuseless-cast\"")
#define YY_IGNORE_USELESS_CAST_END _Pragma("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
#define YY_IGNORE_USELESS_CAST_BEGIN
#define YY_IGNORE_USELESS_CAST_END
#endif

#define YY_ASSERT(E) ((void)(0 && (E)))

#if 1

/* The parser invokes alloca or malloc; define the necessary symbols.  */

#ifdef YYSTACK_ALLOC
/* Pacify GCC's 'empty if-body' warning.  */
#define YYSTACK_FREE(Ptr)                                                      \
  do { /* empty */                                                             \
    ;                                                                          \
  } while (0)
#ifndef YYSTACK_ALLOC_MAXIMUM
/* The OS might guarantee only one guard page at the bottom of the stack,
   and a page size can be as small as 4096 bytes.  So we cannot safely
   invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
   to allow for a few compiler-allocated temporary stack slots.  */
#define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#endif
#else
#define YYSTACK_ALLOC YYMALLOC
#define YYSTACK_FREE YYFREE
#ifndef YYSTACK_ALLOC_MAXIMUM
#define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#endif
#if (defined __cplusplus && !defined EXIT_SUCCESS &&                           \
     !((defined YYMALLOC || defined malloc) &&                                 \
       (defined YYFREE || defined free)))
#include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif
#endif
#ifndef YYMALLOC
#define YYMALLOC malloc
#if !defined malloc && !defined EXIT_SUCCESS
void *malloc(YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#endif
#endif
#ifndef YYFREE
#define YYFREE free
#if !defined free && !defined EXIT_SUCCESS
void free(void *);      /* INFRINGES ON USER NAME SPACE */
#endif
#endif
#endif
#define YYCOPY_NEEDED 1
#endif /* 1 */

#if (!defined yyoverflow &&                                                    \
     (!defined __cplusplus || (defined FRONTEND_VERILOG_YYLTYPE_IS_TRIVIAL &&  \
                               FRONTEND_VERILOG_YYLTYPE_IS_TRIVIAL &&          \
                               defined FRONTEND_VERILOG_YYSTYPE_IS_TRIVIAL &&  \
                               FRONTEND_VERILOG_YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc {
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
#define YYSTACK_GAP_MAXIMUM (YYSIZEOF(union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
#define YYSTACK_BYTES(N)                                                       \
  ((N) * (YYSIZEOF(yy_state_t) + YYSIZEOF(YYSTYPE) + YYSIZEOF(YYLTYPE)) +      \
   2 * YYSTACK_GAP_MAXIMUM)

#define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
#define YYSTACK_RELOCATE(Stack_alloc, Stack)                                   \
  do {                                                                         \
    YYPTRDIFF_T yynewbytes;                                                    \
    YYCOPY(&yyptr->Stack_alloc, Stack, yysize);                                \
    Stack = &yyptr->Stack_alloc;                                               \
    yynewbytes = yystacksize * YYSIZEOF(*Stack) + YYSTACK_GAP_MAXIMUM;         \
    yyptr += yynewbytes / YYSIZEOF(*yyptr);                                    \
  } while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
#ifndef YYCOPY
#if defined __GNUC__ && 1 < __GNUC__
#define YYCOPY(Dst, Src, Count)                                                \
  __builtin_memcpy(Dst, Src, YY_CAST(YYSIZE_T, (Count)) * sizeof(*(Src)))
#else
#define YYCOPY(Dst, Src, Count)                                                \
  do {                                                                         \
    YYPTRDIFF_T yyi;                                                           \
    for (yyi = 0; yyi < (Count); yyi++)                                        \
      (Dst)[yyi] = (Src)[yyi];                                                 \
  } while (0)
#endif
#endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL 3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST 3269

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS 174
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS 309
/* YYNRULES -- Number of rules.  */
#define YYNRULES 701
/* YYNSTATES -- Number of states.  */
#define YYNSTATES 1357

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK 402

/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                       \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                                            \
       ? YY_CAST(yysymbol_kind_t, yytranslate[YYX])                            \
       : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_uint8 yytranslate[] = {
    0,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,
    2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,
    2,   2,   2,   173, 2,   164, 2,   153, 135, 2,   161, 162, 151, 149, 158,
    150, 160, 152, 2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   165, 163,
    141, 159, 144, 171, 170, 2,   2,   2,   2,   2,   2,   2,   2,   2,   2,
    2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,
    2,   166, 2,   167, 133, 2,   2,   2,   2,   2,   2,   2,   2,   2,   2,
    2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,
    2,   2,   2,   168, 131, 169, 172, 2,   2,   2,   2,   2,   2,   2,   2,
    2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,
    2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,
    2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,
    2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,
    2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,
    2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,
    2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,
    2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,   2,
    2,   1,   2,   3,   4,   5,   6,   7,   8,   9,   10,  11,  12,  13,  14,
    15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,
    30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,
    45,  46,  47,  48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,
    60,  61,  62,  63,  64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,
    75,  76,  77,  78,  79,  80,  81,  82,  83,  84,  85,  86,  87,  88,  89,
    90,  91,  92,  93,  94,  95,  96,  97,  98,  99,  100, 101, 102, 103, 104,
    105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
    120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 132, 134, 136, 137,
    138, 139, 140, 142, 143, 145, 146, 147, 148, 154, 155, 156, 157};

#if FRONTEND_VERILOG_YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] = {
    0,    455,  455,  455,  467,  468,  469,  470,  471,  472,  473,  474,
    475,  476,  479,  479,  495,  498,  501,  508,  501,  519,  519,  522,
    523,  526,  532,  540,  543,  551,  561,  562,  563,  567,  569,  567,
    594,  594,  594,  594,  597,  597,  600,  601,  601,  607,  607,  613,
    616,  616,  616,  619,  619,  622,  622,  625,  643,  646,  646,  660,
    665,  660,  675,  675,  691,  696,  698,  696,  715,  715,  718,  718,
    718,  718,  721,  723,  721,  744,  744,  747,  747,  747,  747,  747,
    747,  747,  748,  748,  751,  756,  760,  751,  788,  793,  794,  797,
    798,  803,  811,  812,  815,  816,  817,  819,  820,  821,  822,  823,
    826,  826,  829,  829,  834,  834,  839,  840,  843,  846,  849,  855,
    856,  857,  860,  864,  867,  870,  873,  873,  877,  879,  882,  885,
    889,  892,  895,  898,  907,  910,  913,  916,  918,  923,  928,  929,
    930,  931,  932,  935,  936,  939,  944,  950,  956,  962,  965,  971,
    974,  979,  980,  983,  985,  986,  987,  988,  991,  991,  991,  991,
    991,  991,  991,  991,  991,  991,  992,  992,  992,  993,  993,  993,
    993,  993,  993,  996,  996,  1007, 1007, 1017, 1017, 1028, 1028, 1040,
    1040, 1052, 1052, 1069, 1069, 1094, 1099, 1102, 1106, 1111, 1112, 1113,
    1117, 1118, 1119, 1122, 1123, 1124, 1128, 1133, 1139, 1140, 1143, 1144,
    1145, 1146, 1149, 1150, 1153, 1153, 1153, 1157, 1153, 1165, 1165, 1168,
    1168, 1189, 1189, 1203, 1204, 1209, 1212, 1213, 1216, 1300, 1374, 1377,
    1382, 1385, 1390, 1393, 1398, 1404, 1410, 1416, 1424, 1425, 1426, 1429,
    1437, 1444, 1453, 1465, 1485, 1491, 1501, 1502, 1505, 1506, 1509, 1512,
    1513, 1517, 1518, 1524, 1527, 1527, 1530, 1533, 1533, 1536, 1542, 1543,
    1547, 1548, 1549, 1553, 1554, 1558, 1558, 1561, 1562, 1563, 1566, 1567,
    1568, 1572, 1573, 1576, 1577, 1580, 1581, 1584, 1584, 1584, 1588, 1592,
    1595, 1598, 1599, 1600, 1603, 1604, 1605, 1612, 1615, 1616, 1623, 1623,
    1629, 1631, 1633, 1636, 1641, 1646, 1652, 1654, 1657, 1660, 1663, 1663,
    1663, 1663, 1664, 1669, 1669, 1678, 1678, 1687, 1687, 1690, 1696, 1709,
    1726, 1729, 1729, 1732, 1745, 1745, 1768, 1769, 1770, 1774, 1780, 1781,
    1785, 1786, 1787, 1790, 1791, 1795, 1810, 1811, 1815, 1816, 1819, 1831,
    1839, 1839, 1846, 1846, 1850, 1851, 1854, 1858, 1859, 1862, 1863, 1864,
    1867, 1868, 1871, 1875, 1876, 1879, 1879, 1888, 1888, 1892, 1893, 1896,
    1898, 1896, 1910, 1911, 1914, 1915, 1918, 1931, 1935, 1931, 1941, 1941,
    1949, 1949, 1959, 1960, 1971, 1971, 1974, 2016, 2044, 2090, 2093, 2093,
    2096, 2102, 2103, 2107, 2120, 2124, 2129, 2138, 2147, 2148, 2152, 2152,
    2161, 2161, 2171, 2174, 2179, 2180, 2183, 2183, 2186, 2186, 2197, 2197,
    2208, 2209, 2212, 2213, 2216, 2217, 2217, 2225, 2225, 2228, 2228, 2231,
    2232, 2237, 2240, 2249, 2276, 2276, 2279, 2284, 2290, 2298, 2305, 2314,
    2322, 2325, 2330, 2333, 2338, 2345, 2338, 2358, 2358, 2374, 2374, 2388,
    2389, 2390, 2391, 2392, 2393, 2396, 2397, 2398, 2401, 2407, 2413, 2420,
    2423, 2428, 2431, 2436, 2439, 2442, 2447, 2453, 2447, 2459, 2459, 2462,
    2462, 2465, 2466, 2469, 2479, 2479, 2482, 2495, 2508, 2521, 2534, 2543,
    2552, 2561, 2576, 2593, 2602, 2611, 2620, 2629, 2638, 2651, 2666, 2672,
    2675, 2678, 2684, 2689, 2690, 2691, 2692, 2693, 2694, 2695, 2696, 2697,
    2698, 2699, 2700, 2705, 2706, 2709, 2717, 2720, 2758, 2758, 2758, 2758,
    2758, 2758, 2759, 2760, 2761, 2764, 2764, 2775, 2775, 2786, 2788, 2786,
    2813, 2818, 2820, 2813, 2831, 2831, 2845, 2845, 2859, 2870, 2859, 2877,
    2877, 2890, 2893, 2897, 2901, 2908, 2911, 2914, 2919, 2923, 2927, 2930,
    2931, 2934, 2934, 2945, 2948, 2949, 2952, 2958, 2952, 2971, 2972, 2975,
    2981, 2975, 2990, 2991, 2994, 2999, 3006, 3009, 3014, 3020, 3030, 3038,
    3041, 3046, 3050, 3056, 3057, 3060, 3061, 3064, 3065, 3068, 3073, 3074,
    3075, 3078, 3078, 3079, 3084, 3091, 3094, 3110, 3118, 3122, 3118, 3129,
    3129, 3138, 3138, 3147, 3147, 3159, 3161, 3159, 3177, 3177, 3184, 3187,
    3187, 3190, 3193, 3201, 3206, 3211, 3214, 3224, 3238, 3245, 3257, 3262,
    3262, 3273, 3277, 3281, 3284, 3289, 3292, 3295, 3300, 3305, 3310, 3315,
    3320, 3325, 3330, 3335, 3341, 3346, 3353, 3358, 3363, 3368, 3373, 3378,
    3383, 3388, 3393, 3398, 3403, 3408, 3413, 3418, 3423, 3428, 3433, 3438,
    3443, 3448, 3453, 3458, 3463, 3468, 3473, 3478, 3484, 3490, 3496, 3503,
    3509, 3512, 3518, 3519, 3520, 3525};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST(yysymbol_kind_t, yystos[State])

#if 1
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name(yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] = {"\"end of file\"",
                                      "error",
                                      "\"invalid token\"",
                                      "TOK_STRING",
                                      "TOK_ID",
                                      "TOK_CONSTVAL",
                                      "TOK_REALVAL",
                                      "TOK_PRIMITIVE",
                                      "TOK_SVA_LABEL",
                                      "TOK_SPECIFY_OPER",
                                      "TOK_MSG_TASKS",
                                      "TOK_BASE",
                                      "TOK_BASED_CONSTVAL",
                                      "TOK_UNBASED_UNSIZED_CONSTVAL",
                                      "TOK_USER_TYPE",
                                      "TOK_PKG_USER_TYPE",
                                      "TOK_ASSERT",
                                      "TOK_ASSUME",
                                      "TOK_RESTRICT",
                                      "TOK_COVER",
                                      "TOK_FINAL",
                                      "ATTR_BEGIN",
                                      "ATTR_END",
                                      "DEFATTR_BEGIN",
                                      "DEFATTR_END",
                                      "TOK_MODULE",
                                      "TOK_ENDMODULE",
                                      "TOK_PARAMETER",
                                      "TOK_LOCALPARAM",
                                      "TOK_DEFPARAM",
                                      "TOK_PACKAGE",
                                      "TOK_ENDPACKAGE",
                                      "TOK_PACKAGESEP",
                                      "TOK_INTERFACE",
                                      "TOK_ENDINTERFACE",
                                      "TOK_MODPORT",
                                      "TOK_VAR",
                                      "TOK_WILDCARD_CONNECT",
                                      "TOK_INPUT",
                                      "TOK_OUTPUT",
                                      "TOK_INOUT",
                                      "TOK_WIRE",
                                      "TOK_WAND",
                                      "TOK_WOR",
                                      "TOK_REG",
                                      "TOK_LOGIC",
                                      "TOK_INTEGER",
                                      "TOK_SIGNED",
                                      "TOK_ASSIGN",
                                      "TOK_ALWAYS",
                                      "TOK_INITIAL",
                                      "TOK_ALWAYS_FF",
                                      "TOK_ALWAYS_COMB",
                                      "TOK_ALWAYS_LATCH",
                                      "TOK_BEGIN",
                                      "TOK_END",
                                      "TOK_IF",
                                      "TOK_ELSE",
                                      "TOK_FOR",
                                      "TOK_WHILE",
                                      "TOK_REPEAT",
                                      "TOK_DPI_FUNCTION",
                                      "TOK_POSEDGE",
                                      "TOK_NEGEDGE",
                                      "TOK_OR",
                                      "TOK_AUTOMATIC",
                                      "TOK_CASE",
                                      "TOK_CASEX",
                                      "TOK_CASEZ",
                                      "TOK_ENDCASE",
                                      "TOK_DEFAULT",
                                      "TOK_FUNCTION",
                                      "TOK_ENDFUNCTION",
                                      "TOK_TASK",
                                      "TOK_ENDTASK",
                                      "TOK_SPECIFY",
                                      "TOK_IGNORED_SPECIFY",
                                      "TOK_ENDSPECIFY",
                                      "TOK_SPECPARAM",
                                      "TOK_SPECIFY_AND",
                                      "TOK_IGNORED_SPECIFY_AND",
                                      "TOK_GENERATE",
                                      "TOK_ENDGENERATE",
                                      "TOK_GENVAR",
                                      "TOK_REAL",
                                      "TOK_SYNOPSYS_FULL_CASE",
                                      "TOK_SYNOPSYS_PARALLEL_CASE",
                                      "TOK_SUPPLY0",
                                      "TOK_SUPPLY1",
                                      "TOK_TO_SIGNED",
                                      "TOK_TO_UNSIGNED",
                                      "TOK_POS_INDEXED",
                                      "TOK_NEG_INDEXED",
                                      "TOK_PROPERTY",
                                      "TOK_ENUM",
                                      "TOK_TYPEDEF",
                                      "TOK_RAND",
                                      "TOK_CONST",
                                      "TOK_CHECKER",
                                      "TOK_ENDCHECKER",
                                      "TOK_EVENTUALLY",
                                      "TOK_INCREMENT",
                                      "TOK_DECREMENT",
                                      "TOK_UNIQUE",
                                      "TOK_UNIQUE0",
                                      "TOK_PRIORITY",
                                      "TOK_STRUCT",
                                      "TOK_PACKED",
                                      "TOK_UNSIGNED",
                                      "TOK_INT",
                                      "TOK_BYTE",
                                      "TOK_SHORTINT",
                                      "TOK_LONGINT",
                                      "TOK_VOID",
                                      "TOK_UNION",
                                      "TOK_BIT_OR_ASSIGN",
                                      "TOK_BIT_AND_ASSIGN",
                                      "TOK_BIT_XOR_ASSIGN",
                                      "TOK_ADD_ASSIGN",
                                      "TOK_SUB_ASSIGN",
                                      "TOK_DIV_ASSIGN",
                                      "TOK_MOD_ASSIGN",
                                      "TOK_MUL_ASSIGN",
                                      "TOK_SHL_ASSIGN",
                                      "TOK_SHR_ASSIGN",
                                      "TOK_SSHL_ASSIGN",
                                      "TOK_SSHR_ASSIGN",
                                      "TOK_BIND",
                                      "TOK_TIME_SCALE",
                                      "OP_LOR",
                                      "OP_LAND",
                                      "'|'",
                                      "OP_NOR",
                                      "'^'",
                                      "OP_XNOR",
                                      "'&'",
                                      "OP_NAND",
                                      "OP_EQ",
                                      "OP_NE",
                                      "OP_EQX",
                                      "OP_NEX",
                                      "'<'",
                                      "OP_LE",
                                      "OP_GE",
                                      "'>'",
                                      "OP_SHL",
                                      "OP_SHR",
                                      "OP_SSHL",
                                      "OP_SSHR",
                                      "'+'",
                                      "'-'",
                                      "'*'",
                                      "'/'",
                                      "'%'",
                                      "OP_POW",
                                      "OP_CAST",
                                      "UNARY_OPS",
                                      "FAKE_THEN",
                                      "','",
                                      "'='",
                                      "'.'",
                                      "'('",
                                      "')'",
                                      "';'",
                                      "'#'",
                                      "':'",
                                      "'['",
                                      "']'",
                                      "'{'",
                                      "'}'",
                                      "'@'",
                                      "'?'",
                                      "'~'",
                                      "'!'",
                                      "$accept",
                                      "input",
                                      "$@1",
                                      "design",
                                      "attr",
                                      "$@2",
                                      "attr_opt",
                                      "defattr",
                                      "$@3",
                                      "$@4",
                                      "opt_attr_list",
                                      "attr_list",
                                      "attr_assign",
                                      "hierarchical_id",
                                      "hierarchical_type_id",
                                      "module",
                                      "$@5",
                                      "$@6",
                                      "module_para_opt",
                                      "$@7",
                                      "$@8",
                                      "module_para_list",
                                      "single_module_para",
                                      "$@9",
                                      "$@10",
                                      "module_args_opt",
                                      "module_args",
                                      "optional_comma",
                                      "module_arg_opt_assignment",
                                      "module_arg",
                                      "$@11",
                                      "$@12",
                                      "$@13",
                                      "$@14",
                                      "package",
                                      "$@15",
                                      "$@16",
                                      "package_body",
                                      "package_body_stmt",
                                      "interface",
                                      "$@17",
                                      "$@18",
                                      "interface_body",
                                      "interface_body_stmt",
                                      "bind_directive",
                                      "$@19",
                                      "$@20",
                                      "$@21",
                                      "bind_target",
                                      "opt_bind_target_instance_list",
                                      "bind_target_instance_list",
                                      "bind_target_instance",
                                      "mintypmax_expr",
                                      "non_opt_delay",
                                      "delay",
                                      "io_wire_type",
                                      "$@22",
                                      "non_io_wire_type",
                                      "$@23",
                                      "wire_type",
                                      "wire_type_token_io",
                                      "wire_type_signedness",
                                      "wire_type_const_rand",
                                      "opt_wire_type_token",
                                      "wire_type_token",
                                      "net_type",
                                      "logic_type",
                                      "integer_atom_type",
                                      "integer_vector_type",
                                      "non_opt_range",
                                      "non_opt_multirange",
                                      "range",
                                      "range_or_multirange",
                                      "module_body",
                                      "module_body_stmt",
                                      "checker_decl",
                                      "$@24",
                                      "task_func_decl",
                                      "$@25",
                                      "$@26",
                                      "$@27",
                                      "$@28",
                                      "$@29",
                                      "$@30",
                                      "func_return_type",
                                      "opt_type_vec",
                                      "opt_signedness_default_signed",
                                      "opt_signedness_default_unsigned",
                                      "dpi_function_arg",
                                      "opt_dpi_function_args",
                                      "dpi_function_args",
                                      "opt_automatic",
                                      "task_func_args_opt",
                                      "$@31",
                                      "$@32",
                                      "task_func_args",
                                      "task_func_port",
                                      "$@33",
                                      "$@34",
                                      "task_func_body",
                                      "specify_block",
                                      "specify_item_list",
                                      "specify_item",
                                      "specify_opt_triple",
                                      "specify_if",
                                      "specify_condition",
                                      "specify_target",
                                      "specify_edge",
                                      "specify_rise_fall",
                                      "specify_triple",
                                      "ignored_specify_block",
                                      "ignored_specify_item_opt",
                                      "ignored_specify_item",
                                      "specparam_declaration",
                                      "specparam_range",
                                      "list_of_specparam_assignments",
                                      "specparam_assignment",
                                      "ignspec_opt_cond",
                                      "path_declaration",
                                      "simple_path_declaration",
                                      "path_delay_value",
                                      "list_of_path_delay_extra_expressions",
                                      "specify_edge_identifier",
                                      "parallel_path_description",
                                      "full_path_description",
                                      "list_of_path_inputs",
                                      "more_path_inputs",
                                      "list_of_path_outputs",
                                      "opt_polarity_operator",
                                      "specify_input_terminal_descriptor",
                                      "specify_output_terminal_descriptor",
                                      "system_timing_declaration",
                                      "system_timing_arg",
                                      "system_timing_args",
                                      "ignspec_constant_expression",
                                      "ignspec_expr",
                                      "ignspec_id",
                                      "$@35",
                                      "param_signed",
                                      "param_integer",
                                      "param_real",
                                      "param_range",
                                      "param_integer_type",
                                      "param_range_type",
                                      "param_implicit_type",
                                      "param_type",
                                      "param_decl",
                                      "$@36",
                                      "localparam_decl",
                                      "$@37",
                                      "param_decl_list",
                                      "single_param_decl",
                                      "single_param_decl_ident",
                                      "defparam_decl",
                                      "defparam_decl_list",
                                      "single_defparam_decl",
                                      "enum_type",
                                      "$@38",
                                      "enum_base_type",
                                      "type_atom",
                                      "type_vec",
                                      "type_signing",
                                      "enum_name_list",
                                      "enum_name_decl",
                                      "opt_enum_init",
                                      "enum_var_list",
                                      "enum_var",
                                      "enum_decl",
                                      "struct_decl",
                                      "$@39",
                                      "struct_type",
                                      "$@40",
                                      "struct_union",
                                      "struct_body",
                                      "opt_packed",
                                      "opt_signed_struct",
                                      "struct_member_list",
                                      "struct_member",
                                      "member_name_list",
                                      "member_name",
                                      "$@41",
                                      "struct_member_type",
                                      "$@42",
                                      "member_type_token",
                                      "$@43",
                                      "$@44",
                                      "member_type",
                                      "struct_var_list",
                                      "struct_var",
                                      "wire_decl",
                                      "$@45",
                                      "$@46",
                                      "$@47",
                                      "$@48",
                                      "opt_supply_wires",
                                      "wire_name_list",
                                      "wire_name_and_opt_assign",
                                      "wire_name",
                                      "assign_stmt",
                                      "assign_expr_list",
                                      "assign_expr",
                                      "type_name",
                                      "typedef_decl",
                                      "typedef_base_type",
                                      "enum_struct_type",
                                      "cell_stmt",
                                      "$@49",
                                      "$@50",
                                      "tok_prim_wrapper",
                                      "cell_list",
                                      "single_cell",
                                      "single_cell_no_array",
                                      "$@51",
                                      "single_cell_arraylist",
                                      "$@52",
                                      "cell_list_no_array",
                                      "prim_list",
                                      "single_prim",
                                      "$@53",
                                      "cell_parameter_list_opt",
                                      "cell_parameter_list",
                                      "cell_parameter",
                                      "cell_port_list",
                                      "cell_port_list_rules",
                                      "cell_port",
                                      "always_comb_or_latch",
                                      "always_or_always_ff",
                                      "always_stmt",
                                      "$@54",
                                      "$@55",
                                      "$@56",
                                      "$@57",
                                      "always_cond",
                                      "always_events",
                                      "always_event",
                                      "opt_label",
                                      "opt_sva_label",
                                      "opt_property",
                                      "modport_stmt",
                                      "$@58",
                                      "$@59",
                                      "modport_args_opt",
                                      "modport_args",
                                      "modport_arg",
                                      "modport_member",
                                      "modport_type_token",
                                      "assert",
                                      "assert_property",
                                      "simple_behavioral_stmt",
                                      "asgn_binop",
                                      "inc_or_dec_op",
                                      "for_initialization",
                                      "behavioral_stmt",
                                      "$@60",
                                      "$@61",
                                      "$@62",
                                      "$@63",
                                      "$@64",
                                      "$@65",
                                      "$@66",
                                      "$@67",
                                      "$@68",
                                      "$@69",
                                      "$@70",
                                      "$@71",
                                      "case_attr",
                                      "case_type",
                                      "opt_synopsys_attr",
                                      "behavioral_stmt_list",
                                      "optional_else",
                                      "$@72",
                                      "case_body",
                                      "case_item",
                                      "$@73",
                                      "$@74",
                                      "gen_case_body",
                                      "gen_case_item",
                                      "$@75",
                                      "$@76",
                                      "case_select",
                                      "case_expr_list",
                                      "rvalue",
                                      "lvalue",
                                      "lvalue_concat_list",
                                      "opt_arg_list",
                                      "arg_list",
                                      "arg_list2",
                                      "single_arg",
                                      "module_gen_body",
                                      "gen_stmt_or_module_body_stmt",
                                      "genvar_identifier",
                                      "genvar_initialization",
                                      "gen_stmt",
                                      "$@77",
                                      "$@78",
                                      "$@79",
                                      "$@80",
                                      "$@81",
                                      "gen_block",
                                      "$@82",
                                      "$@83",
                                      "gen_stmt_block",
                                      "$@84",
                                      "opt_gen_else",
                                      "expr",
                                      "basic_expr",
                                      "$@85",
                                      "concat_list",
                                      "integral_number",
                                      YY_NULLPTR};

static const char *yysymbol_name(yysymbol_kind_t yysymbol) {
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-1117)

#define yypact_value_is_default(Yyn) ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-593)

#define yytable_value_is_error(Yyn) 0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] = {
    -1117, 105,   76,    -1117, -1117, -1117, 969,   -1117, -1117, 708,   -1117,
    76,    76,    76,    76,    76,    76,    76,    76,    76,    166,   220,
    -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117,
    -1117, 274,   -1117, 222,   230,   -1117, -1117, -1117, 174,   118,   166,
    -1117, -1117, -1117, -1117, 351,   298,   298,   407,   -1117, -1117, -1117,
    -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, 214,   -1117, 131,
    -1117, 593,   327,   -1117, -1117, -1117, -1117, -1117, -1117, 342,   2265,
    -1117, 118,   -1117, -1117, 340,   19,    -1117, 358,   522,   728,   728,
    530,   91,    -1117, 359,   546,   166,   553,   166,   576,   2265,  586,
    431,   -1117, -1117, -1117, 441,   234,   234,   -1117, 294,   -1117, 444,
    -1117, 603,   -1117, 605,   -1117, 467,   -1117, -1117, -1117, -1117, 498,
    -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, 2265,  2265,  -1117,
    -1117, 433,   -1117, 171,   267,   2978,  -1117, 174,   -1117, 641,   166,
    -1117, -1117, -1117, -1117, -1117, -1117, 174,   304,   -1117, -1117, -1117,
    -1117, 652,   -1117, 304,   652,   -1117, -1117, 662,   666,   -1117, -1117,
    677,   -1117, 222,   685,   230,   -1117, 674,   -1117, -1117, -1117, -1117,
    -1117, 502,   538,   697,   -1117, -1117, -1117, 174,   -1117, -1117, -1117,
    -1117, 695,   -1117, 551,   564,   571,   573,   982,   982,   982,   982,
    982,   982,   982,   982,   1812,  263,   558,   982,   982,   2265,  -1117,
    174,   174,   -1117, -1117, 166,   -1117, 2265,  2265,  2265,  -1117, -1117,
    -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117,
    -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117,
    -1117, -1117, -1117, 580,   -1117, 174,   -1117, 581,   -1117, 588,   -1117,
    431,   -1117, -1117, -1117, -1117, -11,   -1117, 589,   174,   295,   584,
    590,   745,   594,   -1117, -1117, -1117, 174,   591,   -1117, -1117, 65,
    592,   607,   612,   -1117, -1117, 609,   -1117, 772,   753,   -1117, 2265,
    2265,  2265,  2265,  -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117,
    -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117,
    -1117, -1117, 2265,  315,   2265,  2265,  2265,  2265,  -1117, -1117, -1117,
    316,   619,   -1117, -1117, 95,    -1117, 616,   617,   621,   982,   982,
    982,   982,   982,   982,   982,   982,   982,   982,   982,   982,   982,
    982,   982,   982,   982,   982,   982,   982,   982,   982,   982,   982,
    982,   982,   2265,  2265,  -1117, 622,   166,   538,   652,   -1117, 2265,
    -1117, -1117, -1117, 787,   629,   -1117, 789,   591,   591,   -1117, 632,
    633,   70,    800,   645,   -1117, 436,   649,   -1117, -1117, 982,   -1117,
    697,   644,   -1117, -1117, -1117, 297,   -1117, -1117, 234,   234,   -1117,
    32,    -1117, 654,   656,   657,   658,   664,   -1117, 646,   668,   663,
    -1117, 655,   681,   2265,  -1117, -1117, -1117, 3047,  3072,  3095,  3095,
    2390,  2390,  3114,  3114,  2047,  2047,  2047,  2047,  1505,  1505,  1505,
    1505,  696,   696,   696,   696,   721,   721,   362,   362,   362,   687,
    691,   690,   653,   853,   -1117, 703,   -1117, -1117, 93,    854,   -1117,
    10,    -1117, 590,   863,   705,   707,   -1117, 875,   -1117, 513,   722,
    -1117, -1117, 724,   886,   731,   -1117, 566,   -1117, 523,   174,   69,
    730,   385,   3021,  -1117, -1117, 174,   772,   -1117, -1117, 174,   -1117,
    -1117, -1117, -1117, -1117, -1117, 2265,  -1117, 725,   166,   735,   -1117,
    -1117, -1117, -1117, 2265,  1535,  -1117, -1117, 319,   -1117, 739,   624,
    -1117, -1117, -1117, -1117, -1117, -1117, 787,   -1117, 748,   -1117, -1117,
    -1117, 436,   754,   -1117, 909,   490,   -1117, -1117, 70,    755,   2265,
    -1117, -1117, -1117, -1117, -1117, -1117, 523,   818,   -1117, 935,   912,
    -1117, -1117, 174,   -1117, 914,   757,   857,   -1117, -1117, -1117, -1117,
    -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, 342,   761,
    -1117, -1117, 2265,  763,   -1117, 922,   181,   -1117, -1117, 766,   853,
    -1117, 2671,  930,   -1117, -1117, -1117, 590,   642,   659,   174,   92,
    -1117, 174,   -1117, 773,   -1117, 278,   2554,  -1117, 552,   -1117, -1117,
    -1117, -1117, 625,   -1117, 774,   -1117, 643,   728,   728,   -1117, -1117,
    -1117, 724,   935,   -1117, 1125,  -1117, -1117, -1117, -1117, -1117, -1117,
    -1117, 317,   921,   -1117, -1117, -1117, 166,   321,   -1117, -1117, -1117,
    89,    -1117, -1117, -1117, -1117, -1117, 936,   939,   174,   -1117, -1117,
    -1117, -1117, -1117, -1117, 778,   1535,  -1117, -1117, -1117, -1117, 739,
    -1117, 790,   791,   -1117, -1117, -1117, 98,    79,    36,    -1117, 951,
    -1117, 2992,  -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117,
    -1117, -1117, 952,   -1117, -1117, -1117, -1117, -1117, -1117, -1117, 675,
    -1117, 796,   -1117, -1117, -1117, 798,   -1117, -1117, -1117, -1117, 801,
    -1117, -1117, -1117, -1117, -1117, -1117, 1615,  -1117, -1117, 799,   803,
    811,   812,   -1117, -1117, -1117, -1117, 2265,  20,    -1117, -1117, 1267,
    -1117, 57,    57,    57,    60,    -1117, 828,   652,   652,   -1117, 317,
    -1117, -1117, -1117, -1117, -1117, -1117, -1117, 724,   832,   174,   -1117,
    835,   366,   -1117, 838,   552,   -1117, -1117, -1117, 552,   792,   1636,
    -1117, 1268,  837,   843,   -1117, 841,   -1117, 739,   2265,  -1117, 845,
    846,   927,   98,    847,   -1117, 848,   -1117, 437,   -1117, -1117, 849,
    -1117, 856,   -1117, 850,   2265,  1008,  369,   -1117, 858,   2799,  860,
    -1117, -1117, -1117, -1117, 757,   -1117, 372,   -1117, 923,   928,   931,
    932,   2265,  -1117, 909,   -1117, 866,   299,   855,   841,   739,   2265,
    -1117, 2265,  2265,  865,   877,   841,   89,    757,   757,   171,   757,
    -1117, -1117, 879,   881,   882,   -1117, 887,   2265,  -1117, -1117, -1117,
    -1117, 2265,  -1117, 329,   -1117, 89,    -1117, 2265,  -1117, -1117, -1117,
    757,   -1117, 124,   -1117, -1117, 876,   -1117, 1045,  -1117, -1117, -1117,
    2265,  888,   -1117, 890,   78,    563,   2265,  -1117, -1117, 563,   174,
    2265,  -1117, -1117, 283,   891,   895,   -1117, 797,   892,   -1117, 375,
    1008,  -1117, 2265,  -1117, 2622,  -1117, -1117, -1117, -1117, -1117, 622,
    1051,  -1117, 952,   -1117, 897,   899,   900,   901,   902,   -1117, -1117,
    2265,  -1117, 2265,  903,   -1117, 905,   1064,  907,   908,   -1117, 2265,
    910,   -1117, 2265,  2265,  -1117, 2265,  1716,  1790,  1865,  1939,  915,
    -1117, -1117, -1117, -1117, -1117, 918,   -1117, -1117, 1081,  924,   -1117,
    -1117, 379,   393,   909,   926,   -1117, 1349,  552,   -1117, 925,   -1117,
    735,   -1117, -1117, -1117, -1117, 1085,  933,   934,   -1117, -1117, 2265,
    929,   2265,  -1117, 938,   940,   -1117, -1117, 1008,  942,   -29,   -1117,
    2316,  2316,  1008,  1008,  -1117, 191,   -1117, 2265,  -1117, -1117, -1117,
    -1117, -1117, 1090,  -1117, 399,   -1117, 1090,  -1117, 2014,  2114,  2165,
    2265,  -1117, 305,   941,   -1117, -1117, -1117, 944,   174,   945,   -1117,
    -1117, -1117, -1117, -1117, -1117, -1117, 2265,  947,   2265,  949,   2265,
    950,   958,   961,   -1117, 331,   962,   -1117, -1117, 1091,  -1117, -1117,
    943,   -1117, 967,   -1117, -1117, 2265,  2265,  966,   7,     -1117, -1117,
    -1117, 2215,  971,   2853,  1044,  970,   2265,  2265,  1025,  -1117, 1126,
    -1117, 2265,  985,   352,   993,   -1117, -1117, 1008,  287,   983,   2265,
    -1117, 986,   -1117, -1117, -1117, 797,   797,   984,   978,   2725,  174,
    400,   -1117, -1117, -1117, 1051,  -1117, -1117, 403,   -1117, 988,   2265,
    989,   2265,  990,   2265,  994,   995,   -1117, 2265,  -1117, 2265,  536,
    552,   2265,  1142,  2265,  552,   552,   996,   997,   999,   1000,  1002,
    1003,  -1117, 1004,  -1117, -1117, -1117, -1117, 909,   1005,  2265,  -1117,
    -1117, -1117, 1441,  1441,  -1117, -1117, 1010,  -1117, 739,   -1117, 1105,
    2927,  2265,  -1117, -1117, 2265,  1007,  2416,  1009,  1029,  1031,  -1117,
    1008,  -1117, 1008,  -1117, 1033,  986,   1887,  2265,  -1117, -1117, -1117,
    -1117, -1117, -1117, -1117, 1090,  -1117, -1117, 1090,  -1117, -1117, 1016,
    1020,  1017,  1022,  1024,  1026,  1037,  1118,  1040,  -1117, 739,   -1117,
    -1117, -1117, 1049,  -1117, -1117, -1117, 1046,  -1117, 1048,  -1117, 1050,
    -1117, -1117, 543,   -1117, -1117, -1117, -1117, -1117, -1117, -1117, 1044,
    -1117, -1117, -1117, 1053,  -1117, 563,   2265,  1052,  -1117, 2265,  1056,
    1057,  416,   -1117, -1117, -1117, 1008,  1058,  2265,  986,   1061,  -1117,
    -1117, 1063,  1060,  -1117, 1067,  -1117, 1068,  -1117, -1117, -1117, -1117,
    1462,  -1117, -1117, 1155,  2265,  1069,  -1117, -1117, -1117, -1117, -1117,
    1150,  -1117, -1117, 2265,  1738,  1079,  -1117, 1008,  1008,  1008,  -1117,
    1065,  -1117, 1074,  -1117, -1117, -1117, -1117, -1117, -1117, -1117, 179,
    -1117, 282,   -1117, -1117, -1117, -1117, -1117, -1117, -1117, 1462,  90,
    1080,  1025,  2265,  2265,  2265,  2467,  266,   261,   -1117, -1117, 2265,
    1083,  1044,  2265,  -1117, 552,   1084,  -1117, 1044,  1089,  1086,  1088,
    22,    2265,  1078,  -1117, 1092,  2265,  1093,  2265,  1095,  761,   -1117,
    -1117, -1117, -1117, -1117, 552,   -1117, 2265,  -1117, -1117, -1117, 1097,
    2015,  -1117, 2265,  1100,  2265,  1102,  2265,  552,   -1117, 1110,  2265,
    2265,  1104,  1108,  1113,  1114,  1115,  -1117, 2265,  1116,  424,   1117,
    2265,  -1117, 1121,  -1117, 1122,  -1117, 1124,  2265,  -1117, 2265,  -1117,
    -1117, -1117, -1117, 434,   761,   2265,  -1117, 1127,  2265,  1130,  2265,
    439,   2265,  -1117, 1131,  2265,  1132,  2265,  1133,  2265,  1134,  2265,
    1135,  2265,  1136,  -1117};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] = {
    2,   0,   14,  1,   18,  74,  0,   88,  3,   0,   17,  14,  14,  14,  14,
    14,  14,  14,  14,  14,  22,  0,   30,  31,  148, 147, 142, 340, 364, 143,
    146, 144, 145, 365, 0,   413, 204, 207, 416, 417, 362, 156, 0,   0,   33,
    327, 329, 65,  0,   219, 219, 15,  5,   4,   10,  11,  12,  6,   7,   8,
    9,   27,  19,  21,  23,  25,  75,  344, 0,   205, 206, 415, 208, 209, 414,
    368, 0,   155, 0,   409, 410, 0,   97,  89,  94,  0,   314, 314, 0,   0,
    218, 201, 0,   22,  0,   0,   0,   0,   0,   39,  346, 347, 345, 0,   350,
    350, 32,  371, 363, 0,   646, 698, 645, 0,   699, 0,   14,  14,  531, 532,
    0,   14,  14,  14,  14,  14,  14,  14,  14,  0,   0,   14,  14,  156, 14,
    641, 0,   637, 644, 156, 412, 0,   0,   92,  34,  312, 316, 313, 326, 156,
    314, 323, 322, 324, 325, 0,   315, 314, 0,   66,  185, 0,   0,   202, 203,
    0,   197, 204, 0,   207, 191, 0,   20,  24,  28,  26,  29,  0,   49,  0,
    348, 349, 342, 156, 369, 370, 367, 379, 0,   700, 0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,   696, 0,   0,   0,   0,   647, 155,
    598, 597, 643, 0,   640, 0,   0,   0,   152, 14,  14,  14,  14,  14,  14,
    14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,  14,
    14,  14,  14,  14,  14,  0,   14,  158, 157, 0,   90,  93,  95,  39,  317,
    321, 318, 335, 0,   331, 334, 319, 0,   0,   213, 0,   0,   193, 200, 195,
    198, 221, 16,  36,  14,  0,   355, 54,  351, 343, 379, 372, 0,   383, 701,
    0,   0,   0,   0,   641, 664, 665, 666, 667, 662, 663, 686, 687, 519, 520,
    521, 522, 523, 524, 525, 526, 527, 528, 529, 530, 0,   651, 0,   0,   0,
    0,   653, 655, 690, 0,   0,   153, 154, 156, 639, 0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   411, 440, 0,   49,
    0,   328, 0,   320, 330, 69,  217, 0,   187, 0,   221, 221, 199, 222, 0,
    14,  57,  0,   48,  112, 54,  51,  78,  0,   353, 53,  0,   366, 373, 377,
    0,   375, 382, 350, 350, 380, 0,   381, 0,   0,   0,   0,   0,   642, 0,
    0,   696, 697, 0,   152, 606, 150, 151, 149, 689, 688, 658, 659, 660, 661,
    656, 657, 674, 675, 676, 677, 672, 673, 678, 679, 668, 669, 670, 671, 680,
    681, 682, 683, 684, 685, 0,   0,   0,   0,   96,  0,   332, 333, 14,  211,
    216, 0,   186, 213, 0,   0,   0,   220, 14,  232, 0,   37,  40,  47,  56,
    0,   0,   114, 0,   115, 125, 156, 14,  0,   14,  354, 352, 341, 156, 0,
    374, 386, 156, 384, 691, 649, 650, 692, 694, 0,   695, 0,   0,   54,  605,
    607, 609, 693, 0,   443, 428, 432, 0,   163, 480, 0,   68,  73,  72,  71,
    70,  210, 215, 212, 0,   189, 232, 232, 112, 54,  225, 0,   14,  43,  45,
    14,  0,   0,   58,  60,  64,  116, 117, 118, 125, 124, 123, 0,   0,   52,
    50,  156, 76,  0,   109, 112, 77,  87,  79,  80,  82,  83,  85,  81,  84,
    86,  378, 376, 387, 368, 0,   654, 596, 53,  0,   638, 0,   0,   441, 444,
    0,   0,   91,  14,  0,   67,  214, 188, 213, 14,  14,  156, 14,  223, 156,
    230, 0,   192, 0,   112, 536, 14,  539, 540, 538, 541, 0,   537, 0,   231,
    0,   314, 314, 41,  38,  55,  56,  127, 122, 132, 138, 137, 136, 130, 139,
    135, 141, 121, 128, 134, 140, 62,  0,   0,   337, 486, 108, 0,   458, 465,
    459, 456, 457, 0,   0,   156, 463, 460, 385, 652, 608, 648, 0,   443, 439,
    14,  433, 627, 480, 629, 0,   0,   569, 570, 571, 235, 272, 0,   612, 0,
    162, 112, 179, 176, 159, 181, 164, 165, 182, 170, 166, 167, 169, 0,   174,
    175, 171, 172, 168, 173, 177, 0,   180, 0,   160, 161, 479, 0,   194, 196,
    227, 226, 0,   404, 481, 100, 101, 102, 103, 0,   547, 549, 0,   0,   0,
    0,   568, 566, 567, 544, 0,   156, 14,  599, 14,  542, 485, 485, 485, 485,
    543, 0,   0,   0,   61,  121, 126, 131, 133, 119, 120, 113, 129, 56,  0,
    156, 336, 0,   0,   406, 0,   14,  394, 396, 391, 14,  472, 0,   442, 450,
    0,   447, 448, 604, 35,  480, 0,   620, 0,   0,   0,   235, 0,   310, 0,
    259, 272, 261, 262, 0,   263, 0,   264, 0,   0,   0,   0,   268, 0,   14,
    0,   418, 422, 423, 360, 109, 358, 0,   356, 0,   0,   0,   0,   0,   190,
    0,   224, 0,   0,   98,  604, 480, 0,   552, 0,   0,   0,   601, 604, 0,
    109, 109, 0,   109, 484, 483, 0,   0,   0,   504, 0,   0,   44,  46,  111,
    63,  0,   338, 0,   487, 0,   405, 0,   466, 398, 398, 109, 464, 0,   461,
    445, 0,   455, 0,   451, 429, 14,  606, 0,   630, 0,   0,   250, 0,   233,
    234, 250, 156, 0,   258, 260, 0,   0,   0,   273, 0,   0,   307, 0,   0,
    265, 0,   178, 112, 614, 610, 613, 611, 183, 440, 0,   420, 0,   359, 0,
    0,   0,   0,   0,   228, 104, 0,   105, 0,   0,   550, 0,   112, 0,   0,
    600, 0,   0,   516, 0,   0,   515, 0,   0,   0,   0,   0,   0,   339, 495,
    496, 497, 489, 54,  491, 494, 0,   0,   407, 408, 0,   0,   0,   0,   471,
    0,   14,  446, 454, 449, 54,  628, 612, 623, 616, 0,   0,   0,   248, 249,
    0,   0,   0,   311, 0,   308, 281, 282, 0,   0,   297, 298, 0,   0,   0,
    0,   304, 0,   303, 0,   266, 269, 270, 615, 163, 0,   390, 0,   388, 437,
    357, 0,   0,   0,   0,   625, 0,   0,   548, 576, 560, 0,   156, 0,   556,
    558, 602, 546, 517, 514, 518, 0,   0,   0,   0,   0,   0,   0,   0,   563,
    53,  0,   493, 488, 0,   395, 397, 392, 400, 402, 469, 470, 0,   0,   0,
    0,   473, 478, 462, 0,   0,   14,  632, 617, 0,   0,   243, 240, 0,   271,
    0,   0,   297, 0,   295, 296, 0,   297, 289, 0,   274, 277, 275, 301, 302,
    0,   0,   0,   0,   14,  428, 0,   424, 426, 427, 0,   361, 436, 0,   434,
    0,   0,   0,   0,   0,   0,   0,   0,   586, 0,   106, 0,   14,  14,  0,
    0,   0,   14,  14,  0,   0,   0,   0,   0,   0,   503, 0,   574, 492, 490,
    399, 0,   0,   0,   476, 477, 468, 0,   0,   467, 453, 0,   603, 480, 634,
    636, 14,  0,   619, 621, 0,   0,   0,   0,   0,   0,   289, 0,   291, 0,
    290, 0,   0,   308, 0,   278, 305, 306, 300, 267, 184, 430, 0,   419, 389,
    437, 421, 14,  0,   0,   0,   0,   0,   0,   0,   587, 0,   99,  480, 575,
    561, 533, 534, 553, 557, 559, 0,   498, 0,   499, 0,   505, 502, 581, 401,
    393, 403, 474, 475, 452, 631, 632, 624, 633, 618, 0,   242, 250, 0,   0,
    244, 0,   0,   0,   0,   293, 299, 292, 0,   0,   0,   279, 0,   425, 435,
    0,   0,   507, 0,   508, 0,   512, 511, 626, 585, 0,   107, 551, 579, 0,
    0,   500, 501, 506, 572, 573, 582, 635, 14,  0,   0,   0,   309, 0,   0,
    0,   286, 0,   276, 0,   280, 14,  438, 509, 510, 513, 593, 591, 588, 0,
    594, 577, 562, 535, 14,  564, 580, 0,   0,   0,   243, 0,   0,   0,   0,
    297, 297, 294, 283, 0,   0,   632, 0,   590, 14,  0,   583, 632, 0,   0,
    0,   0,   0,   0,   251, 256, 0,   0,   0,   0,   309, 431, 589, 595, 578,
    554, 14,  622, 0,   247, 246, 245, 0,   256, 236, 0,   0,   0,   0,   0,
    14,  584, 239, 0,   0,   0,   0,   0,   0,   0,   555, 0,   0,   0,   0,
    0,   288, 0,   285, 0,   238, 0,   0,   252, 0,   257, 287, 284, 237, 0,
    257, 0,   253, 0,   0,   0,   0,   0,   0,   254, 0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   255};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] = {
    -1117, -1117, -1117, 746,   73,    -1117, -1117, 99,    -1117, -1117, 1158,
    -1117, 1148,  13,    6,     -1117, -1117, -1117, 1027,  -1117, -1117, -1117,
    769,   -1117, -1117, 948,   -1117, -370,  -525,  823,   -1117, -1117, -1117,
    -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117,
    -454,  -1117, -1117, -1117, -1117, -1117, -1117, -128,  -852,  -384,  -123,
    -1117, -1117, 402,   -1117, -300,  -1117, 572,   767,   -1117, 699,   -1117,
    -484,  9,     -1117, 4,     -130,  -5,    -448,  333,   -566,  -1117, -1117,
    -447,  -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, 1137,  1139,
    802,   -311,  -1117, 1253,  262,   -1117, -1117, -1117, 729,   -1117, -1117,
    132,   -1117, 549,   -1117, -1117, -1117, 58,    -1117, -851,  -1117, -87,
    -1117, -1117, 550,   -584,  -1117, 537,   448,   -1117, -1117, -1117, 360,
    -1023, -1117, -1117, -1117, 367,   -1117, 96,    -939,  -875,  -864,  -1117,
    -367,  -1117, 355,   -694,  -599,  -1117, 293,   -1117, -1117, -1117, -1117,
    -1117, -1117, -83,   11,    -1117, 30,    -1117, 1163,  -305,  -1117, 851,
    -1117, 626,   1317,  -1117, -1117, -50,   -48,   -70,   -1117, 974,   -1117,
    -1117, 474,   -1117, -1117, -1117, 1357,  -1117, 968,   806,   -1117, -1117,
    -1117, 1087,  -1117, 889,   -1117, -1117, -1117, -1117, -1117, -1117, -1117,
    -1117, 306,   -443,  -1117, -1117, -1117, -1117, 539,   -1117, 276,   -515,
    904,   -1117, 545,   1295,  48,    -1117, -1117, -1117, -1117, -1117, -1117,
    -1117, -952,  -295,  -1117, -1117, -1117, -1117, -1117, 237,   -1117, 500,
    -1117, 737,   -968,  -1117, 548,   -1117, -1117, 946,   -1117, -1117, -1117,
    -1117, -1117, -1117, -379,  -580,  -562,  2,     -1117, -1117, -1117, -1117,
    -1117, 390,   476,   -1117, -1117, -1117, -1116, 684,   -133,  -1117, -583,
    -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117,
    -1117, -1117, 805,   -1117, -1117, -1117, -1117, -1117, -1117, -1117, -1117,
    -1117, -1117, -1117, -1117, 154,   -1117, 47,    -590,  506,   -649,  561,
    -1117, 852,   473,   302,   479,   -1117, -557,  -1117, -1117, -1117, -1117,
    -1117, -564,  -1117, -1117, -910,  -1117, -1117, -76,   998,   -1117, 425,
    -95};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] = {
    0,    1,    2,    8,    589,  10,   51,   590,  20,   94,   62,   63,
    64,   133,  148,  12,   85,   253,  178,  375,  526,  461,  462,  601,
    602,  275,  380,  386,  528,  381,  464,  465,  606,  733,  13,   88,
    263,  448,  506,  14,   21,   99,   474,  546,  15,   43,   141,  357,
    83,   143,  251,  84,   798,  591,  627,  467,  468,  469,  470,  635,
    534,  731,  537,  725,  617,  618,  619,  102,  37,   77,   247,  248,
    249,  573,  874,  665,  969,  16,   264,  453,  578,  271,  370,  371,
    168,  169,  71,   74,   450,  367,  451,  91,   374,  458,  692,  519,
    520,  795,  521,  522,  667,  760,  761,  1317, 762,  1116, 1184, 945,
    1278, 1279, 668,  766,  767,  669,  775,  776,  777,  769,  770,  771,
    1045, 1130, 953,  862,  863,  954,  1042, 1189, 1043, 955,  1190, 772,
    961,  962,  866,  963,  1191, 857,  149,  150,  151,  255,  152,  153,
    154,  155,  592,  86,   593,  87,   258,  259,  260,  672,  623,  624,
    673,  67,   103,  156,  157,  182,  277,  278,  384,  787,  788,  674,
    675,  880,  784,  75,   40,   108,  109,  186,  280,  281,  390,  391,
    478,  282,  283,  395,  396,  559,  397,  972,  973,  594,  836,  1097,
    834,  835,  925,  1012, 1013, 1014, 677,  738,  739,  81,   595,  41,
    42,   679,  879,  974,  785,  1056, 1062, 1058, 570,  1059, 1197, 502,
    1063, 1064, 1065, 443,  567,  568,  750,  751,  752,  636,  637,  680,
    746,  931,  745,  741,  839,  1020, 1021, 575,  596,  816,  555,  737,
    922,  829,  918,  919,  920,  921,  597,  682,  598,  313,  134,  988,
    599,  808,  800,  801,  984,  897,  1215, 1305, 1082, 1083, 1078, 1213,
    1092, 600,  683,  1168, 1077, 1247, 1269, 1221, 1251, 1252, 1291, 1150,
    1209, 1210, 1266, 1243, 1244, 135,  714,  806,  848,  493,  494,  495,
    779,  875,  941,  942,  876,  851,  1180, 1027, 1073, 753,  1109, 755,
    937,  1110, 1111, 1177, 1280, 137,  320,  204,  138};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] = {
    136,  507,  214,  210,  158,  947,  585,  664,  715,  685,  473,  681,
    35,   17,   252,  36,   684,  104,  1057, 105,  547,  175,  17,   17,
    17,   17,   17,   17,   17,   17,   17,   551,  18,   65,   558,  183,
    78,   740,  212,  981,  763,  18,   18,   18,   18,   18,   18,   18,
    18,   18,   19,   96,   96,   202,  203,  446,  82,   773,  778,  19,
    19,   19,   19,   19,   19,   19,   19,   19,   754,  376,  463,  1102,
    768,  376,  257,  9,    -13,  814,  1037, 471,  814,  724,  939,  763,
    9,    9,    9,    9,    9,    9,    9,    9,    9,    61,   61,   160,
    -229, 166,  1121, 4,    167,  11,   758,  1125, 1194, 3,    65,   1254,
    65,   5,    11,   11,   11,   11,   11,   11,   11,   11,   11,   663,
    1039, 1040, 79,   564,  504,  728,  666,  96,   211,  1041, 676,  319,
    80,   1270, 732,  764,  693,  209,  28,   325,  326,  327,  514,  209,
    254,  928,  33,   360,  501,  583,  815,  894,  361,  815,  759,  82,
    765,  657,  833,  902,  626,  940,  837,  96,   949,  1103, 1123, 773,
    512,  1104, 61,   6,    513,  1235, 1200, 849,  778,  967,  279,  98,
    98,   -545, 768,  -545, 1296, 1198, 207,  489,  6,    191,  192,  118,
    119,  210,  194,  195,  196,  197,  198,  199,  200,  201,  774,  7,
    205,  206,  208,  213,  825,  398,  399,  400,  401,  321,  322,  877,
    403,  681,  581,  903,  463,  895,  1151, 819,  66,   377,  323,  378,
    -42,  377,  444,  -53,  -42,  393,  402,  394,  404,  405,  406,  406,
    740,  289,  289,  289,  289,  289,  289,  289,  289,  1192, 161,  322,
    289,  289,  -53,  98,   162,  710,  710,  -241, 324,  207,  956,  1046,
    1046, 363,  1222, 687,  1265, 69,   778,  1050, 118,  119,  372,  929,
    646,  72,   440,  441,  889,  180,  695,  696,  697,  930,  447,  763,
    68,   392,  97,   98,   328,  329,  330,  331,  332,  333,  334,  335,
    336,  337,  338,  339,  340,  341,  342,  343,  344,  345,  346,  347,
    348,  349,  350,  351,  352,  353,  211,  355,  111,  1282, 1284, 481,
    482,  663,  113,  209,  114,  1232, 70,   1224, 666,  914,  496,  914,
    676,  -592, 73,   643,  76,   184,  181,  644,  -592, 951,  952,  379,
    1023, 1051, 1127, 145,  1283, 1052, 956,  89,   1287, 1281, 215,  216,
    1048, 1049, 1292, 90,   729,  1261, 1262, 915,  916,  915,  916,  82,
    95,   22,   23,   289,  289,  289,  289,  289,  289,  289,  289,  289,
    289,  289,  289,  289,  289,  289,  289,  289,  289,  289,  289,  289,
    289,  289,  289,  289,  289,  626,  185,  163,  164,  26,   698,  215,
    216,  948,  1039, 1040, 147,  560,  541,  1039, 1040, 822,  823,  542,
    543,  314,  565,  569,  1230, 730,  626,  626,  93,   626,  289,  315,
    217,  544,  218,  1196, 1039, 1040, 111,  699,  1267, 763,  956,  256,
    113,  1124, 114,  1268, 460,  107,  261,  605,  626,  360,  -14,  479,
    712,  891,  364,  508,  480,  892,  877,  1074, 681,  96,   538,  1075,
    29,   30,   31,   32,   165,  556,  -110, -110, -110, 571,  509,  735,
    6,    217,  572,  409,  736,  548,  209,  496,  664,  106,  685,  917,
    681,  764,  1154, 1155, 510,  684,  586,  1159, 1160, 1039, 1040, 140,
    549,  323,  -482, -482, -482, -482, 1041, 1120, 7,    4,    859,  657,
    244,  245,  722,  723,  34,   505,  553,  142,  830,  956,  144,  869,
    1175, 831,  882,  518,  870,  869,  159,  883,  622,  1009, 965,  562,
    523,  524,  1010, 616,  586,  379,  620,  545,  1006, 681,  170,  1009,
    -482, -482, -482, -482, 1011, 1060, 1137, 4,    586,  1140, 1061, 1138,
    587,  1025, 1141, 569,  -482, -482, -482, -482, 663,  1212, 1230, 4,
    690,  172,  1231, 666,  174,  210,  1327, 676,  670,  6,    1328, 1301,
    209,  1303, 176,  1153, 1336, 98,   -14,  177,  1337, 1343, 460,  207,
    663,  1344, 711,  671,  531,  532,  533,  666,  1312, 179,  1314, 676,
    187,  616,  188,  616,  620,  189,  620,  535,  536,  678,  190,  799,
    616,  943,  944,  620,  1219, 1220, 744,  6,    455,  456,  807,  323,
    713,  100,  101,  26,   323,  716,  717,  718,  719,  250,  661,  6,
    579,  580,  586,  45,   46,   193,  588,  518,  257,  663,  -482, -482,
    -482, -482, 881,  273,  666,  4,    265,  586,  676,  734,  266,  841,
    662,  844,  713,  -482, -482, -482, -482, 906,  850,  267,  4,    1131,
    1132, 48,   1289, 904,  905,  269,  907,  789,  790,  791,  792,  49,
    272,  50,   867,  274,  588,  276,  29,   30,   31,   32,   211,  284,
    1306, 652,  653,  654,  285,  927,  688,  209,  588,  888,  749,  817,
    818,  820,  1315, 1172, 1173, 286,  896,  316,  898,  899,  622,  689,
    287,  44,   288,  45,   46,   6,    47,   407,  408,  354,  22,   23,
    356,  912,  358,  365,  362,  368,  913,  366,  373,  369,  6,    382,
    924,  52,   53,   54,   55,   56,   57,   58,   59,   60,   383,  22,
    23,   48,   385,  496,  100,  101,  26,   145,  389,  946,  387,  49,
    410,  50,   950,  411,  412,  809,  442,  812,  413,  950,  670,  449,
    452,  454,  457,  950,  459,  100,  101,  26,   110,  61,   111,  112,
    -59,  466,  588,  472,  113,  671,  114,  489,  146,  477,  499,  799,
    484,  982,  485,  486,  487,  314,  323,  588,  491,  807,  488,  678,
    993,  994,  490,  995,  997,  999,  1001, 1003, 147,  29,   30,   31,
    32,   492,  245,  323,  115,  239,  240,  241,  242,  243,  244,  245,
    873,  497,  1022, 498,  713,  500,  511,  959,  960,  209,  29,   30,
    31,   32,   503,  515,  516,  1031, 517,  1033, 241,  242,  243,  244,
    245,  713,  662,  -229, 525,  950,  950,  527,  45,   46,   116,  117,
    867,  34,   529,  530,  540,  563,  561,  -110, -110, -110, 118,  119,
    1067, 1069, 1071, 1072, 574,  120,  628,  629,  630,  631,  632,  577,
    582,  584,  34,   608,  621,  604,  625,  749,  1084, 588,  1086, 639,
    1088, 641,  642,  645,  121,  122,  123,  124,  125,  126,  686,  22,
    23,   720,  694,  747,  742,  1099, 1100, 743,  633,  634,  127,  128,
    1106, 22,   23,   756,  757,  1113, 1114, 780,  786,  793,  129,  1118,
    802,  794,  838,  796,  803,  130,  614,  26,   1128, 131,  132,  609,
    804,  805,  950,  950,  610,  611,  612,  613,  614,  26,   1080, 22,
    23,   110,  61,   111,  112,  821,  1143, 826,  1145, 113,  1147, 114,
    828,  832,  799,  845,  1152, 846,  847,  1156, 854,  1158, 852,  853,
    856,  858,  861,  865,  763,  24,   25,   26,   884,  871,  615,  864,
    893,  885,  1171, 878,  886,  887,  1022, 1022, 890,  115,  29,   30,
    31,   32,   900,  901,  1179, 670,  932,  1181, 908,  1185, 909,  910,
    29,   30,   31,   32,   911,  933,  957,  936,  938,  950,  958,  971,
    671,  964,  976,  1136, 977,  978,  979,  27,   980,  670,  983,  985,
    986,  989,  990,  116,  117,  992,  678,  28,   1005, 1004, 29,   30,
    31,   32,   34,   33,   671,  914,  1024, 1008, 1015, 939,  120,  1032,
    1029, 1038, 1055, 1095, 34,   1030, 649,  873,  1034, 1096, 678,  1079,
    1115, 1035, 1076, 1225, 1081, 1085, 1227, 1087, 1089, 121,  122,  123,
    124,  125,  126,  1234, 712,  1090, 670,  1091, 1094, 662,  1098, 661,
    1101, 1112, 34,   127,  128,  1107, 1245, 1117, 1119, 1122, 1248, 22,
    23,   671,  1126, 129,  1129, 1134, 1157, 1133, 1255, 1142, 130,  1144,
    1146, 662,  131,  132,  1148, 1149, 1161, 678,  1162, 1163, 1176, 1164,
    1165, 1182, 1166, 1167, 1170, 727,  614,  26,   1174, 1187, 1186, 1188,
    1245, 1193, 1201, 1203, 1274, 1275, 1276, 1202, 873,  1204, 1205, 1208,
    1285, 1206, 1297, 1288, 290,  291,  292,  293,  294,  295,  296,  297,
    1207, 1298, 1211, 317,  318,  950,  1307, 950,  1214, 1216, 662,  1217,
    1246, 1218, 1226, 749,  1223, 1228, 1229, 1250, 1233, 1318, 1236, 1238,
    1310, 1237, 950,  1263, 950,  1325, 1239, 1240, 1249, 1319, 29,   30,
    31,   32,   1259, 1264, 1334, 1299, 1272, 173,  1330, 1286, 1290, 1293,
    1294, 1338, 1295, 171,  1340, 1335, 1342, 1308, 1345, 1300, 1302, 1347,
    1304, 1349, 1311, 1351, 1313, 1353, 323,  1355, 1316, 1320, 1321, 110,
    61,   111,  112,  1322, 1323, 1324, 1326, 113,  359,  114,  1329, 1331,
    1332, 1339, 34,   1333, 1341, 1346, 1348, 1350, 1352, 1354, 603,  539,
    1253, 824,  1356, 987,  713,  607,  1054, 92,   268,  842,  726,  445,
    270,  749,  855,  691,  868,  1273, 576,  115,  860,  966,  1047, 1053,
    1036, 262,  1253, 38,   1260, 550,  414,  415,  416,  417,  418,  419,
    420,  421,  422,  423,  424,  425,  426,  427,  428,  429,  430,  431,
    432,  433,  434,  435,  436,  437,  438,  439,  110,  61,   111,  112,
    975,  116,  117,  476,  113,  827,  114,  39,   483,  638,  1139, 388,
    557,  118,  119,  1016, 1169, 139,  926,  923,  120,  1199, 552,  970,
    748,  475,  298,  299,  300,  301,  302,  303,  304,  305,  306,  307,
    308,  309,  934,  1093, 115,  1007, 813,  121,  122,  123,  124,  125,
    126,  721,  1271, 991,  935,  810,  1026, 1017, 1018, 1178, 0,    640,
    0,    127,  128,  1028, 554,  0,    0,    0,    0,    0,    811,  0,
    843,  129,  0,    0,    0,    0,    0,    0,    130,  0,    116,  117,
    131,  132,  0,    0,    110,  61,   111,  112,  0,    0,    118,  119,
    113,  0,    114,  0,    0,    120,  0,    0,    0,    0,    0,    0,
    0,    110,  61,   111,  112,  0,    1241, 0,    0,    113,  0,    114,
    0,    0,    0,    0,    121,  122,  123,  124,  125,  126,  0,    0,
    115,  0,    0,    0,    0,    0,    0,    0,    0,    0,    127,  128,
    1019, 0,    0,    1017, 1018, 0,    0,    0,    0,    115,  129,  0,
    0,    0,    0,    0,    0,    130,  0,    0,    0,    131,  132,  0,
    0,    0,    0,    0,    0,    0,    116,  117,  1242, 0,    0,    0,
    0,    0,    110,  61,   111,  112,  118,  119,  0,    0,    113,  0,
    114,  120,  0,    116,  117,  0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    118,  119,  0,    0,    0,    0,    0,    120,  0,
    121,  122,  123,  124,  125,  126,  0,    0,    0,    0,    115,  0,
    0,    0,    0,    0,    0,    0,    127,  128,  0,    121,  122,  123,
    124,  125,  126,  0,    0,    0,    129,  0,    0,    0,    0,    0,
    0,    130,  0,    127,  128,  131,  132,  0,    0,    0,    110,  61,
    111,  112,  0,    129,  116,  117,  113,  0,    114,  0,    130,  0,
    0,    0,    131,  132,  118,  119,  0,    110,  61,   111,  112,  120,
    0,    0,    0,    113,  0,    114,  235,  236,  237,  238,  239,  240,
    241,  242,  243,  244,  245,  0,    115,  0,    0,    0,    121,  122,
    123,  124,  125,  126,  0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    115,  127,  128,  0,    0,    0,    0,    0,    0,
    0,    0,    0,    566,  129,  0,    0,    0,    0,    0,    0,    130,
    116,  117,  0,    131,  132,  0,    0,    0,    0,    0,    0,    0,
    118,  119,  0,    110,  61,   111,  112,  120,  0,    116,  117,  113,
    0,    114,  0,    0,    0,    0,    0,    0,    0,    118,  119,  0,
    0,    0,    0,    797,  120,  0,    121,  122,  123,  124,  125,  126,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    115,
    127,  128,  0,    121,  122,  123,  124,  125,  126,  0,    0,    0,
    129,  0,    0,    0,    0,    0,    0,    130,  0,    127,  128,  131,
    132,  0,    0,    0,    0,    110,  61,   111,  112,  129,  840,  0,
    0,    113,  0,    114,  130,  116,  117,  0,    131,  132,  0,    0,
    0,    0,    0,    0,    996,  118,  119,  0,    0,    0,    0,    0,
    120,  0,    0,    0,    0,    1256, 1257, 0,    0,    0,    0,    0,
    0,    115,  0,    0,    0,    0,    0,    0,    0,    0,    0,    121,
    122,  123,  124,  125,  126,  298,  299,  300,  301,  302,  303,  304,
    305,  306,  307,  308,  309,  127,  128,  0,    110,  61,   111,  112,
    0,    0,    0,    0,    113,  129,  114,  116,  117,  0,    0,    0,
    130,  0,    0,    0,    131,  132,  998,  118,  119,  0,    0,    0,
    0,    310,  120,  0,    311,  0,    0,    1258, 0,    0,    0,    0,
    0,    0,    0,    0,    115,  0,    0,    0,    0,    0,    0,    0,
    0,    121,  122,  123,  124,  125,  126,  298,  299,  300,  301,  302,
    303,  304,  305,  306,  307,  308,  309,  127,  128,  0,    110,  61,
    111,  112,  0,    0,    0,    0,    113,  129,  114,  0,    116,  117,
    0,    0,    130,  0,    0,    0,    131,  132,  0,    1000, 118,  119,
    0,    0,    0,    310,  0,    120,  311,  0,    0,    312,  0,    0,
    0,    0,    0,    0,    0,    0,    115,  0,    0,    0,    0,    0,
    0,    0,    0,    0,    121,  122,  123,  124,  125,  126,  298,  299,
    300,  301,  302,  303,  304,  305,  306,  307,  308,  309,  127,  128,
    0,    110,  61,   111,  112,  0,    0,    0,    0,    113,  129,  114,
    116,  117,  0,    0,    0,    130,  0,    0,    0,    131,  132,  0,
    118,  119,  0,    0,    0,    0,    310,  120,  0,    311,  0,    0,
    1195, 0,    0,    0,    0,    0,    0,    0,    0,    115,  0,    0,
    0,    0,    0,    0,    0,    0,    121,  122,  123,  124,  125,  126,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    127,  128,  0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    129,  1002, 0,    116,  117,  0,    0,    130,  0,    0,    0,    131,
    132,  0,    1066, 118,  119,  110,  61,   111,  112,  0,    120,  0,
    0,    113,  0,    114,  0,    0,    298,  299,  300,  301,  302,  303,
    304,  305,  306,  307,  308,  309,  0,    0,    0,    121,  122,  123,
    124,  125,  126,  0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    115,  0,    127,  128,  0,    0,    0,    110,  61,   111,  112,
    0,    0,    310,  129,  113,  311,  114,  0,    1309, 0,    130,  0,
    0,    0,    131,  132,  231,  232,  233,  234,  235,  236,  237,  238,
    239,  240,  241,  242,  243,  244,  245,  116,  117,  0,    0,    0,
    0,    0,    0,    0,    115,  0,    1068, 118,  119,  0,    110,  61,
    111,  112,  120,  0,    0,    0,    113,  0,    114,  0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    121,  122,  123,  124,  125,  126,  0,    0,    0,    116,  117,
    0,    0,    0,    0,    0,    0,    115,  127,  128,  1070, 118,  119,
    110,  61,   111,  112,  0,    120,  0,    129,  113,  0,    114,  0,
    0,    0,    130,  0,    0,    0,    131,  132,  0,    0,    0,    0,
    0,    0,    0,    0,    121,  122,  123,  124,  125,  126,  0,    0,
    116,  117,  0,    0,    0,    0,    0,    0,    115,  0,    127,  128,
    118,  119,  0,    110,  61,   111,  112,  120,  0,    0,    129,  113,
    0,    114,  0,    0,    0,    130,  0,    0,    0,    131,  132,  0,
    0,    0,    0,    0,    0,    0,    121,  122,  123,  124,  125,  126,
    0,    0,    116,  117,  0,    0,    0,    0,    0,    0,    0,    115,
    127,  128,  118,  119,  0,    0,    0,    0,    0,    120,  0,    0,
    129,  1105, 0,    0,    0,    0,    0,    130,  0,    0,    0,    131,
    132,  0,    0,    0,    0,    0,    0,    0,    121,  122,  123,  124,
    125,  126,  0,    0,    0,    116,  117,  0,    0,    0,    0,    0,
    0,    0,    127,  128,  0,    118,  119,  110,  61,   111,  112,  0,
    120,  0,    129,  113,  0,    114,  0,    0,    0,    130,  0,    0,
    0,    131,  132,  0,    0,    0,    0,    0,    0,    0,    0,    121,
    122,  123,  124,  125,  126,  0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    115,  0,    127,  128,  0,    0,    0,    110,  61,
    111,  112,  0,    0,    0,    1044, 113,  0,    114,  0,    0,    0,
    130,  0,    0,    0,    131,  132,  0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    116,  117,  0,
    0,    0,    0,    0,    0,    0,    115,  0,    0,    118,  119,  0,
    0,    0,    0,    0,    120,  225,  226,  227,  228,  229,  230,  231,
    232,  233,  234,  235,  236,  237,  238,  239,  240,  241,  242,  243,
    244,  245,  0,    121,  122,  123,  124,  125,  126,  0,    0,    0,
    116,  117,  61,   0,    0,    0,    0,    0,    700,  127,  128,  0,
    118,  119,  0,    0,    0,    0,    0,    120,  0,    1183, 0,    0,
    0,    45,   46,   0,    130,  0,    0,    0,    131,  132,  0,    0,
    -110, -110, -110, 0,    0,    0,    121,  122,  123,  124,  125,  126,
    0,    0,    0,    0,    701,  0,    702,  0,    703,  704,  705,  0,
    127,  128,  0,    0,    -565, -565, -565, 0,    0,    0,    781,  0,
    1277, 782,  0,    0,    0,    0,    0,    130,  0,    0,    0,    131,
    132,  633,  634,  0,    0,    0,    0,    0,    0,    45,   46,   0,
    0,    0,    0,    118,  119,  706,  707,  708,  -110, -110, -110, 0,
    0,    0,    0,    0,    0,    0,    0,    628,  629,  630,  631,  632,
    0,    0,    0,    586,  0,    647,  0,    48,   0,    0,    783,  -482,
    -482, -482, -482, 0,    0,    49,   4,    50,   0,    648,  0,    0,
    541,  0,    0,    0,    0,    0,    0,    0,    0,    633,  634,  0,
    0,    0,    0,    0,    0,    709,  0,    544,  0,    0,    710,  0,
    0,    649,  0,    650,  28,   651,  0,    0,    0,    586,  0,    647,
    33,   652,  653,  654,  0,    -482, -482, -482, -482, 0,    655,  656,
    4,    657,  0,    0,    658,  0,    541,  0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    27,   6,    0,    0,    659,  0,    0,
    0,    544,  0,    0,    0,    0,    0,    649,  0,    650,  0,    651,
    0,    968,  0,    0,    0,    0,    0,    652,  653,  654,  0,    0,
    0,    0,    7,    0,    655,  656,  0,    657,  0,    0,    658,  586,
    0,    647,  0,    0,    0,    0,    0,    -482, -482, -482, -482, 27,
    6,    0,    4,    659,  1135, 0,    0,    0,    541,  0,    0,    0,
    0,    0,    660,  0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    544,  0,    0,    0,    0,    7,    649,  0,    650,
    0,    651,  0,    0,    0,    586,  0,    647,  0,    652,  653,  654,
    0,    -482, -482, -482, -482, 0,    655,  656,  4,    657,  0,    0,
    658,  872,  541,  0,    0,    0,    0,    0,    660,  0,    0,    0,
    0,    27,   6,    0,    0,    659,  0,    0,    0,    544,  0,    0,
    0,    0,    0,    649,  1108, 650,  0,    651,  0,    0,    0,    0,
    0,    0,    0,    652,  653,  654,  0,    0,    0,    0,    7,    0,
    655,  656,  0,    657,  0,    0,    658,  586,  0,    647,  0,    0,
    0,    0,    0,    -482, -482, -482, -482, 27,   6,    0,    4,    659,
    0,    0,    0,    0,    541,  0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    544,
    0,    0,    0,    0,    7,    0,    0,    650,  0,    651,  0,    0,
    0,    0,    0,    0,    0,    652,  653,  654,  781,  0,    0,    782,
    0,    0,    655,  656,  0,    657,  0,    0,    658,  0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    45,   46,   27,   6,    0,
    0,    659,  0,    0,    0,    0,    -110, -110, -110, 0,    0,    0,
    0,    0,    0,    0,    0,    628,  629,  630,  631,  632,  0,    0,
    0,    0,    0,    0,    0,    48,   7,    0,    783,  0,    0,    0,
    0,    0,    0,    49,   0,    50,   0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    633,  634,  0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    28,   0,    0,    0,    0,    0,    0,    0,    33,   219,
    220,  221,  222,  223,  224,  225,  226,  227,  228,  229,  230,  231,
    232,  233,  234,  235,  236,  237,  238,  239,  240,  241,  242,  243,
    244,  245,  0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    246,  219,  220,  221,  222,  223,  224,
    225,  226,  227,  228,  229,  230,  231,  232,  233,  234,  235,  236,
    237,  238,  239,  240,  241,  242,  243,  244,  245,  220,  221,  222,
    223,  224,  225,  226,  227,  228,  229,  230,  231,  232,  233,  234,
    235,  236,  237,  238,  239,  240,  241,  242,  243,  244,  245,  221,
    222,  223,  224,  225,  226,  227,  228,  229,  230,  231,  232,  233,
    234,  235,  236,  237,  238,  239,  240,  241,  242,  243,  244,  245,
    223,  224,  225,  226,  227,  228,  229,  230,  231,  232,  233,  234,
    235,  236,  237,  238,  239,  240,  241,  242,  243,  244,  245,  227,
    228,  229,  230,  231,  232,  233,  234,  235,  236,  237,  238,  239,
    240,  241,  242,  243,  244,  245};

static const yytype_int16 yycheck[] = {
    76,   448,  135,  133,  87,   856,  521,  573,  591,  573,  380,  573,
    6,    2,    142,  6,    573,  67,   970,  67,   474,  97,   11,   12,
    13,   14,   15,   16,   17,   18,   19,   474,  2,    20,   482,  105,
    41,   627,  133,  891,  4,    11,   12,   13,   14,   15,   16,   17,
    18,   19,   2,    32,   32,   129,  130,  360,  43,   656,  657,  11,
    12,   13,   14,   15,   16,   17,   18,   19,   648,  4,    375,  64,
    656,  4,    4,    2,    0,    20,   953,  379,  20,   606,  4,    4,
    11,   12,   13,   14,   15,   16,   17,   18,   19,   4,    4,    4,
    4,    91,   1037, 23,   91,   2,    4,    1042, 1127, 0,    93,   1223,
    95,   33,   11,   12,   13,   14,   15,   16,   17,   18,   19,   573,
    149,  150,  4,    493,  31,   609,  573,  32,   133,  158,  573,  207,
    14,   1249, 618,  56,   584,  133,  106,  215,  216,  217,  453,  139,
    149,  21,   114,  158,  443,  519,  93,   800,  163,  93,   56,   142,
    77,   78,   741,  808,  544,  83,   745,  32,   858,  158,  1041, 766,
    158,  162,  4,    95,   162,  1196, 1142, 755,  775,  871,  183,  160,
    160,  161,  766,  163,  162,  1137, 166,  165,  95,   116,  117,  101,
    102,  323,  121,  122,  123,  124,  125,  126,  127,  128,  166,  127,
    131,  132,  133,  134,  733,  285,  286,  287,  288,  209,  210,  779,
    311,  779,  518,  809,  525,  801,  1074, 163,  4,    160,  213,  162,
    158,  160,  358,  162,  162,  283,  310,  283,  312,  313,  314,  315,
    830,  194,  195,  196,  197,  198,  199,  200,  201,  1124, 159,  247,
    205,  206,  162,  160,  165,  168,  168,  161,  213,  166,  861,  957,
    958,  261,  1176, 578,  1236, 47,   869,  80,   101,  102,  270,  151,
    571,  47,   354,  355,  795,  47,   4,    5,    6,    161,  362,  4,
    14,   283,  159,  160,  219,  220,  221,  222,  223,  224,  225,  226,
    227,  228,  229,  230,  231,  232,  233,  234,  235,  236,  237,  238,
    239,  240,  241,  242,  243,  244,  323,  246,  5,    1260, 1261, 393,
    394,  779,  11,   323,  13,   1193, 108,  1182, 779,  4,    410,  4,
    779,  158,  108,  158,  166,  47,   108,  162,  165,  62,   63,   274,
    931,  158,  1044, 47,   91,   162,  953,  4,    1266, 91,   91,   92,
    959,  960,  1272, 65,   47,   1229, 1230, 38,   39,   38,   39,   358,
    158,  14,   15,   328,  329,  330,  331,  332,  333,  334,  335,  336,
    337,  338,  339,  340,  341,  342,  343,  344,  345,  346,  347,  348,
    349,  350,  351,  352,  353,  785,  108,  44,   45,   46,   128,  91,
    92,   857,  149,  150,  108,  489,  29,   149,  150,  722,  723,  34,
    35,   158,  498,  499,  158,  108,  810,  811,  21,   813,  383,  168,
    165,  48,   167,  1129, 149,  150,  5,    161,  158,  4,    1041, 150,
    11,   158,  13,   165,  375,  107,  157,  527,  836,  158,  21,   158,
    589,  158,  163,  448,  163,  162,  1026, 158,  1026, 32,   471,  162,
    109,  110,  111,  112,  113,  478,  38,   39,   40,   158,  448,  158,
    95,   165,  163,  167,  163,  474,  482,  563,  1054, 162,  1054, 162,
    1054, 56,   1077, 1078, 448,  1054, 8,    1082, 1083, 149,  150,  163,
    474,  492,  16,   17,   18,   19,   158,  159,  127,  23,   77,   78,
    154,  155,  601,  602,  161,  448,  474,  165,  158,  1124, 4,    158,
    1108, 163,  158,  458,  163,  158,  4,    163,  541,  158,  163,  492,
    27,   28,   163,  537,  8,    472,  537,  474,  918,  1111, 4,    158,
    16,   17,   18,   19,   163,  158,  158,  23,   8,    158,  163,  163,
    74,   935,  163,  643,  16,   17,   18,   19,   1026, 1153, 158,  23,
    581,  24,   162,  1026, 4,    711,  158,  1026, 573,  95,   162,  1281,
    584,  1283, 4,    55,   158,  160,  161,  164,  162,  158,  525,  166,
    1054, 162,  589,  573,  38,   39,   40,   1054, 1302, 168,  1304, 1054,
    168,  607,  11,   609,  607,  12,   609,  96,   97,   573,  155,  699,
    618,  62,   63,   618,  85,   86,   635,  95,   370,  371,  710,  622,
    589,  44,   45,   46,   627,  16,   17,   18,   19,   4,    573,  95,
    516,  517,  8,    27,   28,   155,  164,  582,  4,    1111, 16,   17,
    18,   19,   785,  161,  1111, 23,   4,    8,    1111, 622,  4,    747,
    573,  749,  627,  16,   17,   18,   19,   812,  756,  4,    23,   1050,
    1051, 61,   1269, 810,  811,  4,    813,  16,   17,   18,   19,   71,
    22,   73,   774,  161,  164,  4,    109,  110,  111,  112,  711,  12,
    1291, 66,   67,   68,   161,  836,  72,   711,  164,  793,  645,  717,
    718,  719,  1305, 1102, 1103, 161,  802,  169,  804,  805,  735,  72,
    161,  25,   161,  27,   28,   95,   30,   314,  315,  161,  14,   15,
    163,  821,  158,  163,  159,  4,    826,  161,  161,  159,  95,   163,
    832,  11,   12,   13,   14,   15,   16,   17,   18,   19,   159,  14,
    15,   61,   158,  847,  44,   45,   46,   47,   4,    853,  169,  71,
    161,  73,   858,  167,  167,  712,  164,  714,  167,  865,  779,  4,
    163,  4,    162,  871,  163,  44,   45,   46,   3,    4,    5,    6,
    4,    160,  164,  158,  11,   779,  13,   165,  84,   169,  161,  891,
    162,  893,  162,  162,  162,  158,  809,  164,  169,  901,  162,  779,
    904,  905,  162,  907,  908,  909,  910,  911,  108,  109,  110,  111,
    112,  160,  155,  830,  47,   149,  150,  151,  152,  153,  154,  155,
    779,  162,  930,  165,  809,  4,    4,    62,   63,   857,  109,  110,
    111,  112,  163,  4,    163,  945,  163,  947,  151,  152,  153,  154,
    155,  830,  779,  4,    158,  957,  958,  159,  27,   28,   89,   90,
    964,  161,  4,    160,  162,  158,  169,  38,   39,   40,   101,  102,
    976,  977,  978,  979,  165,  108,  49,   50,   51,   52,   53,   163,
    158,  4,    161,  97,   4,    162,  4,    846,  996,  164,  998,  162,
    1000, 162,  4,    161,  131,  132,  133,  134,  135,  136,  4,    14,
    15,   163,  165,  161,  4,    1017, 1018, 4,    87,   88,   149,  150,
    1024, 14,   15,   161,  161,  1029, 1030, 4,    4,    161,  161,  1035,
    161,  163,  170,  162,  161,  168,  45,   46,   1044, 172,  173,  36,
    161,  161,  1050, 1051, 41,   42,   43,   44,   45,   46,   987,  14,
    15,   3,    4,    5,    6,    161,  1066, 159,  1068, 11,   1070, 13,
    161,  159,  1074, 162,  1076, 158,  161,  1079, 77,   1081, 161,  161,
    161,  161,  161,  161,  4,    44,   45,   46,   93,   159,  83,   163,
    165,  93,   1098, 163,  93,   93,   1102, 1103, 162,  47,   109,  110,
    111,  112,  169,  158,  1112, 1026, 162,  1115, 161,  1117, 161,  161,
    109,  110,  111,  112,  161,  4,    159,  163,  162,  1129, 159,  4,
    1026, 165,  161,  1055, 161,  161,  161,  94,   162,  1054, 163,  162,
    4,    162,  162,  89,   90,   163,  1026, 106,  158,  162,  109,  110,
    111,  112,  161,  114,  1054, 4,    161,  163,  162,  4,    108,  162,
    159,  151,  4,    4,    161,  163,  54,   1026, 162,  158,  1054, 159,
    79,   165,  165,  1183, 163,  162,  1186, 162,  162,  131,  132,  133,
    134,  135,  136,  1195, 1253, 163,  1111, 162,  162,  1026, 159,  1054,
    162,  159,  161,  149,  150,  162,  1210, 9,    151,  144,  1214, 14,
    15,   1111, 159,  161,  158,  167,  4,    163,  1224, 161,  168,  162,
    162,  1054, 172,  173,  162,  162,  162,  1111, 163,  162,  57,   163,
    162,  158,  163,  163,  163,  44,   45,   46,   162,  144,  165,  144,
    1252, 144,  162,  162,  1256, 1257, 1258, 163,  1111, 163,  162,  69,
    1264, 163,  1277, 1267, 194,  195,  196,  197,  198,  199,  200,  201,
    163,  1277, 162,  205,  206,  1281, 1293, 1283, 159,  163,  1111, 163,
    57,   163,  162,  1142, 163,  161,  161,  69,   162,  1308, 161,  163,
    1300, 162,  1302, 162,  1304, 1316, 163,  163,  163,  1309, 109,  110,
    111,  112,  159,  165,  1327, 163,  162,  95,   1320, 162,  162,  158,
    162,  1336, 162,  93,   1339, 1329, 1341, 158,  1343, 165,  165,  1346,
    165,  1348, 162,  1350, 162,  1352, 1253, 1354, 158,  165,  162,  3,
    4,    5,    6,    162,  162,  162,  162,  11,   253,  13,   165,  162,
    162,  158,  161,  163,  158,  158,  158,  158,  158,  158,  525,  472,
    1223, 725,  162,  897,  1253, 534,  969,  50,   167,  37,   607,  359,
    169,  1236, 761,  582,  775,  1255, 512,  47,   766,  869,  958,  964,
    953,  158,  1249, 6,    1228, 474,  328,  329,  330,  331,  332,  333,
    334,  335,  336,  337,  338,  339,  340,  341,  342,  343,  344,  345,
    346,  347,  348,  349,  350,  351,  352,  353,  3,    4,    5,    6,
    882,  89,   90,   385,  11,   735,  13,   6,    396,  559,  1060, 280,
    479,  101,  102,  22,   1096, 78,   835,  830,  108,  1140, 474,  879,
    643,  383,  115,  116,  117,  118,  119,  120,  121,  122,  123,  124,
    125,  126,  846,  1005, 47,   921,  714,  131,  132,  133,  134,  135,
    136,  600,  1252, 901,  847,  142,  937,  62,   63,   1111, -1,   563,
    -1,   149,  150,  940,  474,  -1,   -1,   -1,   -1,   -1,   159,  -1,
    160,  161,  -1,   -1,   -1,   -1,   -1,   -1,   168,  -1,   89,   90,
    172,  173,  -1,   -1,   3,    4,    5,    6,    -1,   -1,   101,  102,
    11,   -1,   13,   -1,   -1,   108,  -1,   -1,   -1,   -1,   -1,   -1,
    -1,   3,    4,    5,    6,    -1,   8,    -1,   -1,   11,   -1,   13,
    -1,   -1,   -1,   -1,   131,  132,  133,  134,  135,  136,  -1,   -1,
    47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   149,  150,
    151,  -1,   -1,   62,   63,   -1,   -1,   -1,   -1,   47,   161,  -1,
    -1,   -1,   -1,   -1,   -1,   168,  -1,   -1,   -1,   172,  173,  -1,
    -1,   -1,   -1,   -1,   -1,   -1,   89,   90,   70,   -1,   -1,   -1,
    -1,   -1,   3,    4,    5,    6,    101,  102,  -1,   -1,   11,   -1,
    13,   108,  -1,   89,   90,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
    -1,   -1,   -1,   101,  102,  -1,   -1,   -1,   -1,   -1,   108,  -1,
    131,  132,  133,  134,  135,  136,  -1,   -1,   -1,   -1,   47,   -1,
    -1,   -1,   -1,   -1,   -1,   -1,   149,  150,  -1,   131,  132,  133,
    134,  135,  136,  -1,   -1,   -1,   161,  -1,   -1,   -1,   -1,   -1,
    -1,   168,  -1,   149,  150,  172,  173,  -1,   -1,   -1,   3,    4,
    5,    6,    -1,   161,  89,   90,   11,   -1,   13,   -1,   168,  -1,
    -1,   -1,   172,  173,  101,  102,  -1,   3,    4,    5,    6,    108,
    -1,   -1,   -1,   11,   -1,   13,   145,  146,  147,  148,  149,  150,
    151,  152,  153,  154,  155,  -1,   47,   -1,   -1,   -1,   131,  132,
    133,  134,  135,  136,  -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
    -1,   -1,   -1,   47,   149,  150,  -1,   -1,   -1,   -1,   -1,   -1,
    -1,   -1,   -1,   160,  161,  -1,   -1,   -1,   -1,   -1,   -1,   168,
    89,   90,   -1,   172,  173,  -1,   -1,   -1,   -1,   -1,   -1,   -1,
    101,  102,  -1,   3,    4,    5,    6,    108,  -1,   89,   90,   11,
    -1,   13,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   101,  102,  -1,
    -1,   -1,   -1,   128,  108,  -1,   131,  132,  133,  134,  135,  136,
    -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   47,
    149,  150,  -1,   131,  132,  133,  134,  135,  136,  -1,   -1,   -1,
    161,  -1,   -1,   -1,   -1,   -1,   -1,   168,  -1,   149,  150,  172,
    173,  -1,   -1,   -1,   -1,   3,    4,    5,    6,    161,  162,  -1,
    -1,   11,   -1,   13,   168,  89,   90,   -1,   172,  173,  -1,   -1,
    -1,   -1,   -1,   -1,   100,  101,  102,  -1,   -1,   -1,   -1,   -1,
    108,  -1,   -1,   -1,   -1,   91,   92,   -1,   -1,   -1,   -1,   -1,
    -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   131,
    132,  133,  134,  135,  136,  115,  116,  117,  118,  119,  120,  121,
    122,  123,  124,  125,  126,  149,  150,  -1,   3,    4,    5,    6,
    -1,   -1,   -1,   -1,   11,   161,  13,   89,   90,   -1,   -1,   -1,
    168,  -1,   -1,   -1,   172,  173,  100,  101,  102,  -1,   -1,   -1,
    -1,   159,  108,  -1,   162,  -1,   -1,   165,  -1,   -1,   -1,   -1,
    -1,   -1,   -1,   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
    -1,   131,  132,  133,  134,  135,  136,  115,  116,  117,  118,  119,
    120,  121,  122,  123,  124,  125,  126,  149,  150,  -1,   3,    4,
    5,    6,    -1,   -1,   -1,   -1,   11,   161,  13,   -1,   89,   90,
    -1,   -1,   168,  -1,   -1,   -1,   172,  173,  -1,   100,  101,  102,
    -1,   -1,   -1,   159,  -1,   108,  162,  -1,   -1,   165,  -1,   -1,
    -1,   -1,   -1,   -1,   -1,   -1,   47,   -1,   -1,   -1,   -1,   -1,
    -1,   -1,   -1,   -1,   131,  132,  133,  134,  135,  136,  115,  116,
    117,  118,  119,  120,  121,  122,  123,  124,  125,  126,  149,  150,
    -1,   3,    4,    5,    6,    -1,   -1,   -1,   -1,   11,   161,  13,
    89,   90,   -1,   -1,   -1,   168,  -1,   -1,   -1,   172,  173,  -1,
    101,  102,  -1,   -1,   -1,   -1,   159,  108,  -1,   162,  -1,   -1,
    165,  -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   47,   -1,   -1,
    -1,   -1,   -1,   -1,   -1,   -1,   131,  132,  133,  134,  135,  136,
    -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
    149,  150,  -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
    161,  162,  -1,   89,   90,   -1,   -1,   168,  -1,   -1,   -1,   172,
    173,  -1,   100,  101,  102,  3,    4,    5,    6,    -1,   108,  -1,
    -1,   11,   -1,   13,   -1,   -1,   115,  116,  117,  118,  119,  120,
    121,  122,  123,  124,  125,  126,  -1,   -1,   -1,   131,  132,  133,
    134,  135,  136,  -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
    -1,   47,   -1,   149,  150,  -1,   -1,   -1,   3,    4,    5,    6,
    -1,   -1,   159,  161,  11,   162,  13,   -1,   165,  -1,   168,  -1,
    -1,   -1,   172,  173,  141,  142,  143,  144,  145,  146,  147,  148,
    149,  150,  151,  152,  153,  154,  155,  89,   90,   -1,   -1,   -1,
    -1,   -1,   -1,   -1,   47,   -1,   100,  101,  102,  -1,   3,    4,
    5,    6,    108,  -1,   -1,   -1,   11,   -1,   13,   -1,   -1,   -1,
    -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
    -1,   131,  132,  133,  134,  135,  136,  -1,   -1,   -1,   89,   90,
    -1,   -1,   -1,   -1,   -1,   -1,   47,   149,  150,  100,  101,  102,
    3,    4,    5,    6,    -1,   108,  -1,   161,  11,   -1,   13,   -1,
    -1,   -1,   168,  -1,   -1,   -1,   172,  173,  -1,   -1,   -1,   -1,
    -1,   -1,   -1,   -1,   131,  132,  133,  134,  135,  136,  -1,   -1,
    89,   90,   -1,   -1,   -1,   -1,   -1,   -1,   47,   -1,   149,  150,
    101,  102,  -1,   3,    4,    5,    6,    108,  -1,   -1,   161,  11,
    -1,   13,   -1,   -1,   -1,   168,  -1,   -1,   -1,   172,  173,  -1,
    -1,   -1,   -1,   -1,   -1,   -1,   131,  132,  133,  134,  135,  136,
    -1,   -1,   89,   90,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   47,
    149,  150,  101,  102,  -1,   -1,   -1,   -1,   -1,   108,  -1,   -1,
    161,  162,  -1,   -1,   -1,   -1,   -1,   168,  -1,   -1,   -1,   172,
    173,  -1,   -1,   -1,   -1,   -1,   -1,   -1,   131,  132,  133,  134,
    135,  136,  -1,   -1,   -1,   89,   90,   -1,   -1,   -1,   -1,   -1,
    -1,   -1,   149,  150,  -1,   101,  102,  3,    4,    5,    6,    -1,
    108,  -1,   161,  11,   -1,   13,   -1,   -1,   -1,   168,  -1,   -1,
    -1,   172,  173,  -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   131,
    132,  133,  134,  135,  136,  -1,   -1,   -1,   -1,   -1,   -1,   -1,
    -1,   -1,   -1,   47,   -1,   149,  150,  -1,   -1,   -1,   3,    4,
    5,    6,    -1,   -1,   -1,   161,  11,   -1,   13,   -1,   -1,   -1,
    168,  -1,   -1,   -1,   172,  173,  -1,   -1,   -1,   -1,   -1,   -1,
    -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   89,   90,   -1,
    -1,   -1,   -1,   -1,   -1,   -1,   47,   -1,   -1,   101,  102,  -1,
    -1,   -1,   -1,   -1,   108,  135,  136,  137,  138,  139,  140,  141,
    142,  143,  144,  145,  146,  147,  148,  149,  150,  151,  152,  153,
    154,  155,  -1,   131,  132,  133,  134,  135,  136,  -1,   -1,   -1,
    89,   90,   4,    -1,   -1,   -1,   -1,   -1,   10,   149,  150,  -1,
    101,  102,  -1,   -1,   -1,   -1,   -1,   108,  -1,   161,  -1,   -1,
    -1,   27,   28,   -1,   168,  -1,   -1,   -1,   172,  173,  -1,   -1,
    38,   39,   40,   -1,   -1,   -1,   131,  132,  133,  134,  135,  136,
    -1,   -1,   -1,   -1,   54,   -1,   56,   -1,   58,   59,   60,   -1,
    149,  150,  -1,   -1,   66,   67,   68,   -1,   -1,   -1,   4,    -1,
    161,  7,    -1,   -1,   -1,   -1,   -1,   168,  -1,   -1,   -1,   172,
    173,  87,   88,   -1,   -1,   -1,   -1,   -1,   -1,   27,   28,   -1,
    -1,   -1,   -1,   101,  102,  103,  104,  105,  38,   39,   40,   -1,
    -1,   -1,   -1,   -1,   -1,   -1,   -1,   49,   50,   51,   52,   53,
    -1,   -1,   -1,   8,    -1,   10,   -1,   61,   -1,   -1,   64,   16,
    17,   18,   19,   -1,   -1,   71,   23,   73,   -1,   26,   -1,   -1,
    29,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   87,   88,   -1,
    -1,   -1,   -1,   -1,   -1,   163,  -1,   48,   -1,   -1,   168,  -1,
    -1,   54,   -1,   56,   106,  58,   -1,   -1,   -1,   8,    -1,   10,
    114,  66,   67,   68,   -1,   16,   17,   18,   19,   -1,   75,   76,
    23,   78,   -1,   -1,   81,   -1,   29,   -1,   -1,   -1,   -1,   -1,
    -1,   -1,   -1,   -1,   -1,   94,   95,   -1,   -1,   98,   -1,   -1,
    -1,   48,   -1,   -1,   -1,   -1,   -1,   54,   -1,   56,   -1,   58,
    -1,   163,  -1,   -1,   -1,   -1,   -1,   66,   67,   68,   -1,   -1,
    -1,   -1,   127,  -1,   75,   76,   -1,   78,   -1,   -1,   81,   8,
    -1,   10,   -1,   -1,   -1,   -1,   -1,   16,   17,   18,   19,   94,
    95,   -1,   23,   98,   99,   -1,   -1,   -1,   29,   -1,   -1,   -1,
    -1,   -1,   163,  -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
    -1,   -1,   -1,   48,   -1,   -1,   -1,   -1,   127,  54,   -1,   56,
    -1,   58,   -1,   -1,   -1,   8,    -1,   10,   -1,   66,   67,   68,
    -1,   16,   17,   18,   19,   -1,   75,   76,   23,   78,   -1,   -1,
    81,   82,   29,   -1,   -1,   -1,   -1,   -1,   163,  -1,   -1,   -1,
    -1,   94,   95,   -1,   -1,   98,   -1,   -1,   -1,   48,   -1,   -1,
    -1,   -1,   -1,   54,   55,   56,   -1,   58,   -1,   -1,   -1,   -1,
    -1,   -1,   -1,   66,   67,   68,   -1,   -1,   -1,   -1,   127,  -1,
    75,   76,   -1,   78,   -1,   -1,   81,   8,    -1,   10,   -1,   -1,
    -1,   -1,   -1,   16,   17,   18,   19,   94,   95,   -1,   23,   98,
    -1,   -1,   -1,   -1,   29,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
    -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   48,
    -1,   -1,   -1,   -1,   127,  -1,   -1,   56,   -1,   58,   -1,   -1,
    -1,   -1,   -1,   -1,   -1,   66,   67,   68,   4,    -1,   -1,   7,
    -1,   -1,   75,   76,   -1,   78,   -1,   -1,   81,   -1,   -1,   -1,
    -1,   -1,   -1,   -1,   -1,   -1,   -1,   27,   28,   94,   95,   -1,
    -1,   98,   -1,   -1,   -1,   -1,   38,   39,   40,   -1,   -1,   -1,
    -1,   -1,   -1,   -1,   -1,   49,   50,   51,   52,   53,   -1,   -1,
    -1,   -1,   -1,   -1,   -1,   61,   127,  -1,   64,   -1,   -1,   -1,
    -1,   -1,   -1,   71,   -1,   73,   -1,   -1,   -1,   -1,   -1,   -1,
    -1,   -1,   -1,   -1,   -1,   -1,   -1,   87,   88,   -1,   -1,   -1,
    -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
    -1,   -1,   106,  -1,   -1,   -1,   -1,   -1,   -1,   -1,   114,  129,
    130,  131,  132,  133,  134,  135,  136,  137,  138,  139,  140,  141,
    142,  143,  144,  145,  146,  147,  148,  149,  150,  151,  152,  153,
    154,  155,  -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
    -1,   -1,   -1,   -1,   -1,   171,  129,  130,  131,  132,  133,  134,
    135,  136,  137,  138,  139,  140,  141,  142,  143,  144,  145,  146,
    147,  148,  149,  150,  151,  152,  153,  154,  155,  130,  131,  132,
    133,  134,  135,  136,  137,  138,  139,  140,  141,  142,  143,  144,
    145,  146,  147,  148,  149,  150,  151,  152,  153,  154,  155,  131,
    132,  133,  134,  135,  136,  137,  138,  139,  140,  141,  142,  143,
    144,  145,  146,  147,  148,  149,  150,  151,  152,  153,  154,  155,
    133,  134,  135,  136,  137,  138,  139,  140,  141,  142,  143,  144,
    145,  146,  147,  148,  149,  150,  151,  152,  153,  154,  155,  137,
    138,  139,  140,  141,  142,  143,  144,  145,  146,  147,  148,  149,
    150,  151,  152,  153,  154,  155};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int16 yystos[] = {
    0,   175, 176, 0,   23,  33,  95,  127, 177, 178, 179, 181, 189, 208, 213,
    218, 251, 320, 322, 376, 182, 214, 14,  15,  44,  45,  46,  94,  106, 109,
    110, 111, 112, 114, 161, 188, 241, 242, 330, 344, 346, 377, 378, 219, 25,
    27,  28,  30,  61,  71,  73,  180, 177, 177, 177, 177, 177, 177, 177, 177,
    177, 4,   184, 185, 186, 187, 4,   331, 14,  47,  108, 260, 47,  108, 261,
    345, 166, 243, 245, 4,   14,  375, 187, 222, 225, 190, 321, 323, 209, 4,
    65,  265, 265, 21,  183, 158, 32,  159, 160, 215, 44,  45,  241, 332, 333,
    334, 162, 107, 347, 348, 3,   5,   6,   11,  13,  47,  89,  90,  101, 102,
    108, 131, 132, 133, 134, 135, 136, 149, 150, 161, 168, 172, 173, 187, 424,
    455, 478, 479, 482, 375, 163, 220, 165, 223, 4,   47,  84,  108, 188, 312,
    313, 314, 316, 317, 318, 319, 333, 334, 319, 4,   4,   159, 165, 44,  45,
    113, 188, 241, 258, 259, 4,   184, 24,  186, 4,   478, 4,   164, 192, 168,
    47,  108, 335, 335, 47,  108, 349, 168, 11,  12,  155, 178, 178, 155, 178,
    178, 178, 178, 178, 178, 178, 178, 478, 478, 481, 178, 178, 166, 178, 243,
    244, 245, 482, 178, 424, 91,  92,  165, 167, 129, 130, 131, 132, 133, 134,
    135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149,
    150, 151, 152, 153, 154, 155, 171, 244, 245, 246, 4,   224, 225, 191, 245,
    315, 312, 4,   324, 325, 326, 312, 324, 210, 252, 4,   4,   4,   260, 4,
    261, 255, 22,  161, 161, 199, 4,   336, 337, 245, 350, 351, 355, 356, 12,
    161, 161, 161, 161, 455, 479, 479, 479, 479, 479, 479, 479, 479, 115, 116,
    117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 159, 162, 165, 423, 158,
    168, 169, 479, 479, 478, 480, 243, 243, 187, 455, 478, 478, 478, 178, 178,
    178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178,
    178, 178, 178, 178, 178, 178, 178, 178, 178, 161, 178, 163, 221, 158, 192,
    158, 163, 159, 243, 163, 163, 161, 263, 4,   159, 256, 257, 243, 161, 266,
    193, 4,   160, 162, 178, 200, 203, 163, 159, 338, 158, 201, 169, 351, 4,
    352, 353, 188, 333, 334, 357, 358, 360, 478, 478, 478, 478, 478, 482, 478,
    478, 478, 481, 481, 167, 161, 167, 167, 167, 479, 479, 479, 479, 479, 479,
    479, 479, 479, 479, 479, 479, 479, 479, 479, 479, 479, 479, 479, 479, 479,
    479, 479, 479, 479, 479, 478, 478, 164, 393, 225, 199, 325, 478, 211, 4,
    262, 264, 163, 253, 4,   266, 266, 162, 267, 163, 178, 195, 196, 325, 204,
    205, 160, 229, 230, 231, 232, 233, 158, 201, 216, 479, 337, 169, 354, 158,
    163, 335, 335, 346, 162, 162, 162, 162, 162, 165, 162, 169, 160, 459, 460,
    461, 478, 162, 165, 161, 4,   385, 389, 163, 31,  178, 212, 251, 320, 322,
    376, 4,   158, 162, 263, 4,   163, 163, 178, 269, 270, 272, 273, 27,  28,
    158, 194, 159, 202, 4,   160, 38,  39,  40,  234, 96,  97,  236, 245, 203,
    162, 29,  34,  35,  48,  178, 217, 218, 320, 322, 327, 363, 372, 376, 401,
    412, 245, 353, 246, 359, 478, 169, 455, 158, 201, 478, 160, 394, 395, 478,
    386, 158, 163, 247, 165, 409, 262, 163, 254, 273, 273, 233, 158, 201, 4,
    371, 8,   74,  164, 178, 181, 227, 320, 322, 363, 376, 410, 420, 422, 426,
    439, 197, 198, 196, 162, 478, 206, 236, 97,  36,  41,  42,  43,  44,  45,
    83,  188, 238, 239, 240, 241, 4,   245, 328, 329, 4,   227, 228, 49,  50,
    51,  52,  53,  87,  88,  233, 399, 400, 347, 162, 461, 162, 4,   158, 162,
    161, 385, 10,  26,  54,  56,  58,  66,  67,  68,  75,  76,  78,  81,  98,
    163, 178, 181, 218, 248, 249, 251, 274, 284, 287, 320, 322, 327, 330, 341,
    342, 363, 372, 376, 379, 401, 410, 421, 440, 466, 472, 4,   263, 72,  72,
    245, 270, 268, 246, 165, 4,   5,   6,   128, 161, 10,  54,  56,  58,  59,
    60,  103, 104, 105, 163, 168, 187, 424, 455, 456, 426, 16,  17,  18,  19,
    163, 440, 319, 319, 202, 237, 238, 44,  240, 47,  108, 235, 240, 207, 455,
    158, 163, 413, 373, 374, 456, 405, 4,   4,   245, 404, 402, 161, 395, 178,
    396, 397, 398, 471, 409, 473, 161, 161, 4,   56,  275, 276, 278, 4,   56,
    77,  285, 286, 287, 291, 292, 293, 305, 310, 166, 288, 289, 290, 310, 462,
    4,   4,   7,   64,  344, 382, 4,   339, 340, 16,  17,  18,  19,  161, 163,
    271, 162, 128, 226, 478, 428, 429, 161, 161, 161, 161, 457, 478, 427, 178,
    142, 159, 178, 423, 20,  93,  411, 411, 411, 163, 411, 161, 325, 325, 235,
    202, 159, 329, 161, 415, 158, 163, 159, 426, 366, 367, 364, 426, 170, 406,
    162, 478, 37,  160, 478, 162, 158, 161, 458, 409, 478, 467, 161, 161, 77,
    275, 161, 311, 161, 77,  286, 161, 297, 298, 163, 161, 308, 478, 289, 158,
    163, 159, 82,  178, 248, 463, 466, 472, 163, 380, 343, 228, 158, 163, 93,
    93,  93,  93,  478, 371, 162, 158, 162, 165, 458, 409, 478, 431, 478, 478,
    169, 158, 458, 456, 228, 228, 424, 228, 161, 161, 161, 161, 478, 478, 4,
    38,  39,  162, 416, 417, 418, 419, 414, 374, 478, 368, 368, 228, 21,  151,
    161, 403, 162, 4,   398, 459, 163, 474, 162, 4,   83,  464, 465, 62,  63,
    281, 478, 281, 246, 309, 478, 62,  63,  296, 299, 303, 310, 159, 159, 62,
    63,  306, 307, 309, 165, 163, 290, 309, 163, 250, 393, 4,   361, 362, 381,
    340, 161, 161, 161, 161, 162, 226, 478, 163, 430, 162, 4,   231, 425, 162,
    162, 457, 163, 478, 478, 478, 100, 478, 100, 478, 100, 478, 162, 478, 162,
    158, 201, 418, 163, 158, 163, 163, 369, 370, 371, 162, 22,  62,  63,  151,
    407, 408, 478, 426, 161, 201, 462, 469, 464, 159, 163, 478, 162, 478, 162,
    165, 299, 303, 151, 149, 150, 158, 300, 302, 161, 294, 309, 294, 310, 310,
    80,  158, 162, 308, 247, 4,   383, 384, 385, 387, 158, 163, 384, 390, 391,
    392, 100, 478, 100, 478, 100, 478, 478, 470, 158, 162, 165, 442, 436, 159,
    245, 163, 434, 435, 478, 162, 478, 162, 478, 162, 163, 162, 438, 417, 162,
    4,   158, 365, 159, 478, 478, 162, 64,  158, 162, 162, 478, 162, 55,  472,
    475, 476, 159, 478, 478, 79,  279, 9,   478, 151, 159, 302, 144, 303, 158,
    302, 159, 309, 478, 158, 295, 306, 306, 163, 167, 99,  243, 158, 163, 362,
    158, 163, 161, 478, 162, 478, 162, 478, 162, 162, 449, 226, 478, 55,  426,
    426, 478, 4,   478, 426, 426, 162, 163, 162, 163, 162, 163, 163, 441, 370,
    163, 478, 408, 408, 162, 409, 57,  477, 463, 478, 468, 478, 158, 161, 280,
    478, 165, 144, 144, 301, 304, 310, 303, 144, 295, 165, 309, 388, 384, 391,
    396, 162, 163, 162, 163, 162, 163, 163, 69,  450, 451, 162, 409, 437, 159,
    432, 163, 163, 163, 85,  86,  445, 475, 163, 281, 478, 162, 478, 161, 161,
    158, 162, 304, 162, 478, 295, 161, 162, 163, 163, 163, 8,   70,  453, 454,
    478, 57,  443, 478, 163, 69,  446, 447, 178, 422, 478, 91,  92,  165, 159,
    301, 304, 304, 162, 165, 396, 452, 158, 165, 444, 422, 453, 162, 279, 478,
    478, 478, 161, 282, 283, 478, 91,  302, 91,  302, 478, 162, 475, 478, 426,
    162, 448, 475, 158, 162, 162, 162, 283, 478, 163, 165, 309, 165, 309, 165,
    433, 426, 283, 158, 165, 478, 162, 309, 162, 309, 426, 158, 277, 283, 478,
    165, 162, 162, 162, 162, 283, 162, 158, 162, 165, 478, 162, 162, 163, 283,
    478, 158, 162, 283, 158, 283, 158, 283, 158, 162, 283, 158, 283, 158, 283,
    158, 283, 158, 283, 158, 283, 162};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int16 yyr1[] = {
    0,   174, 176, 175, 177, 177, 177, 177, 177, 177, 177, 177, 177, 177, 179,
    178, 180, 180, 182, 183, 181, 184, 184, 185, 185, 186, 186, 187, 187, 187,
    188, 188, 188, 190, 191, 189, 193, 194, 192, 192, 195, 195, 196, 197, 196,
    198, 196, 196, 199, 199, 199, 200, 200, 201, 201, 202, 202, 204, 203, 205,
    206, 203, 207, 203, 203, 209, 210, 208, 211, 211, 212, 212, 212, 212, 214,
    215, 213, 216, 216, 217, 217, 217, 217, 217, 217, 217, 217, 217, 219, 220,
    221, 218, 222, 223, 223, 224, 224, 225, 226, 226, 227, 227, 227, 227, 227,
    227, 227, 227, 228, 228, 230, 229, 232, 231, 233, 233, 234, 234, 234, 235,
    235, 235, 236, 236, 236, 236, 237, 237, 238, 238, 238, 238, 238, 238, 238,
    238, 239, 239, 239, 240, 240, 240, 241, 241, 241, 241, 241, 242, 242, 243,
    243, 243, 243, 244, 244, 245, 245, 246, 246, 247, 247, 247, 247, 247, 248,
    248, 248, 248, 248, 248, 248, 248, 248, 248, 248, 248, 248, 248, 248, 248,
    248, 248, 248, 250, 249, 252, 251, 253, 251, 254, 251, 255, 251, 256, 251,
    257, 251, 258, 258, 258, 258, 259, 259, 259, 260, 260, 260, 261, 261, 261,
    262, 262, 263, 263, 264, 264, 264, 264, 265, 265, 266, 266, 267, 268, 266,
    269, 269, 271, 270, 272, 270, 273, 273, 274, 275, 275, 276, 276, 277, 277,
    278, 278, 279, 279, 280, 280, 280, 280, 281, 281, 281, 282, 282, 282, 282,
    282, 283, 283, 284, 284, 285, 285, 286, 286, 286, 287, 287, 288, 289, 289,
    290, 291, 291, 292, 293, 293, 294, 294, 294, 295, 295, 296, 296, 297, 297,
    297, 298, 298, 298, 299, 299, 300, 300, 301, 301, 302, 302, 302, 303, 304,
    305, 306, 306, 306, 307, 307, 307, 308, 309, 309, 311, 310, 312, 312, 312,
    313, 314, 315, 316, 317, 317, 318, 319, 319, 319, 319, 319, 321, 320, 323,
    322, 324, 324, 325, 325, 326, 327, 328, 328, 329, 331, 330, 332, 332, 332,
    333, 334, 334, 335, 335, 335, 336, 336, 337, 338, 338, 339, 339, 340, 341,
    343, 342, 345, 344, 346, 346, 347, 348, 348, 349, 349, 349, 350, 350, 351,
    352, 352, 354, 353, 356, 355, 357, 357, 358, 359, 357, 360, 360, 361, 361,
    362, 364, 365, 363, 366, 363, 367, 363, 368, 368, 369, 369, 370, 370, 371,
    372, 373, 373, 374, 375, 375, 376, 376, 377, 377, 377, 378, 378, 380, 379,
    381, 379, 382, 382, 383, 383, 384, 384, 386, 385, 388, 387, 389, 389, 390,
    390, 391, 392, 391, 393, 393, 394, 394, 395, 395, 395, 395, 396, 397, 397,
    398, 398, 398, 398, 398, 398, 399, 399, 400, 400, 402, 403, 401, 404, 401,
    405, 401, 406, 406, 406, 406, 406, 406, 407, 407, 407, 408, 408, 408, 409,
    409, 410, 410, 411, 411, 411, 413, 414, 412, 415, 415, 416, 416, 417, 417,
    418, 419, 419, 420, 420, 420, 420, 420, 420, 420, 420, 420, 421, 421, 421,
    421, 421, 421, 421, 422, 422, 422, 422, 422, 423, 423, 423, 423, 423, 423,
    423, 423, 423, 423, 423, 423, 424, 424, 425, 425, 425, 426, 426, 426, 426,
    426, 426, 426, 426, 426, 427, 426, 428, 426, 429, 430, 426, 431, 432, 433,
    426, 434, 426, 435, 426, 436, 437, 426, 438, 426, 439, 439, 439, 439, 440,
    440, 440, 441, 441, 441, 442, 442, 444, 443, 443, 445, 445, 447, 448, 446,
    449, 449, 451, 452, 450, 453, 453, 454, 454, 454, 454, 455, 455, 455, 456,
    456, 457, 457, 458, 458, 459, 459, 460, 460, 461, 462, 462, 462, 463, 463,
    463, 464, 465, 465, 465, 467, 468, 466, 469, 466, 470, 466, 471, 466, 473,
    474, 472, 476, 475, 475, 477, 477, 478, 478, 478, 478, 479, 479, 479, 479,
    479, 479, 480, 479, 479, 479, 479, 479, 479, 479, 479, 479, 479, 479, 479,
    479, 479, 479, 479, 479, 479, 479, 479, 479, 479, 479, 479, 479, 479, 479,
    479, 479, 479, 479, 479, 479, 479, 479, 479, 479, 479, 479, 479, 479, 479,
    479, 479, 479, 479, 479, 479, 481, 481, 482, 482, 482, 482};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.
 */
static const yytype_int8 yyr2[] = {
    0, 2,  0,  2,  2, 2,  2,  2,  2, 2,  2, 2,  2,  0,  0,  2,  4, 0, 0, 0, 5,
    1, 0,  1,  3,  1, 3,  1,  3,  3, 1,  1, 3,  0,  0,  11, 0,  0, 6, 0, 1, 3,
    0, 0,  5,  0,  5, 1,  2,  0,  4, 1,  3, 1,  0,  2,  0,  0,  3, 0, 0, 5, 0,
    6, 3,  0,  0,  9, 2,  0,  1,  1, 1,  1, 0,  0,  9,  2,  0,  1, 1, 1, 1, 1,
    1, 1,  1,  1,  0, 0,  0,  9,  2, 2,  0, 1,  3,  1,  1,  5,  2, 2, 2, 2, 4,
    4, 6,  8,  1,  0, 0,  5,  0,  4, 1,  1, 1,  1,  1,  1,  1,  0, 2, 1, 1, 0,
    1, 0,  1,  2,  1, 2,  1,  2,  1, 1,  1, 1,  1,  1,  1,  1,  1, 1, 1, 1, 1,
    1, 1,  5,  5,  5, 3,  2,  2,  1, 0,  1, 1,  2,  2,  2,  2,  0, 1, 1, 1, 1,
    1, 1,  1,  1,  1, 1,  1,  1,  1, 1,  3, 1,  1,  1,  1,  0,  6, 0, 7, 0, 9,
    0, 11, 0,  9,  0, 10, 0,  10, 1, 2,  3, 2,  0,  1,  1,  0,  1, 1, 0, 1, 1,
    2, 1,  3,  0,  3, 2,  1,  0,  1, 0,  2, 0,  0,  0,  6,  1,  3, 0, 5, 0, 2,
    2, 0,  3,  2,  0, 10, 14, 2,  0, 4,  0, 2,  0,  1,  5,  5,  5, 1, 1, 0, 1,
    5, 7,  13, 25, 1, 5,  3,  2,  2, 1,  1, 1,  1,  3,  4,  5,  1, 3, 3, 4, 0,
    2, 4,  4,  4,  1, 2,  2,  3,  1, 1,  7, 12, 11, 6,  12, 11, 2, 3, 2, 3, 1,
    3, 1,  1,  0,  1, 1,  5,  2,  2, 1,  1, 3,  3,  1,  1,  5,  0, 3, 1, 1, 0,
    1, 1,  1,  2,  2, 3,  2,  1,  1, 1,  1, 1,  0,  6,  0,  6,  1, 3, 3, 1, 1,
    3, 1,  3,  4,  0, 7,  2,  3,  0, 1,  1, 1,  1,  1,  0,  1,  3, 2, 2, 0, 1,
    3, 1,  3,  0,  5, 0,  3,  1,  1, 4,  2, 0,  1,  1,  0,  1,  2, 3, 1, 3, 0,
    3, 0,  2,  1,  1, 0,  0,  4,  2, 3,  1, 3,  1,  0,  0,  8,  0, 6, 0, 6, 0,
    3, 1,  3,  1,  3, 2,  4,  1,  3, 3,  1, 1,  6,  4,  1,  2,  2, 1, 1, 0, 6,
    0, 6,  1,  1,  1, 3,  1,  1,  0, 5,  0, 6,  1,  3,  1,  3,  1, 0, 4, 4, 0,
    1, 3,  0,  1,  4, 5,  1,  1,  3, 1,  2, 6,  5,  3,  2,  1,  1, 1, 1, 0, 0,
    6, 0,  4,  0,  4, 4,  4,  3,  3, 2,  0, 1,  3,  3,  2,  2,  1, 2, 0, 2, 0,
    1, 1,  0,  0,  0, 6,  2,  4,  1, 3,  2, 1,  1,  1,  1,  7,  7, 8, 8, 7, 6,
    3, 7,  8,  7,  7, 8,  8,  7,  7, 8,  5, 4,  4,  5,  5,  1,  1, 1, 1, 1, 1,
    1, 1,  1,  1,  1, 1,  1,  1,  3, 3,  5, 1,  1,  1,  1,  1,  1, 2, 2, 2, 0,
    5, 0,  5,  0,  0, 8,  0,  0,  0, 13, 0, 7,  0,  7,  0,  0,  9, 0, 9, 1, 2,
    2, 2,  1,  1,  1, 2,  2,  0,  2, 0,  0, 3,  0,  2,  0,  0,  0, 4, 2, 0, 0,
    0, 4,  2,  1,  1, 1,  1,  3,  6, 2,  2, 1,  3,  1,  3,  4,  0, 1, 0, 1, 3,
    1, 2,  2,  0,  1, 1,  2,  1,  2, 4,  3, 0,  0,  11, 0,  7,  0, 7, 0, 4, 0,
    0, 7,  0,  2,  1, 2,  0,  1,  6, 3,  2, 1,  4,  2,  1,  1,  1, 0, 7, 5, 5,
    3, 7,  3,  6,  3, 4,  4,  4,  4, 4,  4, 3,  3,  3,  3,  3,  3, 4, 4, 4, 4,
    4, 4,  4,  4,  4, 4,  4,  4,  4, 4,  4, 4,  4,  4,  3,  3,  4, 4, 3, 5, 5,
    5, 5,  5,  1,  3, 1,  1,  2,  3};

enum { YYENOMEM = -2 };

#define yyerrok (yyerrstatus = 0)
#define yyclearin (yychar = FRONTEND_VERILOG_YYEMPTY)

#define YYACCEPT goto yyacceptlab
#define YYABORT goto yyabortlab
#define YYERROR goto yyerrorlab
#define YYNOMEM goto yyexhaustedlab

#define YYRECOVERING() (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                                 \
  do                                                                           \
    if (yychar == FRONTEND_VERILOG_YYEMPTY) {                                  \
      yychar = (Token);                                                        \
      yylval = (Value);                                                        \
      YYPOPSTACK(yylen);                                                       \
      yystate = *yyssp;                                                        \
      YY_LAC_DISCARD("YYBACKUP");                                              \
      goto yybackup;                                                           \
    } else {                                                                   \
      yyerror(YY_("syntax error: cannot back up"));                            \
      YYERROR;                                                                 \
    }                                                                          \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use FRONTEND_VERILOG_YYerror or FRONTEND_VERILOG_YYUNDEF. */
#define YYERRCODE FRONTEND_VERILOG_YYUNDEF

/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
#define YYLLOC_DEFAULT(Current, Rhs, N)                                        \
  do                                                                           \
    if (N) {                                                                   \
      (Current).first_line = YYRHSLOC(Rhs, 1).first_line;                      \
      (Current).first_column = YYRHSLOC(Rhs, 1).first_column;                  \
      (Current).last_line = YYRHSLOC(Rhs, N).last_line;                        \
      (Current).last_column = YYRHSLOC(Rhs, N).last_column;                    \
    } else {                                                                   \
      (Current).first_line = (Current).last_line = YYRHSLOC(Rhs, 0).last_line; \
      (Current).first_column = (Current).last_column =                         \
          YYRHSLOC(Rhs, 0).last_column;                                        \
    }                                                                          \
  while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])

/* Enable debugging if requested.  */
#if FRONTEND_VERILOG_YYDEBUG

#ifndef YYFPRINTF
#include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#define YYFPRINTF fprintf
#endif

#define YYDPRINTF(Args)                                                        \
  do {                                                                         \
    if (yydebug)                                                               \
      YYFPRINTF Args;                                                          \
  } while (0)

/* YYLOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YYLOCATION_PRINT

#if defined YY_LOCATION_PRINT

/* Temporary convenience wrapper in case some people defined the
   undocumented and private YY_LOCATION_PRINT macros.  */
#define YYLOCATION_PRINT(File, Loc) YY_LOCATION_PRINT(File, *(Loc))

#elif defined FRONTEND_VERILOG_YYLTYPE_IS_TRIVIAL &&                           \
    FRONTEND_VERILOG_YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static int yy_location_print_(FILE *yyo, YYLTYPE const *const yylocp) {
  int res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line) {
    res += YYFPRINTF(yyo, "%d", yylocp->first_line);
    if (0 <= yylocp->first_column)
      res += YYFPRINTF(yyo, ".%d", yylocp->first_column);
  }
  if (0 <= yylocp->last_line) {
    if (yylocp->first_line < yylocp->last_line) {
      res += YYFPRINTF(yyo, "-%d", yylocp->last_line);
      if (0 <= end_col)
        res += YYFPRINTF(yyo, ".%d", end_col);
    } else if (0 <= end_col && yylocp->first_column < end_col)
      res += YYFPRINTF(yyo, "-%d", end_col);
  }
  return res;
}

#define YYLOCATION_PRINT yy_location_print_

/* Temporary convenience wrapper in case some people defined the
   undocumented and private YY_LOCATION_PRINT macros.  */
#define YY_LOCATION_PRINT(File, Loc) YYLOCATION_PRINT(File, &(Loc))

#else

#define YYLOCATION_PRINT(File, Loc) ((void)0)
/* Temporary convenience wrapper in case some people defined the
   undocumented and private YY_LOCATION_PRINT macros.  */
#define YY_LOCATION_PRINT YYLOCATION_PRINT

#endif
#endif /* !defined YYLOCATION_PRINT */

#define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                          \
  do {                                                                         \
    if (yydebug) {                                                             \
      YYFPRINTF(stderr, "%s ", Title);                                         \
      yy_symbol_print(stderr, Kind, Value, Location);                          \
      YYFPRINTF(stderr, "\n");                                                 \
    }                                                                          \
  } while (0)

/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void yy_symbol_value_print(FILE *yyo, yysymbol_kind_t yykind,
                                  YYSTYPE const *const yyvaluep,
                                  YYLTYPE const *const yylocationp) {
  FILE *yyoutput = yyo;
  YY_USE(yyoutput);
  YY_USE(yylocationp);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE(yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}

/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void yy_symbol_print(FILE *yyo, yysymbol_kind_t yykind,
                            YYSTYPE const *const yyvaluep,
                            YYLTYPE const *const yylocationp) {
  YYFPRINTF(yyo, "%s %s (", yykind < YYNTOKENS ? "token" : "nterm",
            yysymbol_name(yykind));

  YYLOCATION_PRINT(yyo, yylocationp);
  YYFPRINTF(yyo, ": ");
  yy_symbol_value_print(yyo, yykind, yyvaluep, yylocationp);
  YYFPRINTF(yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void yy_stack_print(yy_state_t *yybottom, yy_state_t *yytop) {
  YYFPRINTF(stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++) {
    int yybot = *yybottom;
    YYFPRINTF(stderr, " %d", yybot);
  }
  YYFPRINTF(stderr, "\n");
}

#define YY_STACK_PRINT(Bottom, Top)                                            \
  do {                                                                         \
    if (yydebug)                                                               \
      yy_stack_print((Bottom), (Top));                                         \
  } while (0)

/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void yy_reduce_print(yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp,
                            int yyrule) {
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF(stderr, "Reducing stack by rule %d (line %d):\n", yyrule - 1,
            yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++) {
    YYFPRINTF(stderr, "   $%d = ", yyi + 1);
    yy_symbol_print(stderr, YY_ACCESSING_SYMBOL(+yyssp[yyi + 1 - yynrhs]),
                    &yyvsp[(yyi + 1) - (yynrhs)],
                    &(yylsp[(yyi + 1) - (yynrhs)]));
    YYFPRINTF(stderr, "\n");
  }
}

#define YY_REDUCE_PRINT(Rule)                                                  \
  do {                                                                         \
    if (yydebug)                                                               \
      yy_reduce_print(yyssp, yyvsp, yylsp, Rule);                              \
  } while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !FRONTEND_VERILOG_YYDEBUG */
#define YYDPRINTF(Args) ((void)0)
#define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
#define YY_STACK_PRINT(Bottom, Top)
#define YY_REDUCE_PRINT(Rule)
#endif /* !FRONTEND_VERILOG_YYDEBUG */

/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
#define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
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
static int yy_lac_stack_realloc(YYPTRDIFF_T *yycapacity, YYPTRDIFF_T yyadd,
#if FRONTEND_VERILOG_YYDEBUG
                                char const *yydebug_prefix,
                                char const *yydebug_suffix,
#endif
                                yy_state_t **yybottom,
                                yy_state_t *yybottom_no_free,
                                yy_state_t **yytop, yy_state_t *yytop_empty) {
  YYPTRDIFF_T yysize_old = *yytop == yytop_empty ? 0 : *yytop - *yybottom + 1;
  YYPTRDIFF_T yysize_new = yysize_old + yyadd;
  if (*yycapacity < yysize_new) {
    YYPTRDIFF_T yyalloc = 2 * yysize_new;
    yy_state_t *yybottom_new;
    /* Use YYMAXDEPTH for maximum stack size given that the stack
       should never need to grow larger than the main state stack
       needs to grow without LAC.  */
    if (YYMAXDEPTH < yysize_new) {
      YYDPRINTF(
          (stderr, "%smax size exceeded%s", yydebug_prefix, yydebug_suffix));
      return YYENOMEM;
    }
    if (YYMAXDEPTH < yyalloc)
      yyalloc = YYMAXDEPTH;
    yybottom_new = YY_CAST(
        yy_state_t *,
        YYSTACK_ALLOC(YY_CAST(YYSIZE_T, yyalloc * YYSIZEOF(*yybottom_new))));
    if (!yybottom_new) {
      YYDPRINTF((stderr, "%srealloc failed%s", yydebug_prefix, yydebug_suffix));
      return YYENOMEM;
    }
    if (*yytop != yytop_empty) {
      YYCOPY(yybottom_new, *yybottom, yysize_old);
      *yytop = yybottom_new + (yysize_old - 1);
    }
    if (*yybottom != yybottom_no_free)
      YYSTACK_FREE(*yybottom);
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
#define YY_LAC_ESTABLISH                                                       \
  do {                                                                         \
    if (!yy_lac_established) {                                                 \
      YYDPRINTF((stderr, "LAC: initial context established for %s\n",          \
                 yysymbol_name(yytoken)));                                     \
      yy_lac_established = 1;                                                  \
      switch (yy_lac(yyesa, &yyes, &yyes_capacity, yyssp, yytoken)) {          \
      case YYENOMEM:                                                           \
        YYNOMEM;                                                               \
      case 1:                                                                  \
        goto yyerrlab;                                                         \
      }                                                                        \
    }                                                                          \
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
#define YY_LAC_DISCARD(Event)                                                  \
  do {                                                                         \
    if (yy_lac_established) {                                                  \
      YYDPRINTF(                                                               \
          (stderr, "LAC: initial context discarded due to " Event "\n"));      \
      yy_lac_established = 0;                                                  \
    }                                                                          \
  } while (0)
#else
#define YY_LAC_DISCARD(Event) yy_lac_established = 0
#endif

/* Given the stack whose top is *YYSSP, return 0 iff YYTOKEN can
   eventually (after perhaps some reductions) be shifted, return 1 if
   not, or return YYENOMEM if memory is exhausted.  As preconditions and
   postconditions: *YYES_CAPACITY is the allocated size of the array to
   which *YYES points, and either *YYES = YYESA or *YYES points to an
   array allocated with YYSTACK_ALLOC.  yy_lac may overwrite the
   contents of either array, alter *YYES and *YYES_CAPACITY, and free
   any old *YYES other than YYESA.  */
static int yy_lac(yy_state_t *yyesa, yy_state_t **yyes,
                  YYPTRDIFF_T *yyes_capacity, yy_state_t *yyssp,
                  yysymbol_kind_t yytoken) {
  yy_state_t *yyes_prev = yyssp;
  yy_state_t *yyesp = yyes_prev;
  /* Reduce until we encounter a shift and thereby accept the token.  */
  YYDPRINTF((stderr, "LAC: checking lookahead %s:", yysymbol_name(yytoken)));
  if (yytoken == YYSYMBOL_YYUNDEF) {
    YYDPRINTF((stderr, " Always Err\n"));
    return 1;
  }
  while (1) {
    int yyrule = yypact[+*yyesp];
    if (yypact_value_is_default(yyrule) || (yyrule += yytoken) < 0 ||
        YYLAST < yyrule || yycheck[yyrule] != yytoken) {
      /* Use the default action.  */
      yyrule = yydefact[+*yyesp];
      if (yyrule == 0) {
        YYDPRINTF((stderr, " Err\n"));
        return 1;
      }
    } else {
      /* Use the action from yytable.  */
      yyrule = yytable[yyrule];
      if (yytable_value_is_error(yyrule)) {
        YYDPRINTF((stderr, " Err\n"));
        return 1;
      }
      if (0 < yyrule) {
        YYDPRINTF((stderr, " S%d\n", yyrule));
        return 0;
      }
      yyrule = -yyrule;
    }
    /* By now we know we have to simulate a reduce.  */
    YYDPRINTF((stderr, " R%d", yyrule - 1));
    {
      /* Pop the corresponding number of values from the stack.  */
      YYPTRDIFF_T yylen = yyr2[yyrule];
      /* First pop from the LAC stack as many tokens as possible.  */
      if (yyesp != yyes_prev) {
        YYPTRDIFF_T yysize = yyesp - *yyes + 1;
        if (yylen < yysize) {
          yyesp -= yylen;
          yylen = 0;
        } else {
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
      if (yyesp == yyes_prev) {
        yyesp = *yyes;
        YY_IGNORE_USELESS_CAST_BEGIN
        *yyesp = YY_CAST(yy_state_t, yystate);
        YY_IGNORE_USELESS_CAST_END
      } else {
        if (yy_lac_stack_realloc(yyes_capacity, 1,
#if FRONTEND_VERILOG_YYDEBUG
                                 " (", ")",
#endif
                                 yyes, yyesa, &yyesp, yyes_prev)) {
          YYDPRINTF((stderr, "\n"));
          return YYENOMEM;
        }
        YY_IGNORE_USELESS_CAST_BEGIN
        *++yyesp = YY_CAST(yy_state_t, yystate);
        YY_IGNORE_USELESS_CAST_END
      }
      YYDPRINTF((stderr, " G%d", yystate));
    }
  }
}

/* Context of a parse error.  */
typedef struct {
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
static int yypcontext_expected_tokens(const yypcontext_t *yyctx,
                                      yysymbol_kind_t yyarg[], int yyargn) {
  /* Actual size of YYARG. */
  int yycount = 0;

  int yyx;
  for (yyx = 0; yyx < YYNTOKENS; ++yyx) {
    yysymbol_kind_t yysym = YY_CAST(yysymbol_kind_t, yyx);
    if (yysym != YYSYMBOL_YYerror && yysym != YYSYMBOL_YYUNDEF)
      switch (yy_lac(yyctx->yyesa, yyctx->yyes, yyctx->yyes_capacity,
                     yyctx->yyssp, yysym)) {
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
#if defined __GLIBC__ && defined _STRING_H
#define yystrlen(S) (YY_CAST(YYPTRDIFF_T, strlen(S)))
#else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T yystrlen(const char *yystr) {
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#endif
#endif

#ifndef yystpcpy
#if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#define yystpcpy stpcpy
#else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *yystpcpy(char *yydest, const char *yysrc) {
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#endif
#endif

#ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T yytnamerr(char *yyres, const char *yystr) {
  if (*yystr == '"') {
    YYPTRDIFF_T yyn = 0;
    char const *yyp = yystr;
    for (;;)
      switch (*++yyp) {
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
  do_not_strip_quotes:;
  }

  if (yyres)
    return yystpcpy(yyres, yystr) - yyres;
  else
    return yystrlen(yystr);
}
#endif

static int yy_syntax_error_arguments(const yypcontext_t *yyctx,
                                     yysymbol_kind_t yyarg[], int yyargn) {
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
  if (yyctx->yytoken != YYSYMBOL_YYEMPTY) {
    int yyn;
    YYDPRINTF((stderr, "Constructing syntax error message\n"));
    if (yyarg)
      yyarg[yycount] = yyctx->yytoken;
    ++yycount;
    yyn = yypcontext_expected_tokens(yyctx, yyarg ? yyarg + 1 : yyarg,
                                     yyargn - 1);
    if (yyn == YYENOMEM)
      return YYENOMEM;
    else if (yyn == 0)
      YYDPRINTF((stderr, "No expected tokens.\n"));
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
static int yysyntax_error(YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                          const yypcontext_t *yyctx) {
  enum { YYARGS_MAX = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  yysymbol_kind_t yyarg[YYARGS_MAX];
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* Actual size of YYARG. */
  int yycount = yy_syntax_error_arguments(yyctx, yyarg, YYARGS_MAX);
  if (yycount == YYENOMEM)
    return YYENOMEM;

  switch (yycount) {
#define YYCASE_(N, S)                                                          \
  case N:                                                                      \
    yyformat = S;                                                              \
    break
  default: /* Avoid compiler warnings. */
    YYCASE_(0, YY_("syntax error"));
    YYCASE_(1, YY_("syntax error, unexpected %s"));
    YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
    YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
    YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
    YYCASE_(5,
            YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
  }

  /* Compute error message size.  Don't count the "%s"s, but reserve
     room for the terminator.  */
  yysize = yystrlen(yyformat) - 2 * yycount + 1;
  {
    int yyi;
    for (yyi = 0; yyi < yycount; ++yyi) {
      YYPTRDIFF_T yysize1 = yysize + yytnamerr(YY_NULLPTR, yytname[yyarg[yyi]]);
      if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
        yysize = yysize1;
      else
        return YYENOMEM;
    }
  }

  if (*yymsg_alloc < yysize) {
    *yymsg_alloc = 2 * yysize;
    if (!(yysize <= *yymsg_alloc && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
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
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount) {
        yyp += yytnamerr(yyp, yytname[yyarg[yyi++]]);
        yyformat += 2;
      } else {
        ++yyp;
        ++yyformat;
      }
  }
  return 0;
}

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void yydestruct(const char *yymsg, yysymbol_kind_t yykind,
                       YYSTYPE *yyvaluep, YYLTYPE *yylocationp) {
  YY_USE(yyvaluep);
  YY_USE(yylocationp);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT(yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE(yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}

/*----------.
| yyparse.  |
`----------*/

int yyparse(void) {
  /* Lookahead token kind.  */
  int yychar;

  /* The semantic value of the lookahead symbol.  */
  /* Default value used for initialization, for pacifying older GCCs
     or non-GCC compilers.  */
  YY_INITIAL_VALUE(static YYSTYPE yyval_default;)
  YYSTYPE yylval YY_INITIAL_VALUE(= yyval_default);

  /* Location data for the lookahead symbol.  */
  static YYLTYPE yyloc_default
#if defined FRONTEND_VERILOG_YYLTYPE_IS_TRIVIAL &&                             \
    FRONTEND_VERILOG_YYLTYPE_IS_TRIVIAL
      = {1, 1, 1, 1}
#endif
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

#define YYPOPSTACK(N) (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF((stderr, "Starting parse\n"));

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
  YYDPRINTF((stderr, "Entering state %d\n", yystate));
  YY_ASSERT(0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST(yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT(yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
  {
    /* Get the current used size of the three stacks, in elements.  */
    YYPTRDIFF_T yysize = yyssp - yyss + 1;

#if defined yyoverflow
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
      yyoverflow(YY_("memory exhausted"), &yyss1, yysize * YYSIZEOF(*yyssp),
                 &yyvs1, yysize * YYSIZEOF(*yyvsp), &yyls1,
                 yysize * YYSIZEOF(*yylsp), &yystacksize);
      yyss = yyss1;
      yyvs = yyvs1;
      yyls = yyls1;
    }
#else /* defined YYSTACK_RELOCATE */
    /* Extend the stack our own way.  */
    if (YYMAXDEPTH <= yystacksize)
      YYNOMEM;
    yystacksize *= 2;
    if (YYMAXDEPTH < yystacksize)
      yystacksize = YYMAXDEPTH;

    {
      yy_state_t *yyss1 = yyss;
      union yyalloc *yyptr =
          YY_CAST(union yyalloc *,
                  YYSTACK_ALLOC(YY_CAST(YYSIZE_T, YYSTACK_BYTES(yystacksize))));
      if (!yyptr)
        YYNOMEM;
      YYSTACK_RELOCATE(yyss_alloc, yyss);
      YYSTACK_RELOCATE(yyvs_alloc, yyvs);
      YYSTACK_RELOCATE(yyls_alloc, yyls);
#undef YYSTACK_RELOCATE
      if (yyss1 != yyssa)
        YYSTACK_FREE(yyss1);
    }
#endif

    yyssp = yyss + yysize - 1;
    yyvsp = yyvs + yysize - 1;
    yylsp = yyls + yysize - 1;

    YY_IGNORE_USELESS_CAST_BEGIN
    YYDPRINTF(
        (stderr, "Stack size increased to %ld\n", YY_CAST(long, yystacksize)));
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
  if (yypact_value_is_default(yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == FRONTEND_VERILOG_YYEMPTY) {
    YYDPRINTF((stderr, "Reading a token\n"));
    yychar = yylex(&yylval, &yylloc);
  }

  if (yychar <= FRONTEND_VERILOG_YYEOF) {
    yychar = FRONTEND_VERILOG_YYEOF;
    yytoken = YYSYMBOL_YYEOF;
    YYDPRINTF((stderr, "Now at end of input.\n"));
  } else if (yychar == FRONTEND_VERILOG_YYerror) {
    /* The scanner already issued an error message, process directly
       to error recovery.  But do not keep the error token as
       lookahead, it is too special and may lead us to an endless
       loop in error recovery. */
    yychar = FRONTEND_VERILOG_YYUNDEF;
    yytoken = YYSYMBOL_YYerror;
    yyerror_range[1] = yylloc;
    goto yyerrlab1;
  } else {
    yytoken = YYTRANSLATE(yychar);
    YY_SYMBOL_PRINT("Next token is", yytoken, &yylval, &yylloc);
  }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken) {
    YY_LAC_ESTABLISH;
    goto yydefault;
  }
  yyn = yytable[yyn];
  if (yyn <= 0) {
    if (yytable_value_is_error(yyn))
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
  YY_SYMBOL_PRINT("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;

  /* Discard the shifted token.  */
  yychar = FRONTEND_VERILOG_YYEMPTY;
  YY_LAC_DISCARD("shift");
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
  yyval = yyvsp[1 - yylen];

  /* Default location. */
  YYLLOC_DEFAULT(yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT(yyn);
  {
    int yychar_backup = yychar;
    switch (yyn) {
    case 2: /* $@1: %empty  */
#line 455 "verilog_parser.y"
    {
      ast_stack.clear();
      ast_stack.push_back(current_ast);
    }
#line 3938 "verilog_parser.tab.cc"
    break;

    case 3: /* input: $@1 design  */
#line 458 "verilog_parser.y"
    {
      ast_stack.pop_back();
      log_assert(GetSize(ast_stack) == 0);
      for (auto &it : default_attr_list)
        delete it.second;
      default_attr_list.clear();
    }
#line 3950 "verilog_parser.tab.cc"
    break;

    case 14: /* $@2: %empty  */
#line 479 "verilog_parser.y"
    {
      if (attr_list != nullptr)
        attr_list_stack.push(attr_list);
      attr_list = new dict<IdString, AstNode *>;
      for (auto &it : default_attr_list)
        (*attr_list)[it.first] = it.second->clone();
    }
#line 3962 "verilog_parser.tab.cc"
    break;

    case 15: /* attr: $@2 attr_opt  */
#line 485 "verilog_parser.y"
    {
      (yyval.al) = attr_list;
      if (!attr_list_stack.empty()) {
        attr_list = attr_list_stack.top();
        attr_list_stack.pop();
      } else
        attr_list = nullptr;
    }
#line 3975 "verilog_parser.tab.cc"
    break;

    case 16: /* attr_opt: attr_opt ATTR_BEGIN opt_attr_list ATTR_END  */
#line 495 "verilog_parser.y"
    {
      SET_RULE_LOC((yyloc), (yylsp[-2]), (yyloc));
    }
#line 3983 "verilog_parser.tab.cc"
    break;

    case 18: /* $@3: %empty  */
#line 501 "verilog_parser.y"
    {
      if (attr_list != nullptr)
        attr_list_stack.push(attr_list);
      attr_list = new dict<IdString, AstNode *>;
      for (auto &it : default_attr_list)
        delete it.second;
      default_attr_list.clear();
    }
#line 3996 "verilog_parser.tab.cc"
    break;

    case 19: /* $@4: %empty  */
#line 508 "verilog_parser.y"
    {
      attr_list->swap(default_attr_list);
      delete attr_list;
      if (!attr_list_stack.empty()) {
        attr_list = attr_list_stack.top();
        attr_list_stack.pop();
      } else
        attr_list = nullptr;
    }
#line 4010 "verilog_parser.tab.cc"
    break;

    case 25: /* attr_assign: hierarchical_id  */
#line 526 "verilog_parser.y"
    {
      if (attr_list->count(*(yyvsp[0].string)) != 0)
        delete (*attr_list)[*(yyvsp[0].string)];
      (*attr_list)[*(yyvsp[0].string)] = AstNode::mkconst_int(1, false);
      delete (yyvsp[0].string);
    }
#line 4021 "verilog_parser.tab.cc"
    break;

    case 26: /* attr_assign: hierarchical_id '=' expr  */
#line 532 "verilog_parser.y"
    {
      if (attr_list->count(*(yyvsp[-2].string)) != 0)
        delete (*attr_list)[*(yyvsp[-2].string)];
      (*attr_list)[*(yyvsp[-2].string)] = (yyvsp[0].ast);
      delete (yyvsp[-2].string);
    }
#line 4032 "verilog_parser.tab.cc"
    break;

    case 27: /* hierarchical_id: TOK_ID  */
#line 540 "verilog_parser.y"
    {
      (yyval.string) = (yyvsp[0].string);
    }
#line 4040 "verilog_parser.tab.cc"
    break;

    case 28: /* hierarchical_id: hierarchical_id TOK_PACKAGESEP TOK_ID  */
#line 543 "verilog_parser.y"
    {
      if ((yyvsp[0].string)->compare(0, 1, "\\") == 0)
        *(yyvsp[-2].string) += "::" + (yyvsp[0].string)->substr(1);
      else
        *(yyvsp[-2].string) += "::" + *(yyvsp[0].string);
      delete (yyvsp[0].string);
      (yyval.string) = (yyvsp[-2].string);
    }
#line 4053 "verilog_parser.tab.cc"
    break;

    case 29: /* hierarchical_id: hierarchical_id '.' TOK_ID  */
#line 551 "verilog_parser.y"
    {
      if ((yyvsp[0].string)->compare(0, 1, "\\") == 0)
        *(yyvsp[-2].string) += "." + (yyvsp[0].string)->substr(1);
      else
        *(yyvsp[-2].string) += "." + *(yyvsp[0].string);
      delete (yyvsp[0].string);
      (yyval.string) = (yyvsp[-2].string);
    }
#line 4066 "verilog_parser.tab.cc"
    break;

    case 32: /* hierarchical_type_id: '(' TOK_USER_TYPE ')'  */
#line 563 "verilog_parser.y"
    {
      (yyval.string) = (yyvsp[-1].string);
    }
#line 4072 "verilog_parser.tab.cc"
    break;

    case 33: /* $@5: %empty  */
#line 567 "verilog_parser.y"
    {
      enterTypeScope();
    }
#line 4080 "verilog_parser.tab.cc"
    break;

    case 34: /* $@6: %empty  */
#line 569 "verilog_parser.y"
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
#line 4096 "verilog_parser.tab.cc"
    break;

    case 35: /* module: attr TOK_MODULE $@5 TOK_ID $@6 module_para_opt
                module_args_opt ';' module_body TOK_ENDMODULE opt_label  */
#line 579 "verilog_parser.y"
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
#line 4114 "verilog_parser.tab.cc"
    break;

    case 36: /* $@7: %empty  */
#line 594 "verilog_parser.y"
    {
      astbuf1 = nullptr;
    }
#line 4120 "verilog_parser.tab.cc"
    break;

    case 37: /* $@8: %empty  */
#line 594 "verilog_parser.y"
    {
      if (astbuf1)
        delete astbuf1;
    }
#line 4126 "verilog_parser.tab.cc"
    break;

    case 43: /* $@9: %empty  */
#line 601 "verilog_parser.y"
    {
      if (astbuf1)
        delete astbuf1;
      astbuf1 = new AstNode(AST_PARAMETER);
      astbuf1->children.push_back(AstNode::mkconst_int(0, true));
      append_attr(astbuf1, (yyvsp[-1].al));
    }
#line 4137 "verilog_parser.tab.cc"
    break;

    case 45: /* $@10: %empty  */
#line 607 "verilog_parser.y"
    {
      if (astbuf1)
        delete astbuf1;
      astbuf1 = new AstNode(AST_LOCALPARAM);
      astbuf1->children.push_back(AstNode::mkconst_int(0, true));
      append_attr(astbuf1, (yyvsp[-1].al));
    }
#line 4148 "verilog_parser.tab.cc"
    break;

    case 55: /* module_arg_opt_assignment: '=' expr  */
#line 625 "verilog_parser.y"
    {
      if (ast_stack.back()->children.size() > 0 &&
          ast_stack.back()->children.back()->type == AST_WIRE) {
        if (ast_stack.back()->children.back()->is_input) {
          AstNode *n = ast_stack.back()->children.back();
          if (n->attributes.count(ID::defaultvalue))
            delete n->attributes.at(ID::defaultvalue);
          n->attributes[ID::defaultvalue] = (yyvsp[0].ast);
        } else {
          AstNode *wire = new AstNode(AST_IDENTIFIER);
          wire->str = ast_stack.back()->children.back()->str;
          if (ast_stack.back()->children.back()->is_reg ||
              ast_stack.back()->children.back()->is_logic)
            ast_stack.back()->children.push_back(new AstNode(
                AST_INITIAL,
                new AstNode(AST_BLOCK,
                            new AstNode(AST_ASSIGN_LE, wire, (yyvsp[0].ast)))));
          else
            ast_stack.back()->children.push_back(
                new AstNode(AST_ASSIGN, wire, (yyvsp[0].ast)));
        }
      } else
        frontend_verilog_yyerror("SystemVerilog interface in module port list "
                                 "cannot have a default value.");
    }
#line 4171 "verilog_parser.tab.cc"
    break;

    case 57: /* $@11: %empty  */
#line 646 "verilog_parser.y"
    {
      if (ast_stack.back()->children.size() > 0 &&
          ast_stack.back()->children.back()->type == AST_WIRE) {
        AstNode *node = ast_stack.back()->children.back()->clone();
        node->str = *(yyvsp[0].string);
        node->port_id = ++port_counter;
        ast_stack.back()->children.push_back(node);
        SET_AST_NODE_LOC(node, (yylsp[0]), (yylsp[0]));
      } else {
        if (port_stubs.count(*(yyvsp[0].string)) != 0)
          frontend_verilog_yyerror("Duplicate module port `%s'.",
                                   (yyvsp[0].string)->c_str());
        port_stubs[*(yyvsp[0].string)] = ++port_counter;
      }
      delete (yyvsp[0].string);
    }
#line 4190 "verilog_parser.tab.cc"
    break;

    case 59: /* $@12: %empty  */
#line 660 "verilog_parser.y"
    {
      astbuf1 = new AstNode(AST_INTERFACEPORT);
      astbuf1->children.push_back(new AstNode(AST_INTERFACEPORTTYPE));
      astbuf1->children[0]->str = *(yyvsp[0].string);
      delete (yyvsp[0].string);
    }
#line 4201 "verilog_parser.tab.cc"
    break;

    case 60: /* $@13: %empty  */
#line 665 "verilog_parser.y"
    { /* SV interfaces */
      if (!sv_mode)
        frontend_verilog_yyerror(
            "Interface found in port list (%s). This is not supported unless "
            "read_verilog is called with -sv!",
            (yyvsp[0].string)->c_str());
      astbuf2 = astbuf1->clone(); // really only needed if multiple instances of
                                  // same type.
      astbuf2->str = *(yyvsp[0].string);
      delete (yyvsp[0].string);
      astbuf2->port_id = ++port_counter;
      ast_stack.back()->children.push_back(astbuf2);
      delete astbuf1; // really only needed if multiple instances of same type.
    }
#line 4216 "verilog_parser.tab.cc"
    break;

    case 62: /* $@14: %empty  */
#line 675 "verilog_parser.y"
    {
      AstNode *node = (yyvsp[-2].ast);
      node->str = *(yyvsp[0].string);
      SET_AST_NODE_LOC(node, (yylsp[0]), (yylsp[0]));
      node->port_id = ++port_counter;
      AstNode *range = checkRange(node, (yyvsp[-1].ast));
      if (range != NULL)
        node->children.push_back(range);
      if (!node->is_input && !node->is_output)
        frontend_verilog_yyerror(
            "Module port `%s' is neither input nor output.",
            (yyvsp[0].string)->c_str());
      if (node->is_reg && node->is_input && !node->is_output && !sv_mode)
        frontend_verilog_yyerror("Input port `%s' is declared as register.",
                                 (yyvsp[0].string)->c_str());
      ast_stack.back()->children.push_back(node);
      append_attr(node, (yyvsp[-3].al));
      delete (yyvsp[0].string);
    }
#line 4237 "verilog_parser.tab.cc"
    break;

    case 64: /* module_arg: '.' '.' '.'  */
#line 691 "verilog_parser.y"
    {
      do_not_require_port_stubs = true;
    }
#line 4245 "verilog_parser.tab.cc"
    break;

    case 65: /* $@15: %empty  */
#line 696 "verilog_parser.y"
    {
      enterTypeScope();
    }
#line 4253 "verilog_parser.tab.cc"
    break;

    case 66: /* $@16: %empty  */
#line 698 "verilog_parser.y"
    {
      AstNode *mod = new AstNode(AST_PACKAGE);
      ast_stack.back()->children.push_back(mod);
      ast_stack.push_back(mod);
      current_ast_mod = mod;
      mod->str = *(yyvsp[0].string);
      append_attr(mod, (yyvsp[-3].al));
    }
#line 4266 "verilog_parser.tab.cc"
    break;

    case 67: /* package: attr TOK_PACKAGE $@15 TOK_ID $@16 ';' package_body
                TOK_ENDPACKAGE opt_label  */
#line 705 "verilog_parser.y"
    {
      ast_stack.pop_back();
      checkLabelsMatch("Package name", (yyvsp[-5].string), (yyvsp[0].string));
      current_ast_mod = NULL;
      delete (yyvsp[-5].string);
      delete (yyvsp[0].string);
      exitTypeScope();
    }
#line 4279 "verilog_parser.tab.cc"
    break;

    case 74: /* $@17: %empty  */
#line 721 "verilog_parser.y"
    {
      enterTypeScope();
    }
#line 4287 "verilog_parser.tab.cc"
    break;

    case 75: /* $@18: %empty  */
#line 723 "verilog_parser.y"
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
#line 4303 "verilog_parser.tab.cc"
    break;

    case 76: /* interface: TOK_INTERFACE $@17 TOK_ID $@18 module_para_opt
                module_args_opt ';' interface_body TOK_ENDINTERFACE  */
#line 733 "verilog_parser.y"
    {
      if (port_stubs.size() != 0)
        frontend_verilog_yyerror("Missing details for module port `%s'.",
                                 port_stubs.begin()->first.c_str());
      ast_stack.pop_back();
      log_assert(ast_stack.size() == 1);
      current_ast_mod = NULL;
      exitTypeScope();
    }
#line 4317 "verilog_parser.tab.cc"
    break;

    case 88: /* $@19: %empty  */
#line 751 "verilog_parser.y"
    {
      AstNode *bnode = new AstNode(AST_BIND);
      ast_stack.back()->children.push_back(bnode);
      ast_stack.push_back(bnode);
    }
#line 4327 "verilog_parser.tab.cc"
    break;

    case 89: /* $@20: %empty  */
#line 756 "verilog_parser.y"
    {
      // bind_target should have added at least one child
      log_assert(ast_stack.back()->children.size() >= 1);
    }
#line 4336 "verilog_parser.tab.cc"
    break;

    case 90: /* $@21: %empty  */
#line 760 "verilog_parser.y"
    {
      // The single_cell parser in cell_list_no_array uses astbuf1 as
      // a sort of template for constructing cells.
      astbuf1 = new AstNode(AST_CELL);
      astbuf1->children.push_back(new AstNode(AST_CELLTYPE));
      astbuf1->children[0]->str = *(yyvsp[0].string);
      delete (yyvsp[0].string);
    }
#line 4349 "verilog_parser.tab.cc"
    break;

    case 91: /* bind_directive: TOK_BIND $@19 bind_target $@20 TOK_ID $@21
                cell_parameter_list_opt cell_list_no_array ';'  */
#line 768 "verilog_parser.y"
    {
      // cell_list should have added at least one more child
      log_assert(ast_stack.back()->children.size() >= 2);
      delete astbuf1;
      ast_stack.pop_back();
    }
#line 4360 "verilog_parser.tab.cc"
    break;

    case 97: /* bind_target_instance: hierarchical_id  */
#line 803 "verilog_parser.y"
    {
      auto *node = new AstNode(AST_IDENTIFIER);
      node->str = *(yyvsp[0].string);
      delete (yyvsp[0].string);
      ast_stack.back()->children.push_back(node);
    }
#line 4371 "verilog_parser.tab.cc"
    break;

    case 98: /* mintypmax_expr: expr  */
#line 811 "verilog_parser.y"
    {
      delete (yyvsp[0].ast);
    }
#line 4377 "verilog_parser.tab.cc"
    break;

    case 99: /* mintypmax_expr: expr ':' expr ':' expr  */
#line 812 "verilog_parser.y"
    {
      delete (yyvsp[-4].ast);
      delete (yyvsp[-2].ast);
      delete (yyvsp[0].ast);
    }
#line 4383 "verilog_parser.tab.cc"
    break;

    case 100: /* non_opt_delay: '#' TOK_ID  */
#line 815 "verilog_parser.y"
    {
      delete (yyvsp[0].string);
    }
#line 4389 "verilog_parser.tab.cc"
    break;

    case 101: /* non_opt_delay: '#' TOK_CONSTVAL  */
#line 816 "verilog_parser.y"
    {
      delete (yyvsp[0].string);
    }
#line 4395 "verilog_parser.tab.cc"
    break;

    case 102: /* non_opt_delay: '#' TOK_REALVAL  */
#line 817 "verilog_parser.y"
    {
      delete (yyvsp[0].string);
    }
#line 4401 "verilog_parser.tab.cc"
    break;

    case 110: /* $@22: %empty  */
#line 829 "verilog_parser.y"
    {
      astbuf3 = new AstNode(AST_WIRE);
      current_wire_rand = false;
      current_wire_const = false;
    }
#line 4407 "verilog_parser.tab.cc"
    break;

    case 111: /* io_wire_type: $@22 wire_type_token_io wire_type_const_rand
                 opt_wire_type_token wire_type_signedness  */
#line 831 "verilog_parser.y"
    {
      (yyval.ast) = astbuf3;
      SET_RULE_LOC((yyloc), (yylsp[-3]), (yyloc));
    }
#line 4413 "verilog_parser.tab.cc"
    break;

    case 112: /* $@23: %empty  */
#line 834 "verilog_parser.y"
    {
      astbuf3 = new AstNode(AST_WIRE);
      current_wire_rand = false;
      current_wire_const = false;
    }
#line 4419 "verilog_parser.tab.cc"
    break;

    case 113: /* non_io_wire_type: $@23 wire_type_const_rand wire_type_token
                 wire_type_signedness  */
#line 836 "verilog_parser.y"
    {
      (yyval.ast) = astbuf3;
      SET_RULE_LOC((yyloc), (yylsp[-2]), (yyloc));
    }
#line 4425 "verilog_parser.tab.cc"
    break;

    case 116: /* wire_type_token_io: TOK_INPUT  */
#line 843 "verilog_parser.y"
    {
      astbuf3->is_input = true;
    }
#line 4433 "verilog_parser.tab.cc"
    break;

    case 117: /* wire_type_token_io: TOK_OUTPUT  */
#line 846 "verilog_parser.y"
    {
      astbuf3->is_output = true;
    }
#line 4441 "verilog_parser.tab.cc"
    break;

    case 118: /* wire_type_token_io: TOK_INOUT  */
#line 849 "verilog_parser.y"
    {
      astbuf3->is_input = true;
      astbuf3->is_output = true;
    }
#line 4450 "verilog_parser.tab.cc"
    break;

    case 119: /* wire_type_signedness: TOK_SIGNED  */
#line 855 "verilog_parser.y"
    {
      astbuf3->is_signed = true;
    }
#line 4456 "verilog_parser.tab.cc"
    break;

    case 120: /* wire_type_signedness: TOK_UNSIGNED  */
#line 856 "verilog_parser.y"
    {
      astbuf3->is_signed = false;
    }
#line 4462 "verilog_parser.tab.cc"
    break;

    case 122: /* wire_type_const_rand: TOK_RAND TOK_CONST  */
#line 860 "verilog_parser.y"
    {
      current_wire_rand = true;
      current_wire_const = true;
    }
#line 4471 "verilog_parser.tab.cc"
    break;

    case 123: /* wire_type_const_rand: TOK_CONST  */
#line 864 "verilog_parser.y"
    {
      current_wire_const = true;
    }
#line 4479 "verilog_parser.tab.cc"
    break;

    case 124: /* wire_type_const_rand: TOK_RAND  */
#line 867 "verilog_parser.y"
    {
      current_wire_rand = true;
    }
#line 4487 "verilog_parser.tab.cc"
    break;

    case 128: /* wire_type_token: net_type  */
#line 877 "verilog_parser.y"
    {
    }
#line 4494 "verilog_parser.tab.cc"
    break;

    case 129: /* wire_type_token: net_type logic_type  */
#line 879 "verilog_parser.y"
    {
    }
#line 4501 "verilog_parser.tab.cc"
    break;

    case 130: /* wire_type_token: TOK_REG  */
#line 882 "verilog_parser.y"
    {
      astbuf3->is_reg = true;
    }
#line 4509 "verilog_parser.tab.cc"
    break;

    case 131: /* wire_type_token: TOK_VAR TOK_REG  */
#line 885 "verilog_parser.y"
    {
      astbuf3->is_reg = true;
    }
#line 4517 "verilog_parser.tab.cc"
    break;

    case 132: /* wire_type_token: TOK_VAR  */
#line 889 "verilog_parser.y"
    {
      astbuf3->is_logic = true;
    }
#line 4525 "verilog_parser.tab.cc"
    break;

    case 133: /* wire_type_token: TOK_VAR logic_type  */
#line 892 "verilog_parser.y"
    {
      astbuf3->is_logic = true;
    }
#line 4533 "verilog_parser.tab.cc"
    break;

    case 134: /* wire_type_token: logic_type  */
#line 895 "verilog_parser.y"
    {
      astbuf3->is_logic = true;
    }
#line 4541 "verilog_parser.tab.cc"
    break;

    case 135: /* wire_type_token: TOK_GENVAR  */
#line 898 "verilog_parser.y"
    {
      astbuf3->type = AST_GENVAR;
      astbuf3->is_reg = true;
      astbuf3->is_signed = true;
      astbuf3->range_left = 31;
      astbuf3->range_right = 0;
    }
#line 4553 "verilog_parser.tab.cc"
    break;

    case 136: /* net_type: TOK_WOR  */
#line 907 "verilog_parser.y"
    {
      astbuf3->is_wor = true;
    }
#line 4561 "verilog_parser.tab.cc"
    break;

    case 137: /* net_type: TOK_WAND  */
#line 910 "verilog_parser.y"
    {
      astbuf3->is_wand = true;
    }
#line 4569 "verilog_parser.tab.cc"
    break;

    case 139: /* logic_type: TOK_LOGIC  */
#line 916 "verilog_parser.y"
    {
    }
#line 4576 "verilog_parser.tab.cc"
    break;

    case 140: /* logic_type: integer_atom_type  */
#line 918 "verilog_parser.y"
    {
      astbuf3->range_left = (yyvsp[0].integer) - 1;
      astbuf3->range_right = 0;
      astbuf3->is_signed = true;
    }
#line 4586 "verilog_parser.tab.cc"
    break;

    case 141: /* logic_type: hierarchical_type_id  */
#line 923 "verilog_parser.y"
    {
      addWiretypeNode((yyvsp[0].string), astbuf3);
    }
#line 4594 "verilog_parser.tab.cc"
    break;

    case 142: /* integer_atom_type: TOK_INTEGER  */
#line 928 "verilog_parser.y"
    {
      (yyval.integer) = 32;
    }
#line 4600 "verilog_parser.tab.cc"
    break;

    case 143: /* integer_atom_type: TOK_INT  */
#line 929 "verilog_parser.y"
    {
      (yyval.integer) = 32;
    }
#line 4606 "verilog_parser.tab.cc"
    break;

    case 144: /* integer_atom_type: TOK_SHORTINT  */
#line 930 "verilog_parser.y"
    {
      (yyval.integer) = 16;
    }
#line 4612 "verilog_parser.tab.cc"
    break;

    case 145: /* integer_atom_type: TOK_LONGINT  */
#line 931 "verilog_parser.y"
    {
      (yyval.integer) = 64;
    }
#line 4618 "verilog_parser.tab.cc"
    break;

    case 146: /* integer_atom_type: TOK_BYTE  */
#line 932 "verilog_parser.y"
    {
      (yyval.integer) = 8;
    }
#line 4624 "verilog_parser.tab.cc"
    break;

    case 147: /* integer_vector_type: TOK_LOGIC  */
#line 935 "verilog_parser.y"
    {
      (yyval.integer) = TOK_LOGIC;
    }
#line 4630 "verilog_parser.tab.cc"
    break;

    case 148: /* integer_vector_type: TOK_REG  */
#line 936 "verilog_parser.y"
    {
      (yyval.integer) = TOK_REG;
    }
#line 4636 "verilog_parser.tab.cc"
    break;

    case 149: /* non_opt_range: '[' expr ':' expr ']'  */
#line 939 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_RANGE);
      (yyval.ast)->children.push_back((yyvsp[-3].ast));
      (yyval.ast)->children.push_back((yyvsp[-1].ast));
    }
#line 4646 "verilog_parser.tab.cc"
    break;

    case 150: /* non_opt_range: '[' expr TOK_POS_INDEXED expr ']'  */
#line 944 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_RANGE);
      AstNode *expr = new AstNode(AST_SELFSZ, (yyvsp[-3].ast));
      (yyval.ast)->children.push_back(new AstNode(
          AST_SUB, new AstNode(AST_ADD, expr->clone(), (yyvsp[-1].ast)),
          AstNode::mkconst_int(1, true)));
      (yyval.ast)->children.push_back(
          new AstNode(AST_ADD, expr, AstNode::mkconst_int(0, true)));
    }
#line 4657 "verilog_parser.tab.cc"
    break;

    case 151: /* non_opt_range: '[' expr TOK_NEG_INDEXED expr ']'  */
#line 950 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_RANGE);
      AstNode *expr = new AstNode(AST_SELFSZ, (yyvsp[-3].ast));
      (yyval.ast)->children.push_back(
          new AstNode(AST_ADD, expr, AstNode::mkconst_int(0, true)));
      (yyval.ast)->children.push_back(new AstNode(
          AST_SUB,
          new AstNode(AST_ADD, expr->clone(), AstNode::mkconst_int(1, true)),
          (yyvsp[-1].ast)));
    }
#line 4668 "verilog_parser.tab.cc"
    break;

    case 152: /* non_opt_range: '[' expr ']'  */
#line 956 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_RANGE);
      (yyval.ast)->children.push_back((yyvsp[-1].ast));
    }
#line 4677 "verilog_parser.tab.cc"
    break;

    case 153: /* non_opt_multirange: non_opt_range non_opt_range  */
#line 962 "verilog_parser.y"
    {
      (yyval.ast) =
          new AstNode(AST_MULTIRANGE, (yyvsp[-1].ast), (yyvsp[0].ast));
    }
#line 4685 "verilog_parser.tab.cc"
    break;

    case 154: /* non_opt_multirange: non_opt_multirange non_opt_range  */
#line 965 "verilog_parser.y"
    {
      (yyval.ast) = (yyvsp[-1].ast);
      (yyval.ast)->children.push_back((yyvsp[0].ast));
    }
#line 4694 "verilog_parser.tab.cc"
    break;

    case 155: /* range: non_opt_range  */
#line 971 "verilog_parser.y"
    {
      (yyval.ast) = (yyvsp[0].ast);
    }
#line 4702 "verilog_parser.tab.cc"
    break;

    case 156: /* range: %empty  */
#line 974 "verilog_parser.y"
    {
      (yyval.ast) = NULL;
    }
#line 4710 "verilog_parser.tab.cc"
    break;

    case 157: /* range_or_multirange: range  */
#line 979 "verilog_parser.y"
    {
      (yyval.ast) = (yyvsp[0].ast);
    }
#line 4716 "verilog_parser.tab.cc"
    break;

    case 158: /* range_or_multirange: non_opt_multirange  */
#line 980 "verilog_parser.y"
    {
      (yyval.ast) = (yyvsp[0].ast);
    }
#line 4722 "verilog_parser.tab.cc"
    break;

    case 183: /* $@24: %empty  */
#line 996 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_GENBLOCK);
      node->str = *(yyvsp[-1].string);
      ast_stack.back()->children.push_back(node);
      ast_stack.push_back(node);
    }
#line 4733 "verilog_parser.tab.cc"
    break;

    case 184: /* checker_decl: TOK_CHECKER TOK_ID ';' $@24 module_body
                 TOK_ENDCHECKER  */
#line 1001 "verilog_parser.y"
    {
      delete (yyvsp[-4].string);
      ast_stack.pop_back();
    }
#line 4742 "verilog_parser.tab.cc"
    break;

    case 185: /* $@25: %empty  */
#line 1007 "verilog_parser.y"
    {
      current_function_or_task = new AstNode(
          AST_DPI_FUNCTION, AstNode::mkconst_str(*(yyvsp[-1].string)),
          AstNode::mkconst_str(*(yyvsp[0].string)));
      current_function_or_task->str = *(yyvsp[0].string);
      append_attr(current_function_or_task, (yyvsp[-3].al));
      ast_stack.back()->children.push_back(current_function_or_task);
      delete (yyvsp[-1].string);
      delete (yyvsp[0].string);
    }
#line 4755 "verilog_parser.tab.cc"
    break;

    case 186: /* task_func_decl: attr TOK_DPI_FUNCTION TOK_ID TOK_ID $@25
                 opt_dpi_function_args ';'  */
#line 1014 "verilog_parser.y"
    {
      current_function_or_task = NULL;
    }
#line 4763 "verilog_parser.tab.cc"
    break;

    case 187: /* $@26: %empty  */
#line 1017 "verilog_parser.y"
    {
      current_function_or_task = new AstNode(
          AST_DPI_FUNCTION, AstNode::mkconst_str(*(yyvsp[-1].string)),
          AstNode::mkconst_str(*(yyvsp[-3].string)));
      current_function_or_task->str = *(yyvsp[0].string);
      append_attr(current_function_or_task, (yyvsp[-5].al));
      ast_stack.back()->children.push_back(current_function_or_task);
      delete (yyvsp[-3].string);
      delete (yyvsp[-1].string);
      delete (yyvsp[0].string);
    }
#line 4777 "verilog_parser.tab.cc"
    break;

    case 188: /* task_func_decl: attr TOK_DPI_FUNCTION TOK_ID '=' TOK_ID TOK_ID
                 $@26 opt_dpi_function_args ';'  */
#line 1025 "verilog_parser.y"
    {
      current_function_or_task = NULL;
    }
#line 4785 "verilog_parser.tab.cc"
    break;

    case 189: /* $@27: %empty  */
#line 1028 "verilog_parser.y"
    {
      current_function_or_task = new AstNode(
          AST_DPI_FUNCTION, AstNode::mkconst_str(*(yyvsp[-1].string)),
          AstNode::mkconst_str(*(yyvsp[-5].string) + ":" +
                               RTLIL::unescape_id(*(yyvsp[-3].string))));
      current_function_or_task->str = *(yyvsp[0].string);
      append_attr(current_function_or_task, (yyvsp[-7].al));
      ast_stack.back()->children.push_back(current_function_or_task);
      delete (yyvsp[-5].string);
      delete (yyvsp[-3].string);
      delete (yyvsp[-1].string);
      delete (yyvsp[0].string);
    }
#line 4800 "verilog_parser.tab.cc"
    break;

    case 190: /* task_func_decl: attr TOK_DPI_FUNCTION TOK_ID ':' TOK_ID '='
                 TOK_ID TOK_ID $@27 opt_dpi_function_args ';'  */
#line 1037 "verilog_parser.y"
    {
      current_function_or_task = NULL;
    }
#line 4808 "verilog_parser.tab.cc"
    break;

    case 191: /* $@28: %empty  */
#line 1040 "verilog_parser.y"
    {
      current_function_or_task = new AstNode(AST_TASK);
      current_function_or_task->str = *(yyvsp[0].string);
      append_attr(current_function_or_task, (yyvsp[-3].al));
      ast_stack.back()->children.push_back(current_function_or_task);
      ast_stack.push_back(current_function_or_task);
      current_function_or_task_port_id = 1;
      delete (yyvsp[0].string);
    }
#line 4822 "verilog_parser.tab.cc"
    break;

    case 192: /* task_func_decl: attr TOK_TASK opt_automatic TOK_ID $@28
                 task_func_args_opt ';' task_func_body TOK_ENDTASK  */
#line 1048 "verilog_parser.y"
    {
      current_function_or_task = NULL;
      ast_stack.pop_back();
    }
#line 4831 "verilog_parser.tab.cc"
    break;

    case 193: /* $@29: %empty  */
#line 1052 "verilog_parser.y"
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
#line 4850 "verilog_parser.tab.cc"
    break;

    case 194: /* task_func_decl: attr TOK_FUNCTION opt_automatic TOK_VOID TOK_ID
                 $@29 task_func_args_opt ';' task_func_body TOK_ENDFUNCTION  */
#line 1065 "verilog_parser.y"
    {
      current_function_or_task = NULL;
      ast_stack.pop_back();
    }
#line 4859 "verilog_parser.tab.cc"
    break;

    case 195: /* $@30: %empty  */
#line 1069 "verilog_parser.y"
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
#line 4884 "verilog_parser.tab.cc"
    break;

    case 196: /* task_func_decl: attr TOK_FUNCTION opt_automatic
                 func_return_type TOK_ID $@30 task_func_args_opt ';'
                 task_func_body TOK_ENDFUNCTION  */
#line 1088 "verilog_parser.y"
    {
      current_function_or_task = NULL;
      ast_stack.pop_back();
    }
#line 4893 "verilog_parser.tab.cc"
    break;

    case 197: /* func_return_type: hierarchical_type_id  */
#line 1094 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_WIRETYPE);
      (yyval.ast)->str = *(yyvsp[0].string);
      delete (yyvsp[0].string);
    }
#line 4903 "verilog_parser.tab.cc"
    break;

    case 198: /* func_return_type: opt_type_vec opt_signedness_default_unsigned
               */
#line 1099 "verilog_parser.y"
    {
      (yyval.ast) = makeRange(0, 0, (yyvsp[0].boolean));
    }
#line 4911 "verilog_parser.tab.cc"
    break;

    case 199: /* func_return_type: opt_type_vec opt_signedness_default_unsigned
                 non_opt_range  */
#line 1102 "verilog_parser.y"
    {
      (yyval.ast) = (yyvsp[0].ast);
      (yyval.ast)->is_signed = (yyvsp[-1].boolean);
    }
#line 4920 "verilog_parser.tab.cc"
    break;

    case 200: /* func_return_type: integer_atom_type
                 opt_signedness_default_signed  */
#line 1106 "verilog_parser.y"
    {
      (yyval.ast) = makeRange((yyvsp[-1].integer) - 1, 0, (yyvsp[0].boolean));
    }
#line 4928 "verilog_parser.tab.cc"
    break;

    case 204: /* opt_signedness_default_signed: %empty  */
#line 1117 "verilog_parser.y"
    {
      (yyval.boolean) = true;
    }
#line 4934 "verilog_parser.tab.cc"
    break;

    case 205: /* opt_signedness_default_signed: TOK_SIGNED  */
#line 1118 "verilog_parser.y"
    {
      (yyval.boolean) = true;
    }
#line 4940 "verilog_parser.tab.cc"
    break;

    case 206: /* opt_signedness_default_signed: TOK_UNSIGNED  */
#line 1119 "verilog_parser.y"
    {
      (yyval.boolean) = false;
    }
#line 4946 "verilog_parser.tab.cc"
    break;

    case 207: /* opt_signedness_default_unsigned: %empty  */
#line 1122 "verilog_parser.y"
    {
      (yyval.boolean) = false;
    }
#line 4952 "verilog_parser.tab.cc"
    break;

    case 208: /* opt_signedness_default_unsigned: TOK_SIGNED  */
#line 1123 "verilog_parser.y"
    {
      (yyval.boolean) = true;
    }
#line 4958 "verilog_parser.tab.cc"
    break;

    case 209: /* opt_signedness_default_unsigned: TOK_UNSIGNED  */
#line 1124 "verilog_parser.y"
    {
      (yyval.boolean) = false;
    }
#line 4964 "verilog_parser.tab.cc"
    break;

    case 210: /* dpi_function_arg: TOK_ID TOK_ID  */
#line 1128 "verilog_parser.y"
    {
      current_function_or_task->children.push_back(
          AstNode::mkconst_str(*(yyvsp[-1].string)));
      delete (yyvsp[-1].string);
      delete (yyvsp[0].string);
    }
#line 4974 "verilog_parser.tab.cc"
    break;

    case 211: /* dpi_function_arg: TOK_ID  */
#line 1133 "verilog_parser.y"
    {
      current_function_or_task->children.push_back(
          AstNode::mkconst_str(*(yyvsp[0].string)));
      delete (yyvsp[0].string);
    }
#line 4983 "verilog_parser.tab.cc"
    break;

    case 222: /* $@31: %empty  */
#line 1153 "verilog_parser.y"
    {
      albuf = nullptr;
      astbuf1 = nullptr;
      astbuf2 = nullptr;
    }
#line 4993 "verilog_parser.tab.cc"
    break;

    case 223: /* $@32: %empty  */
#line 1157 "verilog_parser.y"
    {
      delete astbuf1;
      if (astbuf2 != NULL)
        delete astbuf2;
      free_attr(albuf);
    }
#line 5004 "verilog_parser.tab.cc"
    break;

    case 227: /* $@33: %empty  */
#line 1168 "verilog_parser.y"
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
#line 5030 "verilog_parser.tab.cc"
    break;

    case 229: /* $@34: %empty  */
#line 1189 "verilog_parser.y"
    {
      if (!astbuf1) {
        if (!sv_mode)
          frontend_verilog_yyerror("task/function argument direction missing");
        albuf = new dict<IdString, AstNode *>;
        astbuf1 = new AstNode(AST_WIRE);
        current_wire_rand = false;
        current_wire_const = false;
        astbuf1->is_input = true;
        astbuf2 = NULL;
      }
    }
#line 5047 "verilog_parser.tab.cc"
    break;

    case 236: /* specify_item: specify_if '(' specify_edge expr TOK_SPECIFY_OPER
                 specify_target ')' '=' specify_rise_fall ';'  */
#line 1216 "verilog_parser.y"
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

      cell->children.push_back(new AstNode(
          AST_PARASET, AstNode::mkconst_int(oper_type == '*', false, 1)));
      cell->children.back()->str = "\\FULL";

      cell->children.push_back(new AstNode(
          AST_PARASET, AstNode::mkconst_int(oper_polarity != 0, false, 1)));
      cell->children.back()->str = "\\SRC_DST_PEN";

      cell->children.push_back(new AstNode(
          AST_PARASET, AstNode::mkconst_int(oper_polarity == '+', false, 1)));
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

      cell->children.push_back(new AstNode(
          AST_ARGUMENT, en_expr ? en_expr : AstNode::mkconst_int(1, false, 1)));
      cell->children.back()->str = "\\EN";

      cell->children.push_back(new AstNode(AST_ARGUMENT, src_expr));
      cell->children.back()->str = "\\SRC";

      cell->children.push_back(new AstNode(AST_ARGUMENT, target->dst));
      cell->children.back()->str = "\\DST";

      if (target->dat) {
        cell->children.push_back(new AstNode(
            AST_PARASET, AstNode::mkconst_int(specify_edge != 0, false, 1)));
        cell->children.back()->str = "\\EDGE_EN";

        cell->children.push_back(new AstNode(
            AST_PARASET, AstNode::mkconst_int(specify_edge == 'p', false, 1)));
        cell->children.back()->str = "\\EDGE_POL";

        cell->children.push_back(new AstNode(
            AST_PARASET,
            AstNode::mkconst_int(target->polarity_op != 0, false, 1)));
        cell->children.back()->str = "\\DAT_DST_PEN";

        cell->children.push_back(new AstNode(
            AST_PARASET,
            AstNode::mkconst_int(target->polarity_op == '+', false, 1)));
        cell->children.back()->str = "\\DAT_DST_POL";

        cell->children.push_back(new AstNode(AST_ARGUMENT, target->dat));
        cell->children.back()->str = "\\DAT";
      }

      delete oper;
      delete target;
      delete timing;
    }
#line 5136 "verilog_parser.tab.cc"
    break;

    case 237: /* specify_item: TOK_ID '(' specify_edge expr specify_condition
                 ',' specify_edge expr specify_condition ',' specify_triple
                 specify_opt_triple ')' ';'  */
#line 1300 "verilog_parser.y"
    {
      if (*(yyvsp[-13].string) != "$setup" && *(yyvsp[-13].string) != "$hold" &&
          *(yyvsp[-13].string) != "$setuphold" &&
          *(yyvsp[-13].string) != "$removal" &&
          *(yyvsp[-13].string) != "$recovery" &&
          *(yyvsp[-13].string) != "$recrem" &&
          *(yyvsp[-13].string) != "$skew" &&
          *(yyvsp[-13].string) != "$timeskew" &&
          *(yyvsp[-13].string) != "$fullskew" &&
          *(yyvsp[-13].string) != "$nochange")
        frontend_verilog_yyerror("Unsupported specify rule type: %s\n",
                                 (yyvsp[-13].string)->c_str());

      AstNode *src_pen = AstNode::mkconst_int((yyvsp[-11].ch) != 0, false, 1);
      AstNode *src_pol = AstNode::mkconst_int((yyvsp[-11].ch) == 'p', false, 1);
      AstNode *src_expr = (yyvsp[-10].ast),
              *src_en = (yyvsp[-9].ast) ? (yyvsp[-9].ast)
                                        : AstNode::mkconst_int(1, false, 1);

      AstNode *dst_pen = AstNode::mkconst_int((yyvsp[-7].ch) != 0, false, 1);
      AstNode *dst_pol = AstNode::mkconst_int((yyvsp[-7].ch) == 'p', false, 1);
      AstNode *dst_expr = (yyvsp[-6].ast),
              *dst_en = (yyvsp[-5].ast) ? (yyvsp[-5].ast)
                                        : AstNode::mkconst_int(1, false, 1);

      specify_triple *limit = (yyvsp[-3].specify_triple_ptr);
      specify_triple *limit2 = (yyvsp[-2].specify_triple_ptr);

      AstNode *cell = new AstNode(AST_CELL);
      ast_stack.back()->children.push_back(cell);
      cell->str = stringf("$specify$%d", autoidx++);
      cell->children.push_back(new AstNode(AST_CELLTYPE));
      cell->children.back()->str = "$specrule";
      SET_AST_NODE_LOC(cell, (yylsp[-13]), (yylsp[0]));

      cell->children.push_back(
          new AstNode(AST_PARASET, AstNode::mkconst_str(*(yyvsp[-13].string))));
      cell->children.back()->str = "\\TYPE";

      cell->children.push_back(new AstNode(AST_PARASET, limit->t_min));
      cell->children.back()->str = "\\T_LIMIT_MIN";

      cell->children.push_back(new AstNode(AST_PARASET, limit->t_avg));
      cell->children.back()->str = "\\T_LIMIT_TYP";

      cell->children.push_back(new AstNode(AST_PARASET, limit->t_max));
      cell->children.back()->str = "\\T_LIMIT_MAX";

      cell->children.push_back(new AstNode(
          AST_PARASET, limit2 ? limit2->t_min : AstNode::mkconst_int(0, true)));
      cell->children.back()->str = "\\T_LIMIT2_MIN";

      cell->children.push_back(new AstNode(
          AST_PARASET, limit2 ? limit2->t_avg : AstNode::mkconst_int(0, true)));
      cell->children.back()->str = "\\T_LIMIT2_TYP";

      cell->children.push_back(new AstNode(
          AST_PARASET, limit2 ? limit2->t_max : AstNode::mkconst_int(0, true)));
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
#line 5213 "verilog_parser.tab.cc"
    break;

    case 238: /* specify_opt_triple: ',' specify_triple  */
#line 1374 "verilog_parser.y"
    {
      (yyval.specify_triple_ptr) = (yyvsp[0].specify_triple_ptr);
    }
#line 5221 "verilog_parser.tab.cc"
    break;

    case 239: /* specify_opt_triple: %empty  */
#line 1377 "verilog_parser.y"
    {
      (yyval.specify_triple_ptr) = nullptr;
    }
#line 5229 "verilog_parser.tab.cc"
    break;

    case 240: /* specify_if: TOK_IF '(' expr ')'  */
#line 1382 "verilog_parser.y"
    {
      (yyval.ast) = (yyvsp[-1].ast);
    }
#line 5237 "verilog_parser.tab.cc"
    break;

    case 241: /* specify_if: %empty  */
#line 1385 "verilog_parser.y"
    {
      (yyval.ast) = nullptr;
    }
#line 5245 "verilog_parser.tab.cc"
    break;

    case 242: /* specify_condition: TOK_SPECIFY_AND expr  */
#line 1390 "verilog_parser.y"
    {
      (yyval.ast) = (yyvsp[0].ast);
    }
#line 5253 "verilog_parser.tab.cc"
    break;

    case 243: /* specify_condition: %empty  */
#line 1393 "verilog_parser.y"
    {
      (yyval.ast) = nullptr;
    }
#line 5261 "verilog_parser.tab.cc"
    break;

    case 244: /* specify_target: expr  */
#line 1398 "verilog_parser.y"
    {
      (yyval.specify_target_ptr) = new specify_target;
      (yyval.specify_target_ptr)->polarity_op = 0;
      (yyval.specify_target_ptr)->dst = (yyvsp[0].ast);
      (yyval.specify_target_ptr)->dat = nullptr;
    }
#line 5272 "verilog_parser.tab.cc"
    break;

    case 245: /* specify_target: '(' expr ':' expr ')'  */
#line 1404 "verilog_parser.y"
    {
      (yyval.specify_target_ptr) = new specify_target;
      (yyval.specify_target_ptr)->polarity_op = 0;
      (yyval.specify_target_ptr)->dst = (yyvsp[-3].ast);
      (yyval.specify_target_ptr)->dat = (yyvsp[-1].ast);
    }
#line 5283 "verilog_parser.tab.cc"
    break;

    case 246: /* specify_target: '(' expr TOK_NEG_INDEXED expr ')'  */
#line 1410 "verilog_parser.y"
    {
      (yyval.specify_target_ptr) = new specify_target;
      (yyval.specify_target_ptr)->polarity_op = '-';
      (yyval.specify_target_ptr)->dst = (yyvsp[-3].ast);
      (yyval.specify_target_ptr)->dat = (yyvsp[-1].ast);
    }
#line 5294 "verilog_parser.tab.cc"
    break;

    case 247: /* specify_target: '(' expr TOK_POS_INDEXED expr ')'  */
#line 1416 "verilog_parser.y"
    {
      (yyval.specify_target_ptr) = new specify_target;
      (yyval.specify_target_ptr)->polarity_op = '+';
      (yyval.specify_target_ptr)->dst = (yyvsp[-3].ast);
      (yyval.specify_target_ptr)->dat = (yyvsp[-1].ast);
    }
#line 5305 "verilog_parser.tab.cc"
    break;

    case 248: /* specify_edge: TOK_POSEDGE  */
#line 1424 "verilog_parser.y"
    {
      (yyval.ch) = 'p';
    }
#line 5311 "verilog_parser.tab.cc"
    break;

    case 249: /* specify_edge: TOK_NEGEDGE  */
#line 1425 "verilog_parser.y"
    {
      (yyval.ch) = 'n';
    }
#line 5317 "verilog_parser.tab.cc"
    break;

    case 250: /* specify_edge: %empty  */
#line 1426 "verilog_parser.y"
    {
      (yyval.ch) = 0;
    }
#line 5323 "verilog_parser.tab.cc"
    break;

    case 251: /* specify_rise_fall: specify_triple  */
#line 1429 "verilog_parser.y"
    {
      (yyval.specify_rise_fall_ptr) = new specify_rise_fall;
      (yyval.specify_rise_fall_ptr)->rise = *(yyvsp[0].specify_triple_ptr);
      (yyval.specify_rise_fall_ptr)->fall.t_min =
          (yyvsp[0].specify_triple_ptr)->t_min->clone();
      (yyval.specify_rise_fall_ptr)->fall.t_avg =
          (yyvsp[0].specify_triple_ptr)->t_avg->clone();
      (yyval.specify_rise_fall_ptr)->fall.t_max =
          (yyvsp[0].specify_triple_ptr)->t_max->clone();
      delete (yyvsp[0].specify_triple_ptr);
    }
#line 5336 "verilog_parser.tab.cc"
    break;

    case 252: /* specify_rise_fall: '(' specify_triple ',' specify_triple ')' */
#line 1437 "verilog_parser.y"
    {
      (yyval.specify_rise_fall_ptr) = new specify_rise_fall;
      (yyval.specify_rise_fall_ptr)->rise = *(yyvsp[-3].specify_triple_ptr);
      (yyval.specify_rise_fall_ptr)->fall = *(yyvsp[-1].specify_triple_ptr);
      delete (yyvsp[-3].specify_triple_ptr);
      delete (yyvsp[-1].specify_triple_ptr);
    }
#line 5348 "verilog_parser.tab.cc"
    break;

    case 253: /* specify_rise_fall: '(' specify_triple ',' specify_triple ','
                 specify_triple ')'  */
#line 1444 "verilog_parser.y"
    {
      (yyval.specify_rise_fall_ptr) = new specify_rise_fall;
      (yyval.specify_rise_fall_ptr)->rise = *(yyvsp[-5].specify_triple_ptr);
      (yyval.specify_rise_fall_ptr)->fall = *(yyvsp[-3].specify_triple_ptr);
      delete (yyvsp[-5].specify_triple_ptr);
      delete (yyvsp[-3].specify_triple_ptr);
      delete (yyvsp[-1].specify_triple_ptr);
      log_file_warning(current_filename, get_line_num(),
                       "Path delay expressions beyond rise/fall not currently "
                       "supported. Ignoring.\n");
    }
#line 5362 "verilog_parser.tab.cc"
    break;

    case 254: /* specify_rise_fall: '(' specify_triple ',' specify_triple ','
                 specify_triple ',' specify_triple ',' specify_triple ','
                 specify_triple ')'  */
#line 1453 "verilog_parser.y"
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
      log_file_warning(current_filename, get_line_num(),
                       "Path delay expressions beyond rise/fall not currently "
                       "supported. Ignoring.\n");
    }
#line 5379 "verilog_parser.tab.cc"
    break;

    case 255: /* specify_rise_fall: '(' specify_triple ',' specify_triple ','
                 specify_triple ',' specify_triple ',' specify_triple ','
                 specify_triple ',' specify_triple ',' specify_triple ','
                 specify_triple ',' specify_triple ',' specify_triple ','
                 specify_triple ')'  */
#line 1465 "verilog_parser.y"
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
      log_file_warning(current_filename, get_line_num(),
                       "Path delay expressions beyond rise/fall not currently "
                       "supported. Ignoring.\n");
    }
#line 5402 "verilog_parser.tab.cc"
    break;

    case 256: /* specify_triple: expr  */
#line 1485 "verilog_parser.y"
    {
      (yyval.specify_triple_ptr) = new specify_triple;
      (yyval.specify_triple_ptr)->t_min = (yyvsp[0].ast);
      (yyval.specify_triple_ptr)->t_avg = (yyvsp[0].ast)->clone();
      (yyval.specify_triple_ptr)->t_max = (yyvsp[0].ast)->clone();
    }
#line 5413 "verilog_parser.tab.cc"
    break;

    case 257: /* specify_triple: expr ':' expr ':' expr  */
#line 1491 "verilog_parser.y"
    {
      (yyval.specify_triple_ptr) = new specify_triple;
      (yyval.specify_triple_ptr)->t_min = (yyvsp[-4].ast);
      (yyval.specify_triple_ptr)->t_avg = (yyvsp[-2].ast);
      (yyval.specify_triple_ptr)->t_max = (yyvsp[0].ast);
    }
#line 5424 "verilog_parser.tab.cc"
    break;

    case 307: /* ignspec_constant_expression: expr  */
#line 1612 "verilog_parser.y"
    {
      delete (yyvsp[0].ast);
    }
#line 5430 "verilog_parser.tab.cc"
    break;

    case 308: /* ignspec_expr: expr  */
#line 1615 "verilog_parser.y"
    {
      delete (yyvsp[0].ast);
    }
#line 5436 "verilog_parser.tab.cc"
    break;

    case 309: /* ignspec_expr: expr ':' expr ':' expr  */
#line 1616 "verilog_parser.y"
    {
      delete (yyvsp[-4].ast);
      delete (yyvsp[-2].ast);
      delete (yyvsp[0].ast);
    }
#line 5446 "verilog_parser.tab.cc"
    break;

    case 310: /* $@35: %empty  */
#line 1623 "verilog_parser.y"
    {
      delete (yyvsp[0].string);
    }
#line 5452 "verilog_parser.tab.cc"
    break;

    case 311: /* ignspec_id: TOK_ID $@35 range_or_multirange  */
#line 1624 "verilog_parser.y"
    {
      delete (yyvsp[0].ast);
    }
#line 5458 "verilog_parser.tab.cc"
    break;

    case 312: /* param_signed: TOK_SIGNED  */
#line 1629 "verilog_parser.y"
    {
      astbuf1->is_signed = true;
    }
#line 5466 "verilog_parser.tab.cc"
    break;

    case 313: /* param_signed: TOK_UNSIGNED  */
#line 1631 "verilog_parser.y"
    {
      astbuf1->is_signed = false;
    }
#line 5474 "verilog_parser.tab.cc"
    break;

    case 315: /* param_integer: type_atom  */
#line 1636 "verilog_parser.y"
    {
      astbuf1->is_reg = false;
    }
#line 5482 "verilog_parser.tab.cc"
    break;

    case 316: /* param_real: TOK_REAL  */
#line 1641 "verilog_parser.y"
    {
      astbuf1->children.push_back(new AstNode(AST_REALVALUE));
    }
#line 5490 "verilog_parser.tab.cc"
    break;

    case 317: /* param_range: range  */
#line 1646 "verilog_parser.y"
    {
      if ((yyvsp[0].ast) != NULL) {
        astbuf1->children.push_back((yyvsp[0].ast));
      }
    }
#line 5500 "verilog_parser.tab.cc"
    break;

    case 319: /* param_range_type: type_vec param_signed  */
#line 1654 "verilog_parser.y"
    {
      addRange(astbuf1, 0, 0);
    }
#line 5508 "verilog_parser.tab.cc"
    break;

    case 320: /* param_range_type: type_vec param_signed non_opt_range  */
#line 1657 "verilog_parser.y"
    {
      astbuf1->children.push_back((yyvsp[0].ast));
    }
#line 5516 "verilog_parser.tab.cc"
    break;

    case 326: /* param_type: hierarchical_type_id  */
#line 1664 "verilog_parser.y"
    {
      addWiretypeNode((yyvsp[0].string), astbuf1);
    }
#line 5524 "verilog_parser.tab.cc"
    break;

    case 327: /* $@36: %empty  */
#line 1669 "verilog_parser.y"
    {
      astbuf1 = new AstNode(AST_PARAMETER);
      astbuf1->children.push_back(AstNode::mkconst_int(0, true));
      append_attr(astbuf1, (yyvsp[-1].al));
    }
#line 5534 "verilog_parser.tab.cc"
    break;

    case 328: /* param_decl: attr TOK_PARAMETER $@36 param_type param_decl_list
                 ';'  */
#line 1673 "verilog_parser.y"
    {
      delete astbuf1;
    }
#line 5542 "verilog_parser.tab.cc"
    break;

    case 329: /* $@37: %empty  */
#line 1678 "verilog_parser.y"
    {
      astbuf1 = new AstNode(AST_LOCALPARAM);
      astbuf1->children.push_back(AstNode::mkconst_int(0, true));
      append_attr(astbuf1, (yyvsp[-1].al));
    }
#line 5552 "verilog_parser.tab.cc"
    break;

    case 330: /* localparam_decl: attr TOK_LOCALPARAM $@37 param_type
                 param_decl_list ';'  */
#line 1682 "verilog_parser.y"
    {
      delete astbuf1;
    }
#line 5560 "verilog_parser.tab.cc"
    break;

    case 333: /* single_param_decl: single_param_decl_ident '=' expr  */
#line 1690 "verilog_parser.y"
    {
      AstNode *decl = ast_stack.back()->children.back();
      log_assert(decl->type == AST_PARAMETER || decl->type == AST_LOCALPARAM);
      delete decl->children[0];
      decl->children[0] = (yyvsp[0].ast);
    }
#line 5571 "verilog_parser.tab.cc"
    break;

    case 334: /* single_param_decl: single_param_decl_ident  */
#line 1696 "verilog_parser.y"
    {
      AstNode *decl = ast_stack.back()->children.back();
      if (decl->type != AST_PARAMETER) {
        log_assert(decl->type == AST_LOCALPARAM);
        frontend_verilog_yyerror("localparam initialization is missing!");
      }
      if (!sv_mode)
        frontend_verilog_yyerror(
            "Parameter defaults can only be omitted in SystemVerilog mode!");
      delete decl->children[0];
      decl->children.erase(decl->children.begin());
    }
#line 5587 "verilog_parser.tab.cc"
    break;

    case 335: /* single_param_decl_ident: TOK_ID  */
#line 1709 "verilog_parser.y"
    {
      AstNode *node;
      if (astbuf1 == nullptr) {
        if (!sv_mode)
          frontend_verilog_yyerror(
              "In pure Verilog (not SystemVerilog), parameter/localparam with "
              "an initializer must use the parameter/localparam keyword");
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
#line 5607 "verilog_parser.tab.cc"
    break;

    case 339: /* single_defparam_decl: range rvalue '=' expr  */
#line 1732 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_DEFPARAM);
      node->children.push_back((yyvsp[-2].ast));
      node->children.push_back((yyvsp[0].ast));
      if ((yyvsp[-3].ast) != NULL)
        node->children.push_back((yyvsp[-3].ast));
      ast_stack.back()->children.push_back(node);
    }
#line 5620 "verilog_parser.tab.cc"
    break;

    case 340: /* $@38: %empty  */
#line 1745 "verilog_parser.y"
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
#line 5636 "verilog_parser.tab.cc"
    break;

    case 341: /* enum_type: TOK_ENUM $@38 enum_base_type '{' enum_name_list
                 optional_comma '}'  */
#line 1755 "verilog_parser.y"
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
#line 5653 "verilog_parser.tab.cc"
    break;

    case 343: /* enum_base_type: type_vec type_signing range  */
#line 1769 "verilog_parser.y"
    {
      if ((yyvsp[0].ast))
        astbuf1->children.push_back((yyvsp[0].ast));
    }
#line 5659 "verilog_parser.tab.cc"
    break;

    case 344: /* enum_base_type: %empty  */
#line 1770 "verilog_parser.y"
    {
      astbuf1->is_reg = true;
      addRange(astbuf1);
    }
#line 5665 "verilog_parser.tab.cc"
    break;

    case 345: /* type_atom: integer_atom_type  */
#line 1774 "verilog_parser.y"
    {
      astbuf1->is_reg = true;
      astbuf1->is_signed = true;
      addRange(astbuf1, (yyvsp[0].integer) - 1, 0);
    }
#line 5675 "verilog_parser.tab.cc"
    break;

    case 346: /* type_vec: TOK_REG  */
#line 1780 "verilog_parser.y"
    {
      astbuf1->is_reg = true;
    }
#line 5681 "verilog_parser.tab.cc"
    break;

    case 347: /* type_vec: TOK_LOGIC  */
#line 1781 "verilog_parser.y"
    {
      astbuf1->is_logic = true;
    }
#line 5687 "verilog_parser.tab.cc"
    break;

    case 348: /* type_signing: TOK_SIGNED  */
#line 1785 "verilog_parser.y"
    {
      astbuf1->is_signed = true;
    }
#line 5693 "verilog_parser.tab.cc"
    break;

    case 349: /* type_signing: TOK_UNSIGNED  */
#line 1786 "verilog_parser.y"
    {
      astbuf1->is_signed = false;
    }
#line 5699 "verilog_parser.tab.cc"
    break;

    case 353: /* enum_name_decl: TOK_ID opt_enum_init  */
#line 1795 "verilog_parser.y"
    {
      // put in fn
      log_assert(astbuf1);
      log_assert(astbuf2);
      auto node = astbuf1->clone();
      node->str = *(yyvsp[-1].string);
      delete (yyvsp[-1].string);
      SET_AST_NODE_LOC(node, (yylsp[-1]), (yylsp[-1]));
      delete node->children[0];
      node->children[0] =
          (yyvsp[0].ast) ? (yyvsp[0].ast) : new AstNode(AST_NONE);
      astbuf2->children.push_back(node);
    }
#line 5716 "verilog_parser.tab.cc"
    break;

    case 354: /* opt_enum_init: '=' basic_expr  */
#line 1810 "verilog_parser.y"
    {
      (yyval.ast) = (yyvsp[0].ast);
    }
#line 5722 "verilog_parser.tab.cc"
    break;

    case 355: /* opt_enum_init: %empty  */
#line 1811 "verilog_parser.y"
    {
      (yyval.ast) = NULL;
    }
#line 5728 "verilog_parser.tab.cc"
    break;

    case 358: /* enum_var: TOK_ID  */
#line 1819 "verilog_parser.y"
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
#line 5743 "verilog_parser.tab.cc"
    break;

    case 359: /* enum_decl: enum_type enum_var_list ';'  */
#line 1831 "verilog_parser.y"
    {
      delete (yyvsp[-2].ast);
    }
#line 5749 "verilog_parser.tab.cc"
    break;

    case 360: /* $@39: %empty  */
#line 1839 "verilog_parser.y"
    {
      append_attr((yyvsp[0].ast), (yyvsp[-1].al));
    }
#line 5757 "verilog_parser.tab.cc"
    break;

    case 361: /* struct_decl: attr struct_type $@39 struct_var_list ';'  */
#line 1841 "verilog_parser.y"
    {
      delete astbuf2;
    }
#line 5765 "verilog_parser.tab.cc"
    break;

    case 362: /* $@40: %empty  */
#line 1846 "verilog_parser.y"
    {
      astbuf2 = (yyvsp[0].ast);
    }
#line 5771 "verilog_parser.tab.cc"
    break;

    case 363: /* struct_type: struct_union $@40 struct_body  */
#line 1846 "verilog_parser.y"
    {
      (yyval.ast) = astbuf2;
    }
#line 5777 "verilog_parser.tab.cc"
    break;

    case 364: /* struct_union: TOK_STRUCT  */
#line 1850 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_STRUCT);
    }
#line 5783 "verilog_parser.tab.cc"
    break;

    case 365: /* struct_union: TOK_UNION  */
#line 1851 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_UNION);
    }
#line 5789 "verilog_parser.tab.cc"
    break;

    case 368: /* opt_packed: %empty  */
#line 1859 "verilog_parser.y"
    {
      frontend_verilog_yyerror("Only PACKED supported at this time");
    }
#line 5795 "verilog_parser.tab.cc"
    break;

    case 369: /* opt_signed_struct: TOK_SIGNED  */
#line 1862 "verilog_parser.y"
    {
      astbuf2->is_signed = true;
    }
#line 5801 "verilog_parser.tab.cc"
    break;

    case 370: /* opt_signed_struct: TOK_UNSIGNED  */
#line 1863 "verilog_parser.y"
    {
      astbuf2->is_signed = false;
    }
#line 5807 "verilog_parser.tab.cc"
    break;

    case 374: /* struct_member: struct_member_type member_name_list ';'  */
#line 1871 "verilog_parser.y"
    {
      delete astbuf1;
    }
#line 5813 "verilog_parser.tab.cc"
    break;

    case 377: /* $@41: %empty  */
#line 1879 "verilog_parser.y"
    {
      astbuf1->str = (yyvsp[0].string)->substr(1);
      delete (yyvsp[0].string);
      astbuf3 = astbuf1->clone();
      SET_AST_NODE_LOC(astbuf3, (yylsp[0]), (yylsp[0]));
      astbuf2->children.push_back(astbuf3);
    }
#line 5825 "verilog_parser.tab.cc"
    break;

    case 378: /* member_name: TOK_ID $@41 range  */
#line 1885 "verilog_parser.y"
    {
      if ((yyvsp[0].ast))
        astbuf3->children.push_back((yyvsp[0].ast));
    }
#line 5831 "verilog_parser.tab.cc"
    break;

    case 379: /* $@42: %empty  */
#line 1888 "verilog_parser.y"
    {
      astbuf1 = new AstNode(AST_STRUCT_ITEM);
    }
#line 5837 "verilog_parser.tab.cc"
    break;

    case 382: /* member_type_token: hierarchical_type_id  */
#line 1893 "verilog_parser.y"
    {
      addWiretypeNode((yyvsp[0].string), astbuf1);
    }
#line 5845 "verilog_parser.tab.cc"
    break;

    case 383: /* $@43: %empty  */
#line 1896 "verilog_parser.y"
    {
      delete astbuf1;
    }
#line 5853 "verilog_parser.tab.cc"
    break;

    case 384: /* $@44: %empty  */
#line 1898 "verilog_parser.y"
    {
      // stash state on ast_stack
      ast_stack.push_back(astbuf2);
      astbuf2 = (yyvsp[0].ast);
    }
#line 5863 "verilog_parser.tab.cc"
    break;

    case 385: /* member_type_token: $@43 struct_union $@44 struct_body  */
#line 1902 "verilog_parser.y"
    {
      astbuf1 = astbuf2;
      // recover state
      astbuf2 = ast_stack.back();
      ast_stack.pop_back();
    }
#line 5874 "verilog_parser.tab.cc"
    break;

    case 387: /* member_type: type_vec type_signing range_or_multirange  */
#line 1911 "verilog_parser.y"
    {
      if ((yyvsp[0].ast))
        astbuf1->children.push_back((yyvsp[0].ast));
    }
#line 5880 "verilog_parser.tab.cc"
    break;

    case 390: /* struct_var: TOK_ID  */
#line 1918 "verilog_parser.y"
    {
      auto *var_node = astbuf2->clone();
      var_node->str = *(yyvsp[0].string);
      delete (yyvsp[0].string);
      SET_AST_NODE_LOC(var_node, (yylsp[0]), (yylsp[0]));
      ast_stack.back()->children.push_back(var_node);
    }
#line 5891 "verilog_parser.tab.cc"
    break;

    case 391: /* $@45: %empty  */
#line 1931 "verilog_parser.y"
    {
      albuf = (yyvsp[-2].al);
      astbuf1 = (yyvsp[-1].ast);
      astbuf2 = checkRange(astbuf1, (yyvsp[0].ast));
    }
#line 5901 "verilog_parser.tab.cc"
    break;

    case 392: /* $@46: %empty  */
#line 1935 "verilog_parser.y"
    {
      delete astbuf1;
      if (astbuf2 != NULL)
        delete astbuf2;
      free_attr(albuf);
    }
#line 5912 "verilog_parser.tab.cc"
    break;

    case 394: /* $@47: %empty  */
#line 1941 "verilog_parser.y"
    {
      ast_stack.back()->children.push_back(new AstNode(AST_WIRE));
      ast_stack.back()->children.back()->str = *(yyvsp[0].string);
      append_attr(ast_stack.back()->children.back(), (yyvsp[-2].al));
      ast_stack.back()->children.push_back(
          new AstNode(AST_ASSIGN, new AstNode(AST_IDENTIFIER),
                      AstNode::mkconst_int(0, false, 1)));
      ast_stack.back()->children.back()->children[0]->str = *(yyvsp[0].string);
      delete (yyvsp[0].string);
    }
#line 5925 "verilog_parser.tab.cc"
    break;

    case 396: /* $@48: %empty  */
#line 1949 "verilog_parser.y"
    {
      ast_stack.back()->children.push_back(new AstNode(AST_WIRE));
      ast_stack.back()->children.back()->str = *(yyvsp[0].string);
      append_attr(ast_stack.back()->children.back(), (yyvsp[-2].al));
      ast_stack.back()->children.push_back(
          new AstNode(AST_ASSIGN, new AstNode(AST_IDENTIFIER),
                      AstNode::mkconst_int(1, false, 1)));
      ast_stack.back()->children.back()->children[0]->str = *(yyvsp[0].string);
      delete (yyvsp[0].string);
    }
#line 5938 "verilog_parser.tab.cc"
    break;

    case 399: /* opt_supply_wires: opt_supply_wires ',' TOK_ID  */
#line 1960 "verilog_parser.y"
    {
      AstNode *wire_node =
          ast_stack.back()
              ->children.at(GetSize(ast_stack.back()->children) - 2)
              ->clone();
      AstNode *assign_node =
          ast_stack.back()
              ->children.at(GetSize(ast_stack.back()->children) - 1)
              ->clone();
      wire_node->str = *(yyvsp[0].string);
      assign_node->children[0]->str = *(yyvsp[0].string);
      ast_stack.back()->children.push_back(wire_node);
      ast_stack.back()->children.push_back(assign_node);
      delete (yyvsp[0].string);
    }
#line 5952 "verilog_parser.tab.cc"
    break;

    case 402: /* wire_name_and_opt_assign: wire_name  */
#line 1974 "verilog_parser.y"
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
      if (current_wire_rand || attr_anyconst || attr_anyseq || attr_allconst ||
          attr_allseq) {
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
        fcall->attributes[ID::reg] =
            AstNode::mkconst_str(RTLIL::unescape_id(wire->str));
        ast_stack.back()->children.push_back(
            new AstNode(AST_ASSIGN, wire, fcall));
      }
    }
#line 5999 "verilog_parser.tab.cc"
    break;

    case 403: /* wire_name_and_opt_assign: wire_name '=' expr  */
#line 2016 "verilog_parser.y"
    {
      AstNode *wire = new AstNode(AST_IDENTIFIER);
      wire->str = ast_stack.back()->children.back()->str;
      if (astbuf1->is_input) {
        if (astbuf1->attributes.count(ID::defaultvalue))
          delete astbuf1->attributes.at(ID::defaultvalue);
        astbuf1->attributes[ID::defaultvalue] = (yyvsp[0].ast);
      } else if (astbuf1->is_reg || astbuf1->is_logic) {
        AstNode *assign = new AstNode(AST_ASSIGN_LE, wire, (yyvsp[0].ast));
        AstNode *block = new AstNode(AST_BLOCK, assign);
        AstNode *init = new AstNode(AST_INITIAL, block);

        SET_AST_NODE_LOC(assign, (yylsp[-2]), (yylsp[0]));
        SET_AST_NODE_LOC(block, (yylsp[-2]), (yylsp[0]));
        SET_AST_NODE_LOC(init, (yylsp[-2]), (yylsp[0]));

        ast_stack.back()->children.push_back(init);
      } else {
        AstNode *assign = new AstNode(AST_ASSIGN, wire, (yyvsp[0].ast));
        SET_AST_NODE_LOC(assign, (yylsp[-2]), (yylsp[0]));
        ast_stack.back()->children.push_back(assign);
      }

    }
#line 6030 "verilog_parser.tab.cc"
    break;

    case 404: /* wire_name: TOK_ID range_or_multirange  */
#line 2044 "verilog_parser.y"
    {
      if (astbuf1 == nullptr)
        frontend_verilog_yyerror(
            "Internal error - should not happen - no AST_WIRE node.");
      AstNode *node = astbuf1->clone();
      node->str = *(yyvsp[-1].string);
      append_attr_clone(node, albuf);
      if (astbuf2 != NULL)
        node->children.push_back(astbuf2->clone());
      if ((yyvsp[0].ast) != NULL) {
        if (node->is_input || node->is_output)
          frontend_verilog_yyerror(
              "input/output/inout ports cannot have unpacked dimensions.");
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
          frontend_verilog_yyerror(
              "Cannot declare module port `%s' within a generate block.",
              (yyvsp[-1].string)->c_str());
      } else {
        if (do_not_require_port_stubs && (node->is_input || node->is_output) &&
            port_stubs.count(*(yyvsp[-1].string)) == 0) {
          port_stubs[*(yyvsp[-1].string)] = ++port_counter;
        }
        if (port_stubs.count(*(yyvsp[-1].string)) != 0) {
          if (!node->is_input && !node->is_output)
            frontend_verilog_yyerror(
                "Module port `%s' is neither input nor output.",
                (yyvsp[-1].string)->c_str());
          if (node->is_reg && node->is_input && !node->is_output && !sv_mode)
            frontend_verilog_yyerror("Input port `%s' is declared as register.",
                                     (yyvsp[-1].string)->c_str());
          node->port_id = port_stubs[*(yyvsp[-1].string)];
          port_stubs.erase(*(yyvsp[-1].string));
        } else {
          if (node->is_input || node->is_output)
            frontend_verilog_yyerror(
                "Module port `%s' is not declared in module header.",
                (yyvsp[-1].string)->c_str());
        }
      }
      // FIXME: for some reason, TOK_ID has a location which always points to
      // one column *after* the real last column...
      SET_AST_NODE_LOC(node, (yylsp[-1]), (yylsp[-1]));
      ast_stack.back()->children.push_back(node);

      delete (yyvsp[-1].string);
    }
#line 6079 "verilog_parser.tab.cc"
    break;

    case 408: /* assign_expr: lvalue '=' expr  */
#line 2096 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_ASSIGN, (yyvsp[-2].ast), (yyvsp[0].ast));
      SET_AST_NODE_LOC(node, (yyloc), (yyloc));
      ast_stack.back()->children.push_back(node);
    }
#line 6089 "verilog_parser.tab.cc"
    break;

    case 410: /* type_name: TOK_USER_TYPE  */
#line 2103 "verilog_parser.y"
    {
      if (isInLocalScope((yyvsp[0].string)))
        frontend_verilog_yyerror("Duplicate declaration of TYPEDEF '%s'",
                                 (yyvsp[0].string)->c_str() + 1);
    }
#line 6095 "verilog_parser.tab.cc"
    break;

    case 411: /* typedef_decl: TOK_TYPEDEF typedef_base_type range type_name
                 range_or_multirange ';'  */
#line 2107 "verilog_parser.y"
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
      addTypedefNode((yyvsp[-2].string), astbuf1);
    }
#line 6113 "verilog_parser.tab.cc"
    break;

    case 412: /* typedef_decl: TOK_TYPEDEF enum_struct_type type_name ';'  */
#line 2120 "verilog_parser.y"
    {
      addTypedefNode((yyvsp[-1].string), (yyvsp[-2].ast));
    }
#line 6119 "verilog_parser.tab.cc"
    break;

    case 413: /* typedef_base_type: hierarchical_type_id  */
#line 2124 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_WIRE);
      (yyval.ast)->is_logic = true;
      addWiretypeNode((yyvsp[0].string), (yyval.ast));
    }
#line 6129 "verilog_parser.tab.cc"
    break;

    case 414: /* typedef_base_type: integer_vector_type
                 opt_signedness_default_unsigned  */
#line 2129 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_WIRE);
      if ((yyvsp[-1].integer) == TOK_REG) {
        (yyval.ast)->is_reg = true;
      } else {
        (yyval.ast)->is_logic = true;
      }
      (yyval.ast)->is_signed = (yyvsp[0].boolean);
    }
#line 6143 "verilog_parser.tab.cc"
    break;

    case 415: /* typedef_base_type: integer_atom_type
                 opt_signedness_default_signed  */
#line 2138 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_WIRE);
      (yyval.ast)->is_logic = true;
      (yyval.ast)->is_signed = (yyvsp[0].boolean);
      (yyval.ast)->range_left = (yyvsp[-1].integer) - 1;
      (yyval.ast)->range_right = 0;
    }
#line 6155 "verilog_parser.tab.cc"
    break;

    case 418: /* $@49: %empty  */
#line 2152 "verilog_parser.y"
    {
      astbuf1 = new AstNode(AST_CELL);
      append_attr(astbuf1, (yyvsp[-1].al));
      astbuf1->children.push_back(new AstNode(AST_CELLTYPE));
      astbuf1->children[0]->str = *(yyvsp[0].string);
      delete (yyvsp[0].string);
    }
#line 6167 "verilog_parser.tab.cc"
    break;

    case 419: /* cell_stmt: attr TOK_ID $@49 cell_parameter_list_opt cell_list
                 ';'  */
#line 2158 "verilog_parser.y"
    {
      delete astbuf1;
    }
#line 6175 "verilog_parser.tab.cc"
    break;

    case 420: /* $@50: %empty  */
#line 2161 "verilog_parser.y"
    {
      astbuf1 = new AstNode(AST_PRIMITIVE);
      astbuf1->str = *(yyvsp[-1].string);
      append_attr(astbuf1, (yyvsp[-2].al));
      delete (yyvsp[-1].string);
    }
#line 6186 "verilog_parser.tab.cc"
    break;

    case 421: /* cell_stmt: attr tok_prim_wrapper delay $@50 prim_list ';'  */
#line 2166 "verilog_parser.y"
    {
      delete astbuf1;
    }
#line 6194 "verilog_parser.tab.cc"
    break;

    case 422: /* tok_prim_wrapper: TOK_PRIMITIVE  */
#line 2171 "verilog_parser.y"
    {
      (yyval.string) = (yyvsp[0].string);
    }
#line 6202 "verilog_parser.tab.cc"
    break;

    case 423: /* tok_prim_wrapper: TOK_OR  */
#line 2174 "verilog_parser.y"
    {
      (yyval.string) = new std::string("or");
    }
#line 6210 "verilog_parser.tab.cc"
    break;

    case 428: /* $@51: %empty  */
#line 2186 "verilog_parser.y"
    {
      astbuf2 = astbuf1->clone();
      if (astbuf2->type != AST_PRIMITIVE)
        astbuf2->str = *(yyvsp[0].string);
      delete (yyvsp[0].string);
      ast_stack.back()->children.push_back(astbuf2);
    }
#line 6222 "verilog_parser.tab.cc"
    break;

    case 429: /* single_cell_no_array: TOK_ID $@51 '(' cell_port_list ')'  */
#line 2192 "verilog_parser.y"
    {
      SET_AST_NODE_LOC(astbuf2, (yylsp[-4]), (yyloc));
    }
#line 6230 "verilog_parser.tab.cc"
    break;

    case 430: /* $@52: %empty  */
#line 2197 "verilog_parser.y"
    {
      astbuf2 = astbuf1->clone();
      if (astbuf2->type != AST_PRIMITIVE)
        astbuf2->str = *(yyvsp[-1].string);
      delete (yyvsp[-1].string);
      ast_stack.back()->children.push_back(
          new AstNode(AST_CELLARRAY, (yyvsp[0].ast), astbuf2));
    }
#line 6242 "verilog_parser.tab.cc"
    break;

    case 431: /* single_cell_arraylist: TOK_ID non_opt_range $@52 '('
                 cell_port_list ')'  */
#line 2203 "verilog_parser.y"
    {
      SET_AST_NODE_LOC(astbuf2, (yylsp[-5]), (yyloc));
    }
#line 6250 "verilog_parser.tab.cc"
    break;

    case 437: /* $@53: %empty  */
#line 2217 "verilog_parser.y"
    {
      astbuf2 = astbuf1->clone();
      ast_stack.back()->children.push_back(astbuf2);
    }
#line 6259 "verilog_parser.tab.cc"
    break;

    case 438: /* single_prim: $@53 '(' cell_port_list ')'  */
#line 2220 "verilog_parser.y"
    {
      SET_AST_NODE_LOC(astbuf2, (yylsp[-3]), (yyloc));
    }
#line 6267 "verilog_parser.tab.cc"
    break;

    case 444: /* cell_parameter: expr  */
#line 2232 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_PARASET);
      astbuf1->children.push_back(node);
      node->children.push_back((yyvsp[0].ast));
    }
#line 6277 "verilog_parser.tab.cc"
    break;

    case 445: /* cell_parameter: '.' TOK_ID '(' ')'  */
#line 2237 "verilog_parser.y"
    {
      // just ignore empty parameters
    }
#line 6285 "verilog_parser.tab.cc"
    break;

    case 446: /* cell_parameter: '.' TOK_ID '(' expr ')'  */
#line 2240 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_PARASET);
      node->str = *(yyvsp[-3].string);
      astbuf1->children.push_back(node);
      node->children.push_back((yyvsp[-1].ast));
      delete (yyvsp[-3].string);
    }
#line 6297 "verilog_parser.tab.cc"
    break;

    case 447: /* cell_port_list: cell_port_list_rules  */
#line 2249 "verilog_parser.y"
    {
      // remove empty args from end of list
      while (!astbuf2->children.empty()) {
        AstNode *node = astbuf2->children.back();
        if (node->type != AST_ARGUMENT)
          break;
        if (!node->children.empty())
          break;
        if (!node->str.empty())
          break;
        astbuf2->children.pop_back();
        delete node;
      }

      // check port types
      bool has_positional_args = false;
      bool has_named_args = false;
      for (auto node : astbuf2->children) {
        if (node->type != AST_ARGUMENT)
          continue;
        if (node->str.empty())
          has_positional_args = true;
        else
          has_named_args = true;
      }

      if (has_positional_args && has_named_args)
        frontend_verilog_yyerror("Mix of positional and named cell ports.");
    }
#line 6327 "verilog_parser.tab.cc"
    break;

    case 450: /* cell_port: attr  */
#line 2279 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_ARGUMENT);
      astbuf2->children.push_back(node);
      free_attr((yyvsp[0].al));
    }
#line 6337 "verilog_parser.tab.cc"
    break;

    case 451: /* cell_port: attr expr  */
#line 2284 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_ARGUMENT);
      astbuf2->children.push_back(node);
      node->children.push_back((yyvsp[0].ast));
      free_attr((yyvsp[-1].al));
    }
#line 6348 "verilog_parser.tab.cc"
    break;

    case 452: /* cell_port: attr '.' TOK_ID '(' expr ')'  */
#line 2290 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_ARGUMENT);
      node->str = *(yyvsp[-3].string);
      astbuf2->children.push_back(node);
      node->children.push_back((yyvsp[-1].ast));
      delete (yyvsp[-3].string);
      free_attr((yyvsp[-5].al));
    }
#line 6361 "verilog_parser.tab.cc"
    break;

    case 453: /* cell_port: attr '.' TOK_ID '(' ')'  */
#line 2298 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_ARGUMENT);
      node->str = *(yyvsp[-2].string);
      astbuf2->children.push_back(node);
      delete (yyvsp[-2].string);
      free_attr((yyvsp[-4].al));
    }
#line 6373 "verilog_parser.tab.cc"
    break;

    case 454: /* cell_port: attr '.' TOK_ID  */
#line 2305 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_ARGUMENT);
      node->str = *(yyvsp[0].string);
      astbuf2->children.push_back(node);
      node->children.push_back(new AstNode(AST_IDENTIFIER));
      node->children.back()->str = *(yyvsp[0].string);
      delete (yyvsp[0].string);
      free_attr((yyvsp[-2].al));
    }
#line 6387 "verilog_parser.tab.cc"
    break;

    case 455: /* cell_port: attr TOK_WILDCARD_CONNECT  */
#line 2314 "verilog_parser.y"
    {
      if (!sv_mode)
        frontend_verilog_yyerror("Wildcard port connections are only supported "
                                 "in SystemVerilog mode.");
      astbuf2->attributes[ID::wildcard_port_conns] =
          AstNode::mkconst_int(1, false);
      free_attr((yyvsp[-1].al));
    }
#line 6398 "verilog_parser.tab.cc"
    break;

    case 456: /* always_comb_or_latch: TOK_ALWAYS_COMB  */
#line 2322 "verilog_parser.y"
    {
      (yyval.boolean) = false;
    }
#line 6406 "verilog_parser.tab.cc"
    break;

    case 457: /* always_comb_or_latch: TOK_ALWAYS_LATCH  */
#line 2325 "verilog_parser.y"
    {
      (yyval.boolean) = true;
    }
#line 6414 "verilog_parser.tab.cc"
    break;

    case 458: /* always_or_always_ff: TOK_ALWAYS  */
#line 2330 "verilog_parser.y"
    {
      (yyval.boolean) = false;
    }
#line 6422 "verilog_parser.tab.cc"
    break;

    case 459: /* always_or_always_ff: TOK_ALWAYS_FF  */
#line 2333 "verilog_parser.y"
    {
      (yyval.boolean) = true;
    }
#line 6430 "verilog_parser.tab.cc"
    break;

    case 460: /* $@54: %empty  */
#line 2338 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_ALWAYS);
      append_attr(node, (yyvsp[-1].al));
      if ((yyvsp[0].boolean))
        node->attributes[ID::always_ff] = AstNode::mkconst_int(1, false);
      ast_stack.back()->children.push_back(node);
      ast_stack.push_back(node);
    }
#line 6443 "verilog_parser.tab.cc"
    break;

    case 461: /* $@55: %empty  */
#line 2345 "verilog_parser.y"
    {
      AstNode *block = new AstNode(AST_BLOCK);
      ast_stack.back()->children.push_back(block);
      ast_stack.push_back(block);
    }
#line 6453 "verilog_parser.tab.cc"
    break;

    case 462: /* always_stmt: attr always_or_always_ff $@54 always_cond $@55
                 behavioral_stmt  */
#line 2349 "verilog_parser.y"
    {
      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[0]), (yylsp[0]));
      ast_stack.pop_back();

      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-4]), (yyloc));
      ast_stack.pop_back();

      SET_RULE_LOC((yyloc), (yylsp[-4]), (yyloc));
    }
#line 6467 "verilog_parser.tab.cc"
    break;

    case 463: /* $@56: %empty  */
#line 2358 "verilog_parser.y"
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
#line 6485 "verilog_parser.tab.cc"
    break;

    case 464: /* always_stmt: attr always_comb_or_latch $@56 behavioral_stmt  */
#line 2370 "verilog_parser.y"
    {
      ast_stack.pop_back();
      ast_stack.pop_back();
    }
#line 6494 "verilog_parser.tab.cc"
    break;

    case 465: /* $@57: %empty  */
#line 2374 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_INITIAL);
      append_attr(node, (yyvsp[-1].al));
      ast_stack.back()->children.push_back(node);
      ast_stack.push_back(node);
      AstNode *block = new AstNode(AST_BLOCK);
      ast_stack.back()->children.push_back(block);
      ast_stack.push_back(block);
    }
#line 6508 "verilog_parser.tab.cc"
    break;

    case 466: /* always_stmt: attr TOK_INITIAL $@57 behavioral_stmt  */
#line 2382 "verilog_parser.y"
    {
      ast_stack.pop_back();
      ast_stack.pop_back();
    }
#line 6517 "verilog_parser.tab.cc"
    break;

    case 476: /* always_event: TOK_POSEDGE expr  */
#line 2401 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_POSEDGE);
      SET_AST_NODE_LOC(node, (yylsp[-1]), (yylsp[-1]));
      ast_stack.back()->children.push_back(node);
      node->children.push_back((yyvsp[0].ast));
    }
#line 6528 "verilog_parser.tab.cc"
    break;

    case 477: /* always_event: TOK_NEGEDGE expr  */
#line 2407 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_NEGEDGE);
      SET_AST_NODE_LOC(node, (yylsp[-1]), (yylsp[-1]));
      ast_stack.back()->children.push_back(node);
      node->children.push_back((yyvsp[0].ast));
    }
#line 6539 "verilog_parser.tab.cc"
    break;

    case 478: /* always_event: expr  */
#line 2413 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_EDGE);
      ast_stack.back()->children.push_back(node);
      node->children.push_back((yyvsp[0].ast));
    }
#line 6549 "verilog_parser.tab.cc"
    break;

    case 479: /* opt_label: ':' TOK_ID  */
#line 2420 "verilog_parser.y"
    {
      (yyval.string) = (yyvsp[0].string);
    }
#line 6557 "verilog_parser.tab.cc"
    break;

    case 480: /* opt_label: %empty  */
#line 2423 "verilog_parser.y"
    {
      (yyval.string) = NULL;
    }
#line 6565 "verilog_parser.tab.cc"
    break;

    case 481: /* opt_sva_label: TOK_SVA_LABEL ':'  */
#line 2428 "verilog_parser.y"
    {
      (yyval.string) = (yyvsp[-1].string);
    }
#line 6573 "verilog_parser.tab.cc"
    break;

    case 482: /* opt_sva_label: %empty  */
#line 2431 "verilog_parser.y"
    {
      (yyval.string) = NULL;
    }
#line 6581 "verilog_parser.tab.cc"
    break;

    case 483: /* opt_property: TOK_PROPERTY  */
#line 2436 "verilog_parser.y"
    {
      (yyval.boolean) = true;
    }
#line 6589 "verilog_parser.tab.cc"
    break;

    case 484: /* opt_property: TOK_FINAL  */
#line 2439 "verilog_parser.y"
    {
      (yyval.boolean) = false;
    }
#line 6597 "verilog_parser.tab.cc"
    break;

    case 485: /* opt_property: %empty  */
#line 2442 "verilog_parser.y"
    {
      (yyval.boolean) = false;
    }
#line 6605 "verilog_parser.tab.cc"
    break;

    case 486: /* $@58: %empty  */
#line 2447 "verilog_parser.y"
    {
      AstNode *modport = new AstNode(AST_MODPORT);
      ast_stack.back()->children.push_back(modport);
      ast_stack.push_back(modport);
      modport->str = *(yyvsp[0].string);
      delete (yyvsp[0].string);
    }
#line 6617 "verilog_parser.tab.cc"
    break;

    case 487: /* $@59: %empty  */
#line 2453 "verilog_parser.y"
    {
      ast_stack.pop_back();
      log_assert(ast_stack.size() == 2);
    }
#line 6626 "verilog_parser.tab.cc"
    break;

    case 495: /* modport_member: TOK_ID  */
#line 2469 "verilog_parser.y"
    {
      AstNode *modport_member = new AstNode(AST_MODPORTMEMBER);
      ast_stack.back()->children.push_back(modport_member);
      modport_member->str = *(yyvsp[0].string);
      modport_member->is_input = current_modport_input;
      modport_member->is_output = current_modport_output;
      delete (yyvsp[0].string);
    }
#line 6639 "verilog_parser.tab.cc"
    break;

    case 496: /* modport_type_token: TOK_INPUT  */
#line 2479 "verilog_parser.y"
    {
      current_modport_input = 1;
      current_modport_output = 0;
    }
#line 6645 "verilog_parser.tab.cc"
    break;

    case 497: /* modport_type_token: TOK_OUTPUT  */
#line 2479 "verilog_parser.y"
    {
      current_modport_input = 0;
      current_modport_output = 1;
    }
#line 6651 "verilog_parser.tab.cc"
    break;

    case 498: /* assert: opt_sva_label TOK_ASSERT opt_property '(' expr ')' ';'
               */
#line 2482 "verilog_parser.y"
    {
      if (noassert_mode) {
        delete (yyvsp[-2].ast);
      } else {
        AstNode *node = new AstNode(
            assume_asserts_mode ? AST_ASSUME : AST_ASSERT, (yyvsp[-2].ast));
        SET_AST_NODE_LOC(node, (yylsp[-6]), (yylsp[-1]));
        if ((yyvsp[-6].string) != nullptr)
          node->str = *(yyvsp[-6].string);
        ast_stack.back()->children.push_back(node);
      }
      if ((yyvsp[-6].string) != nullptr)
        delete (yyvsp[-6].string);
    }
#line 6669 "verilog_parser.tab.cc"
    break;

    case 499: /* assert: opt_sva_label TOK_ASSUME opt_property '(' expr ')' ';'
               */
#line 2495 "verilog_parser.y"
    {
      if (noassume_mode) {
        delete (yyvsp[-2].ast);
      } else {
        AstNode *node = new AstNode(
            assert_assumes_mode ? AST_ASSERT : AST_ASSUME, (yyvsp[-2].ast));
        SET_AST_NODE_LOC(node, (yylsp[-6]), (yylsp[-1]));
        if ((yyvsp[-6].string) != nullptr)
          node->str = *(yyvsp[-6].string);
        ast_stack.back()->children.push_back(node);
      }
      if ((yyvsp[-6].string) != nullptr)
        delete (yyvsp[-6].string);
    }
#line 6687 "verilog_parser.tab.cc"
    break;

    case 500: /* assert: opt_sva_label TOK_ASSERT opt_property '('
                 TOK_EVENTUALLY expr ')' ';'  */
#line 2508 "verilog_parser.y"
    {
      if (noassert_mode) {
        delete (yyvsp[-2].ast);
      } else {
        AstNode *node = new AstNode(assume_asserts_mode ? AST_FAIR : AST_LIVE,
                                    (yyvsp[-2].ast));
        SET_AST_NODE_LOC(node, (yylsp[-7]), (yylsp[-1]));
        if ((yyvsp[-7].string) != nullptr)
          node->str = *(yyvsp[-7].string);
        ast_stack.back()->children.push_back(node);
      }
      if ((yyvsp[-7].string) != nullptr)
        delete (yyvsp[-7].string);
    }
#line 6705 "verilog_parser.tab.cc"
    break;

    case 501: /* assert: opt_sva_label TOK_ASSUME opt_property '('
                 TOK_EVENTUALLY expr ')' ';'  */
#line 2521 "verilog_parser.y"
    {
      if (noassume_mode) {
        delete (yyvsp[-2].ast);
      } else {
        AstNode *node = new AstNode(assert_assumes_mode ? AST_LIVE : AST_FAIR,
                                    (yyvsp[-2].ast));
        SET_AST_NODE_LOC(node, (yylsp[-7]), (yylsp[-1]));
        if ((yyvsp[-7].string) != nullptr)
          node->str = *(yyvsp[-7].string);
        ast_stack.back()->children.push_back(node);
      }
      if ((yyvsp[-7].string) != nullptr)
        delete (yyvsp[-7].string);
    }
#line 6723 "verilog_parser.tab.cc"
    break;

    case 502: /* assert: opt_sva_label TOK_COVER opt_property '(' expr ')' ';'
               */
#line 2534 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_COVER, (yyvsp[-2].ast));
      SET_AST_NODE_LOC(node, (yylsp[-6]), (yylsp[-1]));
      if ((yyvsp[-6].string) != nullptr) {
        node->str = *(yyvsp[-6].string);
        delete (yyvsp[-6].string);
      }
      ast_stack.back()->children.push_back(node);
    }
#line 6737 "verilog_parser.tab.cc"
    break;

    case 503: /* assert: opt_sva_label TOK_COVER opt_property '(' ')' ';'  */
#line 2543 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_COVER, AstNode::mkconst_int(1, false));
      SET_AST_NODE_LOC(node, (yylsp[-5]), (yylsp[-1]));
      if ((yyvsp[-5].string) != nullptr) {
        node->str = *(yyvsp[-5].string);
        delete (yyvsp[-5].string);
      }
      ast_stack.back()->children.push_back(node);
    }
#line 6751 "verilog_parser.tab.cc"
    break;

    case 504: /* assert: opt_sva_label TOK_COVER ';'  */
#line 2552 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_COVER, AstNode::mkconst_int(1, false));
      SET_AST_NODE_LOC(node, (yylsp[-2]), (yylsp[-1]));
      if ((yyvsp[-2].string) != nullptr) {
        node->str = *(yyvsp[-2].string);
        delete (yyvsp[-2].string);
      }
      ast_stack.back()->children.push_back(node);
    }
#line 6765 "verilog_parser.tab.cc"
    break;

    case 505: /* assert: opt_sva_label TOK_RESTRICT opt_property '(' expr ')'
                 ';'  */
#line 2561 "verilog_parser.y"
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
        log_file_warning(current_filename, get_line_num(),
                         "SystemVerilog does not allow \"restrict\" without "
                         "\"property\".\n");
      if ((yyvsp[-6].string) != nullptr)
        delete (yyvsp[-6].string);
    }
#line 6785 "verilog_parser.tab.cc"
    break;

    case 506: /* assert: opt_sva_label TOK_RESTRICT opt_property '('
                 TOK_EVENTUALLY expr ')' ';'  */
#line 2576 "verilog_parser.y"
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
        log_file_warning(current_filename, get_line_num(),
                         "SystemVerilog does not allow \"restrict\" without "
                         "\"property\".\n");
      if ((yyvsp[-7].string) != nullptr)
        delete (yyvsp[-7].string);
    }
#line 6805 "verilog_parser.tab.cc"
    break;

    case 507: /* assert_property: opt_sva_label TOK_ASSERT TOK_PROPERTY '(' expr
                 ')' ';'  */
#line 2593 "verilog_parser.y"
    {
      AstNode *node = new AstNode(assume_asserts_mode ? AST_ASSUME : AST_ASSERT,
                                  (yyvsp[-2].ast));
      SET_AST_NODE_LOC(node, (yylsp[-6]), (yylsp[-1]));
      ast_stack.back()->children.push_back(node);
      if ((yyvsp[-6].string) != nullptr) {
        ast_stack.back()->children.back()->str = *(yyvsp[-6].string);
        delete (yyvsp[-6].string);
      }
    }
#line 6819 "verilog_parser.tab.cc"
    break;

    case 508: /* assert_property: opt_sva_label TOK_ASSUME TOK_PROPERTY '(' expr
                 ')' ';'  */
#line 2602 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_ASSUME, (yyvsp[-2].ast));
      SET_AST_NODE_LOC(node, (yylsp[-6]), (yylsp[-1]));
      ast_stack.back()->children.push_back(node);
      if ((yyvsp[-6].string) != nullptr) {
        ast_stack.back()->children.back()->str = *(yyvsp[-6].string);
        delete (yyvsp[-6].string);
      }
    }
#line 6833 "verilog_parser.tab.cc"
    break;

    case 509: /* assert_property: opt_sva_label TOK_ASSERT TOK_PROPERTY '('
                 TOK_EVENTUALLY expr ')' ';'  */
#line 2611 "verilog_parser.y"
    {
      AstNode *node = new AstNode(assume_asserts_mode ? AST_FAIR : AST_LIVE,
                                  (yyvsp[-2].ast));
      SET_AST_NODE_LOC(node, (yylsp[-7]), (yylsp[-1]));
      ast_stack.back()->children.push_back(node);
      if ((yyvsp[-7].string) != nullptr) {
        ast_stack.back()->children.back()->str = *(yyvsp[-7].string);
        delete (yyvsp[-7].string);
      }
    }
#line 6847 "verilog_parser.tab.cc"
    break;

    case 510: /* assert_property: opt_sva_label TOK_ASSUME TOK_PROPERTY '('
                 TOK_EVENTUALLY expr ')' ';'  */
#line 2620 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_FAIR, (yyvsp[-2].ast));
      SET_AST_NODE_LOC(node, (yylsp[-7]), (yylsp[-1]));
      ast_stack.back()->children.push_back(node);
      if ((yyvsp[-7].string) != nullptr) {
        ast_stack.back()->children.back()->str = *(yyvsp[-7].string);
        delete (yyvsp[-7].string);
      }
    }
#line 6861 "verilog_parser.tab.cc"
    break;

    case 511: /* assert_property: opt_sva_label TOK_COVER TOK_PROPERTY '(' expr
                 ')' ';'  */
#line 2629 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_COVER, (yyvsp[-2].ast));
      SET_AST_NODE_LOC(node, (yylsp[-6]), (yylsp[-1]));
      ast_stack.back()->children.push_back(node);
      if ((yyvsp[-6].string) != nullptr) {
        ast_stack.back()->children.back()->str = *(yyvsp[-6].string);
        delete (yyvsp[-6].string);
      }
    }
#line 6875 "verilog_parser.tab.cc"
    break;

    case 512: /* assert_property: opt_sva_label TOK_RESTRICT TOK_PROPERTY '('
                 expr ')' ';'  */
#line 2638 "verilog_parser.y"
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
#line 6893 "verilog_parser.tab.cc"
    break;

    case 513: /* assert_property: opt_sva_label TOK_RESTRICT TOK_PROPERTY '('
                 TOK_EVENTUALLY expr ')' ';'  */
#line 2651 "verilog_parser.y"
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
#line 6911 "verilog_parser.tab.cc"
    break;

    case 514: /* simple_behavioral_stmt: attr lvalue '=' delay expr  */
#line 2666 "verilog_parser.y"
    {
      AstNode *node =
          new AstNode(AST_ASSIGN_EQ, (yyvsp[-3].ast), (yyvsp[0].ast));
      ast_stack.back()->children.push_back(node);
      SET_AST_NODE_LOC(node, (yylsp[-3]), (yylsp[0]));
      append_attr(node, (yyvsp[-4].al));
    }
#line 6922 "verilog_parser.tab.cc"
    break;

    case 515: /* simple_behavioral_stmt: attr lvalue attr inc_or_dec_op  */
#line 2672 "verilog_parser.y"
    {
      addIncOrDecStmt((yyvsp[-3].al), (yyvsp[-2].ast), (yyvsp[-1].al),
                      (yyvsp[0].ast_node_type), (yylsp[-3]), (yylsp[0]));
    }
#line 6930 "verilog_parser.tab.cc"
    break;

    case 516: /* simple_behavioral_stmt: attr inc_or_dec_op attr lvalue  */
#line 2675 "verilog_parser.y"
    {
      addIncOrDecStmt((yyvsp[-3].al), (yyvsp[0].ast), (yyvsp[-1].al),
                      (yyvsp[-2].ast_node_type), (yylsp[-3]), (yylsp[0]));
    }
#line 6938 "verilog_parser.tab.cc"
    break;

    case 517: /* simple_behavioral_stmt: attr lvalue OP_LE delay expr  */
#line 2678 "verilog_parser.y"
    {
      AstNode *node =
          new AstNode(AST_ASSIGN_LE, (yyvsp[-3].ast), (yyvsp[0].ast));
      ast_stack.back()->children.push_back(node);
      SET_AST_NODE_LOC(node, (yylsp[-3]), (yylsp[0]));
      append_attr(node, (yyvsp[-4].al));
    }
#line 6949 "verilog_parser.tab.cc"
    break;

    case 518: /* simple_behavioral_stmt: attr lvalue asgn_binop delay expr  */
#line 2684 "verilog_parser.y"
    {
      addAsgnBinopStmt((yyvsp[-4].al), (yyvsp[-3].ast),
                       (yyvsp[-2].ast_node_type), (yyvsp[0].ast), (yylsp[-3]),
                       (yylsp[0]));
    }
#line 6957 "verilog_parser.tab.cc"
    break;

    case 519: /* asgn_binop: TOK_BIT_OR_ASSIGN  */
#line 2689 "verilog_parser.y"
    {
      (yyval.ast_node_type) = AST_BIT_OR;
    }
#line 6963 "verilog_parser.tab.cc"
    break;

    case 520: /* asgn_binop: TOK_BIT_AND_ASSIGN  */
#line 2690 "verilog_parser.y"
    {
      (yyval.ast_node_type) = AST_BIT_AND;
    }
#line 6969 "verilog_parser.tab.cc"
    break;

    case 521: /* asgn_binop: TOK_BIT_XOR_ASSIGN  */
#line 2691 "verilog_parser.y"
    {
      (yyval.ast_node_type) = AST_BIT_XOR;
    }
#line 6975 "verilog_parser.tab.cc"
    break;

    case 522: /* asgn_binop: TOK_ADD_ASSIGN  */
#line 2692 "verilog_parser.y"
    {
      (yyval.ast_node_type) = AST_ADD;
    }
#line 6981 "verilog_parser.tab.cc"
    break;

    case 523: /* asgn_binop: TOK_SUB_ASSIGN  */
#line 2693 "verilog_parser.y"
    {
      (yyval.ast_node_type) = AST_SUB;
    }
#line 6987 "verilog_parser.tab.cc"
    break;

    case 524: /* asgn_binop: TOK_DIV_ASSIGN  */
#line 2694 "verilog_parser.y"
    {
      (yyval.ast_node_type) = AST_DIV;
    }
#line 6993 "verilog_parser.tab.cc"
    break;

    case 525: /* asgn_binop: TOK_MOD_ASSIGN  */
#line 2695 "verilog_parser.y"
    {
      (yyval.ast_node_type) = AST_MOD;
    }
#line 6999 "verilog_parser.tab.cc"
    break;

    case 526: /* asgn_binop: TOK_MUL_ASSIGN  */
#line 2696 "verilog_parser.y"
    {
      (yyval.ast_node_type) = AST_MUL;
    }
#line 7005 "verilog_parser.tab.cc"
    break;

    case 527: /* asgn_binop: TOK_SHL_ASSIGN  */
#line 2697 "verilog_parser.y"
    {
      (yyval.ast_node_type) = AST_SHIFT_LEFT;
    }
#line 7011 "verilog_parser.tab.cc"
    break;

    case 528: /* asgn_binop: TOK_SHR_ASSIGN  */
#line 2698 "verilog_parser.y"
    {
      (yyval.ast_node_type) = AST_SHIFT_RIGHT;
    }
#line 7017 "verilog_parser.tab.cc"
    break;

    case 529: /* asgn_binop: TOK_SSHL_ASSIGN  */
#line 2699 "verilog_parser.y"
    {
      (yyval.ast_node_type) = AST_SHIFT_SLEFT;
    }
#line 7023 "verilog_parser.tab.cc"
    break;

    case 530: /* asgn_binop: TOK_SSHR_ASSIGN  */
#line 2700 "verilog_parser.y"
    {
      (yyval.ast_node_type) = AST_SHIFT_SRIGHT;
    }
#line 7029 "verilog_parser.tab.cc"
    break;

    case 531: /* inc_or_dec_op: TOK_INCREMENT  */
#line 2705 "verilog_parser.y"
    {
      (yyval.ast_node_type) = AST_ADD;
    }
#line 7035 "verilog_parser.tab.cc"
    break;

    case 532: /* inc_or_dec_op: TOK_DECREMENT  */
#line 2706 "verilog_parser.y"
    {
      (yyval.ast_node_type) = AST_SUB;
    }
#line 7041 "verilog_parser.tab.cc"
    break;

    case 533: /* for_initialization: TOK_ID '=' expr  */
#line 2709 "verilog_parser.y"
    {
      AstNode *ident = new AstNode(AST_IDENTIFIER);
      ident->str = *(yyvsp[-2].string);
      AstNode *node = new AstNode(AST_ASSIGN_EQ, ident, (yyvsp[0].ast));
      ast_stack.back()->children.push_back(node);
      SET_AST_NODE_LOC(node, (yylsp[-2]), (yylsp[0]));
      delete (yyvsp[-2].string);
    }
#line 7054 "verilog_parser.tab.cc"
    break;

    case 534: /* for_initialization: non_io_wire_type range TOK_ID  */
#line 2717 "verilog_parser.y"
    {
      frontend_verilog_yyerror(
          "For loop variable declaration is missing initialization!");
    }
#line 7062 "verilog_parser.tab.cc"
    break;

    case 535: /* for_initialization: non_io_wire_type range TOK_ID '=' expr  */
#line 2720 "verilog_parser.y"
    {
      if (!sv_mode)
        frontend_verilog_yyerror("For loop inline variable declaration is only "
                                 "supported in SystemVerilog mode!");

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
#line 7102 "verilog_parser.tab.cc"
    break;

    case 544: /* behavioral_stmt: attr ';'  */
#line 2761 "verilog_parser.y"
    {
      free_attr((yyvsp[-1].al));
    }
#line 7110 "verilog_parser.tab.cc"
    break;

    case 545: /* $@60: %empty  */
#line 2764 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_TCALL);
      node->str = *(yyvsp[0].string);
      delete (yyvsp[0].string);
      ast_stack.back()->children.push_back(node);
      ast_stack.push_back(node);
      append_attr(node, (yyvsp[-1].al));
    }
#line 7123 "verilog_parser.tab.cc"
    break;

    case 546: /* behavioral_stmt: attr hierarchical_id $@60 opt_arg_list ';'  */
#line 2771 "verilog_parser.y"
    {
      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-3]), (yylsp[0]));
      ast_stack.pop_back();
    }
#line 7132 "verilog_parser.tab.cc"
    break;

    case 547: /* $@61: %empty  */
#line 2775 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_TCALL);
      node->str = *(yyvsp[0].string);
      delete (yyvsp[0].string);
      ast_stack.back()->children.push_back(node);
      ast_stack.push_back(node);
      append_attr(node, (yyvsp[-1].al));
    }
#line 7145 "verilog_parser.tab.cc"
    break;

    case 548: /* behavioral_stmt: attr TOK_MSG_TASKS $@61 opt_arg_list ';'  */
#line 2782 "verilog_parser.y"
    {
      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-3]), (yylsp[0]));
      ast_stack.pop_back();
    }
#line 7154 "verilog_parser.tab.cc"
    break;

    case 549: /* $@62: %empty  */
#line 2786 "verilog_parser.y"
    {
      enterTypeScope();
    }
#line 7162 "verilog_parser.tab.cc"
    break;

    case 550: /* $@63: %empty  */
#line 2788 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_BLOCK);
      ast_stack.back()->children.push_back(node);
      ast_stack.push_back(node);
      append_attr(node, (yyvsp[-3].al));
      if ((yyvsp[0].string) != NULL)
        node->str = *(yyvsp[0].string);
    }
#line 7175 "verilog_parser.tab.cc"
    break;

    case 551: /* behavioral_stmt: attr TOK_BEGIN $@62 opt_label $@63
                 behavioral_stmt_list TOK_END opt_label  */
#line 2795 "verilog_parser.y"
    {
      exitTypeScope();
      checkLabelsMatch("Begin label", (yyvsp[-4].string), (yyvsp[0].string));
      AstNode *node = ast_stack.back();
      // In SystemVerilog, unnamed blocks with block item declarations
      // create an implicit hierarchy scope
      if (sv_mode && node->str.empty())
        for (const AstNode *child : node->children)
          if (child->type == AST_WIRE || child->type == AST_MEMORY ||
              child->type == AST_PARAMETER || child->type == AST_LOCALPARAM ||
              child->type == AST_TYPEDEF) {
            node->str = "$unnamed_block$" + std::to_string(autoidx++);
            break;
          }
      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-6]), (yylsp[0]));
      delete (yyvsp[-4].string);
      delete (yyvsp[0].string);
      ast_stack.pop_back();
    }
#line 7198 "verilog_parser.tab.cc"
    break;

    case 552: /* $@64: %empty  */
#line 2813 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_FOR);
      ast_stack.back()->children.push_back(node);
      ast_stack.push_back(node);
      append_attr(node, (yyvsp[-2].al));
    }
#line 7209 "verilog_parser.tab.cc"
    break;

    case 553: /* $@65: %empty  */
#line 2818 "verilog_parser.y"
    {
      ast_stack.back()->children.push_back((yyvsp[0].ast));
    }
#line 7217 "verilog_parser.tab.cc"
    break;

    case 554: /* $@66: %empty  */
#line 2820 "verilog_parser.y"
    {
      AstNode *block = new AstNode(AST_BLOCK);
      block->str = "$for_loop$" + std::to_string(autoidx++);
      ast_stack.back()->children.push_back(block);
      ast_stack.push_back(block);
    }
#line 7228 "verilog_parser.tab.cc"
    break;

    case 555: /* behavioral_stmt: attr TOK_FOR '(' $@64 for_initialization ';'
                 expr $@65 ';' simple_behavioral_stmt ')' $@66 behavioral_stmt
               */
#line 2825 "verilog_parser.y"
    {
      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[0]), (yylsp[0]));
      ast_stack.pop_back();
      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-11]), (yylsp[0]));
      ast_stack.pop_back();
    }
#line 7239 "verilog_parser.tab.cc"
    break;

    case 556: /* $@67: %empty  */
#line 2831 "verilog_parser.y"
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
#line 7254 "verilog_parser.tab.cc"
    break;

    case 557: /* behavioral_stmt: attr TOK_WHILE '(' expr ')' $@67
                 behavioral_stmt  */
#line 2840 "verilog_parser.y"
    {
      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[0]), (yylsp[0]));
      ast_stack.pop_back();
      ast_stack.pop_back();
    }
#line 7264 "verilog_parser.tab.cc"
    break;

    case 558: /* $@68: %empty  */
#line 2845 "verilog_parser.y"
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
#line 7279 "verilog_parser.tab.cc"
    break;

    case 559: /* behavioral_stmt: attr TOK_REPEAT '(' expr ')' $@68
                 behavioral_stmt  */
#line 2854 "verilog_parser.y"
    {
      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[0]), (yylsp[0]));
      ast_stack.pop_back();
      ast_stack.pop_back();
    }
#line 7289 "verilog_parser.tab.cc"
    break;

    case 560: /* $@69: %empty  */
#line 2859 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_CASE);
      AstNode *block = new AstNode(AST_BLOCK);
      AstNode *cond =
          new AstNode(AST_COND, AstNode::mkconst_int(1, false, 1), block);
      SET_AST_NODE_LOC(cond, (yylsp[-1]), (yylsp[-1]));
      ast_stack.back()->children.push_back(node);
      node->children.push_back(new AstNode(AST_REDUCE_BOOL, (yyvsp[-1].ast)));
      node->children.push_back(cond);
      ast_stack.push_back(node);
      ast_stack.push_back(block);
      append_attr(node, (yyvsp[-4].al));
    }
#line 7306 "verilog_parser.tab.cc"
    break;

    case 561: /* $@70: %empty  */
#line 2870 "verilog_parser.y"
    {
      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[0]), (yylsp[0]));
    }
#line 7314 "verilog_parser.tab.cc"
    break;

    case 562: /* behavioral_stmt: attr TOK_IF '(' expr ')' $@69 behavioral_stmt
                 $@70 optional_else  */
#line 2872 "verilog_parser.y"
    {
      ast_stack.pop_back();
      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-7]), (yylsp[0]));
      ast_stack.pop_back();
    }
#line 7324 "verilog_parser.tab.cc"
    break;

    case 563: /* $@71: %empty  */
#line 2877 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_CASE, (yyvsp[-1].ast));
      ast_stack.back()->children.push_back(node);
      ast_stack.push_back(node);
      append_attr(node, (yyvsp[-4].al));
      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-1]), (yylsp[-1]));
    }
#line 7336 "verilog_parser.tab.cc"
    break;

    case 564: /* behavioral_stmt: case_attr case_type '(' expr ')' $@71
                 opt_synopsys_attr case_body TOK_ENDCASE  */
#line 2883 "verilog_parser.y"
    {
      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-7]), (yylsp[0]));
      case_type_stack.pop_back();
      ast_stack.pop_back();
    }
#line 7346 "verilog_parser.tab.cc"
    break;

    case 565: /* case_attr: attr  */
#line 2890 "verilog_parser.y"
    {
      (yyval.al) = (yyvsp[0].al);
    }
#line 7354 "verilog_parser.tab.cc"
    break;

    case 566: /* case_attr: attr TOK_UNIQUE0  */
#line 2893 "verilog_parser.y"
    {
      (*(yyvsp[-1].al))[ID::parallel_case] = AstNode::mkconst_int(1, false);
      (yyval.al) = (yyvsp[-1].al);
    }
#line 7363 "verilog_parser.tab.cc"
    break;

    case 567: /* case_attr: attr TOK_PRIORITY  */
#line 2897 "verilog_parser.y"
    {
      (*(yyvsp[-1].al))[ID::full_case] = AstNode::mkconst_int(1, false);
      (yyval.al) = (yyvsp[-1].al);
    }
#line 7372 "verilog_parser.tab.cc"
    break;

    case 568: /* case_attr: attr TOK_UNIQUE  */
#line 2901 "verilog_parser.y"
    {
      (*(yyvsp[-1].al))[ID::full_case] = AstNode::mkconst_int(1, false);
      (*(yyvsp[-1].al))[ID::parallel_case] = AstNode::mkconst_int(1, false);
      (yyval.al) = (yyvsp[-1].al);
    }
#line 7382 "verilog_parser.tab.cc"
    break;

    case 569: /* case_type: TOK_CASE  */
#line 2908 "verilog_parser.y"
    {
      case_type_stack.push_back(0);
    }
#line 7390 "verilog_parser.tab.cc"
    break;

    case 570: /* case_type: TOK_CASEX  */
#line 2911 "verilog_parser.y"
    {
      case_type_stack.push_back('x');
    }
#line 7398 "verilog_parser.tab.cc"
    break;

    case 571: /* case_type: TOK_CASEZ  */
#line 2914 "verilog_parser.y"
    {
      case_type_stack.push_back('z');
    }
#line 7406 "verilog_parser.tab.cc"
    break;

    case 572: /* opt_synopsys_attr: opt_synopsys_attr TOK_SYNOPSYS_FULL_CASE  */
#line 2919 "verilog_parser.y"
    {
      if (ast_stack.back()->attributes.count(ID::full_case) == 0)
        ast_stack.back()->attributes[ID::full_case] =
            AstNode::mkconst_int(1, false);
    }
#line 7415 "verilog_parser.tab.cc"
    break;

    case 573: /* opt_synopsys_attr: opt_synopsys_attr TOK_SYNOPSYS_PARALLEL_CASE
               */
#line 2923 "verilog_parser.y"
    {
      if (ast_stack.back()->attributes.count(ID::parallel_case) == 0)
        ast_stack.back()->attributes[ID::parallel_case] =
            AstNode::mkconst_int(1, false);
    }
#line 7424 "verilog_parser.tab.cc"
    break;

    case 577: /* $@72: %empty  */
#line 2934 "verilog_parser.y"
    {
      AstNode *block = new AstNode(AST_BLOCK);
      AstNode *cond = new AstNode(AST_COND, new AstNode(AST_DEFAULT), block);
      SET_AST_NODE_LOC(cond, (yylsp[0]), (yylsp[0]));

      ast_stack.pop_back();
      ast_stack.back()->children.push_back(cond);
      ast_stack.push_back(block);
    }
#line 7438 "verilog_parser.tab.cc"
    break;

    case 578: /* optional_else: TOK_ELSE $@72 behavioral_stmt  */
#line 2942 "verilog_parser.y"
    {
      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[0]), (yylsp[0]));
    }
#line 7446 "verilog_parser.tab.cc"
    break;

    case 582: /* $@73: %empty  */
#line 2952 "verilog_parser.y"
    {
      AstNode *node = new AstNode(
          case_type_stack.size() && case_type_stack.back() == 'x'   ? AST_CONDX
          : case_type_stack.size() && case_type_stack.back() == 'z' ? AST_CONDZ
                                                                    : AST_COND);
      ast_stack.back()->children.push_back(node);
      ast_stack.push_back(node);
    }
#line 7458 "verilog_parser.tab.cc"
    break;

    case 583: /* $@74: %empty  */
#line 2958 "verilog_parser.y"
    {
      AstNode *block = new AstNode(AST_BLOCK);
      ast_stack.back()->children.push_back(block);
      ast_stack.push_back(block);
      case_type_stack.push_back(0);
    }
#line 7469 "verilog_parser.tab.cc"
    break;

    case 584: /* case_item: $@73 case_select $@74 behavioral_stmt  */
#line 2963 "verilog_parser.y"
    {
      case_type_stack.pop_back();
      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[0]), (yylsp[0]));
      ast_stack.pop_back();
      ast_stack.pop_back();
    }
#line 7480 "verilog_parser.tab.cc"
    break;

    case 587: /* $@75: %empty  */
#line 2975 "verilog_parser.y"
    {
      AstNode *node = new AstNode(
          case_type_stack.size() && case_type_stack.back() == 'x'   ? AST_CONDX
          : case_type_stack.size() && case_type_stack.back() == 'z' ? AST_CONDZ
                                                                    : AST_COND);
      ast_stack.back()->children.push_back(node);
      ast_stack.push_back(node);
    }
#line 7492 "verilog_parser.tab.cc"
    break;

    case 588: /* $@76: %empty  */
#line 2981 "verilog_parser.y"
    {
      case_type_stack.push_back(0);
      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[0]), (yylsp[0]));
    }
#line 7501 "verilog_parser.tab.cc"
    break;

    case 589: /* gen_case_item: $@75 case_select $@76 gen_stmt_block  */
#line 2984 "verilog_parser.y"
    {
      case_type_stack.pop_back();
      ast_stack.pop_back();
    }
#line 7510 "verilog_parser.tab.cc"
    break;

    case 592: /* case_expr_list: TOK_DEFAULT  */
#line 2994 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_DEFAULT);
      SET_AST_NODE_LOC(node, (yylsp[0]), (yylsp[0]));
      ast_stack.back()->children.push_back(node);
    }
#line 7520 "verilog_parser.tab.cc"
    break;

    case 593: /* case_expr_list: TOK_SVA_LABEL  */
#line 2999 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_IDENTIFIER);
      SET_AST_NODE_LOC(node, (yylsp[0]), (yylsp[0]));
      ast_stack.back()->children.push_back(node);
      ast_stack.back()->children.back()->str = *(yyvsp[0].string);
      delete (yyvsp[0].string);
    }
#line 7532 "verilog_parser.tab.cc"
    break;

    case 594: /* case_expr_list: expr  */
#line 3006 "verilog_parser.y"
    {
      ast_stack.back()->children.push_back((yyvsp[0].ast));
    }
#line 7540 "verilog_parser.tab.cc"
    break;

    case 595: /* case_expr_list: case_expr_list ',' expr  */
#line 3009 "verilog_parser.y"
    {
      ast_stack.back()->children.push_back((yyvsp[0].ast));
    }
#line 7548 "verilog_parser.tab.cc"
    break;

    case 596: /* rvalue: hierarchical_id '[' expr ']' '.' rvalue  */
#line 3014 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_PREFIX, (yyvsp[-3].ast), (yyvsp[0].ast));
      (yyval.ast)->str = *(yyvsp[-5].string);
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-5]), (yylsp[0]));
      delete (yyvsp[-5].string);
    }
#line 7559 "verilog_parser.tab.cc"
    break;

    case 597: /* rvalue: hierarchical_id range  */
#line 3020 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_IDENTIFIER, (yyvsp[0].ast));
      (yyval.ast)->str = *(yyvsp[-1].string);
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-1]), (yylsp[-1]));
      delete (yyvsp[-1].string);
      if ((yyvsp[0].ast) == nullptr && ((yyval.ast)->str == "\\$initstate" ||
                                        (yyval.ast)->str == "\\$anyconst" ||
                                        (yyval.ast)->str == "\\$anyseq" ||
                                        (yyval.ast)->str == "\\$allconst" ||
                                        (yyval.ast)->str == "\\$allseq"))
        (yyval.ast)->type = AST_FCALL;
    }
#line 7574 "verilog_parser.tab.cc"
    break;

    case 598: /* rvalue: hierarchical_id non_opt_multirange  */
#line 3030 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_IDENTIFIER, (yyvsp[0].ast));
      (yyval.ast)->str = *(yyvsp[-1].string);
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-1]), (yylsp[-1]));
      delete (yyvsp[-1].string);
    }
#line 7585 "verilog_parser.tab.cc"
    break;

    case 599: /* lvalue: rvalue  */
#line 3038 "verilog_parser.y"
    {
      (yyval.ast) = (yyvsp[0].ast);
    }
#line 7593 "verilog_parser.tab.cc"
    break;

    case 600: /* lvalue: '{' lvalue_concat_list '}'  */
#line 3041 "verilog_parser.y"
    {
      (yyval.ast) = (yyvsp[-1].ast);
    }
#line 7601 "verilog_parser.tab.cc"
    break;

    case 601: /* lvalue_concat_list: expr  */
#line 3046 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_CONCAT);
      (yyval.ast)->children.push_back((yyvsp[0].ast));
    }
#line 7610 "verilog_parser.tab.cc"
    break;

    case 602: /* lvalue_concat_list: expr ',' lvalue_concat_list  */
#line 3050 "verilog_parser.y"
    {
      (yyval.ast) = (yyvsp[0].ast);
      (yyval.ast)->children.push_back((yyvsp[-2].ast));
    }
#line 7619 "verilog_parser.tab.cc"
    break;

    case 609: /* single_arg: expr  */
#line 3068 "verilog_parser.y"
    {
      ast_stack.back()->children.push_back((yyvsp[0].ast));
    }
#line 7627 "verilog_parser.tab.cc"
    break;

    case 615: /* gen_stmt_or_module_body_stmt: attr ';'  */
#line 3079 "verilog_parser.y"
    {
      free_attr((yyvsp[-1].al));
    }
#line 7635 "verilog_parser.tab.cc"
    break;

    case 616: /* genvar_identifier: TOK_ID  */
#line 3084 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_IDENTIFIER);
      (yyval.ast)->str = *(yyvsp[0].string);
      delete (yyvsp[0].string);
    }
#line 7645 "verilog_parser.tab.cc"
    break;

    case 617: /* genvar_initialization: TOK_GENVAR genvar_identifier  */
#line 3091 "verilog_parser.y"
    {
      frontend_verilog_yyerror(
          "Generate for loop variable declaration is missing initialization!");
    }
#line 7653 "verilog_parser.tab.cc"
    break;

    case 618: /* genvar_initialization: TOK_GENVAR genvar_identifier '=' expr */
#line 3094 "verilog_parser.y"
    {
      if (!sv_mode)
        frontend_verilog_yyerror(
            "Generate for loop inline variable declaration is only supported "
            "in SystemVerilog mode!");
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
#line 7674 "verilog_parser.tab.cc"
    break;

    case 619: /* genvar_initialization: genvar_identifier '=' expr  */
#line 3110 "verilog_parser.y"
    {
      AstNode *node =
          new AstNode(AST_ASSIGN_EQ, (yyvsp[-2].ast), (yyvsp[0].ast));
      ast_stack.back()->children.push_back(node);
      SET_AST_NODE_LOC(node, (yylsp[-2]), (yylsp[0]));
    }
#line 7684 "verilog_parser.tab.cc"
    break;

    case 620: /* $@77: %empty  */
#line 3118 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_GENFOR);
      ast_stack.back()->children.push_back(node);
      ast_stack.push_back(node);
    }
#line 7694 "verilog_parser.tab.cc"
    break;

    case 621: /* $@78: %empty  */
#line 3122 "verilog_parser.y"
    {
      ast_stack.back()->children.push_back((yyvsp[0].ast));
    }
#line 7702 "verilog_parser.tab.cc"
    break;

    case 622: /* gen_stmt: TOK_FOR '(' $@77 genvar_initialization ';' expr $@78
                 ';' simple_behavioral_stmt ')' gen_stmt_block  */
#line 3124 "verilog_parser.y"
    {
      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-10]), (yylsp[0]));
      rewriteGenForDeclInit(ast_stack.back());
      ast_stack.pop_back();
    }
#line 7712 "verilog_parser.tab.cc"
    break;

    case 623: /* $@79: %empty  */
#line 3129 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_GENIF);
      ast_stack.back()->children.push_back(node);
      ast_stack.push_back(node);
      ast_stack.back()->children.push_back((yyvsp[-1].ast));
    }
#line 7723 "verilog_parser.tab.cc"
    break;

    case 624: /* gen_stmt: TOK_IF '(' expr ')' $@79 gen_stmt_block opt_gen_else
               */
#line 3134 "verilog_parser.y"
    {
      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-6]), (yylsp[0]));
      ast_stack.pop_back();
    }
#line 7732 "verilog_parser.tab.cc"
    break;

    case 625: /* $@80: %empty  */
#line 3138 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_GENCASE, (yyvsp[-1].ast));
      ast_stack.back()->children.push_back(node);
      ast_stack.push_back(node);
    }
#line 7742 "verilog_parser.tab.cc"
    break;

    case 626: /* gen_stmt: case_type '(' expr ')' $@80 gen_case_body TOK_ENDCASE
               */
#line 3142 "verilog_parser.y"
    {
      case_type_stack.pop_back();
      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-6]), (yylsp[0]));
      ast_stack.pop_back();
    }
#line 7752 "verilog_parser.tab.cc"
    break;

    case 627: /* $@81: %empty  */
#line 3147 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_TECALL);
      node->str = *(yyvsp[0].string);
      delete (yyvsp[0].string);
      ast_stack.back()->children.push_back(node);
      ast_stack.push_back(node);
    }
#line 7764 "verilog_parser.tab.cc"
    break;

    case 628: /* gen_stmt: TOK_MSG_TASKS $@81 opt_arg_list ';'  */
#line 3153 "verilog_parser.y"
    {
      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-3]), (yylsp[-1]));
      ast_stack.pop_back();
    }
#line 7773 "verilog_parser.tab.cc"
    break;

    case 629: /* $@82: %empty  */
#line 3159 "verilog_parser.y"
    {
      enterTypeScope();
    }
#line 7781 "verilog_parser.tab.cc"
    break;

    case 630: /* $@83: %empty  */
#line 3161 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_GENBLOCK);
      node->str = (yyvsp[0].string) ? *(yyvsp[0].string) : std::string();
      ast_stack.back()->children.push_back(node);
      ast_stack.push_back(node);
    }
#line 7792 "verilog_parser.tab.cc"
    break;

    case 631: /* gen_block: TOK_BEGIN $@82 opt_label $@83 module_gen_body
                 TOK_END opt_label  */
#line 3166 "verilog_parser.y"
    {
      exitTypeScope();
      checkLabelsMatch("Begin label", (yyvsp[-4].string), (yyvsp[0].string));
      delete (yyvsp[-4].string);
      delete (yyvsp[0].string);
      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[-6]), (yylsp[0]));
      ast_stack.pop_back();
    }
#line 7805 "verilog_parser.tab.cc"
    break;

    case 632: /* $@84: %empty  */
#line 3177 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_GENBLOCK);
      ast_stack.back()->children.push_back(node);
      ast_stack.push_back(node);
    }
#line 7815 "verilog_parser.tab.cc"
    break;

    case 633: /* gen_stmt_block: $@84 gen_stmt_or_module_body_stmt  */
#line 3181 "verilog_parser.y"
    {
      SET_AST_NODE_LOC(ast_stack.back(), (yylsp[0]), (yylsp[0]));
      ast_stack.pop_back();
    }
#line 7824 "verilog_parser.tab.cc"
    break;

    case 637: /* expr: basic_expr  */
#line 3190 "verilog_parser.y"
    {
      (yyval.ast) = (yyvsp[0].ast);
    }
#line 7832 "verilog_parser.tab.cc"
    break;

    case 638: /* expr: basic_expr '?' attr expr ':' expr  */
#line 3193 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_TERNARY);
      (yyval.ast)->children.push_back((yyvsp[-5].ast));
      (yyval.ast)->children.push_back((yyvsp[-2].ast));
      (yyval.ast)->children.push_back((yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-5]), (yyloc));
      append_attr((yyval.ast), (yyvsp[-3].al));
    }
#line 7845 "verilog_parser.tab.cc"
    break;

    case 639: /* expr: inc_or_dec_op attr rvalue  */
#line 3201 "verilog_parser.y"
    {
      (yyval.ast) = addIncOrDecExpr((yyvsp[0].ast), (yyvsp[-1].al),
                                    (yyvsp[-2].ast_node_type), (yylsp[-2]),
                                    (yylsp[0]), false);
    }
#line 7853 "verilog_parser.tab.cc"
    break;

    case 640: /* expr: rvalue inc_or_dec_op  */
#line 3206 "verilog_parser.y"
    {
      (yyval.ast) =
          addIncOrDecExpr((yyvsp[-1].ast), nullptr, (yyvsp[0].ast_node_type),
                          (yylsp[-1]), (yylsp[0]), true);
    }
#line 7861 "verilog_parser.tab.cc"
    break;

    case 641: /* basic_expr: rvalue  */
#line 3211 "verilog_parser.y"
    {
      (yyval.ast) = (yyvsp[0].ast);
    }
#line 7869 "verilog_parser.tab.cc"
    break;

    case 642: /* basic_expr: '(' expr ')' integral_number  */
#line 3214 "verilog_parser.y"
    {
      if ((yyvsp[0].string)->compare(0, 1, "'") != 0)
        frontend_verilog_yyerror(
            "Cast operation must be applied on sized constants e.g. "
            "(<expr>)<constval> , while %s is not a sized constant.",
            (yyvsp[0].string)->c_str());
      AstNode *bits = (yyvsp[-2].ast);
      AstNode *val = const2ast(
          *(yyvsp[0].string),
          case_type_stack.size() == 0 ? 0 : case_type_stack.back(), !lib_mode);
      if (val == NULL)
        log_error("Value conversion failed: `%s'\n",
                  (yyvsp[0].string)->c_str());
      (yyval.ast) = new AstNode(AST_TO_BITS, bits, val);
      delete (yyvsp[0].string);
    }
#line 7884 "verilog_parser.tab.cc"
    break;

    case 643: /* basic_expr: hierarchical_id integral_number  */
#line 3224 "verilog_parser.y"
    {
      if ((yyvsp[0].string)->compare(0, 1, "'") != 0)
        frontend_verilog_yyerror(
            "Cast operation must be applied on sized constants, e.g. <ID>\'d0, "
            "while %s is not a sized constant.",
            (yyvsp[0].string)->c_str());
      AstNode *bits = new AstNode(AST_IDENTIFIER);
      bits->str = *(yyvsp[-1].string);
      SET_AST_NODE_LOC(bits, (yylsp[-1]), (yylsp[-1]));
      AstNode *val = const2ast(
          *(yyvsp[0].string),
          case_type_stack.size() == 0 ? 0 : case_type_stack.back(), !lib_mode);
      SET_AST_NODE_LOC(val, (yylsp[0]), (yylsp[0]));
      if (val == NULL)
        log_error("Value conversion failed: `%s'\n",
                  (yyvsp[0].string)->c_str());
      (yyval.ast) = new AstNode(AST_TO_BITS, bits, val);
      delete (yyvsp[-1].string);
      delete (yyvsp[0].string);
    }
#line 7903 "verilog_parser.tab.cc"
    break;

    case 644: /* basic_expr: integral_number  */
#line 3238 "verilog_parser.y"
    {
      (yyval.ast) = const2ast(
          *(yyvsp[0].string),
          case_type_stack.size() == 0 ? 0 : case_type_stack.back(), !lib_mode);
      SET_AST_NODE_LOC((yyval.ast), (yylsp[0]), (yylsp[0]));
      if ((yyval.ast) == NULL)
        log_error("Value conversion failed: `%s'\n",
                  (yyvsp[0].string)->c_str());
      delete (yyvsp[0].string);
    }
#line 7915 "verilog_parser.tab.cc"
    break;

    case 645: /* basic_expr: TOK_REALVAL  */
#line 3245 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_REALVALUE);
      char *p = (char *)malloc(GetSize(*(yyvsp[0].string)) + 1), *q;
      for (int i = 0, j = 0; j < GetSize(*(yyvsp[0].string)); j++)
        if ((*(yyvsp[0].string))[j] != '_')
          p[i++] = (*(yyvsp[0].string))[j], p[i] = 0;
      (yyval.ast)->realvalue = strtod(p, &q);
      SET_AST_NODE_LOC((yyval.ast), (yylsp[0]), (yylsp[0]));
      log_assert(*q == 0);
      delete (yyvsp[0].string);
      free(p);
    }
#line 7932 "verilog_parser.tab.cc"
    break;

    case 646: /* basic_expr: TOK_STRING  */
#line 3257 "verilog_parser.y"
    {
      (yyval.ast) = AstNode::mkconst_str(*(yyvsp[0].string));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[0]), (yylsp[0]));
      delete (yyvsp[0].string);
    }
#line 7942 "verilog_parser.tab.cc"
    break;

    case 647: /* $@85: %empty  */
#line 3262 "verilog_parser.y"
    {
      AstNode *node = new AstNode(AST_FCALL);
      node->str = *(yyvsp[-1].string);
      delete (yyvsp[-1].string);
      ast_stack.push_back(node);
      SET_AST_NODE_LOC(node, (yylsp[-1]), (yylsp[-1]));
      append_attr(node, (yyvsp[0].al));
    }
#line 7955 "verilog_parser.tab.cc"
    break;

    case 648: /* basic_expr: hierarchical_id attr $@85 '(' arg_list
                 optional_comma ')'  */
#line 3269 "verilog_parser.y"
    {
      (yyval.ast) = ast_stack.back();
      ast_stack.pop_back();
    }
#line 7964 "verilog_parser.tab.cc"
    break;

    case 649: /* basic_expr: TOK_TO_SIGNED attr '(' expr ')'  */
#line 3273 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_TO_SIGNED, (yyvsp[-1].ast));
      append_attr((yyval.ast), (yyvsp[-3].al));
    }
#line 7973 "verilog_parser.tab.cc"
    break;

    case 650: /* basic_expr: TOK_TO_UNSIGNED attr '(' expr ')'  */
#line 3277 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_TO_UNSIGNED, (yyvsp[-1].ast));
      append_attr((yyval.ast), (yyvsp[-3].al));
    }
#line 7982 "verilog_parser.tab.cc"
    break;

    case 651: /* basic_expr: '(' expr ')'  */
#line 3281 "verilog_parser.y"
    {
      (yyval.ast) = (yyvsp[-1].ast);
    }
#line 7990 "verilog_parser.tab.cc"
    break;

    case 652: /* basic_expr: '(' expr ':' expr ':' expr ')'  */
#line 3284 "verilog_parser.y"
    {
      delete (yyvsp[-5].ast);
      (yyval.ast) = (yyvsp[-3].ast);
      delete (yyvsp[-1].ast);
    }
#line 8000 "verilog_parser.tab.cc"
    break;

    case 653: /* basic_expr: '{' concat_list '}'  */
#line 3289 "verilog_parser.y"
    {
      (yyval.ast) = (yyvsp[-1].ast);
    }
#line 8008 "verilog_parser.tab.cc"
    break;

    case 654: /* basic_expr: '{' expr '{' concat_list '}' '}'  */
#line 3292 "verilog_parser.y"
    {
      (yyval.ast) =
          new AstNode(AST_REPLICATE, (yyvsp[-4].ast), (yyvsp[-2].ast));
    }
#line 8016 "verilog_parser.tab.cc"
    break;

    case 655: /* basic_expr: '~' attr basic_expr  */
#line 3295 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_BIT_NOT, (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8026 "verilog_parser.tab.cc"
    break;

    case 656: /* basic_expr: basic_expr '&' attr basic_expr  */
#line 3300 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_BIT_AND, (yyvsp[-3].ast), (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8036 "verilog_parser.tab.cc"
    break;

    case 657: /* basic_expr: basic_expr OP_NAND attr basic_expr  */
#line 3305 "verilog_parser.y"
    {
      (yyval.ast) =
          new AstNode(AST_BIT_NOT, new AstNode(AST_BIT_AND, (yyvsp[-3].ast),
                                               (yyvsp[0].ast)));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8046 "verilog_parser.tab.cc"
    break;

    case 658: /* basic_expr: basic_expr '|' attr basic_expr  */
#line 3310 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_BIT_OR, (yyvsp[-3].ast), (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8056 "verilog_parser.tab.cc"
    break;

    case 659: /* basic_expr: basic_expr OP_NOR attr basic_expr  */
#line 3315 "verilog_parser.y"
    {
      (yyval.ast) =
          new AstNode(AST_BIT_NOT,
                      new AstNode(AST_BIT_OR, (yyvsp[-3].ast), (yyvsp[0].ast)));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8066 "verilog_parser.tab.cc"
    break;

    case 660: /* basic_expr: basic_expr '^' attr basic_expr  */
#line 3320 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_BIT_XOR, (yyvsp[-3].ast), (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8076 "verilog_parser.tab.cc"
    break;

    case 661: /* basic_expr: basic_expr OP_XNOR attr basic_expr  */
#line 3325 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_BIT_XNOR, (yyvsp[-3].ast), (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8086 "verilog_parser.tab.cc"
    break;

    case 662: /* basic_expr: '&' attr basic_expr  */
#line 3330 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_REDUCE_AND, (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8096 "verilog_parser.tab.cc"
    break;

    case 663: /* basic_expr: OP_NAND attr basic_expr  */
#line 3335 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_REDUCE_AND, (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
      (yyval.ast) = new AstNode(AST_LOGIC_NOT, (yyval.ast));
    }
#line 8107 "verilog_parser.tab.cc"
    break;

    case 664: /* basic_expr: '|' attr basic_expr  */
#line 3341 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_REDUCE_OR, (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8117 "verilog_parser.tab.cc"
    break;

    case 665: /* basic_expr: OP_NOR attr basic_expr  */
#line 3346 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_REDUCE_OR, (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
      (yyval.ast) = new AstNode(AST_LOGIC_NOT, (yyval.ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
    }
#line 8129 "verilog_parser.tab.cc"
    break;

    case 666: /* basic_expr: '^' attr basic_expr  */
#line 3353 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_REDUCE_XOR, (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8139 "verilog_parser.tab.cc"
    break;

    case 667: /* basic_expr: OP_XNOR attr basic_expr  */
#line 3358 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_REDUCE_XNOR, (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8149 "verilog_parser.tab.cc"
    break;

    case 668: /* basic_expr: basic_expr OP_SHL attr basic_expr  */
#line 3363 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_SHIFT_LEFT, (yyvsp[-3].ast),
                                new AstNode(AST_TO_UNSIGNED, (yyvsp[0].ast)));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8159 "verilog_parser.tab.cc"
    break;

    case 669: /* basic_expr: basic_expr OP_SHR attr basic_expr  */
#line 3368 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_SHIFT_RIGHT, (yyvsp[-3].ast),
                                new AstNode(AST_TO_UNSIGNED, (yyvsp[0].ast)));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8169 "verilog_parser.tab.cc"
    break;

    case 670: /* basic_expr: basic_expr OP_SSHL attr basic_expr  */
#line 3373 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_SHIFT_SLEFT, (yyvsp[-3].ast),
                                new AstNode(AST_TO_UNSIGNED, (yyvsp[0].ast)));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8179 "verilog_parser.tab.cc"
    break;

    case 671: /* basic_expr: basic_expr OP_SSHR attr basic_expr  */
#line 3378 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_SHIFT_SRIGHT, (yyvsp[-3].ast),
                                new AstNode(AST_TO_UNSIGNED, (yyvsp[0].ast)));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8189 "verilog_parser.tab.cc"
    break;

    case 672: /* basic_expr: basic_expr '<' attr basic_expr  */
#line 3383 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_LT, (yyvsp[-3].ast), (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8199 "verilog_parser.tab.cc"
    break;

    case 673: /* basic_expr: basic_expr OP_LE attr basic_expr  */
#line 3388 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_LE, (yyvsp[-3].ast), (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8209 "verilog_parser.tab.cc"
    break;

    case 674: /* basic_expr: basic_expr OP_EQ attr basic_expr  */
#line 3393 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_EQ, (yyvsp[-3].ast), (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8219 "verilog_parser.tab.cc"
    break;

    case 675: /* basic_expr: basic_expr OP_NE attr basic_expr  */
#line 3398 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_NE, (yyvsp[-3].ast), (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8229 "verilog_parser.tab.cc"
    break;

    case 676: /* basic_expr: basic_expr OP_EQX attr basic_expr  */
#line 3403 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_EQX, (yyvsp[-3].ast), (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8239 "verilog_parser.tab.cc"
    break;

    case 677: /* basic_expr: basic_expr OP_NEX attr basic_expr  */
#line 3408 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_NEX, (yyvsp[-3].ast), (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8249 "verilog_parser.tab.cc"
    break;

    case 678: /* basic_expr: basic_expr OP_GE attr basic_expr  */
#line 3413 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_GE, (yyvsp[-3].ast), (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8259 "verilog_parser.tab.cc"
    break;

    case 679: /* basic_expr: basic_expr '>' attr basic_expr  */
#line 3418 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_GT, (yyvsp[-3].ast), (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8269 "verilog_parser.tab.cc"
    break;

    case 680: /* basic_expr: basic_expr '+' attr basic_expr  */
#line 3423 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_ADD, (yyvsp[-3].ast), (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8279 "verilog_parser.tab.cc"
    break;

    case 681: /* basic_expr: basic_expr '-' attr basic_expr  */
#line 3428 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_SUB, (yyvsp[-3].ast), (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8289 "verilog_parser.tab.cc"
    break;

    case 682: /* basic_expr: basic_expr '*' attr basic_expr  */
#line 3433 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_MUL, (yyvsp[-3].ast), (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8299 "verilog_parser.tab.cc"
    break;

    case 683: /* basic_expr: basic_expr '/' attr basic_expr  */
#line 3438 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_DIV, (yyvsp[-3].ast), (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8309 "verilog_parser.tab.cc"
    break;

    case 684: /* basic_expr: basic_expr '%' attr basic_expr  */
#line 3443 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_MOD, (yyvsp[-3].ast), (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8319 "verilog_parser.tab.cc"
    break;

    case 685: /* basic_expr: basic_expr OP_POW attr basic_expr  */
#line 3448 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_POW, (yyvsp[-3].ast), (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8329 "verilog_parser.tab.cc"
    break;

    case 686: /* basic_expr: '+' attr basic_expr  */
#line 3453 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_POS, (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8339 "verilog_parser.tab.cc"
    break;

    case 687: /* basic_expr: '-' attr basic_expr  */
#line 3458 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_NEG, (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8349 "verilog_parser.tab.cc"
    break;

    case 688: /* basic_expr: basic_expr OP_LAND attr basic_expr  */
#line 3463 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_LOGIC_AND, (yyvsp[-3].ast), (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8359 "verilog_parser.tab.cc"
    break;

    case 689: /* basic_expr: basic_expr OP_LOR attr basic_expr  */
#line 3468 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_LOGIC_OR, (yyvsp[-3].ast), (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-3]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8369 "verilog_parser.tab.cc"
    break;

    case 690: /* basic_expr: '!' attr basic_expr  */
#line 3473 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_LOGIC_NOT, (yyvsp[0].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-2]), (yylsp[0]));
      append_attr((yyval.ast), (yyvsp[-1].al));
    }
#line 8379 "verilog_parser.tab.cc"
    break;

    case 691: /* basic_expr: TOK_SIGNED OP_CAST '(' expr ')'  */
#line 3478 "verilog_parser.y"
    {
      if (!sv_mode)
        frontend_verilog_yyerror(
            "Static cast is only supported in SystemVerilog mode.");
      (yyval.ast) = new AstNode(AST_TO_SIGNED, (yyvsp[-1].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-4]), (yylsp[-1]));
    }
#line 8390 "verilog_parser.tab.cc"
    break;

    case 692: /* basic_expr: TOK_UNSIGNED OP_CAST '(' expr ')'  */
#line 3484 "verilog_parser.y"
    {
      if (!sv_mode)
        frontend_verilog_yyerror(
            "Static cast is only supported in SystemVerilog mode.");
      (yyval.ast) = new AstNode(AST_TO_UNSIGNED, (yyvsp[-1].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-4]), (yylsp[-1]));
    }
#line 8401 "verilog_parser.tab.cc"
    break;

    case 693: /* basic_expr: basic_expr OP_CAST '(' expr ')'  */
#line 3490 "verilog_parser.y"
    {
      if (!sv_mode)
        frontend_verilog_yyerror(
            "Static cast is only supported in SystemVerilog mode.");
      (yyval.ast) =
          new AstNode(AST_CAST_SIZE, (yyvsp[-4].ast), (yyvsp[-1].ast));
      SET_AST_NODE_LOC((yyval.ast), (yylsp[-4]), (yylsp[-1]));
    }
#line 8412 "verilog_parser.tab.cc"
    break;

    case 694: /* basic_expr: '(' expr '=' expr ')'  */
#line 3496 "verilog_parser.y"
    {
      ensureAsgnExprAllowed();
      AstNode *node =
          new AstNode(AST_ASSIGN_EQ, (yyvsp[-3].ast), (yyvsp[-1].ast));
      ast_stack.back()->children.push_back(node);
      SET_AST_NODE_LOC(node, (yylsp[-3]), (yylsp[-1]));
      (yyval.ast) = (yyvsp[-3].ast)->clone();
    }
#line 8424 "verilog_parser.tab.cc"
    break;

    case 695: /* basic_expr: '(' expr asgn_binop expr ')'  */
#line 3503 "verilog_parser.y"
    {
      ensureAsgnExprAllowed();
      (yyval.ast) =
          addAsgnBinopStmt(nullptr, (yyvsp[-3].ast), (yyvsp[-2].ast_node_type),
                           (yyvsp[-1].ast), (yylsp[-3]), (yylsp[-1]))
              ->clone();
    }
#line 8433 "verilog_parser.tab.cc"
    break;

    case 696: /* concat_list: expr  */
#line 3509 "verilog_parser.y"
    {
      (yyval.ast) = new AstNode(AST_CONCAT, (yyvsp[0].ast));
    }
#line 8441 "verilog_parser.tab.cc"
    break;

    case 697: /* concat_list: expr ',' concat_list  */
#line 3512 "verilog_parser.y"
    {
      (yyval.ast) = (yyvsp[0].ast);
      (yyval.ast)->children.push_back((yyvsp[-2].ast));
    }
#line 8450 "verilog_parser.tab.cc"
    break;

    case 698: /* integral_number: TOK_CONSTVAL  */
#line 3518 "verilog_parser.y"
    {
      (yyval.string) = (yyvsp[0].string);
    }
#line 8456 "verilog_parser.tab.cc"
    break;

    case 699: /* integral_number: TOK_UNBASED_UNSIZED_CONSTVAL  */
#line 3519 "verilog_parser.y"
    {
      (yyval.string) = (yyvsp[0].string);
    }
#line 8462 "verilog_parser.tab.cc"
    break;

    case 700: /* integral_number: TOK_BASE TOK_BASED_CONSTVAL  */
#line 3520 "verilog_parser.y"
    {
      (yyvsp[-1].string)->append(*(yyvsp[0].string));
      (yyval.string) = (yyvsp[-1].string);
      delete (yyvsp[0].string);
    }
#line 8472 "verilog_parser.tab.cc"
    break;

    case 701: /* integral_number: TOK_CONSTVAL TOK_BASE TOK_BASED_CONSTVAL  */
#line 3525 "verilog_parser.y"
    {
      (yyvsp[-2].string)
          ->append(*(yyvsp[-1].string))
          .append(*(yyvsp[0].string));
      (yyval.string) = (yyvsp[-2].string);
      delete (yyvsp[-1].string);
      delete (yyvsp[0].string);
    }
#line 8483 "verilog_parser.tab.cc"
    break;

#line 8487 "verilog_parser.tab.cc"

    default:
      break;
    }
    if (yychar_backup != yychar)
      YY_LAC_DISCARD("yychar change");
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
  YY_SYMBOL_PRINT("-> $$ =", YY_CAST(yysymbol_kind_t, yyr1[yyn]), &yyval,
                  &yyloc);

  YYPOPSTACK(yylen);
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
  yytoken = yychar == FRONTEND_VERILOG_YYEMPTY ? YYSYMBOL_YYEMPTY
                                               : YYTRANSLATE(yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus) {
    ++yynerrs;
    {
      yypcontext_t yyctx = {yyssp,          yyesa,   &yyes,
                            &yyes_capacity, yytoken, &yylloc};
      char const *yymsgp = YY_("syntax error");
      int yysyntax_error_status;
      if (yychar != FRONTEND_VERILOG_YYEMPTY)
        YY_LAC_ESTABLISH;
      yysyntax_error_status = yysyntax_error(&yymsg_alloc, &yymsg, &yyctx);
      if (yysyntax_error_status == 0)
        yymsgp = yymsg;
      else if (yysyntax_error_status == -1) {
        if (yymsg != yymsgbuf)
          YYSTACK_FREE(yymsg);
        yymsg = YY_CAST(char *, YYSTACK_ALLOC(YY_CAST(YYSIZE_T, yymsg_alloc)));
        if (yymsg) {
          yysyntax_error_status = yysyntax_error(&yymsg_alloc, &yymsg, &yyctx);
          yymsgp = yymsg;
        } else {
          yymsg = yymsgbuf;
          yymsg_alloc = sizeof yymsgbuf;
          yysyntax_error_status = YYENOMEM;
        }
      }
      yyerror(yymsgp);
      if (yysyntax_error_status == YYENOMEM)
        YYNOMEM;
    }
  }

  yyerror_range[1] = yylloc;
  if (yyerrstatus == 3) {
    /* If just tried and failed to reuse lookahead token after an
       error, discard it.  */

    if (yychar <= FRONTEND_VERILOG_YYEOF) {
      /* Return failure if at end of input.  */
      if (yychar == FRONTEND_VERILOG_YYEOF)
        YYABORT;
    } else {
      yydestruct("Error: discarding", yytoken, &yylval, &yylloc);
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
  YYPOPSTACK(yylen);
  yylen = 0;
  YY_STACK_PRINT(yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;

/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3; /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;) {
    yyn = yypact[yystate];
    if (!yypact_value_is_default(yyn)) {
      yyn += YYSYMBOL_YYerror;
      if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror) {
        yyn = yytable[yyn];
        if (0 < yyn)
          break;
      }
    }

    /* Pop the current state because it cannot handle the error token.  */
    if (yyssp == yyss)
      YYABORT;

    yyerror_range[1] = *yylsp;
    yydestruct("Error: popping", YY_ACCESSING_SYMBOL(yystate), yyvsp, yylsp);
    YYPOPSTACK(1);
    yystate = *yyssp;
    YY_STACK_PRINT(yyss, yyssp);
  }

  /* If the stack popping above didn't lose the initial context for the
     current lookahead token, the shift below will for sure.  */
  YY_LAC_DISCARD("error recovery");

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  ++yylsp;
  YYLLOC_DEFAULT(*yylsp, yyerror_range, 2);

  /* Shift the error token.  */
  YY_SYMBOL_PRINT("Shifting", YY_ACCESSING_SYMBOL(yyn), yyvsp, yylsp);

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
  yyerror(YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;

/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != FRONTEND_VERILOG_YYEMPTY) {
    /* Make sure we have latest lookahead translation.  See comments at
       user semantic actions for why this is necessary.  */
    yytoken = YYTRANSLATE(yychar);
    yydestruct("Cleanup: discarding lookahead", yytoken, &yylval, &yylloc);
  }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK(yylen);
  YY_STACK_PRINT(yyss, yyssp);
  while (yyssp != yyss) {
    yydestruct("Cleanup: popping", YY_ACCESSING_SYMBOL(+*yyssp), yyvsp, yylsp);
    YYPOPSTACK(1);
  }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE(yyss);
#endif
  if (yyes != yyesa)
    YYSTACK_FREE(yyes);
  if (yymsg != yymsgbuf)
    YYSTACK_FREE(yymsg);
  return yyresult;
}
