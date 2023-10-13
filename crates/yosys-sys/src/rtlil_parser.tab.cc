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
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Substitute the type names.  */
#define YYSTYPE         RTLIL_FRONTEND_YYSTYPE
/* Substitute the variable and function names.  */
#define yyparse         rtlil_frontend_yyparse
#define yylex           rtlil_frontend_yylex
#define yyerror         rtlil_frontend_yyerror
#define yydebug         rtlil_frontend_yydebug
#define yynerrs         rtlil_frontend_yynerrs
#define yylval          rtlil_frontend_yylval
#define yychar          rtlil_frontend_yychar

/* First part of user prologue.  */
#line 27 "yosys/frontends/rtlil/rtlil_parser.y"

#include <list>
#include "frontends/rtlil/rtlil_frontend.h"
YOSYS_NAMESPACE_BEGIN
namespace RTLIL_FRONTEND {
	std::istream *lexin;
	RTLIL::Design *current_design;
	RTLIL::Module *current_module;
	RTLIL::Wire *current_wire;
	RTLIL::Memory *current_memory;
	RTLIL::Cell *current_cell;
	RTLIL::Process *current_process;
	std::vector<std::vector<RTLIL::SwitchRule*>*> switch_stack;
	std::vector<RTLIL::CaseRule*> case_stack;
	dict<RTLIL::IdString, RTLIL::Const> attrbuf;
	bool flag_nooverwrite, flag_overwrite, flag_lib;
	bool delete_current_module;
}
using namespace RTLIL_FRONTEND;
YOSYS_NAMESPACE_END
USING_YOSYS_NAMESPACE

#line 102 "rtlil_parser.tab.cc"

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

#include "rtlil_parser.tab.hh"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_TOK_ID = 3,                     /* TOK_ID  */
  YYSYMBOL_TOK_VALUE = 4,                  /* TOK_VALUE  */
  YYSYMBOL_TOK_STRING = 5,                 /* TOK_STRING  */
  YYSYMBOL_TOK_INT = 6,                    /* TOK_INT  */
  YYSYMBOL_TOK_AUTOIDX = 7,                /* TOK_AUTOIDX  */
  YYSYMBOL_TOK_MODULE = 8,                 /* TOK_MODULE  */
  YYSYMBOL_TOK_WIRE = 9,                   /* TOK_WIRE  */
  YYSYMBOL_TOK_WIDTH = 10,                 /* TOK_WIDTH  */
  YYSYMBOL_TOK_INPUT = 11,                 /* TOK_INPUT  */
  YYSYMBOL_TOK_OUTPUT = 12,                /* TOK_OUTPUT  */
  YYSYMBOL_TOK_INOUT = 13,                 /* TOK_INOUT  */
  YYSYMBOL_TOK_CELL = 14,                  /* TOK_CELL  */
  YYSYMBOL_TOK_CONNECT = 15,               /* TOK_CONNECT  */
  YYSYMBOL_TOK_SWITCH = 16,                /* TOK_SWITCH  */
  YYSYMBOL_TOK_CASE = 17,                  /* TOK_CASE  */
  YYSYMBOL_TOK_ASSIGN = 18,                /* TOK_ASSIGN  */
  YYSYMBOL_TOK_SYNC = 19,                  /* TOK_SYNC  */
  YYSYMBOL_TOK_LOW = 20,                   /* TOK_LOW  */
  YYSYMBOL_TOK_HIGH = 21,                  /* TOK_HIGH  */
  YYSYMBOL_TOK_POSEDGE = 22,               /* TOK_POSEDGE  */
  YYSYMBOL_TOK_NEGEDGE = 23,               /* TOK_NEGEDGE  */
  YYSYMBOL_TOK_EDGE = 24,                  /* TOK_EDGE  */
  YYSYMBOL_TOK_ALWAYS = 25,                /* TOK_ALWAYS  */
  YYSYMBOL_TOK_GLOBAL = 26,                /* TOK_GLOBAL  */
  YYSYMBOL_TOK_INIT = 27,                  /* TOK_INIT  */
  YYSYMBOL_TOK_UPDATE = 28,                /* TOK_UPDATE  */
  YYSYMBOL_TOK_MEMWR = 29,                 /* TOK_MEMWR  */
  YYSYMBOL_TOK_PROCESS = 30,               /* TOK_PROCESS  */
  YYSYMBOL_TOK_END = 31,                   /* TOK_END  */
  YYSYMBOL_TOK_INVALID = 32,               /* TOK_INVALID  */
  YYSYMBOL_TOK_EOL = 33,                   /* TOK_EOL  */
  YYSYMBOL_TOK_OFFSET = 34,                /* TOK_OFFSET  */
  YYSYMBOL_TOK_PARAMETER = 35,             /* TOK_PARAMETER  */
  YYSYMBOL_TOK_ATTRIBUTE = 36,             /* TOK_ATTRIBUTE  */
  YYSYMBOL_TOK_MEMORY = 37,                /* TOK_MEMORY  */
  YYSYMBOL_TOK_SIZE = 38,                  /* TOK_SIZE  */
  YYSYMBOL_TOK_SIGNED = 39,                /* TOK_SIGNED  */
  YYSYMBOL_TOK_REAL = 40,                  /* TOK_REAL  */
  YYSYMBOL_TOK_UPTO = 41,                  /* TOK_UPTO  */
  YYSYMBOL_42_ = 42,                       /* ','  */
  YYSYMBOL_43_ = 43,                       /* '['  */
  YYSYMBOL_44_ = 44,                       /* ']'  */
  YYSYMBOL_45_ = 45,                       /* ':'  */
  YYSYMBOL_46_ = 46,                       /* '{'  */
  YYSYMBOL_47_ = 47,                       /* '}'  */
  YYSYMBOL_YYACCEPT = 48,                  /* $accept  */
  YYSYMBOL_input = 49,                     /* input  */
  YYSYMBOL_50_1 = 50,                      /* $@1  */
  YYSYMBOL_EOL = 51,                       /* EOL  */
  YYSYMBOL_optional_eol = 52,              /* optional_eol  */
  YYSYMBOL_design = 53,                    /* design  */
  YYSYMBOL_module = 54,                    /* module  */
  YYSYMBOL_55_2 = 55,                      /* $@2  */
  YYSYMBOL_56_3 = 56,                      /* $@3  */
  YYSYMBOL_module_body = 57,               /* module_body  */
  YYSYMBOL_module_stmt = 58,               /* module_stmt  */
  YYSYMBOL_param_stmt = 59,                /* param_stmt  */
  YYSYMBOL_param_defval_stmt = 60,         /* param_defval_stmt  */
  YYSYMBOL_attr_stmt = 61,                 /* attr_stmt  */
  YYSYMBOL_autoidx_stmt = 62,              /* autoidx_stmt  */
  YYSYMBOL_wire_stmt = 63,                 /* wire_stmt  */
  YYSYMBOL_64_4 = 64,                      /* $@4  */
  YYSYMBOL_wire_options = 65,              /* wire_options  */
  YYSYMBOL_memory_stmt = 66,               /* memory_stmt  */
  YYSYMBOL_67_5 = 67,                      /* $@5  */
  YYSYMBOL_memory_options = 68,            /* memory_options  */
  YYSYMBOL_cell_stmt = 69,                 /* cell_stmt  */
  YYSYMBOL_70_6 = 70,                      /* $@6  */
  YYSYMBOL_cell_body = 71,                 /* cell_body  */
  YYSYMBOL_proc_stmt = 72,                 /* proc_stmt  */
  YYSYMBOL_73_7 = 73,                      /* $@7  */
  YYSYMBOL_switch_stmt = 74,               /* switch_stmt  */
  YYSYMBOL_75_8 = 75,                      /* $@8  */
  YYSYMBOL_attr_list = 76,                 /* attr_list  */
  YYSYMBOL_switch_body = 77,               /* switch_body  */
  YYSYMBOL_78_9 = 78,                      /* $@9  */
  YYSYMBOL_compare_list = 79,              /* compare_list  */
  YYSYMBOL_case_body = 80,                 /* case_body  */
  YYSYMBOL_assign_stmt = 81,               /* assign_stmt  */
  YYSYMBOL_sync_list = 82,                 /* sync_list  */
  YYSYMBOL_83_10 = 83,                     /* $@10  */
  YYSYMBOL_84_11 = 84,                     /* $@11  */
  YYSYMBOL_85_12 = 85,                     /* $@12  */
  YYSYMBOL_86_13 = 86,                     /* $@13  */
  YYSYMBOL_sync_type = 87,                 /* sync_type  */
  YYSYMBOL_update_list = 88,               /* update_list  */
  YYSYMBOL_constant = 89,                  /* constant  */
  YYSYMBOL_sigspec = 90,                   /* sigspec  */
  YYSYMBOL_sigspec_list_reversed = 91,     /* sigspec_list_reversed  */
  YYSYMBOL_sigspec_list = 92,              /* sigspec_list  */
  YYSYMBOL_conn_stmt = 93                  /* conn_stmt  */
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
typedef yytype_uint8 yy_state_t;

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

#if !defined yyoverflow

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
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

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
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined RTLIL_FRONTEND_YYSTYPE_IS_TRIVIAL && RTLIL_FRONTEND_YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

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
#define YYLAST   188

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  48
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  46
/* YYNRULES -- Number of rules.  */
#define YYNRULES  97
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  181

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   296


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,    42,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    45,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    43,     2,    44,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    46,     2,    47,     2,     2,     2,     2,
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
      35,    36,    37,    38,    39,    40,    41
};

#if RTLIL_FRONTEND_YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,    88,    88,    88,    96,    99,    99,   102,   103,   104,
     104,   108,   132,   108,   144,   144,   148,   148,   148,   148,
     148,   148,   148,   148,   151,   157,   165,   172,   177,   177,
     189,   192,   195,   198,   201,   204,   209,   214,   218,   222,
     222,   235,   238,   241,   243,   247,   247,   258,   263,   269,
     275,   281,   285,   285,   299,   299,   308,   310,   313,   313,
     323,   327,   331,   334,   338,   339,   340,   340,   344,   353,
     353,   360,   360,   366,   366,   372,   372,   377,   381,   382,
     383,   384,   385,   388,   393,   408,   412,   443,   446,   452,
     456,   462,   468,   474,   479,   483,   487,   495
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if RTLIL_FRONTEND_YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "TOK_ID", "TOK_VALUE",
  "TOK_STRING", "TOK_INT", "TOK_AUTOIDX", "TOK_MODULE", "TOK_WIRE",
  "TOK_WIDTH", "TOK_INPUT", "TOK_OUTPUT", "TOK_INOUT", "TOK_CELL",
  "TOK_CONNECT", "TOK_SWITCH", "TOK_CASE", "TOK_ASSIGN", "TOK_SYNC",
  "TOK_LOW", "TOK_HIGH", "TOK_POSEDGE", "TOK_NEGEDGE", "TOK_EDGE",
  "TOK_ALWAYS", "TOK_GLOBAL", "TOK_INIT", "TOK_UPDATE", "TOK_MEMWR",
  "TOK_PROCESS", "TOK_END", "TOK_INVALID", "TOK_EOL", "TOK_OFFSET",
  "TOK_PARAMETER", "TOK_ATTRIBUTE", "TOK_MEMORY", "TOK_SIZE", "TOK_SIGNED",
  "TOK_REAL", "TOK_UPTO", "','", "'['", "']'", "':'", "'{'", "'}'",
  "$accept", "input", "$@1", "EOL", "optional_eol", "design", "module",
  "$@2", "$@3", "module_body", "module_stmt", "param_stmt",
  "param_defval_stmt", "attr_stmt", "autoidx_stmt", "wire_stmt", "$@4",
  "wire_options", "memory_stmt", "$@5", "memory_options", "cell_stmt",
  "$@6", "cell_body", "proc_stmt", "$@7", "switch_stmt", "$@8",
  "attr_list", "switch_body", "$@9", "compare_list", "case_body",
  "assign_stmt", "sync_list", "$@10", "$@11", "$@12", "$@13", "sync_type",
  "update_list", "constant", "sigspec", "sigspec_list_reversed",
  "sigspec_list", "conn_stmt", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-68)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-77)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     -68,    76,    -9,   -68,   -68,   -68,    34,    71,    79,    81,
     -68,   -68,   -68,   -68,   -68,   103,   -68,    52,   -68,   -68,
     -68,   -68,   -68,    56,   -68,   -68,   143,   -68,    84,     1,
      88,   -68,    91,   -68,   -68,   -68,   -68,   -68,   -68,   -68,
     -68,   -68,   -68,   -68,    92,   -68,   -68,   -68,     6,   -68,
     -68,   103,   -68,    20,   -68,     1,    50,    94,    55,   -68,
     -68,   -68,   -68,    54,   -68,    -4,    96,    97,    99,   104,
     -68,   -68,   -68,    55,   -68,    24,   -68,   -68,   -68,   -68,
     105,   109,   110,   -68,   -68,   -68,   -68,   -68,   -68,   -68,
     -68,   -68,   113,    27,   -68,   -68,   -68,   -68,     3,    80,
       1,     1,   -68,   -68,   -68,    48,   117,   -68,    16,   -68,
      55,     6,   161,   -68,     1,   -68,   103,   120,   122,   -68,
      55,   -68,   -68,   -68,   -68,   -68,   -68,   -68,   -68,     1,
     -68,    55,   -68,   103,   103,   -68,   -68,   -68,   -68,   -68,
      55,   -68,   -68,   -68,   -68,    95,   -68,   -68,   -68,   -68,
     -68,   -68,   -68,    29,    -6,    -2,    43,   -68,   -68,   -68,
       1,    22,    47,     1,   -68,     6,   124,    86,    55,    55,
       1,     1,   -68,   -68,     6,    55,    27,     6,    10,   -68,
     -68
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       6,     0,     2,     1,     5,    10,     3,     0,     0,     0,
       7,     8,     9,     6,     6,     0,    27,     0,    11,    86,
      88,    87,     6,     4,    15,    26,     0,    28,     0,     0,
       0,    12,     0,    39,    14,    16,    17,    18,    19,    20,
      21,    22,    23,    38,     0,    90,    95,    89,     0,     6,
       6,     6,    44,     0,     6,    96,     0,     0,     6,    52,
      13,    24,     6,     0,     6,     0,     0,     0,     0,     0,
      33,    32,    45,    94,    93,     0,    97,    67,    25,     6,
       0,     0,     0,    29,    30,    31,    35,    36,    37,    34,
      51,    91,     0,    77,    40,    41,    43,    42,     0,     0,
       0,     0,    64,    65,    66,     0,     0,     6,     0,    92,
       6,     0,     0,     6,     0,    46,     0,     0,     0,    54,
       6,    78,    79,    80,    81,    82,     6,     6,     6,     0,
      53,     6,     6,     0,     0,    56,    68,    71,    73,    75,
       6,    50,    47,     6,     6,    60,    85,    85,    85,    69,
      48,    49,    57,     0,    56,    56,    56,    85,    58,     6,
       0,     0,    56,    63,    55,     0,     0,     6,    61,     6,
       0,     0,    67,    83,     0,    62,    59,     0,     0,     6,
      84
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -68,   -68,   -68,   -14,   133,   -68,   -68,   -68,   -68,   -68,
     -68,   -68,   -68,    -5,   -68,   -68,   -68,   -68,   -68,   -68,
     -68,   -68,   -68,   -68,   -68,   -68,   -68,   -68,    -1,   -68,
     -68,   -68,   -36,   -68,   -68,   -68,   -68,   -68,   -68,   -68,
     -67,   -12,   -28,   -68,   -68,   -68
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,     1,     5,    16,    17,     6,    10,    24,    50,    26,
      34,    35,    36,   102,    12,    38,    43,    53,    39,    52,
      63,    40,    90,    98,    41,    77,   103,   135,   161,   153,
     163,   167,    93,   104,   105,   157,   146,   147,   148,   129,
     154,    47,    48,    55,    56,    42
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      18,    11,    84,    22,    45,    19,    20,    21,    25,    45,
      19,    20,    21,   -72,    19,    20,    21,   -74,   106,   116,
      58,    37,   160,    64,     4,   -72,   160,    73,    85,   -74,
      65,    66,    67,    68,   107,    59,    60,    61,   108,    62,
      72,     7,     8,   100,    76,   101,   158,    46,    78,    57,
      83,   166,    46,    57,    69,   117,   118,    79,     9,    70,
     159,    71,   -76,     9,    80,    94,   -70,   112,    91,    92,
       9,   160,   110,   111,   -76,   160,     3,    13,   -70,   113,
     155,   156,    14,   120,    15,    23,   131,    44,    81,    -5,
     162,    49,    82,   115,    51,    54,   119,    74,    57,   130,
      75,   140,    86,    87,   132,    88,   136,    19,    20,    21,
      89,    95,   137,   138,   139,    96,    97,   141,   142,    99,
     114,   143,   144,   133,   109,   134,   149,   170,   171,   150,
     151,     9,   165,     2,   145,   168,   176,   169,     0,     0,
     152,     0,   174,   175,     0,   164,   177,     0,     0,   178,
       0,     0,    27,   172,     0,   173,   152,    28,    29,     0,
       0,     0,     0,     0,     0,   180,   179,     0,     0,     0,
       0,     0,     0,    30,    31,     0,     0,     0,    32,     9,
      33,   121,   122,   123,   124,   125,   126,   127,   128
};

static const yytype_int16 yycheck[] =
{
      14,     6,     6,    15,     3,     4,     5,     6,    22,     3,
       4,     5,     6,    19,     4,     5,     6,    19,    15,     3,
      48,    26,    28,     3,    33,    31,    28,    55,    32,    31,
      10,    11,    12,    13,    31,    49,    50,    51,    35,    51,
      54,     7,     8,    16,    58,    18,    17,    46,    62,    43,
      64,    29,    46,    43,    34,    39,    40,     3,    36,    39,
      31,    41,    19,    36,    10,    79,    19,    19,    44,    45,
      36,    28,   100,   101,    31,    28,     0,     6,    31,    31,
     147,   148,     3,   111,     3,    33,   114,     3,    34,    33,
     157,     3,    38,   107,     3,     3,   110,    47,    43,   113,
       6,   129,     6,     6,   116,     6,   120,     4,     5,     6,
       6,     6,   126,   127,   128,     6,     6,   131,   132,     6,
       3,   133,   134,     3,    44,     3,   140,     3,    42,   143,
     144,    36,   160,     0,   135,   163,   172,   165,    -1,    -1,
     145,    -1,   170,   171,    -1,   159,   174,    -1,    -1,   177,
      -1,    -1,     9,   167,    -1,   169,   161,    14,    15,    -1,
      -1,    -1,    -1,    -1,    -1,   179,   178,    -1,    -1,    -1,
      -1,    -1,    -1,    30,    31,    -1,    -1,    -1,    35,    36,
      37,    20,    21,    22,    23,    24,    25,    26,    27
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,    49,    52,     0,    33,    50,    53,     7,     8,    36,
      54,    61,    62,     6,     3,     3,    51,    52,    51,     4,
       5,     6,    89,    33,    55,    51,    57,     9,    14,    15,
      30,    31,    35,    37,    58,    59,    60,    61,    63,    66,
      69,    72,    93,    64,     3,     3,    46,    89,    90,     3,
      56,     3,    67,    65,     3,    91,    92,    43,    90,    51,
      51,    51,    89,    68,     3,    10,    11,    12,    13,    34,
      39,    41,    51,    90,    47,     6,    51,    73,    51,     3,
      10,    34,    38,    51,     6,    32,     6,     6,     6,     6,
      70,    44,    45,    80,    51,     6,     6,     6,    71,     6,
      16,    18,    61,    74,    81,    82,    15,    31,    35,    44,
      90,    90,    19,    31,     3,    51,     3,    39,    40,    51,
      90,    20,    21,    22,    23,    24,    25,    26,    27,    87,
      51,    90,    89,     3,     3,    75,    51,    51,    51,    51,
      90,    51,    51,    89,    89,    76,    84,    85,    86,    51,
      51,    51,    61,    77,    88,    88,    88,    83,    17,    31,
      28,    76,    88,    78,    51,    90,    29,    79,    90,    90,
       3,    42,    51,    51,    90,    90,    80,    90,    90,    89,
      51
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    48,    50,    49,    51,    52,    52,    53,    53,    53,
      53,    55,    56,    54,    57,    57,    58,    58,    58,    58,
      58,    58,    58,    58,    59,    60,    61,    62,    64,    63,
      65,    65,    65,    65,    65,    65,    65,    65,    65,    67,
      66,    68,    68,    68,    68,    70,    69,    71,    71,    71,
      71,    71,    73,    72,    75,    74,    76,    76,    78,    77,
      77,    79,    79,    79,    80,    80,    80,    80,    81,    83,
      82,    84,    82,    85,    82,    86,    82,    82,    87,    87,
      87,    87,    87,    88,    88,    88,    89,    89,    89,    90,
      90,    90,    90,    90,    91,    91,    92,    93
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     3,     2,     2,     0,     2,     2,     2,
       0,     0,     0,     8,     2,     0,     1,     1,     1,     1,
       1,     1,     1,     1,     3,     4,     4,     3,     0,     5,
       3,     3,     2,     2,     3,     3,     3,     3,     0,     0,
       5,     3,     3,     3,     0,     0,     8,     5,     6,     6,
       5,     0,     0,     8,     0,     8,     0,     2,     0,     6,
       0,     1,     3,     0,     2,     2,     2,     0,     4,     0,
       7,     0,     6,     0,     6,     0,     6,     0,     1,     1,
       1,     1,     1,     5,     9,     0,     1,     1,     1,     1,
       1,     4,     6,     3,     2,     0,     1,     4
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = RTLIL_FRONTEND_YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == RTLIL_FRONTEND_YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use RTLIL_FRONTEND_YYerror or RTLIL_FRONTEND_YYUNDEF. */
#define YYERRCODE RTLIL_FRONTEND_YYUNDEF


/* Enable debugging if requested.  */
#if RTLIL_FRONTEND_YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
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
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
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
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
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
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !RTLIL_FRONTEND_YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !RTLIL_FRONTEND_YYDEBUG */


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






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
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

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = RTLIL_FRONTEND_YYEMPTY; /* Cause a token to be read.  */

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

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
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
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

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
  if (yychar == RTLIL_FRONTEND_YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= RTLIL_FRONTEND_YYEOF)
    {
      yychar = RTLIL_FRONTEND_YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == RTLIL_FRONTEND_YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = RTLIL_FRONTEND_YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
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
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
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

  /* Discard the shifted token.  */
  yychar = RTLIL_FRONTEND_YYEMPTY;
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


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* $@1: %empty  */
#line 88 "yosys/frontends/rtlil/rtlil_parser.y"
                     {
		attrbuf.clear();
	}
#line 1315 "rtlil_parser.tab.cc"
    break;

  case 3: /* input: optional_eol $@1 design  */
#line 90 "yosys/frontends/rtlil/rtlil_parser.y"
                 {
		if (attrbuf.size() != 0)
			rtlil_frontend_yyerror("dangling attribute");
	}
#line 1324 "rtlil_parser.tab.cc"
    break;

  case 11: /* $@2: %empty  */
#line 108 "yosys/frontends/rtlil/rtlil_parser.y"
                              {
		delete_current_module = false;
		if (current_design->has((yyvsp[-1].string))) {
			RTLIL::Module *existing_mod = current_design->module((yyvsp[-1].string));
			if (!flag_overwrite && (flag_lib || (attrbuf.count(ID::blackbox) && attrbuf.at(ID::blackbox).as_bool()))) {
				log("Ignoring blackbox re-definition of module %s.\n", (yyvsp[-1].string));
				delete_current_module = true;
			} else if (!flag_nooverwrite && !flag_overwrite && !existing_mod->get_bool_attribute(ID::blackbox)) {
				rtlil_frontend_yyerror(stringf("RTLIL error: redefinition of module %s.", (yyvsp[-1].string)).c_str());
			} else if (flag_nooverwrite) {
				log("Ignoring re-definition of module %s.\n", (yyvsp[-1].string));
				delete_current_module = true;
			} else {
				log("Replacing existing%s module %s.\n", existing_mod->get_bool_attribute(ID::blackbox) ? " blackbox" : "", (yyvsp[-1].string));
				current_design->remove(existing_mod);
			}
		}
		current_module = new RTLIL::Module;
		current_module->name = (yyvsp[-1].string);
		current_module->attributes = attrbuf;
		if (!delete_current_module)
			current_design->add(current_module);
		attrbuf.clear();
		free((yyvsp[-1].string));
	}
#line 1354 "rtlil_parser.tab.cc"
    break;

  case 12: /* $@3: %empty  */
#line 132 "yosys/frontends/rtlil/rtlil_parser.y"
                              {
		if (attrbuf.size() != 0)
			rtlil_frontend_yyerror("dangling attribute");
		current_module->fixup_ports();
		if (delete_current_module)
			delete current_module;
		else if (flag_lib)
			current_module->makeblackbox();
		current_module = nullptr;
	}
#line 1369 "rtlil_parser.tab.cc"
    break;

  case 24: /* param_stmt: TOK_PARAMETER TOK_ID EOL  */
#line 151 "yosys/frontends/rtlil/rtlil_parser.y"
                                 {
		current_module->avail_parameters((yyvsp[-1].string));
		free((yyvsp[-1].string));
	}
#line 1378 "rtlil_parser.tab.cc"
    break;

  case 25: /* param_defval_stmt: TOK_PARAMETER TOK_ID constant EOL  */
#line 157 "yosys/frontends/rtlil/rtlil_parser.y"
                                          {
		current_module->avail_parameters((yyvsp[-2].string));
		current_module->parameter_default_values[(yyvsp[-2].string)] = *(yyvsp[-1].data);
		delete (yyvsp[-1].data);
		free((yyvsp[-2].string));
	}
#line 1389 "rtlil_parser.tab.cc"
    break;

  case 26: /* attr_stmt: TOK_ATTRIBUTE TOK_ID constant EOL  */
#line 165 "yosys/frontends/rtlil/rtlil_parser.y"
                                          {
		attrbuf[(yyvsp[-2].string)] = *(yyvsp[-1].data);
		delete (yyvsp[-1].data);
		free((yyvsp[-2].string));
	}
#line 1399 "rtlil_parser.tab.cc"
    break;

  case 27: /* autoidx_stmt: TOK_AUTOIDX TOK_INT EOL  */
#line 172 "yosys/frontends/rtlil/rtlil_parser.y"
                                {
		autoidx = max(autoidx, (yyvsp[-1].integer));
	}
#line 1407 "rtlil_parser.tab.cc"
    break;

  case 28: /* $@4: %empty  */
#line 177 "yosys/frontends/rtlil/rtlil_parser.y"
                 {
		current_wire = current_module->addWire("$__rtlil_frontend_tmp__");
		current_wire->attributes = attrbuf;
		attrbuf.clear();
	}
#line 1417 "rtlil_parser.tab.cc"
    break;

  case 29: /* wire_stmt: TOK_WIRE $@4 wire_options TOK_ID EOL  */
#line 181 "yosys/frontends/rtlil/rtlil_parser.y"
                                  {
		if (current_module->wire((yyvsp[-1].string)) != nullptr)
			rtlil_frontend_yyerror(stringf("RTLIL error: redefinition of wire %s.", (yyvsp[-1].string)).c_str());
		current_module->rename(current_wire, (yyvsp[-1].string));
		free((yyvsp[-1].string));
	}
#line 1428 "rtlil_parser.tab.cc"
    break;

  case 30: /* wire_options: wire_options TOK_WIDTH TOK_INT  */
#line 189 "yosys/frontends/rtlil/rtlil_parser.y"
                                       {
		current_wire->width = (yyvsp[0].integer);
	}
#line 1436 "rtlil_parser.tab.cc"
    break;

  case 31: /* wire_options: wire_options TOK_WIDTH TOK_INVALID  */
#line 192 "yosys/frontends/rtlil/rtlil_parser.y"
                                           {
		rtlil_frontend_yyerror("RTLIL error: invalid wire width");
	}
#line 1444 "rtlil_parser.tab.cc"
    break;

  case 32: /* wire_options: wire_options TOK_UPTO  */
#line 195 "yosys/frontends/rtlil/rtlil_parser.y"
                              {
		current_wire->upto = true;
	}
#line 1452 "rtlil_parser.tab.cc"
    break;

  case 33: /* wire_options: wire_options TOK_SIGNED  */
#line 198 "yosys/frontends/rtlil/rtlil_parser.y"
                                {
		current_wire->is_signed = true;
	}
#line 1460 "rtlil_parser.tab.cc"
    break;

  case 34: /* wire_options: wire_options TOK_OFFSET TOK_INT  */
#line 201 "yosys/frontends/rtlil/rtlil_parser.y"
                                        {
		current_wire->start_offset = (yyvsp[0].integer);
	}
#line 1468 "rtlil_parser.tab.cc"
    break;

  case 35: /* wire_options: wire_options TOK_INPUT TOK_INT  */
#line 204 "yosys/frontends/rtlil/rtlil_parser.y"
                                       {
		current_wire->port_id = (yyvsp[0].integer);
		current_wire->port_input = true;
		current_wire->port_output = false;
	}
#line 1478 "rtlil_parser.tab.cc"
    break;

  case 36: /* wire_options: wire_options TOK_OUTPUT TOK_INT  */
#line 209 "yosys/frontends/rtlil/rtlil_parser.y"
                                        {
		current_wire->port_id = (yyvsp[0].integer);
		current_wire->port_input = false;
		current_wire->port_output = true;
	}
#line 1488 "rtlil_parser.tab.cc"
    break;

  case 37: /* wire_options: wire_options TOK_INOUT TOK_INT  */
#line 214 "yosys/frontends/rtlil/rtlil_parser.y"
                                       {
		current_wire->port_id = (yyvsp[0].integer);
		current_wire->port_input = true;
		current_wire->port_output = true;
	}
#line 1498 "rtlil_parser.tab.cc"
    break;

  case 39: /* $@5: %empty  */
#line 222 "yosys/frontends/rtlil/rtlil_parser.y"
                   {
		current_memory = new RTLIL::Memory;
		current_memory->attributes = attrbuf;
		attrbuf.clear();
	}
#line 1508 "rtlil_parser.tab.cc"
    break;

  case 40: /* memory_stmt: TOK_MEMORY $@5 memory_options TOK_ID EOL  */
#line 226 "yosys/frontends/rtlil/rtlil_parser.y"
                                    {
		if (current_module->memories.count((yyvsp[-1].string)) != 0)
			rtlil_frontend_yyerror(stringf("RTLIL error: redefinition of memory %s.", (yyvsp[-1].string)).c_str());
		current_memory->name = (yyvsp[-1].string);
		current_module->memories[(yyvsp[-1].string)] = current_memory;
		free((yyvsp[-1].string));
	}
#line 1520 "rtlil_parser.tab.cc"
    break;

  case 41: /* memory_options: memory_options TOK_WIDTH TOK_INT  */
#line 235 "yosys/frontends/rtlil/rtlil_parser.y"
                                         {
		current_memory->width = (yyvsp[0].integer);
	}
#line 1528 "rtlil_parser.tab.cc"
    break;

  case 42: /* memory_options: memory_options TOK_SIZE TOK_INT  */
#line 238 "yosys/frontends/rtlil/rtlil_parser.y"
                                        {
		current_memory->size = (yyvsp[0].integer);
	}
#line 1536 "rtlil_parser.tab.cc"
    break;

  case 43: /* memory_options: memory_options TOK_OFFSET TOK_INT  */
#line 241 "yosys/frontends/rtlil/rtlil_parser.y"
                                          {
		current_memory->start_offset = (yyvsp[0].integer);
	}
#line 1544 "rtlil_parser.tab.cc"
    break;

  case 45: /* $@6: %empty  */
#line 247 "yosys/frontends/rtlil/rtlil_parser.y"
                                   {
		if (current_module->cell((yyvsp[-1].string)) != nullptr)
			rtlil_frontend_yyerror(stringf("RTLIL error: redefinition of cell %s.", (yyvsp[-1].string)).c_str());
		current_cell = current_module->addCell((yyvsp[-1].string), (yyvsp[-2].string));
		current_cell->attributes = attrbuf;
		attrbuf.clear();
		free((yyvsp[-2].string));
		free((yyvsp[-1].string));
	}
#line 1558 "rtlil_parser.tab.cc"
    break;

  case 47: /* cell_body: cell_body TOK_PARAMETER TOK_ID constant EOL  */
#line 258 "yosys/frontends/rtlil/rtlil_parser.y"
                                                    {
		current_cell->parameters[(yyvsp[-2].string)] = *(yyvsp[-1].data);
		free((yyvsp[-2].string));
		delete (yyvsp[-1].data);
	}
#line 1568 "rtlil_parser.tab.cc"
    break;

  case 48: /* cell_body: cell_body TOK_PARAMETER TOK_SIGNED TOK_ID constant EOL  */
#line 263 "yosys/frontends/rtlil/rtlil_parser.y"
                                                               {
		current_cell->parameters[(yyvsp[-2].string)] = *(yyvsp[-1].data);
		current_cell->parameters[(yyvsp[-2].string)].flags |= RTLIL::CONST_FLAG_SIGNED;
		free((yyvsp[-2].string));
		delete (yyvsp[-1].data);
	}
#line 1579 "rtlil_parser.tab.cc"
    break;

  case 49: /* cell_body: cell_body TOK_PARAMETER TOK_REAL TOK_ID constant EOL  */
#line 269 "yosys/frontends/rtlil/rtlil_parser.y"
                                                             {
		current_cell->parameters[(yyvsp[-2].string)] = *(yyvsp[-1].data);
		current_cell->parameters[(yyvsp[-2].string)].flags |= RTLIL::CONST_FLAG_REAL;
		free((yyvsp[-2].string));
		delete (yyvsp[-1].data);
	}
#line 1590 "rtlil_parser.tab.cc"
    break;

  case 50: /* cell_body: cell_body TOK_CONNECT TOK_ID sigspec EOL  */
#line 275 "yosys/frontends/rtlil/rtlil_parser.y"
                                                 {
		if (current_cell->hasPort((yyvsp[-2].string)))
			rtlil_frontend_yyerror(stringf("RTLIL error: redefinition of cell port %s.", (yyvsp[-2].string)).c_str());
		current_cell->setPort((yyvsp[-2].string), *(yyvsp[-1].sigspec));
		delete (yyvsp[-1].sigspec);
		free((yyvsp[-2].string));
	}
#line 1602 "rtlil_parser.tab.cc"
    break;

  case 52: /* $@7: %empty  */
#line 285 "yosys/frontends/rtlil/rtlil_parser.y"
                               {
		if (current_module->processes.count((yyvsp[-1].string)) != 0)
			rtlil_frontend_yyerror(stringf("RTLIL error: redefinition of process %s.", (yyvsp[-1].string)).c_str());
		current_process = current_module->addProcess((yyvsp[-1].string));
		current_process->attributes = attrbuf;
		switch_stack.clear();
		switch_stack.push_back(&current_process->root_case.switches);
		case_stack.clear();
		case_stack.push_back(&current_process->root_case);
		attrbuf.clear();
		free((yyvsp[-1].string));
	}
#line 1619 "rtlil_parser.tab.cc"
    break;

  case 54: /* $@8: %empty  */
#line 299 "yosys/frontends/rtlil/rtlil_parser.y"
                               {
		RTLIL::SwitchRule *rule = new RTLIL::SwitchRule;
		rule->signal = *(yyvsp[-1].sigspec);
		rule->attributes = attrbuf;
		switch_stack.back()->push_back(rule);
		attrbuf.clear();
		delete (yyvsp[-1].sigspec);
	}
#line 1632 "rtlil_parser.tab.cc"
    break;

  case 58: /* $@9: %empty  */
#line 313 "yosys/frontends/rtlil/rtlil_parser.y"
                             {
		RTLIL::CaseRule *rule = new RTLIL::CaseRule;
		rule->attributes = attrbuf;
		switch_stack.back()->back()->cases.push_back(rule);
		switch_stack.push_back(&rule->switches);
		case_stack.push_back(rule);
		attrbuf.clear();
	}
#line 1645 "rtlil_parser.tab.cc"
    break;

  case 59: /* switch_body: switch_body TOK_CASE $@9 compare_list EOL case_body  */
#line 320 "yosys/frontends/rtlil/rtlil_parser.y"
                                     {
		switch_stack.pop_back();
		case_stack.pop_back();
	}
#line 1654 "rtlil_parser.tab.cc"
    break;

  case 61: /* compare_list: sigspec  */
#line 327 "yosys/frontends/rtlil/rtlil_parser.y"
                {
		case_stack.back()->compare.push_back(*(yyvsp[0].sigspec));
		delete (yyvsp[0].sigspec);
	}
#line 1663 "rtlil_parser.tab.cc"
    break;

  case 62: /* compare_list: compare_list ',' sigspec  */
#line 331 "yosys/frontends/rtlil/rtlil_parser.y"
                                 {
		case_stack.back()->compare.push_back(*(yyvsp[0].sigspec));
		delete (yyvsp[0].sigspec);
	}
#line 1672 "rtlil_parser.tab.cc"
    break;

  case 68: /* assign_stmt: TOK_ASSIGN sigspec sigspec EOL  */
#line 344 "yosys/frontends/rtlil/rtlil_parser.y"
                                       {
		if (attrbuf.size() != 0)
			rtlil_frontend_yyerror("dangling attribute");
		case_stack.back()->actions.push_back(RTLIL::SigSig(*(yyvsp[-2].sigspec), *(yyvsp[-1].sigspec)));
		delete (yyvsp[-2].sigspec);
		delete (yyvsp[-1].sigspec);
	}
#line 1684 "rtlil_parser.tab.cc"
    break;

  case 69: /* $@10: %empty  */
#line 353 "yosys/frontends/rtlil/rtlil_parser.y"
                                                 {
		RTLIL::SyncRule *rule = new RTLIL::SyncRule;
		rule->type = RTLIL::SyncType((yyvsp[-2].integer));
		rule->signal = *(yyvsp[-1].sigspec);
		current_process->syncs.push_back(rule);
		delete (yyvsp[-1].sigspec);
	}
#line 1696 "rtlil_parser.tab.cc"
    break;

  case 71: /* $@11: %empty  */
#line 360 "yosys/frontends/rtlil/rtlil_parser.y"
                                          {
		RTLIL::SyncRule *rule = new RTLIL::SyncRule;
		rule->type = RTLIL::SyncType::STa;
		rule->signal = RTLIL::SigSpec();
		current_process->syncs.push_back(rule);
	}
#line 1707 "rtlil_parser.tab.cc"
    break;

  case 73: /* $@12: %empty  */
#line 366 "yosys/frontends/rtlil/rtlil_parser.y"
                                          {
		RTLIL::SyncRule *rule = new RTLIL::SyncRule;
		rule->type = RTLIL::SyncType::STg;
		rule->signal = RTLIL::SigSpec();
		current_process->syncs.push_back(rule);
	}
#line 1718 "rtlil_parser.tab.cc"
    break;

  case 75: /* $@13: %empty  */
#line 372 "yosys/frontends/rtlil/rtlil_parser.y"
                                        {
		RTLIL::SyncRule *rule = new RTLIL::SyncRule;
		rule->type = RTLIL::SyncType::STi;
		rule->signal = RTLIL::SigSpec();
		current_process->syncs.push_back(rule);
	}
#line 1729 "rtlil_parser.tab.cc"
    break;

  case 78: /* sync_type: TOK_LOW  */
#line 381 "yosys/frontends/rtlil/rtlil_parser.y"
                { (yyval.integer) = RTLIL::ST0; }
#line 1735 "rtlil_parser.tab.cc"
    break;

  case 79: /* sync_type: TOK_HIGH  */
#line 382 "yosys/frontends/rtlil/rtlil_parser.y"
                 { (yyval.integer) = RTLIL::ST1; }
#line 1741 "rtlil_parser.tab.cc"
    break;

  case 80: /* sync_type: TOK_POSEDGE  */
#line 383 "yosys/frontends/rtlil/rtlil_parser.y"
                    { (yyval.integer) = RTLIL::STp; }
#line 1747 "rtlil_parser.tab.cc"
    break;

  case 81: /* sync_type: TOK_NEGEDGE  */
#line 384 "yosys/frontends/rtlil/rtlil_parser.y"
                    { (yyval.integer) = RTLIL::STn; }
#line 1753 "rtlil_parser.tab.cc"
    break;

  case 82: /* sync_type: TOK_EDGE  */
#line 385 "yosys/frontends/rtlil/rtlil_parser.y"
                 { (yyval.integer) = RTLIL::STe; }
#line 1759 "rtlil_parser.tab.cc"
    break;

  case 83: /* update_list: update_list TOK_UPDATE sigspec sigspec EOL  */
#line 388 "yosys/frontends/rtlil/rtlil_parser.y"
                                                   {
		current_process->syncs.back()->actions.push_back(RTLIL::SigSig(*(yyvsp[-2].sigspec), *(yyvsp[-1].sigspec)));
		delete (yyvsp[-2].sigspec);
		delete (yyvsp[-1].sigspec);
	}
#line 1769 "rtlil_parser.tab.cc"
    break;

  case 84: /* update_list: update_list attr_list TOK_MEMWR TOK_ID sigspec sigspec sigspec constant EOL  */
#line 393 "yosys/frontends/rtlil/rtlil_parser.y"
                                                                                    {
		RTLIL::MemWriteAction act;
		act.attributes = attrbuf;
		act.memid = (yyvsp[-5].string);
		act.address = *(yyvsp[-4].sigspec);
		act.data = *(yyvsp[-3].sigspec);
		act.enable = *(yyvsp[-2].sigspec);
		act.priority_mask = *(yyvsp[-1].data);
		current_process->syncs.back()->mem_write_actions.push_back(std::move(act));
		attrbuf.clear();
		free((yyvsp[-5].string));
		delete (yyvsp[-4].sigspec);
		delete (yyvsp[-3].sigspec);
		delete (yyvsp[-2].sigspec);
		delete (yyvsp[-1].data);
	}
#line 1790 "rtlil_parser.tab.cc"
    break;

  case 86: /* constant: TOK_VALUE  */
#line 412 "yosys/frontends/rtlil/rtlil_parser.y"
                  {
		char *ep;
		int width = strtol((yyvsp[0].string), &ep, 10);
		std::list<RTLIL::State> bits;
		while (*(++ep) != 0) {
			RTLIL::State bit = RTLIL::Sx;
			switch (*ep) {
			case '0': bit = RTLIL::S0; break;
			case '1': bit = RTLIL::S1; break;
			case 'x': bit = RTLIL::Sx; break;
			case 'z': bit = RTLIL::Sz; break;
			case '-': bit = RTLIL::Sa; break;
			case 'm': bit = RTLIL::Sm; break;
			}
			bits.push_front(bit);
		}
		if (bits.size() == 0)
			bits.push_back(RTLIL::Sx);
		while ((int)bits.size() < width) {
			RTLIL::State bit = bits.back();
			if (bit == RTLIL::S1)
				bit = RTLIL::S0;
			bits.push_back(bit);
		}
		while ((int)bits.size() > width)
			bits.pop_back();
		(yyval.data) = new RTLIL::Const;
		for (auto it = bits.begin(); it != bits.end(); it++)
			(yyval.data)->bits.push_back(*it);
		free((yyvsp[0].string));
	}
#line 1826 "rtlil_parser.tab.cc"
    break;

  case 87: /* constant: TOK_INT  */
#line 443 "yosys/frontends/rtlil/rtlil_parser.y"
                {
		(yyval.data) = new RTLIL::Const((yyvsp[0].integer), 32);
	}
#line 1834 "rtlil_parser.tab.cc"
    break;

  case 88: /* constant: TOK_STRING  */
#line 446 "yosys/frontends/rtlil/rtlil_parser.y"
                   {
		(yyval.data) = new RTLIL::Const((yyvsp[0].string));
		free((yyvsp[0].string));
	}
#line 1843 "rtlil_parser.tab.cc"
    break;

  case 89: /* sigspec: constant  */
#line 452 "yosys/frontends/rtlil/rtlil_parser.y"
                 {
		(yyval.sigspec) = new RTLIL::SigSpec(*(yyvsp[0].data));
		delete (yyvsp[0].data);
	}
#line 1852 "rtlil_parser.tab.cc"
    break;

  case 90: /* sigspec: TOK_ID  */
#line 456 "yosys/frontends/rtlil/rtlil_parser.y"
               {
		if (current_module->wire((yyvsp[0].string)) == nullptr)
			rtlil_frontend_yyerror(stringf("RTLIL error: wire %s not found", (yyvsp[0].string)).c_str());
		(yyval.sigspec) = new RTLIL::SigSpec(current_module->wire((yyvsp[0].string)));
		free((yyvsp[0].string));
	}
#line 1863 "rtlil_parser.tab.cc"
    break;

  case 91: /* sigspec: sigspec '[' TOK_INT ']'  */
#line 462 "yosys/frontends/rtlil/rtlil_parser.y"
                                {
		if ((yyvsp[-1].integer) >= (yyvsp[-3].sigspec)->size() || (yyvsp[-1].integer) < 0)
			rtlil_frontend_yyerror("bit index out of range");
		(yyval.sigspec) = new RTLIL::SigSpec((yyvsp[-3].sigspec)->extract((yyvsp[-1].integer)));
		delete (yyvsp[-3].sigspec);
	}
#line 1874 "rtlil_parser.tab.cc"
    break;

  case 92: /* sigspec: sigspec '[' TOK_INT ':' TOK_INT ']'  */
#line 468 "yosys/frontends/rtlil/rtlil_parser.y"
                                            {
		if ((yyvsp[-3].integer) >= (yyvsp[-5].sigspec)->size() || (yyvsp[-3].integer) < 0 || (yyvsp[-3].integer) < (yyvsp[-1].integer))
			rtlil_frontend_yyerror("invalid slice");
		(yyval.sigspec) = new RTLIL::SigSpec((yyvsp[-5].sigspec)->extract((yyvsp[-1].integer), (yyvsp[-3].integer) - (yyvsp[-1].integer) + 1));
		delete (yyvsp[-5].sigspec);
	}
#line 1885 "rtlil_parser.tab.cc"
    break;

  case 93: /* sigspec: '{' sigspec_list '}'  */
#line 474 "yosys/frontends/rtlil/rtlil_parser.y"
                             {
		(yyval.sigspec) = (yyvsp[-1].sigspec);
	}
#line 1893 "rtlil_parser.tab.cc"
    break;

  case 94: /* sigspec_list_reversed: sigspec_list_reversed sigspec  */
#line 479 "yosys/frontends/rtlil/rtlil_parser.y"
                                      {
		(yyval.rsigspec)->push_back(*(yyvsp[0].sigspec));
		delete (yyvsp[0].sigspec);
	}
#line 1902 "rtlil_parser.tab.cc"
    break;

  case 95: /* sigspec_list_reversed: %empty  */
#line 483 "yosys/frontends/rtlil/rtlil_parser.y"
                    {
		(yyval.rsigspec) = new std::vector<RTLIL::SigSpec>;
	}
#line 1910 "rtlil_parser.tab.cc"
    break;

  case 96: /* sigspec_list: sigspec_list_reversed  */
#line 487 "yosys/frontends/rtlil/rtlil_parser.y"
                                    {
		(yyval.sigspec) = new RTLIL::SigSpec;
		for (auto it = (yyvsp[0].rsigspec)->rbegin(); it != (yyvsp[0].rsigspec)->rend(); it++)
			(yyval.sigspec)->append(*it);
		delete (yyvsp[0].rsigspec);
	}
#line 1921 "rtlil_parser.tab.cc"
    break;

  case 97: /* conn_stmt: TOK_CONNECT sigspec sigspec EOL  */
#line 495 "yosys/frontends/rtlil/rtlil_parser.y"
                                        {
		if (attrbuf.size() != 0)
			rtlil_frontend_yyerror("dangling attribute");
		current_module->connect(*(yyvsp[-2].sigspec), *(yyvsp[-1].sigspec));
		delete (yyvsp[-2].sigspec);
		delete (yyvsp[-1].sigspec);
	}
#line 1933 "rtlil_parser.tab.cc"
    break;


#line 1937 "rtlil_parser.tab.cc"

      default: break;
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
  yytoken = yychar == RTLIL_FRONTEND_YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= RTLIL_FRONTEND_YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == RTLIL_FRONTEND_YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = RTLIL_FRONTEND_YYEMPTY;
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


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


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
  if (yychar != RTLIL_FRONTEND_YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

