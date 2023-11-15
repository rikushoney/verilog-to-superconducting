#include "verilog.h"
#include "bigint.h"
#include "sha1.h"

#include <cmath>
#include <fstream>
#include <stack>
#include <unordered_set>
#include <utility>

using namespace yosys_mini;
using namespace VERILOG_FRONTEND;

using RTLIL::Const;
using RTLIL::State;
namespace ID = RTLIL::ID;

static std::list<std::string> output_code;
static std::list<std::string> input_buffer;
static size_t input_buffer_charp;
std::vector<char *> log_id_cache;

const char *yosys_mini::log_id(const RTLIL::IdString &str) {
  log_id_cache.push_back(strdup(str.c_str()));
  const char *p = log_id_cache.back();
  if (p[0] != '\\')
    return p;
  if (p[1] == '$' || p[1] == '\\' || p[1] == 0)
    return p;
  if (p[1] >= '0' && p[1] <= '9')
    return p;
  return p + 1;
}

static void return_char(char ch) {
  if (input_buffer_charp == 0)
    input_buffer.push_front(std::string() + ch);
  else
    input_buffer.front()[--input_buffer_charp] = ch;
}

static void insert_input(std::string str) {
  if (input_buffer_charp != 0) {
    input_buffer.front() = input_buffer.front().substr(input_buffer_charp);
    input_buffer_charp = 0;
  }
  input_buffer.push_front(str);
}

static char next_char() {
  if (input_buffer.empty())
    return 0;

  // log_assert(input_buffer_charp <= input_buffer.front().size());
  if (input_buffer_charp == input_buffer.front().size()) {
    input_buffer_charp = 0;
    input_buffer.pop_front();
    return next_char();
  }

  char ch = input_buffer.front()[input_buffer_charp++];
  return ch == '\r' ? next_char() : ch;
}

static std::string skip_spaces() {
  std::string spaces;
  while (1) {
    char ch = next_char();
    if (ch == 0)
      break;
    if (ch != ' ' && ch != '\t') {
      return_char(ch);
      break;
    }
    spaces += ch;
  }
  return spaces;
}

static std::string next_token(bool pass_newline = false) {
  std::string token;

  char ch = next_char();
  if (ch == 0)
    return token;

  token += ch;
  if (ch == '\n') {
    if (pass_newline) {
      output_code.push_back(token);
      return "";
    }
    return token;
  }

  if (ch == ' ' || ch == '\t') {
    while ((ch = next_char()) != 0) {
      if (ch != ' ' && ch != '\t') {
        return_char(ch);
        break;
      }
      token += ch;
    }
  } else if (ch == '"') {
    while ((ch = next_char()) != 0) {
      token += ch;
      if (ch == '"')
        break;
      if (ch == '\\') {
        if ((ch = next_char()) != 0)
          token += ch;
      }
    }
    if (token == "\"\"" && (ch = next_char()) != 0) {
      if (ch == '"')
        token += ch;
      else
        return_char(ch);
    }
  } else if (ch == '\\') {
    while ((ch = next_char()) != 0) {
      if (ch < 33 || ch > 126) {
        return_char(ch);
        break;
      }
      token += ch;
    }
  } else if (ch == '/') {
    if ((ch = next_char()) != 0) {
      if (ch == '/') {
        token += '*';
        char last_ch = 0;
        while ((ch = next_char()) != 0) {
          if (ch == '\n') {
            return_char(ch);
            break;
          }
          if (last_ch != '*' || ch != '/') {
            token += ch;
            last_ch = ch;
          }
        }
        token += " */";
      } else if (ch == '*') {
        token += '*';
        int newline_count = 0;
        char last_ch = 0;
        while ((ch = next_char()) != 0) {
          if (ch == '\n') {
            newline_count++;
            token += ' ';
          } else
            token += ch;
          if (last_ch == '*' && ch == '/')
            break;
          last_ch = ch;
        }
        while (newline_count-- > 0)
          return_char('\n');
      } else
        return_char(ch);
    }
  } else {
    const char *ok =
        "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ$0123456789";
    if (ch == '`' || strchr(ok, ch) != NULL) {
      char first = ch;
      ch = next_char();
      if (first == '`' && (ch == '"' || ch == '`')) {
        token += ch;
      } else
        do {
          if (strchr(ok, ch) == NULL) {
            return_char(ch);
            break;
          }
          token += ch;
        } while ((ch = next_char()) != 0);
    }
  }
  return token;
}

AST::AstNode *AST::dpi_call(const std::string &, const std::string &fname,
                            const std::vector<std::string> &,
                            const std::vector<AstNode *> &) {
  // log_error("Can't call DPI function `%s': this version of yosys is built
  // without plugin support\n", fname.c_str());
  exit(1);
}

struct macro_arg_t {
  macro_arg_t(const std::string &name_, const char *default_value_)
      : name(name_), has_default(default_value_ != nullptr),
        default_value(default_value_ ? default_value_ : "") {}

  std::string name;
  bool has_default;
  std::string default_value;
};

static bool all_white(const std::string &str) {
  for (char c : str)
    if (!isspace(c))
      return false;
  return true;
}

namespace yosys_mini {
struct arg_map_t {
  arg_map_t() {}

  void add_arg(const std::string &name, const char *default_value) {
    // if (find(name)) {
    // 	log_error("Duplicate macro arguments with name `%s'.\n", name.c_str());
    // }

    name_to_pos[name] = args.size();
    args.push_back(macro_arg_t(name, default_value));
  }

  // Find an argument by name; return nullptr if it doesn't exist. If pos is not
  // null, write the argument's position to it on success.
  const macro_arg_t *find(const std::string &name, int *pos = nullptr) const {
    auto it = name_to_pos.find(name);
    if (it == name_to_pos.end())
      return nullptr;

    if (pos)
      *pos = it->second;
    return &args[it->second];
  }

  // Construct the name for the local macro definition we use for the given
  // argument (something like macro_foobar_arg2). This doesn't include the
  // leading backtick.
  static std::string str_token(const std::string &macro_name, int pos) {
    return stringf("macro_%s_arg%d", macro_name.c_str(), pos);
  }

  // Return definitions for the macro arguments (so that substituting in the
  // macro body and then performing macro expansion will do argument
  // substitution properly).
  std::vector<std::pair<std::string, std::string>>
  get_vals(const std::string &macro_name,
           const std::vector<std::string> &arg_vals) const {
    std::vector<std::pair<std::string, std::string>> ret;
    for (int i = 0; i < GetSize(args); ++i) {
      // The SystemVerilog rules are:
      //
      //   - If the call site specifies an argument and it's not whitespace, use
      //     it.
      //
      //   - Otherwise, if the argument has a default value, use it.
      //
      //   - Otherwise, if the call site specified whitespace, use that.
      //
      //   - Otherwise, error.
      const std::string *dflt = nullptr;
      if (args[i].has_default)
        dflt = &args[i].default_value;

      const std::string *given = nullptr;
      if (i < GetSize(arg_vals))
        given = &arg_vals[i];

      const std::string *val = nullptr;
      if (given && (!(dflt && all_white(*given))))
        val = given;
      else if (dflt)
        val = dflt;
      else if (given)
        val = given;
      else {
        // log_error("Cannot expand macro `%s by giving only %d argument%s "
        //           "(argument %d has no default).\n",
        //           macro_name.c_str(), GetSize(arg_vals),
        //           (GetSize(arg_vals) == 1 ? "" : "s"), i + 1);
        exit(1);
      }

      // assert(val);
      ret.push_back(std::make_pair(str_token(macro_name, i), *val));
    }
    return ret;
  }

  std::vector<macro_arg_t> args;
  std::map<std::string, int> name_to_pos;
};

struct define_body_t {
  define_body_t(const std::string &body, const arg_map_t *args = nullptr)
      : body(body), has_args(args != nullptr),
        args(args ? *args : arg_map_t()) {}

  std::string body;
  bool has_args;
  arg_map_t args;
};

define_map_t::define_map_t() { add("YOSYS", "1"); }

// We must define this destructor here (rather than relying on the default),
// because we need to define it somewhere we've got a complete definition of
// define_body_t.
define_map_t::~define_map_t() {}

void define_map_t::add(const std::string &name, const std::string &txt,
                       const arg_map_t *args) {
  defines[name] = std::unique_ptr<define_body_t>(new define_body_t(txt, args));
}

void define_map_t::add(const std::string &name, const define_body_t &body) {
  defines[name] = std::unique_ptr<define_body_t>(new define_body_t(body));
}

void define_map_t::merge(const define_map_t &map) {
  for (const auto &pr : map.defines) {
    // These contortions are so that we take a copy of each definition body in
    // map.defines.
    defines[pr.first] =
        std::unique_ptr<define_body_t>(new define_body_t(*pr.second));
  }
}

const define_body_t *define_map_t::find(const std::string &name) const {
  auto it = defines.find(name);
  return (it == defines.end()) ? nullptr : it->second.get();
}

void define_map_t::erase(const std::string &name) { defines.erase(name); }

void define_map_t::clear() { defines.clear(); }

void define_map_t::log() const {
  for (auto &it : defines) {
    const std::string &name = it.first;
    const define_body_t &body = *it.second;
    // yosys_mini::log("`define %s%s %s\n", name.c_str(), body.has_args ? "()" :
    // "",
    //            body.body.c_str());
  }
}
} // namespace yosys_mini

static void input_file(std::istream &f, std::string filename) {
  char buffer[513];
  int rc;

  insert_input("");
  auto it = input_buffer.begin();

  input_buffer.insert(it, "`file_push \"" + filename + "\"\n");
  while ((rc = readsome(f, buffer, sizeof(buffer) - 1)) > 0) {
    buffer[rc] = 0;
    input_buffer.insert(it, buffer);
  }
  input_buffer.insert(it, "\n`file_pop\n");
}

// Read tokens to get one argument (either a macro argument at a callsite or a
// default argument in a macro definition). Writes the argument to dest. Returns
// true if we finished with ')' (the end of the argument list); false if we
// finished with ','.
static bool read_argument(std::string &dest) {
  skip_spaces();
  std::vector<char> openers;
  for (;;) {
    std::string tok = next_token(true);
    if (tok == ")") {
      if (openers.empty()) {
        while (dest.size() && (dest.back() == ' ' || dest.back() == '\t'))
          dest = dest.substr(0, dest.size() - 1);
        return true;
      }
      if (openers.back() != '(') {
        // log_error("Mismatched brackets in macro argument: %c and %c.\n",
        //           openers.back(), tok[0]);
        exit(1);
      }

      openers.pop_back();
      dest += tok;
      continue;
    }
    if (tok == "]") {
      char opener = openers.empty() ? '(' : openers.back();
      if (opener != '[') {
        // log_error("Mismatched brackets in macro argument: %c and %c.\n",
        // opener,
        //           tok[0]);
        exit(1);
      }

      openers.pop_back();
      dest += tok;
      continue;
    }
    if (tok == "}") {
      char opener = openers.empty() ? '(' : openers.back();
      if (opener != '{') {
        // log_error("Mismatched brackets in macro argument: %c and %c.\n",
        // opener,
        //           tok[0]);
        exit(1);
      }

      openers.pop_back();
      dest += tok;
      continue;
    }

    if (tok == "," && openers.empty()) {
      return false;
    }

    if (tok == "(" || tok == "[" || tok == "{")
      openers.push_back(tok[0]);

    dest += tok;
  }
}

using macro_arg_stack_t = std::stack<std::pair<std::string, define_body_t>>;

static void restore_macro_arg(define_map_t &defines,
                              macro_arg_stack_t &macro_arg_stack) {
  // log_assert(!macro_arg_stack.empty());
  auto &overwritten_arg = macro_arg_stack.top();
  defines.add(overwritten_arg.first, overwritten_arg.second);
  macro_arg_stack.pop();
}

static bool try_expand_macro(define_map_t &defines,
                             macro_arg_stack_t &macro_arg_stack,
                             std::string &tok) {
  if (tok == "`\"") {
    std::string literal("\"");
    // Expand string literal
    while (!input_buffer.empty()) {
      std::string ntok = next_token();
      if (ntok == "`\"") {
        insert_input(literal + "\"");
        return true;
      } else if (!try_expand_macro(defines, macro_arg_stack, ntok)) {
        literal += ntok;
      }
    }
    return false; // error - unmatched `"
  }

  if (tok == "``") {
    // Swallow `` in macro expansion
    return true;
  }

  if (tok.size() <= 1 || tok[0] != '`')
    return false;

  // This token looks like a macro name (`foo).
  std::string macro_name = tok.substr(1);
  const define_body_t *body = defines.find(tok.substr(1));

  if (!body) {
    // Apparently not a name we know.
    return false;
  }

  std::string name = tok.substr(1);
  std::string skipped_spaces = skip_spaces();
  tok = next_token(false);
  if (body->has_args) {
    if (tok != "(") {
      if (tok.size() == 1 && iscntrl(tok[0])) {
        char buf[5];
        snprintf(buf, sizeof(buf), "\\x%02x", tok[0]);
        tok = buf;
      }
      // log_error("Expected to find '(' to begin macro arguments for '%s', but
      // "
      //           "instead found '%s'\n",
      //           name.c_str(), tok.c_str());
      exit(1);
    }
    std::vector<std::string> args;
    bool done = false;
    while (!done) {
      std::string arg;
      done = read_argument(arg);
      args.push_back(arg);
    }
    for (const auto &pr : body->args.get_vals(name, args)) {
      if (const define_body_t *existing = defines.find(pr.first)) {
        macro_arg_stack.push({pr.first, *existing});
        insert_input("`__restore_macro_arg ");
      }
      defines.add(pr.first, pr.second);
    }
  } else {
    insert_input(tok);
    insert_input(skipped_spaces);
  }
  insert_input(body->body);
  return true;
}

// Read the arguments for a `define preprocessor directive with formal
// arguments. This is called just after reading the token containing "(".
// Returns the number of newlines to emit afterwards to keep line numbers in
// sync, together with the map from argument name to data (pos and default
// value).
static std::pair<int, arg_map_t> read_define_args() {
  // Each argument looks like one of the following:
  //
  //     identifier
  //     identifier = default_text
  //     identifier =
  //
  // The first example is an argument with no default value. The second is an
  // argument whose default value is default_text. The third is an argument with
  // default value the empty string.

  int newline_count = 0;
  arg_map_t args;

  // FSM state.
  //
  //   0: At start of identifier
  //   1: After identifier (stored in arg_name)
  //   2: After closing paren
  int state = 0;

  std::string arg_name, default_val;

  skip_spaces();
  for (;;) {
    if (state == 2)
      // We've read the closing paren.
      break;

    std::string tok = next_token();

    // Cope with escaped EOLs
    if (tok == "\\") {
      char ch = next_char();
      if (ch == '\n') {
        // Eat the \, the \n and any trailing space and keep going.
        skip_spaces();
        continue;
      } else {
        // There aren't any other situations where a backslash makes sense.
        // log_error("Backslash in macro arguments (not at end of line).\n");
        exit(1);
      }
    }

    switch (state) {
    case 0:
      // At start of argument. If the token is ')', we've presumably just seen
      // something like "`define foo() ...". Set state to 2 to finish.
      // Otherwise, the token should be a valid simple identifier, but we'll
      // allow anything here.
      if (tok == ")") {
        state = 2;
      } else {
        arg_name = tok;
        state = 1;
      }
      skip_spaces();
      break;

    case 1:
      // After argument. The token should either be an equals sign or a comma or
      // closing paren.
      if (tok == "=") {
        std::string default_val;
        // Read an argument into default_val and set state to 2 if we're at
        //  the end; 0 if we hit a comma.
        state = read_argument(default_val) ? 2 : 0;
        args.add_arg(arg_name, default_val.c_str());
        skip_spaces();
        break;
      }
      if (tok == ",") {
        // Take the identifier as an argument with no default value.
        args.add_arg(arg_name, nullptr);
        state = 0;
        skip_spaces();
        break;
      }
      if (tok == ")") {
        // As with comma, but set state to 2 (end of args)
        args.add_arg(arg_name, nullptr);
        state = 2;
        skip_spaces();
        break;
      }
      // log_error("Trailing contents after identifier in macro argument `%s': "
      //           "expected '=', ',' or ')'.\n",
      //           arg_name.c_str());
      exit(1);

    default:
      // The only FSM states are 0-2 and we dealt with 2 at the start of the
      // loop.
      // log_assert(false);
      exit(1);
    }
  }

  return std::make_pair(newline_count, args);
}

// Read a `define preprocessor directive. This is called just after reading the
// token containing
// "`define".
static void read_define(const std::string &filename, define_map_t &defines_map,
                        define_map_t &global_defines_cache) {
  std::string name, value;
  arg_map_t args;

  skip_spaces();
  name = next_token(true);

  bool here_doc_mode = false;
  int newline_count = 0;

  // The FSM state starts at 0. If it sees space (or enters here_doc_mode), it
  // assumes this is a macro without formal arguments and jumps to state 1.
  //
  // In state 0, if it sees an opening parenthesis, it assumes this is a macro
  // with formal arguments. It reads the arguments with read_define_args() and
  // then jumps to state 2.
  //
  // In states 1 or 2, the FSM reads tokens to the end of line (or end of
  // here_doc): this is the body of the macro definition.
  int state = 0;

  if (skip_spaces() != "")
    state = 1;

  for (;;) {
    std::string tok = next_token();
    if (tok.empty())
      break;

    // printf("define-tok: >>%s<<\n", tok != "\n" ? tok.c_str() : "NEWLINE");

    if (tok == "\"\"\"") {
      here_doc_mode = !here_doc_mode;
      continue;
    }

    if (state == 0 && tok == "(") {
      auto pr = read_define_args();
      newline_count += pr.first;
      args = pr.second;

      state = 2;
      continue;
    }

    // This token isn't an opening parenthesis immediately following the macro
    // name, so it's presumably at or after the start of the macro body. If
    // state isn't already 2 (which would mean we'd parsed an argument list),
    // set it to 1.
    if (state == 0) {
      state = 1;
    }

    if (tok == "\n") {
      if (here_doc_mode) {
        value += " ";
        newline_count++;
      } else {
        return_char('\n');
        break;
      }
      continue;
    }

    if (tok == "\\") {
      char ch = next_char();
      if (ch == '\n') {
        value += " ";
        newline_count++;
      } else {
        value += std::string("\\");
        return_char(ch);
      }
      continue;
    }

    // Is this token the name of a macro argument? If so, replace it with a
    // magic symbol that we'll replace with the argument value.
    int arg_pos;
    if (args.find(tok, &arg_pos)) {
      value += '`' + args.str_token(name, arg_pos);
      continue;
    }

    // This token is nothing special. Insert it verbatim into the macro body.
    value += tok;
  }

  // Append some newlines so that we don't mess up line counts in error
  // messages.
  while (newline_count-- > 0)
    return_char('\n');

  if (strchr("abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ$0123456789",
             name[0])) {
    // printf("define: >>%s<< -> >>%s<<\n", name.c_str(), value.c_str());
    defines_map.add(name, value, (state == 2) ? &args : nullptr);
    global_defines_cache.add(name, value, (state == 2) ? &args : nullptr);
  } else {
    // log_file_error(filename, 0, "Invalid name for macro definition:
    // >>%s<<.\n",
    //                name.c_str());
    exit(1);
  }
}

std::string
frontend_verilog_preproc(std::istream &f, std::string filename,
                         const define_map_t &pre_defines,
                         define_map_t &global_defines_cache,
                         const std::list<std::string> &include_dirs) {
  define_map_t defines;
  defines.merge(pre_defines);
  defines.merge(global_defines_cache);

  macro_arg_stack_t macro_arg_stack;
  std::vector<std::string> filename_stack;
  // We are inside pass_level levels of satisfied ifdefs, and then within
  // fail_level levels of unsatisfied ifdefs.  The unsatisfied ones are
  // always within satisfied ones — even if some condition within is true,
  // the parent condition failing renders it moot.
  int ifdef_fail_level = 0;
  int ifdef_pass_level = 0;
  // For the outermost unsatisfied ifdef, true iff that ifdef already
  // had a satisfied branch, and further elsif/else branches should be
  // considered unsatisfied even if the condition is true.
  // Meaningless if ifdef_fail_level == 0.
  bool ifdef_already_satisfied = false;

  output_code.clear();
  input_buffer.clear();
  input_buffer_charp = 0;

  input_file(f, filename);

  while (!input_buffer.empty()) {
    std::string tok = next_token();
    // printf("token: >>%s<<\n", tok != "\n" ? tok.c_str() : "NEWLINE");

    if (tok == "`endif") {
      if (ifdef_fail_level > 0)
        ifdef_fail_level--;
      else if (ifdef_pass_level > 0)
        ifdef_pass_level--;
      else {
        // log_error("Found %s outside of macro conditional branch!\n",
        //           tok.c_str());
        exit(1);
      }
      continue;
    }

    if (tok == "`else") {
      if (ifdef_fail_level == 0) {
        if (ifdef_pass_level == 0) {
          // log_error("Found %s outside of macro conditional branch!\n",
          //           tok.c_str());
          exit(1);
        }
        ifdef_pass_level--;
        ifdef_fail_level = 1;
        ifdef_already_satisfied = true;
      } else if (ifdef_fail_level == 1 && !ifdef_already_satisfied) {
        ifdef_fail_level = 0;
        ifdef_pass_level++;
        ifdef_already_satisfied = true;
      }
      continue;
    }

    if (tok == "`elsif") {
      skip_spaces();
      std::string name = next_token(true);
      if (ifdef_fail_level == 0) {
        if (ifdef_pass_level == 0) {
          // log_error("Found %s outside of macro conditional branch!\n",
          //           tok.c_str());
          exit(1);
        }
        ifdef_pass_level--;
        ifdef_fail_level = 1;
        ifdef_already_satisfied = true;
      } else if (ifdef_fail_level == 1 && !ifdef_already_satisfied &&
                 defines.find(name)) {
        ifdef_fail_level = 0;
        ifdef_pass_level++;
        ifdef_already_satisfied = true;
      }
      continue;
    }

    if (tok == "`ifdef") {
      skip_spaces();
      std::string name = next_token(true);
      if (ifdef_fail_level > 0 || !defines.find(name)) {
        ifdef_fail_level++;
      } else {
        ifdef_pass_level++;
        ifdef_already_satisfied = true;
      }
      if (ifdef_fail_level == 1)
        ifdef_already_satisfied = false;
      continue;
    }

    if (tok == "`ifndef") {
      skip_spaces();
      std::string name = next_token(true);
      if (ifdef_fail_level > 0 || defines.find(name)) {
        ifdef_fail_level++;
      } else {
        ifdef_pass_level++;
        ifdef_already_satisfied = true;
      }
      if (ifdef_fail_level == 1)
        ifdef_already_satisfied = false;
      continue;
    }

    if (ifdef_fail_level > 0) {
      if (tok == "\n")
        output_code.push_back(tok);
      continue;
    }

    if (tok == "`include") {
      skip_spaces();
      std::string fn = next_token(true);
      while (try_expand_macro(defines, macro_arg_stack, fn)) {
        fn = next_token();
      }
      while (1) {
        size_t pos = fn.find('"');
        if (pos == std::string::npos)
          break;
        if (pos == 0)
          fn = fn.substr(1);
        else
          fn = fn.substr(0, pos) + fn.substr(pos + 1);
      }
      std::ifstream ff;
      ff.clear();
      std::string fixed_fn = fn;
      ff.open(fixed_fn.c_str());

      bool filename_path_sep_found;
      bool fn_relative;
#ifdef _WIN32
      // Both forward and backslash are acceptable separators on Windows.
      filename_path_sep_found =
          (filename.find_first_of("/\\") != std::string::npos);
      // Easier just to invert the check for an absolute path (e.g. C:\ or C:/)
      fn_relative = !(fn[1] == ':' && (fn[2] == '/' || fn[2] == '\\'));
#else
      filename_path_sep_found = (filename.find('/') != std::string::npos);
      fn_relative = (fn[0] != '/');
#endif

      if (ff.fail() && fn.size() > 0 && fn_relative &&
          filename_path_sep_found) {
        // if the include file was not found, it is not given with an absolute
        // path, and the currently read file is given with a path, then try
        // again relative to its directory
        ff.clear();
#ifdef _WIN32
        fixed_fn = filename.substr(0, filename.find_last_of("/\\") + 1) + fn;
#else
        fixed_fn = filename.substr(0, filename.rfind('/') + 1) + fn;
#endif
        ff.open(fixed_fn);
      }
      if (ff.fail() && fn.size() > 0 && fn_relative) {
        // if the include file was not found and it is not given with an
        // absolute path, then search it in the include path
        for (auto incdir : include_dirs) {
          ff.clear();
          fixed_fn = incdir + '/' + fn;
          ff.open(fixed_fn);
          if (!ff.fail())
            break;
        }
      }
      if (ff.fail()) {
        output_code.push_back("`file_notfound " + fn);
      } else {
        input_file(ff, fixed_fn);
        yosys_input_files.insert(fixed_fn);
      }
      continue;
    }

    if (tok == "`file_push") {
      skip_spaces();
      std::string fn = next_token(true);
      if (!fn.empty() && fn.front() == '"' && fn.back() == '"')
        fn = fn.substr(1, fn.size() - 2);
      output_code.push_back(tok + " \"" + fn + "\"");
      filename_stack.push_back(filename);
      filename = fn;
      continue;
    }

    if (tok == "`file_pop") {
      output_code.push_back(tok);
      filename = filename_stack.back();
      filename_stack.pop_back();
      continue;
    }

    if (tok == "`define") {
      read_define(filename, defines, global_defines_cache);
      continue;
    }

    if (tok == "`undef") {
      std::string name;
      skip_spaces();
      name = next_token(true);
      // printf("undef: >>%s<<\n", name.c_str());
      defines.erase(name);
      global_defines_cache.erase(name);
      continue;
    }

    if (tok == "`timescale") {
      skip_spaces();
      while (!tok.empty() && tok != "\n")
        tok = next_token(true);
      if (tok == "\n")
        return_char('\n');
      continue;
    }

    if (tok == "`resetall") {
      default_nettype_wire = true;
      continue;
    }

    if (tok == "`undefineall" && sv_mode) {
      defines.clear();
      global_defines_cache.clear();
      continue;
    }

    if (tok == "`__restore_macro_arg") {
      restore_macro_arg(defines, macro_arg_stack);
      continue;
    }

    if (try_expand_macro(defines, macro_arg_stack, tok))
      continue;

    output_code.push_back(tok);
  }

  if (ifdef_fail_level > 0 || ifdef_pass_level > 0) {
    // log_error("Unterminated preprocessor conditional!\n");
    exit(1);
  }

  std::string output;
  for (auto &str : output_code)
    output += str;

  output_code.clear();
  input_buffer.clear();
  input_buffer_charp = 0;

  return output;
}
using namespace AST;
using namespace AST_INTERNAL;

// instantiate global variables (public API)
namespace yosys_mini::AST {

std::string current_filename;
void (*set_line_num)(int) = NULL;
int (*get_line_num)() = NULL;

} // namespace yosys_mini::AST

// instantiate global variables (private API)
namespace yosys_mini::AST_INTERNAL {

bool flag_dump_ast1, flag_dump_ast2, flag_no_dump_ptr, flag_dump_vlog1,
    flag_dump_vlog2, flag_dump_rtlil, flag_nolatches, flag_nomeminit;
bool flag_nomem2reg, flag_mem2reg, flag_noblackbox, flag_lib, flag_nowb,
    flag_noopt, flag_icells, flag_pwires, flag_autowire;
AstNode *current_ast, *current_ast_mod;
std::map<std::string, AstNode *> current_scope;
const dict<RTLIL::SigBit, RTLIL::SigBit> *genRTLIL_subst_ptr = NULL;
RTLIL::SigSpec ignoreThisSignalsInInitial;
AstNode *current_always, *current_top_block, *current_block,
    *current_block_child;
RTLIL::Module *current_module;
bool current_always_clocked;
dict<std::string, int> current_memwr_count;
dict<std::string, pool<int>> current_memwr_visible;

} // namespace yosys_mini::AST_INTERNAL

// convert node types to string
std::string AST::type2str(AstNodeType type) {
  switch (type) {
#define X(_item)                                                               \
  case _item:                                                                  \
    return #_item;
    X(AST_NONE)
    X(AST_DESIGN)
    X(AST_MODULE)
    X(AST_TASK)
    X(AST_FUNCTION)
    X(AST_DPI_FUNCTION)
    X(AST_WIRE)
    X(AST_MEMORY)
    X(AST_AUTOWIRE)
    X(AST_PARAMETER)
    X(AST_LOCALPARAM)
    X(AST_DEFPARAM)
    X(AST_PARASET)
    X(AST_ARGUMENT)
    X(AST_RANGE)
    X(AST_MULTIRANGE)
    X(AST_CONSTANT)
    X(AST_REALVALUE)
    X(AST_CELLTYPE)
    X(AST_IDENTIFIER)
    X(AST_PREFIX)
    X(AST_ASSERT)
    X(AST_ASSUME)
    X(AST_LIVE)
    X(AST_FAIR)
    X(AST_COVER)
    X(AST_ENUM)
    X(AST_ENUM_ITEM)
    X(AST_FCALL)
    X(AST_TO_BITS)
    X(AST_TO_SIGNED)
    X(AST_TO_UNSIGNED)
    X(AST_SELFSZ)
    X(AST_CAST_SIZE)
    X(AST_CONCAT)
    X(AST_REPLICATE)
    X(AST_BIT_NOT)
    X(AST_BIT_AND)
    X(AST_BIT_OR)
    X(AST_BIT_XOR)
    X(AST_BIT_XNOR)
    X(AST_REDUCE_AND)
    X(AST_REDUCE_OR)
    X(AST_REDUCE_XOR)
    X(AST_REDUCE_XNOR)
    X(AST_REDUCE_BOOL)
    X(AST_SHIFT_LEFT)
    X(AST_SHIFT_RIGHT)
    X(AST_SHIFT_SLEFT)
    X(AST_SHIFT_SRIGHT)
    X(AST_SHIFTX)
    X(AST_SHIFT)
    X(AST_LT)
    X(AST_LE)
    X(AST_EQ)
    X(AST_NE)
    X(AST_EQX)
    X(AST_NEX)
    X(AST_GE)
    X(AST_GT)
    X(AST_ADD)
    X(AST_SUB)
    X(AST_MUL)
    X(AST_DIV)
    X(AST_MOD)
    X(AST_POW)
    X(AST_POS)
    X(AST_NEG)
    X(AST_LOGIC_AND)
    X(AST_LOGIC_OR)
    X(AST_LOGIC_NOT)
    X(AST_TERNARY)
    X(AST_MEMRD)
    X(AST_MEMWR)
    X(AST_MEMINIT)
    X(AST_TCALL)
    X(AST_ASSIGN)
    X(AST_CELL)
    X(AST_PRIMITIVE)
    X(AST_CELLARRAY)
    X(AST_ALWAYS)
    X(AST_INITIAL)
    X(AST_BLOCK)
    X(AST_ASSIGN_EQ)
    X(AST_ASSIGN_LE)
    X(AST_CASE)
    X(AST_COND)
    X(AST_CONDX)
    X(AST_CONDZ)
    X(AST_DEFAULT)
    X(AST_FOR)
    X(AST_WHILE)
    X(AST_REPEAT)
    X(AST_GENVAR)
    X(AST_GENFOR)
    X(AST_GENIF)
    X(AST_GENCASE)
    X(AST_GENBLOCK)
    X(AST_TECALL)
    X(AST_POSEDGE)
    X(AST_NEGEDGE)
    X(AST_EDGE)
    X(AST_INTERFACE)
    X(AST_INTERFACEPORT)
    X(AST_INTERFACEPORTTYPE)
    X(AST_MODPORT)
    X(AST_MODPORTMEMBER)
    X(AST_PACKAGE)
    X(AST_WIRETYPE)
    X(AST_TYPEDEF)
    X(AST_STRUCT)
    X(AST_UNION)
    X(AST_STRUCT_ITEM)
    X(AST_BIND)
#undef X
  default:
    // log_abort();
    exit(1);
  }
}

// check if attribute exists and has non-zero value
bool AstNode::get_bool_attribute(RTLIL::IdString id) {
  if (attributes.count(id) == 0)
    return false;

  AstNode *attr = attributes.at(id);
  if (attr->type != AST_CONSTANT)
    attr->input_error("Attribute `%s' with non-constant value!\n", id.c_str());

  return attr->integer != 0;
}

// create new node (AstNode constructor)
// (the optional child arguments make it easier to create AST trees)
AstNode::AstNode(AstNodeType type, AstNode *child1, AstNode *child2,
                 AstNode *child3, AstNode *child4) {
  static unsigned int hashidx_count = 123456789;
  hashidx_count = mkhash_xorshift(hashidx_count);
  hashidx_ = hashidx_count;

  this->type = type;
  filename = current_filename;
  is_input = false;
  is_output = false;
  is_reg = false;
  is_logic = false;
  is_signed = false;
  is_string = false;
  is_enum = false;
  is_wand = false;
  is_wor = false;
  is_unsized = false;
  was_checked = false;
  range_valid = false;
  range_swapped = false;
  is_custom_type = false;
  port_id = 0;
  range_left = -1;
  range_right = 0;
  integer = 0;
  realvalue = 0;
  id2ast = NULL;
  basic_prep = false;
  lookahead = false;
  in_lvalue_from_above = false;
  in_param_from_above = false;
  in_lvalue = false;
  in_param = false;

  if (child1)
    children.push_back(child1);
  if (child2)
    children.push_back(child2);
  if (child3)
    children.push_back(child3);
  if (child4)
    children.push_back(child4);

  fixup_hierarchy_flags();
}

// create a (deep recursive) copy of a node
AstNode *AstNode::clone() const {
  AstNode *that = new AstNode;
  *that = *this;
  for (auto &it : that->children)
    it = it->clone();
  for (auto &it : that->attributes)
    it.second = it.second->clone();

  that->set_in_lvalue_flag(false);
  that->set_in_param_flag(false);
  that->fixup_hierarchy_flags(); // fixup to set flags on cloned children
  return that;
}

// create a (deep recursive) copy of a node use 'other' as target root node
void AstNode::cloneInto(AstNode *other) const {
  AstNode *tmp = clone();
  tmp->in_lvalue_from_above = other->in_lvalue_from_above;
  tmp->in_param_from_above = other->in_param_from_above;
  other->delete_children();
  *other = *tmp;
  tmp->children.clear();
  tmp->attributes.clear();
  other->fixup_hierarchy_flags();
  delete tmp;
}

// delete all children in this node
void AstNode::delete_children() {
  for (auto &it : children)
    delete it;
  children.clear();

  for (auto &it : attributes)
    delete it.second;
  attributes.clear();
}

// AstNode destructor
AstNode::~AstNode() { delete_children(); }

// create a nice text representation of the node
// (traverse tree by recursion, use 'other' pointer for diffing two AST trees)
void AstNode::dumpAst(FILE *f, std::string indent) const {
  if (f == NULL) {
    // for (auto f : log_files)
    //   dumpAst(f, indent);
    return;
  }

  std::string type_name = type2str(type);
  fprintf(f, "%s%s <%s>", indent.c_str(), type_name.c_str(),
          loc_string().c_str());

  if (!flag_no_dump_ptr) {
    if (id2ast)
      fprintf(f, " [%p -> %p]", this, id2ast);
    else
      fprintf(f, " [%p]", this);
  }

  if (!str.empty())
    fprintf(f, " str='%s'", str.c_str());
  if (!bits.empty()) {
    fprintf(f, " bits='");
    for (size_t i = bits.size(); i > 0; i--)
      fprintf(f, "%c",
              bits[i - 1] == State::S0   ? '0'
              : bits[i - 1] == State::S1 ? '1'
              : bits[i - 1] == RTLIL::Sx ? 'x'
              : bits[i - 1] == RTLIL::Sz ? 'z'
                                         : '?');
    fprintf(f, "'(%d)", GetSize(bits));
  }
  if (is_input)
    fprintf(f, " input");
  if (is_output)
    fprintf(f, " output");
  if (is_logic)
    fprintf(f, " logic");
  if (is_reg) // this is an AST dump, not Verilog - if we see "logic reg" that's
              // fine.
    fprintf(f, " reg");
  if (is_signed)
    fprintf(f, " signed");
  if (is_unsized)
    fprintf(f, " unsized");
  if (basic_prep)
    fprintf(f, " basic_prep");
  if (lookahead)
    fprintf(f, " lookahead");
  if (port_id > 0)
    fprintf(f, " port=%d", port_id);
  if (range_valid || range_left != -1 || range_right != 0)
    fprintf(f, " %srange=[%d:%d]%s", range_swapped ? "swapped_" : "",
            range_left, range_right, range_valid ? "" : "!");
  if (integer != 0)
    fprintf(f, " int=%u", (int)integer);
  if (realvalue != 0)
    fprintf(f, " real=%e", realvalue);
  if (!multirange_dimensions.empty()) {
    fprintf(f, " multirange=[");
    for (int v : multirange_dimensions)
      fprintf(f, " %d", v);
    fprintf(f, " ]");
  }
  if (!multirange_swapped.empty()) {
    fprintf(f, " multirange_swapped=[");
    for (bool v : multirange_swapped)
      fprintf(f, " %d", v);
    fprintf(f, " ]");
  }
  if (is_enum) {
    fprintf(f, " type=enum");
  }
  if (in_lvalue)
    fprintf(f, " in_lvalue");
  if (in_param)
    fprintf(f, " in_param");
  fprintf(f, "\n");

  for (auto &it : attributes) {
    fprintf(f, "%s  ATTR %s:\n", indent.c_str(), it.first.c_str());
    it.second->dumpAst(f, indent + "    ");
  }

  for (size_t i = 0; i < children.size(); i++)
    children[i]->dumpAst(f, indent + "  ");

  fflush(f);
}

// helper function for AstNode::dumpVlog()
static std::string id2vl(std::string txt) {
  if (txt.size() > 1 && txt[0] == '\\')
    txt = txt.substr(1);
  for (size_t i = 0; i < txt.size(); i++) {
    if ('A' <= txt[i] && txt[i] <= 'Z')
      continue;
    if ('a' <= txt[i] && txt[i] <= 'z')
      continue;
    if ('0' <= txt[i] && txt[i] <= '9')
      continue;
    if (txt[i] == '_')
      continue;
    txt = "\\" + txt + " ";
    break;
  }
  return txt;
}

// dump AST node as Verilog pseudo-code
void AstNode::dumpVlog(FILE *f, std::string indent) const {
  bool first = true;
  std::string txt;
  std::vector<AstNode *> rem_children1, rem_children2;

  if (f == NULL) {
    // for (auto f : log_files)
    //   dumpVlog(f, indent);
    return;
  }

  for (auto &it : attributes) {
    fprintf(f,
            "%s"
            "(* %s = ",
            indent.c_str(), id2vl(it.first.str()).c_str());
    it.second->dumpVlog(f, "");
    fprintf(f, " *)%s", indent.empty() ? "" : "\n");
  }

  switch (type) {
  case AST_MODULE:
    fprintf(f,
            "%s"
            "module %s(",
            indent.c_str(), id2vl(str).c_str());
    for (auto child : children)
      if (child->type == AST_WIRE && (child->is_input || child->is_output)) {
        fprintf(f, "%s%s", first ? "" : ", ", id2vl(child->str).c_str());
        first = false;
      }
    fprintf(f, ");\n");

    for (auto child : children)
      if (child->type == AST_PARAMETER || child->type == AST_LOCALPARAM ||
          child->type == AST_DEFPARAM)
        child->dumpVlog(f, indent + "  ");
      else
        rem_children1.push_back(child);

    for (auto child : rem_children1)
      if (child->type == AST_WIRE || child->type == AST_AUTOWIRE ||
          child->type == AST_MEMORY)
        child->dumpVlog(f, indent + "  ");
      else
        rem_children2.push_back(child);
    rem_children1.clear();

    for (auto child : rem_children2)
      if (child->type == AST_TASK || child->type == AST_FUNCTION)
        child->dumpVlog(f, indent + "  ");
      else
        rem_children1.push_back(child);
    rem_children2.clear();

    for (auto child : rem_children1)
      child->dumpVlog(f, indent + "  ");
    rem_children1.clear();

    fprintf(f,
            "%s"
            "endmodule\n",
            indent.c_str());
    break;

  case AST_WIRE:
    if (is_input && is_output)
      fprintf(f,
              "%s"
              "inout",
              indent.c_str());
    else if (is_input)
      fprintf(f,
              "%s"
              "input",
              indent.c_str());
    else if (is_output)
      fprintf(f,
              "%s"
              "output",
              indent.c_str());
    else if (!is_reg)
      fprintf(f,
              "%s"
              "wire",
              indent.c_str());
    if (is_reg)
      fprintf(f,
              "%s"
              "reg",
              (is_input || is_output) ? " " : indent.c_str());
    if (is_signed)
      fprintf(f, " signed");
    for (auto child : children) {
      fprintf(f, " ");
      child->dumpVlog(f, "");
    }
    fprintf(f, " %s", id2vl(str).c_str());
    fprintf(f, ";\n");
    break;

  case AST_MEMORY:
    fprintf(f,
            "%s"
            "memory",
            indent.c_str());
    if (is_signed)
      fprintf(f, " signed");
    for (auto child : children) {
      fprintf(f, " ");
      child->dumpVlog(f, "");
      if (first)
        fprintf(f, " %s", id2vl(str).c_str());
      first = false;
    }
    fprintf(f, ";\n");
    break;

  case AST_RANGE:
    if (range_valid) {
      if (range_swapped)
        fprintf(f, "[%d:%d]", range_right, range_left);
      else
        fprintf(f, "[%d:%d]", range_left, range_right);
    } else {
      for (auto child : children) {
        fprintf(f, "%c", first ? '[' : ':');
        child->dumpVlog(f, "");
        first = false;
      }
      fprintf(f, "]");
    }
    break;

  case AST_ALWAYS:
    fprintf(f,
            "%s"
            "always @",
            indent.c_str());
    for (auto child : children) {
      if (child->type != AST_POSEDGE && child->type != AST_NEGEDGE &&
          child->type != AST_EDGE)
        continue;
      fprintf(f, first ? "(" : ", ");
      child->dumpVlog(f, "");
      first = false;
    }
    fprintf(f, first ? "*\n" : ")\n");
    for (auto child : children) {
      if (child->type != AST_POSEDGE && child->type != AST_NEGEDGE &&
          child->type != AST_EDGE)
        child->dumpVlog(f, indent + "  ");
    }
    break;

  case AST_INITIAL:
    fprintf(f,
            "%s"
            "initial\n",
            indent.c_str());
    for (auto child : children) {
      if (child->type != AST_POSEDGE && child->type != AST_NEGEDGE &&
          child->type != AST_EDGE)
        child->dumpVlog(f, indent + "  ");
    }
    break;

  case AST_POSEDGE:
  case AST_NEGEDGE:
  case AST_EDGE:
    if (type == AST_POSEDGE)
      fprintf(f, "posedge ");
    if (type == AST_NEGEDGE)
      fprintf(f, "negedge ");
    for (auto child : children)
      child->dumpVlog(f, "");
    break;

  case AST_IDENTIFIER: {
    AST::AstNode *member_node = AST::get_struct_member(this);
    if (member_node)
      fprintf(f, "%s[%d:%d]", id2vl(str).c_str(), member_node->range_left,
              member_node->range_right);
    else
      fprintf(f, "%s", id2vl(str).c_str());
  }
    for (auto child : children)
      child->dumpVlog(f, "");
    break;

  case AST_CONSTANT:
    if (!str.empty())
      fprintf(f, "\"%s\"", str.c_str());
    else if (bits.size() == 32)
      fprintf(f, "%d", RTLIL::Const(bits).as_int());
    else
      fprintf(f, "%d'b %s", GetSize(bits),
              RTLIL::Const(bits).as_string().c_str());
    break;

  case AST_REALVALUE:
    fprintf(f, "%e", realvalue);
    break;

  case AST_BLOCK:
    if (children.size() == 1) {
      children[0]->dumpVlog(f, indent);
    } else {
      fprintf(f,
              "%s"
              "begin\n",
              indent.c_str());
      for (auto child : children)
        child->dumpVlog(f, indent + "  ");
      fprintf(f,
              "%s"
              "end\n",
              indent.c_str());
    }
    break;

  case AST_CASE:
    if (children.size() > 1 && children[1]->type == AST_CONDX)
      fprintf(f,
              "%s"
              "casex (",
              indent.c_str());
    else if (children.size() > 1 && children[1]->type == AST_CONDZ)
      fprintf(f,
              "%s"
              "casez (",
              indent.c_str());
    else
      fprintf(f,
              "%s"
              "case (",
              indent.c_str());
    children[0]->dumpVlog(f, "");
    fprintf(f, ")\n");
    for (size_t i = 1; i < children.size(); i++) {
      AstNode *child = children[i];
      child->dumpVlog(f, indent + "  ");
    }
    fprintf(f,
            "%s"
            "endcase\n",
            indent.c_str());
    break;

  case AST_COND:
  case AST_CONDX:
  case AST_CONDZ:
    for (auto child : children) {
      if (child->type == AST_BLOCK) {
        fprintf(f, ":\n");
        child->dumpVlog(f, indent + "  ");
        first = true;
      } else {
        fprintf(f, "%s", first ? indent.c_str() : ", ");
        if (child->type == AST_DEFAULT)
          fprintf(f, "default");
        else
          child->dumpVlog(f, "");
        first = false;
      }
    }
    break;

  case AST_ASSIGN:
    fprintf(f, "%sassign ", indent.c_str());
    children[0]->dumpVlog(f, "");
    fprintf(f, " = ");
    children[1]->dumpVlog(f, "");
    fprintf(f, ";\n");
    break;

  case AST_ASSIGN_EQ:
  case AST_ASSIGN_LE:
    fprintf(f, "%s", indent.c_str());
    children[0]->dumpVlog(f, "");
    fprintf(f, " %s ", type == AST_ASSIGN_EQ ? "=" : "<=");
    children[1]->dumpVlog(f, "");
    fprintf(f, ";\n");
    break;

  case AST_CONCAT:
    fprintf(f, "{");
    for (int i = GetSize(children) - 1; i >= 0; i--) {
      auto child = children[i];
      if (!first)
        fprintf(f, ", ");
      child->dumpVlog(f, "");
      first = false;
    }
    fprintf(f, "}");
    break;

  case AST_REPLICATE:
    fprintf(f, "{");
    children[0]->dumpVlog(f, "");
    fprintf(f, "{");
    children[1]->dumpVlog(f, "");
    fprintf(f, "}}");
    break;

    if (0) {
    case AST_BIT_NOT:
      txt = "~";
    }
    if (0) {
    case AST_REDUCE_AND:
      txt = "&";
    }
    if (0) {
    case AST_REDUCE_OR:
      txt = "|";
    }
    if (0) {
    case AST_REDUCE_XOR:
      txt = "^";
    }
    if (0) {
    case AST_REDUCE_XNOR:
      txt = "~^";
    }
    if (0) {
    case AST_REDUCE_BOOL:
      txt = "|";
    }
    if (0) {
    case AST_POS:
      txt = "+";
    }
    if (0) {
    case AST_NEG:
      txt = "-";
    }
    if (0) {
    case AST_LOGIC_NOT:
      txt = "!";
    }
    if (0) {
    case AST_SELFSZ:
      txt = "@selfsz@";
    }
    fprintf(f, "%s(", txt.c_str());
    children[0]->dumpVlog(f, "");
    fprintf(f, ")");
    break;

    if (0) {
    case AST_BIT_AND:
      txt = "&";
    }
    if (0) {
    case AST_BIT_OR:
      txt = "|";
    }
    if (0) {
    case AST_BIT_XOR:
      txt = "^";
    }
    if (0) {
    case AST_BIT_XNOR:
      txt = "~^";
    }
    if (0) {
    case AST_SHIFT_LEFT:
      txt = "<<";
    }
    if (0) {
    case AST_SHIFT_RIGHT:
      txt = ">>";
    }
    if (0) {
    case AST_SHIFT_SLEFT:
      txt = "<<<";
    }
    if (0) {
    case AST_SHIFT_SRIGHT:
      txt = ">>>";
    }
    if (0) {
    case AST_SHIFTX:
      txt = "@shiftx@";
    }
    if (0) {
    case AST_SHIFT:
      txt = "@shift@";
    }
    if (0) {
    case AST_LT:
      txt = "<";
    }
    if (0) {
    case AST_LE:
      txt = "<=";
    }
    if (0) {
    case AST_EQ:
      txt = "==";
    }
    if (0) {
    case AST_NE:
      txt = "!=";
    }
    if (0) {
    case AST_EQX:
      txt = "===";
    }
    if (0) {
    case AST_NEX:
      txt = "!==";
    }
    if (0) {
    case AST_GE:
      txt = ">=";
    }
    if (0) {
    case AST_GT:
      txt = ">";
    }
    if (0) {
    case AST_ADD:
      txt = "+";
    }
    if (0) {
    case AST_SUB:
      txt = "-";
    }
    if (0) {
    case AST_MUL:
      txt = "*";
    }
    if (0) {
    case AST_DIV:
      txt = "/";
    }
    if (0) {
    case AST_MOD:
      txt = "%";
    }
    if (0) {
    case AST_POW:
      txt = "**";
    }
    if (0) {
    case AST_LOGIC_AND:
      txt = "&&";
    }
    if (0) {
    case AST_LOGIC_OR:
      txt = "||";
    }
    fprintf(f, "(");
    children[0]->dumpVlog(f, "");
    fprintf(f, ")%s(", txt.c_str());
    children[1]->dumpVlog(f, "");
    fprintf(f, ")");
    break;

  case AST_TERNARY:
    fprintf(f, "(");
    children[0]->dumpVlog(f, "");
    fprintf(f, ") ? (");
    children[1]->dumpVlog(f, "");
    fprintf(f, ") : (");
    children[2]->dumpVlog(f, "");
    fprintf(f, ")");
    break;

  default:
    std::string type_name = type2str(type);
    fprintf(f,
            "%s"
            "/** %s **/%s",
            indent.c_str(), type_name.c_str(), indent.empty() ? "" : "\n");
    // dumpAst(f, indent, NULL);
  }

  fflush(f);
}

// check if two AST nodes are identical
bool AstNode::operator==(const AstNode &other) const {
  if (type != other.type)
    return false;
  if (children.size() != other.children.size())
    return false;
  if (str != other.str)
    return false;
  if (bits != other.bits)
    return false;
  if (is_input != other.is_input)
    return false;
  if (is_output != other.is_output)
    return false;
  if (is_logic != other.is_logic)
    return false;
  if (is_reg != other.is_reg)
    return false;
  if (is_signed != other.is_signed)
    return false;
  if (is_string != other.is_string)
    return false;
  if (range_valid != other.range_valid)
    return false;
  if (range_swapped != other.range_swapped)
    return false;
  if (port_id != other.port_id)
    return false;
  if (range_left != other.range_left)
    return false;
  if (range_right != other.range_right)
    return false;
  if (integer != other.integer)
    return false;
  for (size_t i = 0; i < children.size(); i++)
    if (*children[i] != *other.children[i])
      return false;
  return true;
}

// check if two AST nodes are not identical
bool AstNode::operator!=(const AstNode &other) const {
  return !(*this == other);
}

// check if this AST contains the given node
bool AstNode::contains(const AstNode *other) const {
  if (this == other)
    return true;
  for (auto child : children)
    if (child->contains(other))
      return true;
  return false;
}

// create an AST node for a constant (using a 32 bit int as value)
AstNode *AstNode::mkconst_int(uint32_t v, bool is_signed, int width) {
  AstNode *node = new AstNode(AST_CONSTANT);
  node->integer = v;
  node->is_signed = is_signed;
  for (int i = 0; i < width; i++) {
    node->bits.push_back((v & 1) ? State::S1 : State::S0);
    v = v >> 1;
  }
  node->range_valid = true;
  node->range_left = width - 1;
  node->range_right = 0;
  return node;
}

// create an AST node for a constant (using a bit vector as value)
AstNode *AstNode::mkconst_bits(const std::vector<RTLIL::State> &v,
                               bool is_signed, bool is_unsized) {
  AstNode *node = new AstNode(AST_CONSTANT);
  node->is_signed = is_signed;
  node->bits = v;
  for (size_t i = 0; i < 32; i++) {
    if (i < node->bits.size())
      node->integer |= (node->bits[i] == State::S1) << i;
    else if (is_signed && !node->bits.empty())
      node->integer |= (node->bits.back() == State::S1) << i;
  }
  node->range_valid = true;
  node->range_left = node->bits.size() - 1;
  node->range_right = 0;
  node->is_unsized = is_unsized;
  return node;
}

AstNode *AstNode::mkconst_bits(const std::vector<RTLIL::State> &v,
                               bool is_signed) {
  return mkconst_bits(v, is_signed, false);
}

// create an AST node for a constant (using a string in bit vector form as
// value)
AstNode *AstNode::mkconst_str(const std::vector<RTLIL::State> &v) {
  AstNode *node = mkconst_str(RTLIL::Const(v).decode_string());
  while (GetSize(node->bits) < GetSize(v))
    node->bits.push_back(RTLIL::State::S0);
  // log_assert(node->bits == v);
  return node;
}

// create an AST node for a constant (using a string as value)
AstNode *AstNode::mkconst_str(const std::string &str) {
  std::vector<RTLIL::State> data;
  data.reserve(str.size() * 8);
  for (size_t i = 0; i < str.size(); i++) {
    unsigned char ch = str[str.size() - i - 1];
    for (int j = 0; j < 8; j++) {
      data.push_back((ch & 1) ? State::S1 : State::S0);
      ch = ch >> 1;
    }
  }
  AstNode *node = AstNode::mkconst_bits(data, false);
  node->is_string = true;
  node->str = str;
  return node;
}

bool AstNode::bits_only_01() const {
  for (auto bit : bits)
    if (bit != State::S0 && bit != State::S1)
      return false;
  return true;
}

RTLIL::Const AstNode::bitsAsUnsizedConst(int width) {
  RTLIL::State extbit = bits.back();
  while (width > int(bits.size()))
    bits.push_back(extbit);
  return RTLIL::Const(bits);
}

RTLIL::Const AstNode::bitsAsConst(int width, bool is_signed) {
  std::vector<RTLIL::State> bits = this->bits;
  if (width >= 0 && width < int(bits.size()))
    bits.resize(width);
  if (width >= 0 && width > int(bits.size())) {
    RTLIL::State extbit = RTLIL::State::S0;
    if ((is_signed || is_unsized) && !bits.empty())
      extbit = bits.back();
    while (width > int(bits.size()))
      bits.push_back(extbit);
  }
  return RTLIL::Const(bits);
}

RTLIL::Const AstNode::bitsAsConst(int width) {
  return bitsAsConst(width, is_signed);
}

RTLIL::Const AstNode::asAttrConst() const {
  // log_assert(type == AST_CONSTANT);

  RTLIL::Const val;
  val.bits = bits;

  if (is_string) {
    val.flags |= RTLIL::CONST_FLAG_STRING;
    // log_assert(val.decode_string() == str);
  }

  return val;
}

RTLIL::Const AstNode::asParaConst() const {
  if (type == AST_REALVALUE) {
    AstNode *strnode = AstNode::mkconst_str(stringf("%f", realvalue));
    RTLIL::Const val = strnode->asAttrConst();
    val.flags |= RTLIL::CONST_FLAG_REAL;
    delete strnode;
    return val;
  }

  RTLIL::Const val = asAttrConst();
  if (is_signed)
    val.flags |= RTLIL::CONST_FLAG_SIGNED;
  return val;
}

bool AstNode::asBool() const {
  // log_assert(type == AST_CONSTANT);
  for (auto &bit : bits)
    if (bit == RTLIL::State::S1)
      return true;
  return false;
}

int AstNode::isConst() const {
  if (type == AST_CONSTANT)
    return 1;
  if (type == AST_REALVALUE)
    return 2;
  return 0;
}

uint64_t AstNode::asInt(bool is_signed) {
  if (type == AST_CONSTANT) {
    RTLIL::Const v = bitsAsConst(64, is_signed);
    uint64_t ret = 0;

    for (int i = 0; i < 64; i++)
      if (v.bits.at(i) == RTLIL::State::S1)
        ret |= uint64_t(1) << i;

    return ret;
  }

  if (type == AST_REALVALUE)
    return uint64_t(realvalue);

  // log_abort();
  exit(1);
}

double AstNode::asReal(bool is_signed) {
  if (type == AST_CONSTANT) {
    RTLIL::Const val(bits);

    bool is_negative =
        is_signed && !val.bits.empty() && val.bits.back() == RTLIL::State::S1;
    if (is_negative)
      val = const_neg(val, val, false, false, val.bits.size());

    double v = 0;
    for (size_t i = 0; i < val.bits.size(); i++)
      // IEEE Std 1800-2012 Par 6.12.2: Individual bits that are x or z in
      // the net or the variable shall be treated as zero upon conversion.
      if (val.bits.at(i) == RTLIL::State::S1)
        v += exp2(i);
    if (is_negative)
      v *= -1;

    return v;
  }

  if (type == AST_REALVALUE)
    return realvalue;

  // log_abort();
  exit(1);
}

RTLIL::Const AstNode::realAsConst(int width) {
  double v = std::round(realvalue);
  RTLIL::Const result;
#ifdef EMSCRIPTEN
  if (!isfinite(v)) {
#else
  if (!std::isfinite(v)) {
#endif
    result.bits = std::vector<RTLIL::State>(width, RTLIL::State::Sx);
  } else {
    bool is_negative = v < 0;
    if (is_negative)
      v *= -1;
    for (int i = 0; i < width; i++, v /= 2)
      result.bits.push_back((fmod(floor(v), 2) != 0) ? RTLIL::State::S1
                                                     : RTLIL::State::S0);
    if (is_negative)
      result = const_neg(result, result, false, false, result.bits.size());
  }
  return result;
}

std::string AstNode::loc_string() const {
  return stringf("%s:%d.%d-%d.%d", filename.c_str(), location.first_line,
                 location.first_column, location.last_line,
                 location.last_column);
}

void AST::set_src_attr(RTLIL::AttrObject *obj, const AstNode *ast) {
  obj->attributes[RTLIL::ID::src] = ast->loc_string();
}

static bool param_has_no_default(const AstNode *param) {
  const auto &children = param->children;
  // log_assert(param->type == AST_PARAMETER);
  // log_assert(children.size() <= 2);
  return children.empty() ||
         (children.size() == 1 && children[0]->type == AST_RANGE);
}

static RTLIL::Module *process_module(RTLIL::Design *design, AstNode *ast,
                                     bool defer, AstNode *original_ast = NULL,
                                     bool quiet = false) {
  namespace ID = RTLIL::ID;
  // log_assert(current_scope.empty());
  // log_assert(ast->type == AST_MODULE || ast->type == AST_INTERFACE);

  // if (defer)
  //   log("Storing AST representation for module `%s'.\n", ast->str.c_str());
  // else if (!quiet) {
  //   log("Generating RTLIL representation for module `%s'.\n",
  //   ast->str.c_str());
  // }

  AstModule *module = new AstModule;
  current_module = module;

  module->ast = NULL;
  module->name = ast->str;
  set_src_attr(module, ast);
  module->set_bool_attribute(RTLIL::ID::cells_not_processed);

  current_ast_mod = ast;
  AstNode *ast_before_simplify;
  if (original_ast != NULL)
    ast_before_simplify = original_ast;
  else
    ast_before_simplify = ast->clone();

  // if (flag_dump_ast1) {
  //   log("Dumping AST before simplification:\n");
  //   ast->dumpAst(NULL, "    ");
  //   log("--- END OF AST DUMP ---\n");
  // }
  // if (flag_dump_vlog1) {
  //   log("Dumping Verilog AST before simplification:\n");
  //   ast->dumpVlog(NULL, "    ");
  //   log("--- END OF AST DUMP ---\n");
  // }

  if (!defer) {
    for (const AstNode *node : ast->children)
      if (node->type == AST_PARAMETER && param_has_no_default(node))
        node->input_error("Parameter `%s' has no default value and has not "
                          "been overridden!\n",
                          node->str.c_str());

    bool blackbox_module = flag_lib;

    if (!blackbox_module && !flag_noblackbox) {
      blackbox_module = true;
      for (auto child : ast->children) {
        if (child->type == AST_WIRE && (child->is_input || child->is_output))
          continue;
        if (child->type == AST_PARAMETER || child->type == AST_LOCALPARAM)
          continue;
        if (child->type == AST_CELL && child->children.size() > 0 &&
            child->children[0]->type == AST_CELLTYPE &&
            (child->children[0]->str == "$specify2" ||
             child->children[0]->str == "$specify3" ||
             child->children[0]->str == "$specrule"))
          continue;
        blackbox_module = false;
        break;
      }
    }

    // simplify this module or interface using the current design as context
    // for lookup up ports and wires within cells
    set_simplify_design_context(design);
    while (ast->simplify(!flag_noopt, 0, -1, false)) {
    }
    set_simplify_design_context(nullptr);

    // if (flag_dump_ast2) {
    //   log("Dumping AST after simplification:\n");
    //   ast->dumpAst(NULL, "    ");
    //   log("--- END OF AST DUMP ---\n");
    // }

    // if (flag_dump_vlog2) {
    //   log("Dumping Verilog AST after simplification:\n");
    //   ast->dumpVlog(NULL, "    ");
    //   log("--- END OF AST DUMP ---\n");
    // }

    if (flag_nowb && ast->attributes.count(ID::whitebox)) {
      delete ast->attributes.at(ID::whitebox);
      ast->attributes.erase(ID::whitebox);
    }

    if (ast->attributes.count(ID::lib_whitebox)) {
      if (!flag_lib || flag_nowb) {
        delete ast->attributes.at(ID::lib_whitebox);
        ast->attributes.erase(ID::lib_whitebox);
      } else {
        if (ast->attributes.count(ID::whitebox)) {
          delete ast->attributes.at(ID::whitebox);
          ast->attributes.erase(ID::whitebox);
        }
        AstNode *n = ast->attributes.at(ID::lib_whitebox);
        ast->set_attribute(ID::whitebox, n);
        ast->attributes.erase(ID::lib_whitebox);
      }
    }

    if (!blackbox_module && ast->attributes.count(ID::blackbox)) {
      AstNode *n = ast->attributes.at(ID::blackbox);
      if (n->type != AST_CONSTANT)
        ast->input_error("Got blackbox attribute with non-constant value!\n");
      blackbox_module = n->asBool();
    }

    if (blackbox_module && ast->attributes.count(ID::whitebox)) {
      AstNode *n = ast->attributes.at(ID::whitebox);
      if (n->type != AST_CONSTANT)
        ast->input_error("Got whitebox attribute with non-constant value!\n");
      blackbox_module = !n->asBool();
    }

    if (ast->attributes.count(ID::noblackbox)) {
      if (blackbox_module) {
        AstNode *n = ast->attributes.at(ID::noblackbox);
        if (n->type != AST_CONSTANT)
          ast->input_error(
              "Got noblackbox attribute with non-constant value!\n");
        blackbox_module = !n->asBool();
      }
      delete ast->attributes.at(ID::noblackbox);
      ast->attributes.erase(ID::noblackbox);
    }

    if (blackbox_module) {
      if (ast->attributes.count(ID::whitebox)) {
        delete ast->attributes.at(ID::whitebox);
        ast->attributes.erase(ID::whitebox);
      }

      if (ast->attributes.count(ID::lib_whitebox)) {
        delete ast->attributes.at(ID::lib_whitebox);
        ast->attributes.erase(ID::lib_whitebox);
      }

      std::vector<AstNode *> new_children;
      for (auto child : ast->children) {
        if (child->type == AST_WIRE && (child->is_input || child->is_output)) {
          new_children.push_back(child);
        } else if (child->type == AST_PARAMETER) {
          new_children.push_back(child);
        } else if (child->type == AST_CELL && child->children.size() > 0 &&
                   child->children[0]->type == AST_CELLTYPE &&
                   (child->children[0]->str == "$specify2" ||
                    child->children[0]->str == "$specify3" ||
                    child->children[0]->str == "$specrule")) {
          new_children.push_back(child);
        } else {
          delete child;
        }
      }

      ast->children.swap(new_children);

      if (ast->attributes.count(ID::blackbox) == 0) {
        ast->set_attribute(ID::blackbox, AstNode::mkconst_int(1, false));
      }
    }

    ignoreThisSignalsInInitial = RTLIL::SigSpec();

    for (auto &attr : ast->attributes) {
      if (attr.second->type != AST_CONSTANT)
        ast->input_error("Attribute `%s' with non-constant value!\n",
                         attr.first.c_str());
      module->attributes[attr.first] = attr.second->asAttrConst();
    }
    for (size_t i = 0; i < ast->children.size(); i++) {
      AstNode *node = ast->children[i];
      if (node->type == AST_WIRE || node->type == AST_MEMORY)
        node->genRTLIL();
    }
    for (size_t i = 0; i < ast->children.size(); i++) {
      AstNode *node = ast->children[i];
      if (node->type != AST_WIRE && node->type != AST_MEMORY &&
          node->type != AST_INITIAL)
        node->genRTLIL();
    }

    ignoreThisSignalsInInitial.sort_and_unify();

    for (size_t i = 0; i < ast->children.size(); i++) {
      AstNode *node = ast->children[i];
      if (node->type == AST_INITIAL)
        node->genRTLIL();
    }

    ignoreThisSignalsInInitial = RTLIL::SigSpec();
    current_scope.clear();
  } else {
    for (auto &attr : ast->attributes) {
      if (attr.second->type != AST_CONSTANT)
        continue;
      module->attributes[attr.first] = attr.second->asAttrConst();
    }
    for (const AstNode *node : ast->children)
      if (node->type == AST_PARAMETER)
        current_module->avail_parameters(node->str);
  }

  if (ast->type == AST_INTERFACE)
    module->set_bool_attribute(ID::is_interface);
  module->ast = ast_before_simplify;
  module->nolatches = flag_nolatches;
  module->nomeminit = flag_nomeminit;
  module->nomem2reg = flag_nomem2reg;
  module->mem2reg = flag_mem2reg;
  module->noblackbox = flag_noblackbox;
  module->lib = flag_lib;
  module->nowb = flag_nowb;
  module->noopt = flag_noopt;
  module->icells = flag_icells;
  module->pwires = flag_pwires;
  module->autowire = flag_autowire;
  module->fixup_ports();

  // if (flag_dump_rtlil) {
  //   log("Dumping generated RTLIL:\n");
  //   log_module(module);
  //   log("--- END OF RTLIL DUMP ---\n");
  // }

  design->add(current_module);
  return current_module;
}

RTLIL::Module *AST_INTERNAL::process_and_replace_module(
    RTLIL::Design *design, RTLIL::Module *old_module, AstNode *new_ast,
    AstNode *original_ast) {
  namespace ID = RTLIL::ID;

  // The old module will be deleted. Rename and mark for deletion, using
  // a static counter to make sure we get a unique name.
  static unsigned counter;
  std::ostringstream new_name;
  new_name << old_module->name.str() << "_before_process_and_replace_module_"
           << counter;
  ++counter;

  design->rename(old_module, new_name.str());
  old_module->set_bool_attribute(ID::to_delete);

  // Check if the module was the top module. If it was, we need to remove
  // the top attribute and put it on the new module.
  bool is_top = false;
  if (old_module->get_bool_attribute(ID::initial_top)) {
    old_module->attributes.erase(ID::initial_top);
    is_top = true;
  }

  // Generate RTLIL from AST for the new module and add to the design:
  RTLIL::Module *new_module =
      process_module(design, new_ast, false, original_ast);

  if (is_top)
    new_module->set_bool_attribute(ID::top);

  return new_module;
}

// renames identifiers in tasks and functions within a package
static void rename_in_package_stmts(AstNode *pkg) {
  std::unordered_set<std::string> idents;
  for (AstNode *item : pkg->children)
    idents.insert(item->str);
  std::function<void(AstNode *)> rename = [&rename, &idents,
                                           pkg](AstNode *node) {
    for (AstNode *child : node->children) {
      if (idents.count(child->str))
        child->str = pkg->str + "::" + child->str.substr(1);
      rename(child);
    }
  };
  for (AstNode *item : pkg->children)
    if (item->type == AST_FUNCTION || item->type == AST_TASK)
      rename(item);
}

// create AstModule instances for all modules in the AST tree and add them to
// 'design'
void AST::process(RTLIL::Design *design, AstNode *ast, bool dump_ast1,
                  bool dump_ast2, bool no_dump_ptr, bool dump_vlog1,
                  bool dump_vlog2, bool dump_rtlil, bool nolatches,
                  bool nomeminit, bool nomem2reg, bool mem2reg, bool noblackbox,
                  bool lib, bool nowb, bool noopt, bool icells, bool pwires,
                  bool nooverwrite, bool overwrite, bool defer, bool autowire) {
  AST_INTERNAL::current_ast = ast;
  current_ast_mod = nullptr;
  flag_dump_ast1 = dump_ast1;
  flag_dump_ast2 = dump_ast2;
  flag_no_dump_ptr = no_dump_ptr;
  flag_dump_vlog1 = dump_vlog1;
  flag_dump_vlog2 = dump_vlog2;
  flag_dump_rtlil = dump_rtlil;
  flag_nolatches = nolatches;
  flag_nomeminit = nomeminit;
  flag_nomem2reg = nomem2reg;
  flag_mem2reg = mem2reg;
  flag_noblackbox = noblackbox;
  flag_lib = lib;
  flag_nowb = nowb;
  flag_noopt = noopt;
  flag_icells = icells;
  flag_pwires = pwires;
  flag_autowire = autowire;

  ast->fixup_hierarchy_flags(true);

  // log_assert(current_ast->type == AST_DESIGN);
  for (AstNode *child : AST_INTERNAL::current_ast->children) {
    if (child->type == AST_MODULE || child->type == AST_INTERFACE) {
      for (auto n : design->verilog_globals)
        child->children.push_back(n->clone());

      // append nodes from previous packages using package-qualified names
      for (auto &n : design->verilog_packages) {
        for (auto &o : n->children) {
          AstNode *cloned_node = o->clone();
          // log("cloned node %s\n", type2str(cloned_node->type).c_str());
          if (cloned_node->type == AST_ENUM) {
            for (auto &e : cloned_node->children) {
              // log_assert(e->type == AST_ENUM_ITEM);
              e->str = n->str + std::string("::") + e->str.substr(1);
            }
          } else {
            cloned_node->str =
                n->str + std::string("::") + cloned_node->str.substr(1);
          }
          child->children.push_back(cloned_node);
        }
      }

      if (flag_icells && child->str.compare(0, 2, "\\$") == 0)
        child->str = child->str.substr(1);

      bool defer_local = defer;
      if (!defer_local)
        for (const AstNode *node : child->children)
          if (node->type == AST_PARAMETER && param_has_no_default(node)) {
            // log("Deferring `%s' because it contains parameter(s) without "
            //     "defaults.\n",
            //     child->str.c_str());
            defer_local = true;
            break;
          }

      if (defer_local)
        child->str = "$abstract" + child->str;

      if (design->has(child->str)) {
        RTLIL::Module *existing_mod = design->module(child->str);
        if (!nooverwrite && !overwrite &&
            !existing_mod->get_blackbox_attribute()) {
          // log_file_error(child->filename, child->location.first_line,
          //                "Re-definition of module `%s'!\n",
          //                child->str.c_str());
          exit(1);
        } else if (nooverwrite) {
          // log("Ignoring re-definition of module `%s' at %s.\n",
          //     child->str.c_str(), child->loc_string().c_str());
          continue;
        } else {
          // log("Replacing existing%s module `%s' at %s.\n",
          //     existing_mod->get_bool_attribute(ID::blackbox) ? " blackbox" :
          //     "", child->str.c_str(), child->loc_string().c_str());
          design->remove(existing_mod);
        }
      }

      process_module(design, child, defer_local);
      current_ast_mod = nullptr;
    } else if (child->type == AST_PACKAGE) {
      // process enum/other declarations
      child->simplify(true, 1, -1, false);
      rename_in_package_stmts(child);
      design->verilog_packages.push_back(child->clone());
      current_scope.clear();
    } else if (child->type == AST_BIND) {
      // top-level bind construct
      for (RTLIL::Binding *binding : child->genBindings())
        design->add(binding);
    } else {
      // must be global definition
      if (child->type == AST_PARAMETER)
        child->type = AST_LOCALPARAM; // cannot be overridden
      design->verilog_globals.push_back(child->clone());
      current_scope.clear();
    }
  }
}

// AstModule destructor
AstModule::~AstModule() {
  if (ast != NULL)
    delete ast;
}

// An interface port with modport is specified like this:
//    <interface_name>.<modport_name>
// This function splits the interface_name from the modport_name, and fails if
// it is not a valid combination
std::pair<std::string, std::string>
AST::split_modport_from_type(std::string name_type) {
  std::string interface_type = "";
  std::string interface_modport = "";
  size_t ndots = std::count(name_type.begin(), name_type.end(), '.');
  // Separate the interface instance name from any modports:
  if (ndots == 0) { // Does not have modport
    interface_type = name_type;
  } else {
    std::stringstream name_type_stream(name_type);
    std::string segment;
    std::vector<std::string> seglist;
    while (std::getline(name_type_stream, segment, '.')) {
      seglist.push_back(segment);
    }
    if (ndots == 1) { // Has modport
      interface_type = seglist[0];
      interface_modport = seglist[1];
    } else { // Erroneous port type
      // log_error("More than two '.' in signal port type (%s)\n",
      //           name_type.c_str());
    }
  }
  return std::pair<std::string, std::string>(interface_type, interface_modport);
}

AstNode *AST::find_modport(AstNode *intf, std::string name) {
  for (auto &ch : intf->children)
    if (ch->type == AST_MODPORT)
      if (ch->str == name) // Modport found
        return ch;
  return NULL;
}

// Iterate over all wires in an interface and add them as wires in the AST
// module:
void AST::explode_interface_port(AstNode *module_ast, RTLIL::Module *intfmodule,
                                 std::string intfname, AstNode *modport) {
  for (auto w : intfmodule->wires()) {
    AstNode *wire = new AstNode(
        AST_WIRE,
        new AstNode(AST_RANGE, AstNode::mkconst_int(w->width - 1, true),
                    AstNode::mkconst_int(0, true)));
    std::string origname = log_id(w->name);
    std::string newname = intfname + "." + origname;
    wire->str = newname;
    if (modport != NULL) {
      bool found_in_modport = false;
      // Search for the current wire in the wire list for the current modport
      for (auto &ch : modport->children) {
        if (ch->type == AST_MODPORTMEMBER) {
          std::string compare_name = "\\" + origname;
          if (ch->str == compare_name) { // Found signal. The modport decides
                                         // whether it is input or output
            found_in_modport = true;
            wire->is_input = ch->is_input;
            wire->is_output = ch->is_output;
            break;
          }
        }
      }
      if (found_in_modport) {
        module_ast->children.push_back(wire);
      } else { // If not found in modport, do not create port
        delete wire;
      }
    } else { // If no modport, set inout
      wire->is_input = true;
      wire->is_output = true;
      module_ast->children.push_back(wire);
    }
  }
}

// AstModules may contain cells marked with ID::reprocess_after, which indicates
// that it should be reprocessed once the specified module has been elaborated.
bool AstModule::reprocess_if_necessary(RTLIL::Design *design) {
  for (const RTLIL::Cell *cell : cells()) {
    std::string modname = cell->get_string_attribute(ID::reprocess_after);
    if (modname.empty())
      continue;
    if (design->module(modname) || design->module("$abstract" + modname)) {
      // log("Reprocessing module %s because instantiated module %s has become "
      //     "available.\n",
      //     log_id(name), log_id(modname));
      loadconfig();
      process_and_replace_module(design, this, ast, NULL);
      return true;
    }
  }
  return false;
}

// When an interface instance is found in a module, the whole RTLIL for the
// module will be rederived again from AST. The interface members are copied
// into the AST module with the prefix of the interface.
void AstModule::expand_interfaces(
    RTLIL::Design *design,
    const dict<RTLIL::IdString, RTLIL::Module *> &local_interfaces) {
  loadconfig();

  AstNode *new_ast = ast->clone();
  for (auto &intf : local_interfaces) {
    std::string intfname = intf.first.str();
    RTLIL::Module *intfmodule = intf.second;
    for (auto w : intfmodule->wires()) {
      AstNode *wire = new AstNode(
          AST_WIRE,
          new AstNode(AST_RANGE, AstNode::mkconst_int(w->width - 1, true),
                      AstNode::mkconst_int(0, true)));
      std::string newname = log_id(w->name);
      newname = intfname + "." + newname;
      wire->str = newname;
      new_ast->children.push_back(wire);
    }
  }

  AstNode *ast_before_replacing_interface_ports = new_ast->clone();

  // Explode all interface ports. Note this will only have an effect on 'top
  // level' modules. Other sub-modules will have their interface ports
  // exploded via the derive(..) function
  for (size_t i = 0; i < new_ast->children.size(); i++) {
    AstNode *ch2 = new_ast->children[i];
    if (ch2->type == AST_INTERFACEPORT) { // Is an interface port
      std::string name_port = ch2->str;   // Name of the interface port
      if (ch2->children.size() > 0) {
        for (size_t j = 0; j < ch2->children.size(); j++) {
          AstNode *ch = ch2->children[j];
          if (ch->type ==
              AST_INTERFACEPORTTYPE) { // Found the AST node containing the type
                                       // of the interface
            std::pair<std::string, std::string> res =
                split_modport_from_type(ch->str);
            std::string interface_type = res.first;
            std::string interface_modport = res.second; // Is "", if no modport
            if (design->module(interface_type) != nullptr) {
              // Add a cell to the module corresponding to the interface port
              // such that it can further propagated down if needed:
              AstNode *celltype_for_intf = new AstNode(AST_CELLTYPE);
              celltype_for_intf->str = interface_type;
              AstNode *cell_for_intf = new AstNode(AST_CELL, celltype_for_intf);
              cell_for_intf->str = name_port + "_inst_from_top_dummy";
              new_ast->children.push_back(cell_for_intf);

              // Get all members of this non-overridden dummy interface
              // instance:
              RTLIL::Module *intfmodule = design->module(
                  interface_type); // All interfaces should at this point in
                                   // time (assuming reprocess_module is called
                                   // from the hierarchy pass) be present in
                                   // design->modules_
              AstModule *ast_module_of_interface = (AstModule *)intfmodule;
              std::string interface_modport_compare_str =
                  "\\" + interface_modport;
              AstNode *modport = find_modport(
                  ast_module_of_interface->ast,
                  interface_modport_compare_str); // modport == NULL if no
                                                  // modport
              // Iterate over all wires in the interface and add them to the
              // module:
              explode_interface_port(new_ast, intfmodule, name_port, modport);
            }
            break;
          }
        }
      }
    }
  }

  // Generate RTLIL from AST for the new module and add to the design,
  // renaming this module to move it out of the way.
  RTLIL::Module *new_module = process_and_replace_module(
      design, this, new_ast, ast_before_replacing_interface_ports);

  delete new_ast;

  // Set the attribute "interfaces_replaced_in_module" so that it does not
  // happen again.
  new_module->set_bool_attribute(ID::interfaces_replaced_in_module);
}

// create a new parametric module (when needed) and return the name of the
// generated module - WITH support for interfaces This method is used to explode
// the interface when the interface is a port of the module (not instantiated
// inside)
RTLIL::IdString
AstModule::derive(RTLIL::Design *design,
                  const dict<RTLIL::IdString, RTLIL::Const> &parameters,
                  const dict<RTLIL::IdString, RTLIL::Module *> &interfaces,
                  const dict<RTLIL::IdString, RTLIL::IdString> &modports,
                  bool /*mayfail*/) {
  AstNode *new_ast = NULL;
  std::string modname = derive_common(design, parameters, &new_ast);

  // Since interfaces themselves may be instantiated with different parameters,
  // "modname" must also take those into account, so that unique modules
  // are derived for any variant of interface connections:
  std::string interf_info = "";

  bool has_interfaces = false;
  for (auto &intf : interfaces) {
    interf_info += log_id(intf.second->name);
    has_interfaces = true;
  }

  std::string new_modname = modname;
  if (has_interfaces)
    new_modname += "$interfaces$" + interf_info;

  if (!design->has(new_modname)) {
    if (!new_ast) {
      auto mod = dynamic_cast<AstModule *>(design->module(modname));
      new_ast = mod->ast->clone();
    }
    modname = new_modname;
    new_ast->str = modname;

    // Iterate over all interfaces which are ports in this module:
    for (auto &intf : interfaces) {
      RTLIL::Module *intfmodule = intf.second;
      std::string intfname = intf.first.str();
      // Check if a modport applies for the interface port:
      AstNode *modport = NULL;
      if (modports.count(intfname) > 0) {
        std::string interface_modport = modports.at(intfname).str();
        AstModule *ast_module_of_interface = (AstModule *)intfmodule;
        AstNode *ast_node_of_interface = ast_module_of_interface->ast;
        modport = find_modport(ast_node_of_interface, interface_modport);
      }
      // Iterate over all wires in the interface and add them to the module:
      explode_interface_port(new_ast, intfmodule, intfname, modport);
    }

    process_module(design, new_ast, false);
    design->module(modname)->check();

    RTLIL::Module *mod = design->module(modname);

    // Now that the interfaces have been exploded, we can delete the dummy port
    // related to every interface.
    for (auto &intf : interfaces) {
      if (mod->wire(intf.first) != nullptr) {
        // Normally, removing wires would be batched together as it's an
        //   expensive operation, however, in this case doing so would mean
        //   that a cell with the same name cannot be created (below)...
        // Since we won't expect many interfaces to exist in a module,
        //   we can let this slide...
        pool<RTLIL::Wire *> to_remove;
        to_remove.insert(mod->wire(intf.first));
        mod->remove(to_remove);
        mod->fixup_ports();
        // We copy the cell of the interface to the sub-module such that it
        //   can further be found if it is propagated down to sub-sub-modules
        //   etc.
        RTLIL::Cell *new_subcell = mod->addCell(intf.first, intf.second->name);
        new_subcell->set_bool_attribute(ID::is_interface);
      } else {
        // log_error("No port with matching name found (%s) in %s. Stopping\n",
        //           log_id(intf.first), modname.c_str());
        exit(1);
      }
    }

    // If any interfaces were replaced, set the attribute
    // 'interfaces_replaced_in_module':
    if (interfaces.size() > 0) {
      mod->set_bool_attribute(ID::interfaces_replaced_in_module);
    }

  } else {
    modname = new_modname;
    // log("Found cached RTLIL representation for module `%s'.\n",
    //     modname.c_str());
  }

  delete new_ast;
  return modname;
}

// create a new parametric module (when needed) and return the name of the
// generated module - without support for interfaces
RTLIL::IdString
AstModule::derive(RTLIL::Design *design,
                  const dict<RTLIL::IdString, RTLIL::Const> &parameters,
                  bool /*mayfail*/) {
  bool quiet =
      lib || attributes.count(ID::blackbox) || attributes.count(ID::whitebox);

  AstNode *new_ast = NULL;
  std::string modname = derive_common(design, parameters, &new_ast, quiet);

  if (!design->has(modname) && new_ast) {
    new_ast->str = modname;
    process_module(design, new_ast, false, NULL, quiet);
    design->module(modname)->check();
  } else if (!quiet) {
    // log("Found cached RTLIL representation for module `%s'.\n",
    //     modname.c_str());
  }

  delete new_ast;
  return modname;
}

static std::string serialize_param_value(const RTLIL::Const &val) {
  std::string res;
  if (val.flags & RTLIL::ConstFlags::CONST_FLAG_STRING)
    res.push_back('t');
  if (val.flags & RTLIL::ConstFlags::CONST_FLAG_SIGNED)
    res.push_back('s');
  if (val.flags & RTLIL::ConstFlags::CONST_FLAG_REAL)
    res.push_back('r');
  res += stringf("%d", GetSize(val));
  res.push_back('\'');
  for (int i = GetSize(val) - 1; i >= 0; i--) {
    switch (val.bits[i]) {
    case RTLIL::State::S0:
      res.push_back('0');
      break;
    case RTLIL::State::S1:
      res.push_back('1');
      break;
    case RTLIL::State::Sx:
      res.push_back('x');
      break;
    case RTLIL::State::Sz:
      res.push_back('z');
      break;
    case RTLIL::State::Sa:
      res.push_back('?');
      break;
    case RTLIL::State::Sm:
      res.push_back('m');
      break;
    }
  }
  return res;
}

std::string AST::derived_module_name(
    std::string stripped_name,
    const std::vector<std::pair<RTLIL::IdString, RTLIL::Const>> &parameters) {
  std::string para_info;
  for (const auto &elem : parameters)
    para_info += stringf("%s=%s", elem.first.c_str(),
                         serialize_param_value(elem.second).c_str());

  if (para_info.size() > 60)
    return "$paramod$" + sha1(para_info) + stripped_name;
  else
    return "$paramod" + stripped_name + para_info;
}

// create a new parametric module (when needed) and return the name of the
// generated module
std::string
AstModule::derive_common(RTLIL::Design *design,
                         const dict<RTLIL::IdString, RTLIL::Const> &parameters,
                         AstNode **new_ast_out, bool quiet) {
  std::string stripped_name = name.str();
  (*new_ast_out) = nullptr;

  if (stripped_name.compare(0, 9, "$abstract") == 0)
    stripped_name = stripped_name.substr(9);

  int para_counter = 0;
  std::vector<std::pair<RTLIL::IdString, RTLIL::Const>> named_parameters;
  for (const auto child : ast->children) {
    if (child->type != AST_PARAMETER)
      continue;
    para_counter++;
    auto it = parameters.find(child->str);
    if (it != parameters.end()) {
      // if (!quiet)
      //   log("Parameter %s = %s\n", child->str.c_str(),
      //   log_signal(it->second));
      named_parameters.emplace_back(child->str, it->second);
      continue;
    }
    it = parameters.find(stringf("$%d", para_counter));
    if (it != parameters.end()) {
      // if (!quiet)
      //   log("Parameter %d (%s) = %s\n", para_counter, child->str.c_str(),
      //       log_signal(it->second));
      named_parameters.emplace_back(child->str, it->second);
      continue;
    }
  }

  std::string modname = stripped_name;
  if (parameters.size()) // not named_parameters to cover hierarchical defparams
    modname = derived_module_name(stripped_name, named_parameters);

  if (design->has(modname))
    return modname;

  // if (!quiet)
  //   log_header(design,
  //              "Executing AST frontend in derive mode using pre-parsed AST
  //              for " "module `%s'.\n", stripped_name.c_str());
  loadconfig();

  pool<RTLIL::IdString> rewritten;
  rewritten.reserve(GetSize(parameters));

  AstNode *new_ast = ast->clone();
  if (!new_ast->attributes.count(ID::hdlname))
    new_ast->set_attribute(ID::hdlname, AstNode::mkconst_str(stripped_name));

  para_counter = 0;
  for (auto child : new_ast->children) {
    if (child->type != AST_PARAMETER)
      continue;
    para_counter++;
    auto it = parameters.find(child->str);
    if (it != parameters.end()) {
      // if (!quiet)
      //   log("Parameter %s = %s\n", child->str.c_str(),
      //   log_signal(it->second));
      goto rewrite_parameter;
    }
    it = parameters.find(stringf("$%d", para_counter));
    if (it != parameters.end()) {
      // if (!quiet)
      //   log("Parameter %d (%s) = %s\n", para_counter, child->str.c_str(),
      //       log_signal(it->second));
      goto rewrite_parameter;
    }
    continue;
  rewrite_parameter:
    if (param_has_no_default(child))
      child->children.insert(child->children.begin(), nullptr);
    delete child->children.at(0);
    if ((it->second.flags & RTLIL::CONST_FLAG_REAL) != 0) {
      child->children[0] = new AstNode(AST_REALVALUE);
      child->children[0]->realvalue = std::stod(it->second.decode_string());
    } else if ((it->second.flags & RTLIL::CONST_FLAG_STRING) != 0)
      child->children[0] = AstNode::mkconst_str(it->second.decode_string());
    else
      child->children[0] = AstNode::mkconst_bits(
          it->second.bits, (it->second.flags & RTLIL::CONST_FLAG_SIGNED) != 0);
    rewritten.insert(it->first);
  }

  if (GetSize(rewritten) < GetSize(parameters))
    for (const auto &param : parameters) {
      if (rewritten.count(param.first))
        continue;
      AstNode *defparam =
          new AstNode(AST_DEFPARAM, new AstNode(AST_IDENTIFIER));
      defparam->children[0]->str = param.first.str();
      if ((param.second.flags & RTLIL::CONST_FLAG_STRING) != 0)
        defparam->children.push_back(
            AstNode::mkconst_str(param.second.decode_string()));
      else
        defparam->children.push_back(AstNode::mkconst_bits(
            param.second.bits,
            (param.second.flags & RTLIL::CONST_FLAG_SIGNED) != 0));
      new_ast->children.push_back(defparam);
    }

  new_ast->fixup_hierarchy_flags(true);
  (*new_ast_out) = new_ast;
  return modname;
}

RTLIL::Module *AstModule::clone() const {
  AstModule *new_mod = new AstModule;
  new_mod->name = name;
  cloneInto(new_mod);

  new_mod->ast = ast->clone();
  new_mod->nolatches = nolatches;
  new_mod->nomeminit = nomeminit;
  new_mod->nomem2reg = nomem2reg;
  new_mod->mem2reg = mem2reg;
  new_mod->noblackbox = noblackbox;
  new_mod->lib = lib;
  new_mod->nowb = nowb;
  new_mod->noopt = noopt;
  new_mod->icells = icells;
  new_mod->pwires = pwires;
  new_mod->autowire = autowire;

  return new_mod;
}

void AstModule::loadconfig() const {
  AST_INTERNAL::current_ast = NULL;
  flag_dump_ast1 = false;
  flag_dump_ast2 = false;
  flag_dump_vlog1 = false;
  flag_dump_vlog2 = false;
  flag_nolatches = nolatches;
  flag_nomeminit = nomeminit;
  flag_nomem2reg = nomem2reg;
  flag_mem2reg = mem2reg;
  flag_noblackbox = noblackbox;
  flag_lib = lib;
  flag_nowb = nowb;
  flag_noopt = noopt;
  flag_icells = icells;
  flag_pwires = pwires;
  flag_autowire = autowire;
}

void AstNode::input_error(const char *format, ...) const {
  va_list ap;
  va_start(ap, format);
  // logv_file_error(filename, location.first_line, format, ap);
  exit(1);
}

// divide an arbitrary length decimal number by two and return the rest
static int my_decimal_div_by_two(std::vector<uint8_t> &digits) {
  int carry = 0;
  for (size_t i = 0; i < digits.size(); i++) {
    if (digits[i] >= 10) {
      // log_file_error(current_filename, get_line_num(),
      //                "Invalid use of [a-fxz?] in decimal constant.\n");
      exit(1);
    }
    digits[i] += carry * 10;
    carry = digits[i] % 2;
    digits[i] /= 2;
  }
  while (!digits.empty() && !digits.front())
    digits.erase(digits.begin());
  return carry;
}

// find the number of significant bits in a binary number (not including the
// sign bit)
static int my_ilog2(int x) {
  int ret = 0;
  while (x != 0 && x != -1) {
    x = x >> 1;
    ret++;
  }
  return ret;
}

// parse a binary, decimal, hexadecimal or octal number with support for special
// bits ('x', 'z' and '?')
static void my_strtobin(std::vector<RTLIL::State> &data, const char *str,
                        int len_in_bits, int base, char case_type,
                        bool is_unsized) {
  // all digits in string (MSB at index 0)
  std::vector<uint8_t> digits;

  while (*str) {
    if ('0' <= *str && *str <= '9')
      digits.push_back(*str - '0');
    else if ('a' <= *str && *str <= 'f')
      digits.push_back(10 + *str - 'a');
    else if ('A' <= *str && *str <= 'F')
      digits.push_back(10 + *str - 'A');
    else if (*str == 'x' || *str == 'X')
      digits.push_back(0xf0);
    else if (*str == 'z' || *str == 'Z' || *str == '?')
      digits.push_back(0xf1);
    str++;
  }

  if (base == 10 && GetSize(digits) == 1 && digits.front() >= 0xf0)
    base = 2;

  data.clear();

  if (base == 10) {
    while (!digits.empty())
      data.push_back(my_decimal_div_by_two(digits) ? State::S1 : State::S0);
  } else {
    int bits_per_digit = my_ilog2(base - 1);
    for (auto it = digits.rbegin(), e = digits.rend(); it != e; it++) {
      if (*it > (base - 1) && *it < 0xf0) {
        // log_file_error(current_filename, get_line_num(),
        //                "Digit larger than %d used in in base-%d constant.\n",
        //                base - 1, base);
        exit(1);
      }
      for (int i = 0; i < bits_per_digit; i++) {
        int bitmask = 1 << i;
        if (*it == 0xf0)
          data.push_back(case_type == 'x' ? RTLIL::Sa : RTLIL::Sx);
        else if (*it == 0xf1)
          data.push_back(case_type == 'x' || case_type == 'z' ? RTLIL::Sa
                                                              : RTLIL::Sz);
        else
          data.push_back((*it & bitmask) ? State::S1 : State::S0);
      }
    }
  }

  int len = GetSize(data);
  RTLIL::State msb = data.empty() ? State::S0 : data.back();

  if (len_in_bits < 0) {
    if (len < 32)
      data.resize(32, msb == State::S0 || msb == State::S1 ? RTLIL::S0 : msb);
    return;
  }

  if (is_unsized && (len > len_in_bits)) {
    // log_file_error(
    //     current_filename, get_line_num(),
    //     "Unsized constant must have width of 1 bit, but have %d bits!\n",
    //     len);
    exit(1);
  }

  for (len = len - 1; len >= 0; len--)
    if (data[len] == State::S1)
      break;
  if (msb == State::S0 || msb == State::S1) {
    len += 1;
    data.resize(len_in_bits, State::S0);
  } else {
    len += 2;
    data.resize(len_in_bits, msb);
  }

  if (len_in_bits == 0) {
    // log_file_error(
    //     current_filename, get_line_num(),
    //     "Illegal integer constant size of zero (IEEE 1800-2012, 5.7).\n");
    exit(1);
  }

  if (len > len_in_bits) {
    // log_warning(
    //     "Literal has a width of %d bit, but value requires %d bit.
    //     (%s:%d)\n", len_in_bits, len, current_filename.c_str(),
    //     get_line_num());
  }
}

// convert the Verilog code for a constant to an AST node
AstNode *VERILOG_FRONTEND::const2ast(std::string code, char case_type,
                                     bool warn_z) {
  if (warn_z) {
    AstNode *ret = const2ast(code, case_type);
    if (ret != nullptr && std::find(ret->bits.begin(), ret->bits.end(),
                                    RTLIL::State::Sz) != ret->bits.end()) {
      // log_warning("Yosys has only limited support for tri-state logic at the
      // "
      //             "moment. (%s:%d)\n",
      //             current_filename.c_str(), get_line_num());
    }
    return ret;
  }

  const char *str = code.c_str();

  // Strings
  if (*str == '"') {
    int len = strlen(str) - 2;
    std::vector<RTLIL::State> data;
    data.reserve(len * 8);
    for (int i = 0; i < len; i++) {
      unsigned char ch = str[len - i];
      for (int j = 0; j < 8; j++) {
        data.push_back((ch & 1) ? State::S1 : State::S0);
        ch = ch >> 1;
      }
    }
    AstNode *ast = AstNode::mkconst_bits(data, false);
    ast->str = code;
    return ast;
  }

  for (size_t i = 0; i < code.size(); i++)
    if (code[i] == '_' || code[i] == ' ' || code[i] == '\t' ||
        code[i] == '\r' || code[i] == '\n')
      code.erase(code.begin() + (i--));
  str = code.c_str();

  char *endptr;
  long len_in_bits = strtol(str, &endptr, 10);

  // Simple base-10 integer
  if (*endptr == 0) {
    std::vector<RTLIL::State> data;
    my_strtobin(data, str, -1, 10, case_type, false);
    if (data.back() == State::S1)
      data.push_back(State::S0);
    return AstNode::mkconst_bits(data, true);
  }

  // unsized constant
  if (str == endptr)
    len_in_bits = -1;

  // The "<bits>'[sS]?[bodhBODH]<digits>" syntax
  if (*endptr == '\'') {
    std::vector<RTLIL::State> data;
    bool is_signed = false;
    bool is_unsized = len_in_bits < 0;
    if (*(endptr + 1) == 's' || *(endptr + 1) == 'S') {
      is_signed = true;
      endptr++;
    }
    switch (*(endptr + 1)) {
    case 'b':
    case 'B':
      my_strtobin(data, endptr + 2, len_in_bits, 2, case_type, is_unsized);
      break;
    case 'o':
    case 'O':
      my_strtobin(data, endptr + 2, len_in_bits, 8, case_type, is_unsized);
      break;
    case 'd':
    case 'D':
      my_strtobin(data, endptr + 2, len_in_bits, 10, case_type, is_unsized);
      break;
    case 'h':
    case 'H':
      my_strtobin(data, endptr + 2, len_in_bits, 16, case_type, is_unsized);
      break;
    default:
      char next_char = char(tolower(*(endptr + 1)));
      if (next_char == '0' || next_char == '1' || next_char == 'x' ||
          next_char == 'z') {
        is_unsized = true;
        my_strtobin(data, endptr + 1, 1, 2, case_type, is_unsized);
      } else {
        return NULL;
      }
    }
    if (len_in_bits < 0) {
      if (is_signed && data.back() == State::S1)
        data.push_back(State::S0);
    }
    return AstNode::mkconst_bits(data, is_signed, is_unsized);
  }

  return NULL;
}
using namespace AST;
using namespace AST_INTERNAL;

void AstNode::set_in_lvalue_flag(bool flag, bool no_descend) {
  if (flag != in_lvalue_from_above) {
    in_lvalue_from_above = flag;
    if (!no_descend)
      fixup_hierarchy_flags();
  }
}

void AstNode::set_in_param_flag(bool flag, bool no_descend) {
  if (flag != in_param_from_above) {
    in_param_from_above = flag;
    if (!no_descend)
      fixup_hierarchy_flags();
  }
}

void AstNode::fixup_hierarchy_flags(bool force_descend) {
  // With forced descend, we disable the implicit
  // descend from within the set_* functions, instead
  // we do an explicit descend at the end of this function

  in_param = in_param_from_above;

  switch (type) {
  case AST_PARAMETER:
  case AST_LOCALPARAM:
  case AST_DEFPARAM:
  case AST_PARASET:
  case AST_PREFIX:
    in_param = true;
    for (auto child : children)
      child->set_in_param_flag(true, force_descend);
    break;

  case AST_REPLICATE:
  case AST_WIRE:
  case AST_GENIF:
  case AST_GENCASE:
    for (auto child : children)
      child->set_in_param_flag(in_param, force_descend);
    if (children.size() >= 1)
      children[0]->set_in_param_flag(true, force_descend);
    break;

  case AST_GENFOR:
  case AST_FOR:
    for (auto child : children)
      child->set_in_param_flag(in_param, force_descend);
    if (children.size() >= 2)
      children[1]->set_in_param_flag(true, force_descend);
    break;

  default:
    in_param = in_param_from_above;
    for (auto child : children)
      child->set_in_param_flag(in_param, force_descend);
  }

  for (auto attr : attributes)
    attr.second->set_in_param_flag(true, force_descend);

  in_lvalue = in_lvalue_from_above;

  switch (type) {
  case AST_ASSIGN:
  case AST_ASSIGN_EQ:
  case AST_ASSIGN_LE:
    if (children.size() >= 1)
      children[0]->set_in_lvalue_flag(true, force_descend);
    if (children.size() >= 2)
      children[1]->set_in_lvalue_flag(in_lvalue, force_descend);
    break;

  default:
    for (auto child : children)
      child->set_in_lvalue_flag(in_lvalue, force_descend);
  }

  if (force_descend) {
    for (auto child : children)
      child->fixup_hierarchy_flags(true);
    for (auto attr : attributes)
      attr.second->fixup_hierarchy_flags(true);
  }
}

// Process a format string and arguments for $display, $write, $sprintf, etc

Fmt AstNode::processFormat(int stage, bool sformat_like, int default_base,
                           size_t first_arg_at) {
  std::vector<VerilogFmtArg> args;
  for (size_t index = first_arg_at; index < children.size(); index++) {
    AstNode *node_arg = children[index];
    while (node_arg->simplify(true, stage, -1, false)) {
    }

    VerilogFmtArg arg = {};
    arg.filename = filename;
    arg.first_line = location.first_line;
    if (node_arg->type == AST_CONSTANT && node_arg->is_string) {
      arg.type = VerilogFmtArg::STRING;
      arg.str = node_arg->bitsAsConst().decode_string();
      // and in case this will be used as an argument...
      arg.sig = node_arg->bitsAsConst();
      arg.signed_ = false;
    } else if (node_arg->type == AST_IDENTIFIER && node_arg->str == "$time") {
      arg.type = VerilogFmtArg::TIME;
    } else if (node_arg->type == AST_IDENTIFIER &&
               node_arg->str == "$realtime") {
      arg.type = VerilogFmtArg::TIME;
      arg.realtime = true;
    } else if (node_arg->type == AST_CONSTANT) {
      arg.type = VerilogFmtArg::INTEGER;
      arg.sig = node_arg->bitsAsConst();
      arg.signed_ = node_arg->is_signed;
    } else {
      // log_file_error(filename, location.first_line, "Failed to evaluate
      // system task `%s' with non-constant argument at position %zu.\n",
      // str.c_str(), index + 1);
      exit(1);
    }
    args.push_back(arg);
  }

  Fmt fmt = {};
  fmt.parse_verilog(args, sformat_like, default_base, /*task_name=*/str,
                    current_module->name);
  return fmt;
}

void AstNode::annotateTypedEnums(AstNode *template_node) {
  // check if enum
  if (template_node->attributes.count(ID::enum_type)) {
    // get reference to enum node:
    std::string enum_type =
        template_node->attributes[ID::enum_type]->str.c_str();
    //			log("enum_type=%s (count=%lu)\n", enum_type.c_str(),
    // current_scope.count(enum_type)); 			log("current
    // scope:\n");
    // for (auto &it : current_scope) 				log("  %s\n",
    // it.first.c_str());
    // log_assert(current_scope.count(enum_type) == 1);
    AstNode *enum_node = current_scope.at(enum_type);
    // log_assert(enum_node->type == AST_ENUM);
    while (enum_node->simplify(true, 1, -1, false)) {
    }
    // get width from 1st enum item:
    // log_assert(enum_node->children.size() >= 1);
    AstNode *enum_item0 = enum_node->children[0];
    // log_assert(enum_item0->type == AST_ENUM_ITEM);
    int width;
    if (!enum_item0->range_valid)
      width = 1;
    else if (enum_item0->range_swapped)
      width = enum_item0->range_right - enum_item0->range_left + 1;
    else
      width = enum_item0->range_left - enum_item0->range_right + 1;
    // log_assert(width > 0);
    // add declared enum items:
    for (auto enum_item : enum_node->children) {
      // log_assert(enum_item->type == AST_ENUM_ITEM);
      // get is_signed
      bool is_signed;
      if (enum_item->children.size() == 1) {
        is_signed = false;
      } else if (enum_item->children.size() == 2) {
        // log_assert(enum_item->children[1]->type == AST_RANGE);
        is_signed = enum_item->children[1]->is_signed;
      } else {
        // log_error("enum_item children size==%lu, expected 1 or 2 for %s
        // (%s)\n",
        //           enum_item->children.size(), enum_item->str.c_str(),
        //           enum_node->str.c_str());
        exit(1);
      }
      // start building attribute string
      std::string enum_item_str = "\\enum_value_";
      // get enum item value
      if (enum_item->children[0]->type != AST_CONSTANT) {
        // log_error("expected const, got %s for %s (%s)\n",
        //           type2str(enum_item->children[0]->type).c_str(),
        //           enum_item->str.c_str(), enum_node->str.c_str());
        exit(1);
      }
      RTLIL::Const val = enum_item->children[0]->bitsAsConst(width, is_signed);
      enum_item_str.append(val.as_string());
      // set attribute for available val to enum item name mappings
      set_attribute(enum_item_str.c_str(), mkconst_str(enum_item->str));
    }
  }
}

static bool name_has_dot(const std::string &name, std::string &struct_name) {
  // check if plausible struct member name \sss.mmm
  std::string::size_type pos;
  if (name.substr(0, 1) == "\\" &&
      (pos = name.find('.', 0)) != std::string::npos) {
    struct_name = name.substr(0, pos);
    return true;
  }
  return false;
}

static AstNode *make_range(int left, int right, bool is_signed = false) {
  // generate a pre-validated range node for a fixed signal range.
  auto range = new AstNode(AST_RANGE);
  range->range_left = left;
  range->range_right = right;
  range->range_valid = true;
  range->children.push_back(AstNode::mkconst_int(left, true));
  range->children.push_back(AstNode::mkconst_int(right, true));
  range->is_signed = is_signed;
  return range;
}

static int range_width(AstNode *node, AstNode *rnode) {
  // log_assert(rnode->type == AST_RANGE);
  if (!rnode->range_valid) {
    node->input_error(
        "Size must be constant in packed struct/union member %s\n",
        node->str.c_str());
  }
  // note: range swapping has already been checked for
  return rnode->range_left - rnode->range_right + 1;
}

[[noreturn]] static void struct_array_packing_error(AstNode *node) {
  node->input_error("Unpacked array in packed struct/union member %s\n",
                    node->str.c_str());
}

static void save_struct_range_dimensions(AstNode *node, AstNode *rnode) {
  node->multirange_dimensions.push_back(rnode->range_right);
  node->multirange_dimensions.push_back(range_width(node, rnode));
  node->multirange_swapped.push_back(rnode->range_swapped);
}

static int get_struct_range_offset(AstNode *node, int dimension) {
  return node->multirange_dimensions[2 * dimension];
}

static int get_struct_range_width(AstNode *node, int dimension) {
  return node->multirange_dimensions[2 * dimension + 1];
}

static int size_packed_struct(AstNode *snode, int base_offset) {
  // Struct members will be laid out in the structure contiguously from left to
  // right. Union members all have zero offset from the start of the union.
  // Determine total packed size and assign offsets.  Store these in the member
  // node.
  bool is_union = (snode->type == AST_UNION);
  int offset = 0;
  int packed_width = -1;
  // examine members from last to first
  for (auto it = snode->children.rbegin(); it != snode->children.rend(); ++it) {
    auto node = *it;
    int width;
    if (node->type == AST_STRUCT || node->type == AST_UNION) {
      // embedded struct or union
      width = size_packed_struct(node, base_offset + offset);
      // set range of struct
      node->range_right = base_offset + offset;
      node->range_left = base_offset + offset + width - 1;
      node->range_valid = true;
    } else {
      // log_assert(node->type == AST_STRUCT_ITEM);
      if (node->children.size() > 0 && node->children[0]->type == AST_RANGE) {
        // member width e.g. bit [7:0] a
        width = range_width(node, node->children[0]);
        if (node->children.size() == 2) {
          // Unpacked array. Note that this is a Yosys extension; only packed
          // data types and integer data types are allowed in packed structs /
          // unions in SystemVerilog.
          if (node->children[1]->type == AST_RANGE) {
            // Unpacked array, e.g. bit [63:0] a [0:3]
            auto rnode = node->children[1];
            if (rnode->children.size() == 1) {
              // C-style array size, e.g. bit [63:0] a [4]
              node->multirange_dimensions.push_back(0);
              node->multirange_dimensions.push_back(rnode->range_left);
              node->multirange_swapped.push_back(true);
              width *= rnode->range_left;
            } else {
              save_struct_range_dimensions(node, rnode);
              width *= range_width(node, rnode);
            }
            save_struct_range_dimensions(node, node->children[0]);
          } else {
            // The Yosys extension for unpacked arrays in packed structs /
            // unions only supports memories, i.e. e.g. logic [7:0] a [256] -
            // see above.
            struct_array_packing_error(node);
          }
        } else {
          // Vector
          save_struct_range_dimensions(node, node->children[0]);
        }
        // range nodes are now redundant
        for (AstNode *child : node->children)
          delete child;
        node->children.clear();
      } else if (node->children.size() > 0 &&
                 node->children[0]->type == AST_MULTIRANGE) {
        // Packed array, e.g. bit [3:0][63:0] a
        if (node->children.size() != 1) {
          // The Yosys extension for unpacked arrays in packed structs / unions
          // only supports memories, i.e. e.g. logic [7:0] a [256] - see above.
          struct_array_packing_error(node);
        }
        width = 1;
        for (auto rnode : node->children[0]->children) {
          save_struct_range_dimensions(node, rnode);
          width *= range_width(node, rnode);
        }
        // range nodes are now redundant
        for (AstNode *child : node->children)
          delete child;
        node->children.clear();
      } else if (node->range_left < 0) {
        // 1 bit signal: bit, logic or reg
        width = 1;
      } else {
        // already resolved and compacted
        width = node->range_left - node->range_right + 1;
      }
      if (is_union) {
        node->range_right = base_offset;
        node->range_left = base_offset + width - 1;
      } else {
        node->range_right = base_offset + offset;
        node->range_left = base_offset + offset + width - 1;
      }
      node->range_valid = true;
    }
    if (is_union) {
      // check that all members have the same size
      if (packed_width == -1) {
        // first member
        packed_width = width;
      } else {
        if (packed_width != width)
          node->input_error(
              "member %s of a packed union has %d bits, expecting %d\n",
              node->str.c_str(), width, packed_width);
      }
    } else {
      offset += width;
    }
  }
  return (is_union ? packed_width : offset);
}

[[noreturn]] static void struct_op_error(AstNode *node) {
  node->input_error("Unsupported operation for struct/union member %s\n",
                    node->str.c_str() + 1);
}

static AstNode *node_int(int ival) { return AstNode::mkconst_int(ival, true); }

static AstNode *multiply_by_const(AstNode *expr_node, int stride) {
  return new AstNode(AST_MUL, expr_node, node_int(stride));
}

static AstNode *normalize_struct_index(AstNode *expr, AstNode *member_node,
                                       int dimension) {
  expr = expr->clone();

  int offset = get_struct_range_offset(member_node, dimension);
  if (offset) {
    expr = new AstNode(AST_SUB, expr, node_int(offset));
  }

  if (member_node->multirange_swapped[dimension]) {
    // The dimension has swapped range; swap index into the struct accordingly.
    int msb = get_struct_range_width(member_node, dimension) - 1;
    expr = new AstNode(AST_SUB, node_int(msb), expr);
  }

  return expr;
}

static AstNode *struct_index_lsb_offset(AstNode *lsb_offset, AstNode *rnode,
                                        AstNode *member_node, int dimension,
                                        int &stride) {
  stride /= get_struct_range_width(member_node, dimension);
  auto right =
      normalize_struct_index(rnode->children.back(), member_node, dimension);
  auto offset = stride > 1 ? multiply_by_const(right, stride) : right;
  return lsb_offset ? new AstNode(AST_ADD, lsb_offset, offset) : offset;
}

static AstNode *struct_index_msb_offset(AstNode *lsb_offset, AstNode *rnode,
                                        AstNode *member_node, int dimension,
                                        int stride) {
  // log_assert(rnode->children.size() <= 2);

  // Offset to add to LSB
  AstNode *offset;
  if (rnode->children.size() == 1) {
    // Index, e.g. s.a[i]
    offset = node_int(stride - 1);
  } else {
    // rnode->children.size() == 2
    // Slice, e.g. s.a[i:j]
    auto left =
        normalize_struct_index(rnode->children[0], member_node, dimension);
    auto right =
        normalize_struct_index(rnode->children[1], member_node, dimension);
    offset = new AstNode(AST_SUB, left, right);
    if (stride > 1) {
      // offset = (msb - lsb + 1)*stride - 1
      auto slice_width = new AstNode(AST_ADD, offset, node_int(1));
      offset = new AstNode(AST_SUB, multiply_by_const(slice_width, stride),
                           node_int(1));
    }
  }

  return new AstNode(AST_ADD, lsb_offset, offset);
}

AstNode *AST::make_struct_member_range(AstNode *node, AstNode *member_node) {
  // Work out the range in the packed array that corresponds to a struct member
  // taking into account any range operations applicable to the current node
  // such as array indexing or slicing
  int range_left = member_node->range_left;
  int range_right = member_node->range_right;
  if (node->children.empty()) {
    // no range operations apply, return the whole width
    return make_range(range_left - range_right, 0);
  }

  if (node->children.size() != 1) {
    struct_op_error(node);
  }

  // Range operations
  auto rnode = node->children[0];
  AstNode *lsb_offset = NULL;
  int stride = range_left - range_right + 1;
  size_t i = 0;

  // Calculate LSB offset for the final index / slice
  if (rnode->type == AST_RANGE) {
    lsb_offset =
        struct_index_lsb_offset(lsb_offset, rnode, member_node, i, stride);
  } else if (rnode->type == AST_MULTIRANGE) {
    // Add offset for each dimension
    auto mrnode = rnode;
    for (i = 0; i < mrnode->children.size(); i++) {
      rnode = mrnode->children[i];
      lsb_offset =
          struct_index_lsb_offset(lsb_offset, rnode, member_node, i, stride);
    }
    i--; // Step back to the final index / slice
  } else {
    struct_op_error(node);
  }

  // Calculate MSB offset for the final index / slice
  auto msb_offset = struct_index_msb_offset(lsb_offset->clone(), rnode,
                                            member_node, i, stride);

  return new AstNode(AST_RANGE, msb_offset, lsb_offset);
}

AstNode *AST::get_struct_member(const AstNode *node) {
  AST::AstNode *member_node;
  if (node->attributes.count(ID::wiretype) &&
      (member_node = node->attributes.at(ID::wiretype)) &&
      (member_node->type == AST_STRUCT_ITEM ||
       member_node->type == AST_STRUCT || member_node->type == AST_UNION)) {
    return member_node;
  }
  return NULL;
}

static void add_members_to_scope(AstNode *snode, std::string name) {
  // add all the members in a struct or union to local scope
  // in case later referenced in assignments
  // log_assert(snode->type == AST_STRUCT || snode->type == AST_UNION);
  for (auto *node : snode->children) {
    auto member_name = name + "." + node->str;
    current_scope[member_name] = node;
    if (node->type != AST_STRUCT_ITEM) {
      // embedded struct or union
      add_members_to_scope(node, name + "." + node->str);
    }
  }
}

static int get_max_offset(AstNode *node) {
  // get the width from the MS member in the struct
  // as members are laid out from left to right in the packed wire
  // log_assert(node->type == AST_STRUCT || node->type == AST_UNION);
  while (node->type != AST_STRUCT_ITEM) {
    node = node->children[0];
  }
  return node->range_left;
}

static AstNode *make_packed_struct(AstNode *template_node, std::string &name,
                                   decltype(AstNode::attributes) &attributes) {
  // create a wire for the packed struct
  int offset = get_max_offset(template_node);
  auto wnode = new AstNode(AST_WIRE, make_range(offset, 0));
  wnode->str = name;
  wnode->is_logic = true;
  wnode->range_valid = true;
  wnode->is_signed = template_node->is_signed;
  for (auto &pair : attributes) {
    wnode->set_attribute(pair.first, pair.second->clone());
  }
  // make sure this node is the one in scope for this name
  current_scope[name] = wnode;
  // add all the struct members to scope under the wire's name
  add_members_to_scope(template_node, name);
  return wnode;
}

// check if a node or its children contains an assignment to the given variable
static bool node_contains_assignment_to(const AstNode *node,
                                        const AstNode *var) {
  if (node->type == AST_ASSIGN_EQ || node->type == AST_ASSIGN_LE) {
    // current node is iteslf an assignment
    // log_assert(node->children.size() >= 2);
    const AstNode *lhs = node->children[0];
    if (lhs->type == AST_IDENTIFIER && lhs->str == var->str)
      return false;
  }
  for (const AstNode *child : node->children) {
    // if this child shadows the given variable
    if (child != var && child->str == var->str && child->type == AST_WIRE)
      break; // skip the remainder of this block/scope
    // depth-first short circuit
    if (!node_contains_assignment_to(child, var))
      return false;
  }
  return true;
}

static std::string prefix_id(const std::string &prefix,
                             const std::string &str) {
  // log_assert(!prefix.empty() &&
  //            (prefix.front() == '$' || prefix.front() == '\\'));
  // log_assert(!str.empty() && (str.front() == '$' || str.front() == '\\'));
  // log_assert(prefix.back() == '.');
  if (str.front() == '\\')
    return prefix + str.substr(1);
  return prefix + str;
}

// direct access to this global should be limited to the following two functions
static const RTLIL::Design *simplify_design_context = nullptr;

void AST::set_simplify_design_context(const RTLIL::Design *design) {
  // log_assert(!simplify_design_context || !design);
  simplify_design_context = design;
}

// lookup the module with the given name in the current design context
static const RTLIL::Module *lookup_module(const std::string &name) {
  return simplify_design_context->module(name);
}

const RTLIL::Module *AstNode::lookup_cell_module() {
  // log_assert(type == AST_CELL);

  auto reprocess_after = [this](const std::string &modname) {
    if (!attributes.count(ID::reprocess_after))
      set_attribute(ID::reprocess_after, AstNode::mkconst_str(modname));
  };

  const AstNode *celltype = nullptr;
  for (const AstNode *child : children)
    if (child->type == AST_CELLTYPE) {
      celltype = child;
      break;
    }
  // log_assert(celltype != nullptr);

  const RTLIL::Module *module = lookup_module(celltype->str);
  if (!module)
    module = lookup_module("$abstract" + celltype->str);
  if (!module) {
    if (celltype->str.at(0) != '$')
      reprocess_after(celltype->str);
    return nullptr;
  }

  // build a mapping from true param name to param value
  size_t para_counter = 0;
  dict<RTLIL::IdString, RTLIL::Const> cell_params_map;
  for (AstNode *child : children) {
    if (child->type != AST_PARASET)
      continue;

    if (child->str.empty() && para_counter >= module->avail_parameters.size())
      return nullptr; // let hierarchy handle this error
    RTLIL::IdString paraname = child->str.empty()
                                   ? module->avail_parameters[para_counter++]
                                   : child->str;

    const AstNode *value = child->children[0];
    if (value->type != AST_REALVALUE && value->type != AST_CONSTANT)
      return nullptr; // let genrtlil handle this error
    cell_params_map[paraname] = value->asParaConst();
  }

  // put the parameters in order and generate the derived module name
  std::vector<std::pair<RTLIL::IdString, RTLIL::Const>> named_parameters;
  for (RTLIL::IdString param : module->avail_parameters) {
    auto it = cell_params_map.find(param);
    if (it != cell_params_map.end())
      named_parameters.emplace_back(it->first, it->second);
  }
  std::string modname = celltype->str;
  if (cell_params_map
          .size()) // not named_parameters to cover hierarchical defparams
    modname = derived_module_name(celltype->str, named_parameters);

  // try to find the resolved module
  module = lookup_module(modname);
  if (!module) {
    reprocess_after(modname);
    return nullptr;
  }
  return module;
}

// returns whether an expression contains an unbased unsized literal; does not
// check the literal exists in a self-determined context
static bool contains_unbased_unsized(const AstNode *node) {
  if (node->type == AST_CONSTANT)
    return node->is_unsized;
  for (const AstNode *child : node->children)
    if (contains_unbased_unsized(child))
      return true;
  return false;
}

// adds a wire to the current module with the given name that matches the
// dimensions of the given wire reference
void add_wire_for_ref(const RTLIL::Wire *ref, const std::string &str) {
  AstNode *left =
      AstNode::mkconst_int(ref->width - 1 + ref->start_offset, true);
  AstNode *right = AstNode::mkconst_int(ref->start_offset, true);
  if (ref->upto)
    std::swap(left, right);
  AstNode *range = new AstNode(AST_RANGE, left, right);

  AstNode *wire = new AstNode(AST_WIRE, range);
  wire->is_signed = ref->is_signed;
  wire->is_logic = true;
  wire->str = str;

  current_ast_mod->children.push_back(wire);
  current_scope[str] = wire;
}

enum class IdentUsage {
  NotReferenced, // target variable is neither read or written in the block
  Assigned,      // target variable is always assigned before use
  SyncRequired,  // target variable may be used before it has been assigned
};

// determines whether a local variable a block is always assigned before it is
// used, meaning the nosync attribute can automatically be added to that
// variable
static IdentUsage always_asgn_before_use(const AstNode *node,
                                         const std::string &target) {
  // This variable has been referenced before it has necessarily been assigned
  // a value in this procedure.
  if (node->type == AST_IDENTIFIER && node->str == target)
    return IdentUsage::SyncRequired;

  // For case statements (which are also used for if/else), we check each
  // possible branch. If the variable is assigned in all branches, then it is
  // assigned, and a sync isn't required. If it used before assignment in any
  // branch, then a sync is required.
  if (node->type == AST_CASE) {
    bool all_defined = true;
    bool any_used = false;
    bool has_default = false;
    for (const AstNode *child : node->children) {
      if (child->type == AST_COND && child->children.at(0)->type == AST_DEFAULT)
        has_default = true;
      IdentUsage nested = always_asgn_before_use(child, target);
      if (nested != IdentUsage::Assigned && child->type == AST_COND)
        all_defined = false;
      if (nested == IdentUsage::SyncRequired)
        any_used = true;
    }
    if (any_used)
      return IdentUsage::SyncRequired;
    else if (all_defined && has_default)
      return IdentUsage::Assigned;
    else
      return IdentUsage::NotReferenced;
  }

  // Check if this is an assignment to the target variable. For simplicity, we
  // don't analyze sub-ranges of the variable.
  if (node->type == AST_ASSIGN_EQ) {
    const AstNode *ident = node->children.at(0);
    if (ident->type == AST_IDENTIFIER && ident->str == target)
      return IdentUsage::Assigned;
  }

  for (const AstNode *child : node->children) {
    IdentUsage nested = always_asgn_before_use(child, target);
    if (nested != IdentUsage::NotReferenced)
      return nested;
  }
  return IdentUsage::NotReferenced;
}

AstNode *AstNode::clone_at_zero() {
  int width_hint;
  bool sign_hint;
  AstNode *pointee;

  switch (type) {
  case AST_IDENTIFIER:
    if (id2ast)
      pointee = id2ast;
    else if (current_scope.count(str))
      pointee = current_scope[str];
    else
      break;

    if (pointee->type != AST_WIRE && pointee->type != AST_AUTOWIRE &&
        pointee->type != AST_MEMORY)
      break;

    YS_FALLTHROUGH;
  case AST_MEMRD:
    detectSignWidth(width_hint, sign_hint);
    return mkconst_int(0, sign_hint, width_hint);

  default:
    break;
  }

  AstNode *that = new AstNode;
  *that = *this;
  for (auto &it : that->children)
    it = it->clone_at_zero();
  for (auto &it : that->attributes)
    it.second = it.second->clone();

  that->set_in_lvalue_flag(false);
  that->set_in_param_flag(false);
  that->fixup_hierarchy_flags();

  return that;
}

static bool try_determine_range_width(AstNode *range, int &result_width) {
  // log_assert(range->type == AST_RANGE);

  if (range->children.size() == 1) {
    result_width = 1;
    return true;
  }

  AstNode *left_at_zero_ast = range->children[0]->clone_at_zero();
  AstNode *right_at_zero_ast = range->children[1]->clone_at_zero();

  while (left_at_zero_ast->simplify(true, 1, -1, false)) {
  }
  while (right_at_zero_ast->simplify(true, 1, -1, false)) {
  }

  bool ok = false;
  if (left_at_zero_ast->type == AST_CONSTANT &&
      right_at_zero_ast->type == AST_CONSTANT) {
    ok = true;
    result_width =
        abs(int(left_at_zero_ast->integer - right_at_zero_ast->integer)) + 1;
  }

  delete left_at_zero_ast;
  delete right_at_zero_ast;
  return ok;
}

static const std::string auto_nosync_prefix = "\\AutoNosync";

// mark a local variable in an always_comb block for automatic nosync
// consideration
static void mark_auto_nosync(AstNode *block, const AstNode *wire) {
  // log_assert(block->type == AST_BLOCK);
  // log_assert(wire->type == AST_WIRE);
  block->set_attribute(auto_nosync_prefix + wire->str,
                       AstNode::mkconst_int(1, false));
}

// block names can be prefixed with an explicit scope during elaboration
static bool is_autonamed_block(const std::string &str) {
  size_t last_dot = str.rfind('.');
  // unprefixed names: autonamed if the first char is a dollar sign
  if (last_dot == std::string::npos)
    return str.at(0) == '$'; // e.g., `$fordecl_block$1`
  // prefixed names: autonamed if the final chunk begins with a dollar sign
  return str.rfind(".$") == last_dot; // e.g., `\foo.bar.$fordecl_block$1`
}

// check a procedural block for auto-nosync markings, remove them, and add
// nosync to local variables as necessary
static void check_auto_nosync(AstNode *node) {
  std::vector<RTLIL::IdString> attrs_to_drop;
  for (const auto &elem : node->attributes) {
    // skip attributes that don't begin with the prefix
    if (elem.first.compare(0, auto_nosync_prefix.size(),
                           auto_nosync_prefix.c_str()))
      continue;

    // delete and remove the attribute once we're done iterating
    attrs_to_drop.push_back(elem.first);

    // find the wire based on the attribute
    std::string wire_name = elem.first.substr(auto_nosync_prefix.size());
    auto it = current_scope.find(wire_name);
    if (it == current_scope.end())
      continue;

    // analyze the usage of the local variable in this block
    IdentUsage ident_usage = always_asgn_before_use(node, wire_name);
    if (ident_usage != IdentUsage::Assigned)
      continue;

    // mark the wire with `nosync`
    AstNode *wire = it->second;
    // log_assert(wire->type == AST_WIRE);
    wire->set_attribute(ID::nosync, AstNode::mkconst_int(1, false));
  }

  // remove the attributes we've "consumed"
  for (const RTLIL::IdString &str : attrs_to_drop) {
    auto it = node->attributes.find(str);
    delete it->second;
    node->attributes.erase(it);
  }

  // check local variables in any nested blocks
  for (AstNode *child : node->children)
    check_auto_nosync(child);
}

// convert the AST into a simpler AST that has all parameters substituted by
// their values, unrolled for-loops, expanded generate blocks, etc. when this
// function is done with an AST it can be converted into RTLIL using genRTLIL().
//
// this function also does all name resolving and sets the id2ast member of all
// nodes that link to a different node using names and lexical scoping.
bool AstNode::simplify(bool const_fold, int stage, int width_hint,
                       bool sign_hint) {
  static int recursion_counter = 0;
  static bool deep_recursion_warning = false;

  if (recursion_counter++ == 1000 && deep_recursion_warning) {
    // log_warning(
    //     "Deep recursion in AST simplifier.\nDoes this design contain overly "
    //     "long or deeply nested expressions, or excessive recursion?\n");
    deep_recursion_warning = false;
  }

  static bool unevaluated_tern_branch = false;

  AstNode *newNode = NULL;
  bool did_something = false;

#if 0
	log("-------------\n");
	log("AST simplify[%d] depth %d at %s:%d on %s %p:\n", stage, recursion_counter, filename.c_str(), location.first_line, type2str(type).c_str(), this);
	log("const_fold=%d, stage=%d, width_hint=%d, sign_hint=%d\n",
			int(const_fold), int(stage), int(width_hint), int(sign_hint));
	// dumpAst(NULL, "> ");
#endif

  if (stage == 0) {
    // log_assert(type == AST_MODULE || type == AST_INTERFACE);

    deep_recursion_warning = true;
    while (simplify(const_fold, 1, width_hint, sign_hint)) {
    }

    if (!flag_nomem2reg && !get_bool_attribute(ID::nomem2reg)) {
      dict<AstNode *, pool<std::string>> mem2reg_places;
      dict<AstNode *, uint32_t> mem2reg_candidates, dummy_proc_flags;
      uint32_t flags = flag_mem2reg ? AstNode::MEM2REG_FL_ALL : 0;
      mem2reg_as_needed_pass1(mem2reg_places, mem2reg_candidates,
                              dummy_proc_flags, flags);

      pool<AstNode *> mem2reg_set;
      for (auto &it : mem2reg_candidates) {
        AstNode *mem = it.first;
        uint32_t memflags = it.second;
        bool this_nomeminit = flag_nomeminit;
        // log_assert((memflags & ~0x00ffff00) == 0);

        if (mem->get_bool_attribute(ID::nomem2reg))
          continue;

        if (mem->get_bool_attribute(ID::nomeminit) ||
            get_bool_attribute(ID::nomeminit))
          this_nomeminit = true;

        if (memflags & AstNode::MEM2REG_FL_FORCED)
          goto silent_activate;

        if (memflags & AstNode::MEM2REG_FL_EQ2)
          goto verbose_activate;

        if (memflags & AstNode::MEM2REG_FL_SET_ASYNC)
          goto verbose_activate;

        if ((memflags & AstNode::MEM2REG_FL_SET_INIT) &&
            (memflags & AstNode::MEM2REG_FL_SET_ELSE) && this_nomeminit)
          goto verbose_activate;

        if (memflags & AstNode::MEM2REG_FL_CMPLX_LHS)
          goto verbose_activate;

        if ((memflags & AstNode::MEM2REG_FL_CONST_LHS) &&
            !(memflags & AstNode::MEM2REG_FL_VAR_LHS))
          goto verbose_activate;

        // log("Note: Not replacing memory %s with list of registers
        // (flags=0x%08lx).\n", mem->str.c_str(), long(memflags));
        continue;

      verbose_activate:
        if (mem2reg_set.count(mem) == 0) {
          std::string message = stringf(
              "Replacing memory %s with list of registers.", mem->str.c_str());
          bool first_element = true;
          for (auto &place : mem2reg_places[it.first]) {
            message +=
                stringf("%s%s", first_element ? " See " : ", ", place.c_str());
            first_element = false;
          }
          // log_warning("%s\n", message.c_str());
        }

      silent_activate:
        // log("Note: Replacing memory %s with list of registers
        // (flags=0x%08lx).\n", mem->str.c_str(), long(memflags));
        mem2reg_set.insert(mem);
      }

      for (auto node : mem2reg_set) {
        int mem_width, mem_size, addr_bits;
        node->meminfo(mem_width, mem_size, addr_bits);

        int data_range_left = node->children[0]->range_left;
        int data_range_right = node->children[0]->range_right;

        if (node->children[0]->range_swapped)
          std::swap(data_range_left, data_range_right);

        for (int i = 0; i < mem_size; i++) {
          AstNode *reg = new AstNode(
              AST_WIRE,
              new AstNode(AST_RANGE, mkconst_int(data_range_left, true),
                          mkconst_int(data_range_right, true)));
          reg->str = stringf("%s[%d]", node->str.c_str(), i);
          reg->is_reg = true;
          reg->is_signed = node->is_signed;
          for (auto &it : node->attributes)
            if (it.first != ID::mem2reg)
              reg->set_attribute(it.first, it.second->clone());
          reg->filename = node->filename;
          reg->location = node->location;
          children.push_back(reg);
          while (reg->simplify(true, 1, -1, false)) {
          }
        }
      }

      AstNode *async_block = NULL;
      while (mem2reg_as_needed_pass2(mem2reg_set, this, NULL, async_block)) {
      }

      std::vector<AstNode *> delnodes;
      mem2reg_remove(mem2reg_set, delnodes);

      for (auto node : delnodes)
        delete node;
    }

    while (simplify(const_fold, 2, width_hint, sign_hint)) {
    }
    recursion_counter--;
    return false;
  }

  current_filename = filename;

  // we do not look inside a task or function
  // (but as soon as a task or function is instantiated we process the generated
  // AST as usual)
  if (type == AST_FUNCTION || type == AST_TASK) {
    recursion_counter--;
    return false;
  }

  // deactivate all calls to non-synthesis system tasks
  // note that $display, $finish, and $stop are used for synthesis-time DRC so
  // they're not in this list
  if ((type == AST_FCALL || type == AST_TCALL) &&
      (str == "$strobe" || str == "$monitor" || str == "$time" ||
       str == "$dumpfile" || str == "$dumpvars" || str == "$dumpon" ||
       str == "$dumpoff" || str == "$dumpall")) {
    // log_file_warning(filename, location.first_line,
    //                  "Ignoring call to system %s %s.\n",
    //                  type == AST_FCALL ? "function" : "task", str.c_str());
    delete_children();
    str = std::string();
  }

  if ((type == AST_TCALL) &&
      (str == "$display" || str == "$displayb" || str == "$displayh" ||
       str == "$displayo" || str == "$write" || str == "$writeb" ||
       str == "$writeh" || str == "$writeo")) {
    if (!current_always) {
      // log_file_warning(
      //     filename, location.first_line,
      //     "System task `%s' outside initial or always block is
      //     unsupported.\n", str.c_str());
    } else if (current_always->type == AST_INITIAL) {
      int default_base = 10;
      if (str.back() == 'b')
        default_base = 2;
      else if (str.back() == 'o')
        default_base = 8;
      else if (str.back() == 'h')
        default_base = 16;

      // when $display()/$write() functions are used in an initial block, print
      // them during synthesis
      Fmt fmt = processFormat(stage, /*sformat_like=*/false, default_base);
      if (str.substr(0, 8) == "$display")
        fmt.append_string("\n");
      // log("%s", fmt.render().c_str());
    } else {
      // when $display()/$write() functions are used in an always block,
      // simplify the expressions and convert them to a special cell later in
      // genrtlil
      for (auto node : children)
        while (node->simplify(true, stage, -1, false)) {
        }
      return false;
    }

    delete_children();
    str = std::string();
  }

  // activate const folding if this is anything that must be evaluated
  // statically (ranges, parameters, attributes, etc.)
  if (type == AST_WIRE || type == AST_PARAMETER || type == AST_LOCALPARAM ||
      type == AST_ENUM_ITEM || type == AST_DEFPARAM || type == AST_PARASET ||
      type == AST_RANGE || type == AST_PREFIX || type == AST_TYPEDEF)
    const_fold = true;
  if (type == AST_IDENTIFIER && current_scope.count(str) > 0 &&
      (current_scope[str]->type == AST_PARAMETER ||
       current_scope[str]->type == AST_LOCALPARAM ||
       current_scope[str]->type == AST_ENUM_ITEM))
    const_fold = true;

  std::map<std::string, AstNode *> backup_scope;

  // create name resolution entries for all objects with names
  // also merge multiple declarations for the same wire (e.g. "output foobar;
  // reg foobar;")
  if (type == AST_MODULE || type == AST_INTERFACE) {
    current_scope.clear();
    std::set<std::string> existing;
    int counter = 0;
    label_genblks(existing, counter);
    std::map<std::string, AstNode *> this_wire_scope;
    for (size_t i = 0; i < children.size(); i++) {
      AstNode *node = children[i];

      if (node->type == AST_WIRE) {
        if (node->children.size() == 1 &&
            node->children[0]->type == AST_RANGE) {
          for (auto c : node->children[0]->children) {
            if (!c->is_simple_const_expr()) {
              if (attributes.count(ID::dynports))
                delete attributes.at(ID::dynports);
              set_attribute(ID::dynports, AstNode::mkconst_int(1, true));
            }
          }
        }
        if (this_wire_scope.count(node->str) > 0) {
          AstNode *first_node = this_wire_scope[node->str];
          if (first_node->is_input && node->is_reg)
            goto wires_are_incompatible;
          if (!node->is_input && !node->is_output && node->is_reg &&
              node->children.size() == 0)
            goto wires_are_compatible;
          if (first_node->children.size() == 0 && node->children.size() == 1 &&
              node->children[0]->type == AST_RANGE) {
            AstNode *r = node->children[0];
            if (r->range_valid && r->range_left == 0 && r->range_right == 0) {
              delete r;
              node->children.pop_back();
            }
          }
          if (first_node->children.size() != node->children.size())
            goto wires_are_incompatible;
          for (size_t j = 0; j < node->children.size(); j++) {
            AstNode *n1 = first_node->children[j], *n2 = node->children[j];
            if (n1->type == AST_RANGE && n2->type == AST_RANGE &&
                n1->range_valid && n2->range_valid) {
              if (n1->range_left != n2->range_left)
                goto wires_are_incompatible;
              if (n1->range_right != n2->range_right)
                goto wires_are_incompatible;
            } else if (*n1 != *n2)
              goto wires_are_incompatible;
          }
          if (first_node->range_left != node->range_left)
            goto wires_are_incompatible;
          if (first_node->range_right != node->range_right)
            goto wires_are_incompatible;
          if (first_node->port_id == 0 && (node->is_input || node->is_output))
            goto wires_are_incompatible;
        wires_are_compatible:
          if (node->is_input)
            first_node->is_input = true;
          if (node->is_output)
            first_node->is_output = true;
          if (node->is_reg)
            first_node->is_reg = true;
          if (node->is_logic)
            first_node->is_logic = true;
          if (node->is_signed)
            first_node->is_signed = true;
          for (auto &it : node->attributes) {
            if (first_node->attributes.count(it.first) > 0)
              delete first_node->attributes[it.first];
            first_node->set_attribute(it.first, it.second->clone());
          }
          children.erase(children.begin() + (i--));
          did_something = true;
          delete node;
          continue;
        wires_are_incompatible:
          if (stage > 1)
            input_error("Incompatible re-declaration of wire %s.\n",
                        node->str.c_str());
          continue;
        }
        this_wire_scope[node->str] = node;
      }
      // these nodes appear at the top level in a module and can define names
      if (node->type == AST_PARAMETER || node->type == AST_LOCALPARAM ||
          node->type == AST_WIRE || node->type == AST_AUTOWIRE ||
          node->type == AST_GENVAR || node->type == AST_MEMORY ||
          node->type == AST_FUNCTION || node->type == AST_TASK ||
          node->type == AST_DPI_FUNCTION || node->type == AST_CELL ||
          node->type == AST_TYPEDEF) {
        backup_scope[node->str] = current_scope[node->str];
        current_scope[node->str] = node;
      }
      if (node->type == AST_ENUM) {
        current_scope[node->str] = node;
        for (auto enode : node->children) {
          // log_assert(enode->type == AST_ENUM_ITEM);
          if (current_scope.count(enode->str) == 0)
            current_scope[enode->str] = enode;
          else
            input_error("enum item %s already exists\n", enode->str.c_str());
        }
      }
    }
    for (size_t i = 0; i < children.size(); i++) {
      AstNode *node = children[i];
      if (node->type == AST_PARAMETER || node->type == AST_LOCALPARAM ||
          node->type == AST_WIRE || node->type == AST_AUTOWIRE ||
          node->type == AST_MEMORY || node->type == AST_TYPEDEF)
        while (node->simplify(true, 1, -1, false))
          did_something = true;
      if (node->type == AST_ENUM) {
        for (auto enode : node->children) {
          // log_assert(enode->type == AST_ENUM_ITEM);
          while (node->simplify(true, 1, -1, false))
            did_something = true;
        }
      }
    }

    for (AstNode *child : children)
      if (child->type == AST_ALWAYS && child->attributes.count(ID::always_comb))
        check_auto_nosync(child);
  }

  // create name resolution entries for all objects with names
  if (type == AST_PACKAGE) {
    // add names to package scope
    for (size_t i = 0; i < children.size(); i++) {
      AstNode *node = children[i];
      // these nodes appear at the top level in a package and can define names
      if (node->type == AST_PARAMETER || node->type == AST_LOCALPARAM ||
          node->type == AST_TYPEDEF || node->type == AST_FUNCTION ||
          node->type == AST_TASK) {
        current_scope[node->str] = node;
      }
      if (node->type == AST_ENUM) {
        current_scope[node->str] = node;
        for (auto enode : node->children) {
          // log_assert(enode->type == AST_ENUM_ITEM);
          if (current_scope.count(enode->str) == 0)
            current_scope[enode->str] = enode;
          else
            input_error("enum item %s already exists in package\n",
                        enode->str.c_str());
        }
      }
    }
  }

  auto backup_current_block = current_block;
  auto backup_current_block_child = current_block_child;
  auto backup_current_top_block = current_top_block;
  auto backup_current_always = current_always;
  auto backup_current_always_clocked = current_always_clocked;

  if (type == AST_ALWAYS || type == AST_INITIAL) {
    if (current_always != nullptr)
      input_error("Invalid nesting of always blocks and/or initializations.\n");

    current_always = this;
    current_always_clocked = false;

    if (type == AST_ALWAYS)
      for (auto child : children) {
        if (child->type == AST_POSEDGE || child->type == AST_NEGEDGE)
          current_always_clocked = true;
        if (child->type == AST_EDGE && GetSize(child->children) == 1 &&
            child->children[0]->type == AST_IDENTIFIER &&
            child->children[0]->str == "\\$global_clock")
          current_always_clocked = true;
      }
  }

  if (type == AST_CELL) {
    bool lookup_suggested = false;

    for (AstNode *child : children) {
      // simplify any parameters to constants
      if (child->type == AST_PARASET)
        while (child->simplify(true, 1, -1, false)) {
        }

      // look for patterns which _may_ indicate ambiguity requiring
      // resolution of the underlying module
      if (child->type == AST_ARGUMENT) {
        if (child->children.size() != 1)
          continue;
        const AstNode *value = child->children[0];
        if (value->type == AST_IDENTIFIER) {
          const AstNode *elem = value->id2ast;
          if (elem == nullptr) {
            if (current_scope.count(value->str))
              elem = current_scope.at(value->str);
            else
              continue;
          }
          if (elem->type == AST_MEMORY)
            // need to determine is the is a read or wire
            lookup_suggested = true;
          else if (elem->type == AST_WIRE && elem->is_signed &&
                   !value->children.empty())
            // this may be a fully sliced signed wire which needs
            // to be indirected to produce an unsigned connection
            lookup_suggested = true;
        } else if (contains_unbased_unsized(value))
          // unbased unsized literals extend to width of the context
          lookup_suggested = true;
      }
    }

    const RTLIL::Module *module = nullptr;
    if (lookup_suggested)
      module = lookup_cell_module();
    if (module) {
      size_t port_counter = 0;
      for (AstNode *child : children) {
        if (child->type != AST_ARGUMENT)
          continue;

        // determine the full name of port this argument is connected to
        RTLIL::IdString port_name;
        if (child->str.size())
          port_name = child->str;
        else {
          if (port_counter >= module->ports.size())
            input_error("Cell instance has more ports than the module!\n");
          port_name = module->ports[port_counter++];
        }

        // find the port's wire in the underlying module
        const RTLIL::Wire *ref = module->wire(port_name);
        if (ref == nullptr)
          input_error("Cell instance refers to port %s which does not exist in "
                      "module %s!.\n",
                      log_id(port_name), log_id(module->name));

        // select the argument, if present
        // log_assert(child->children.size() <= 1);
        if (child->children.empty())
          continue;
        AstNode *arg = child->children[0];

        // plain identifiers never need indirection; this also prevents
        // adding infinite levels of indirection
        if (arg->type == AST_IDENTIFIER && arg->children.empty())
          continue;

        // only add indirection for standard inputs or outputs
        if (ref->port_input == ref->port_output)
          continue;

        did_something = true;

        // create the indirection wire
        std::stringstream sstr;
        sstr << "$indirect$" << ref->name.c_str() << "$"
             << RTLIL::encode_filename(filename) << ":" << location.first_line
             << "$" << (autoidx++);
        std::string tmp_str = sstr.str();
        add_wire_for_ref(ref, tmp_str);

        AstNode *asgn = new AstNode(AST_ASSIGN);
        current_ast_mod->children.push_back(asgn);

        AstNode *ident = new AstNode(AST_IDENTIFIER);
        ident->str = tmp_str;
        child->children[0] = ident->clone();

        if (ref->port_input && !ref->port_output) {
          asgn->children.push_back(ident);
          asgn->children.push_back(arg);
        } else {
          // log_assert(!ref->port_input && ref->port_output);
          asgn->children.push_back(arg);
          asgn->children.push_back(ident);
        }
        asgn->fixup_hierarchy_flags();
      }
    }
  }

  int backup_width_hint = width_hint;
  bool backup_sign_hint = sign_hint;

  bool detect_width_simple = false;
  bool child_0_is_self_determined = false;
  bool child_1_is_self_determined = false;
  bool child_2_is_self_determined = false;
  bool children_are_self_determined = false;
  bool reset_width_after_children = false;

  switch (type) {
  case AST_ASSIGN_EQ:
  case AST_ASSIGN_LE:
  case AST_ASSIGN:
    while (!children[0]->basic_prep &&
           children[0]->simplify(false, stage, -1, false) == true)
      did_something = true;
    while (!children[1]->basic_prep &&
           children[1]->simplify(false, stage, -1, false) == true)
      did_something = true;
    children[0]->detectSignWidth(backup_width_hint, backup_sign_hint);
    children[1]->detectSignWidth(width_hint, sign_hint);
    width_hint = std::max(width_hint, backup_width_hint);
    child_0_is_self_determined = true;
    // test only once, before optimizations and memory mappings but after
    // assignment LHS was mapped to an identifier
    if (children[0]->id2ast && !children[0]->was_checked) {
      if ((type == AST_ASSIGN_LE || type == AST_ASSIGN_EQ) &&
          children[0]->id2ast->is_logic)
        children[0]->id2ast->is_reg =
            true; // if logic type is used in a block asignment
      if ((type == AST_ASSIGN_LE || type == AST_ASSIGN_EQ) &&
          !children[0]->id2ast->is_reg)
        // log_warning("wire '%s' is assigned in a block at %s.\n",
        //             children[0]->str.c_str(), loc_string().c_str());
        if (type == AST_ASSIGN && children[0]->id2ast->is_reg) {
          bool is_rand_reg = false;
          if (children[1]->type == AST_FCALL) {
            if (children[1]->str == "\\$anyconst")
              is_rand_reg = true;
            if (children[1]->str == "\\$anyseq")
              is_rand_reg = true;
            if (children[1]->str == "\\$allconst")
              is_rand_reg = true;
            if (children[1]->str == "\\$allseq")
              is_rand_reg = true;
          }
          if (!is_rand_reg) {
            // log_warning(
            //     "reg '%s' is assigned in a continuous assignment at %s.\n",
            //     children[0]->str.c_str(), loc_string().c_str());
          }
        }
      children[0]->was_checked = true;
    }
    break;

  case AST_STRUCT:
  case AST_UNION:
    if (!basic_prep) {
      for (auto *node : children) {
        // resolve any ranges
        while (!node->basic_prep && node->simplify(true, stage, -1, false)) {
          did_something = true;
        }
      }
      // determine member offsets and widths
      size_packed_struct(this, 0);

      // instance rather than just a type in a typedef or outer struct?
      if (!str.empty() && str[0] == '\\') {
        // instance so add a wire for the packed structure
        auto wnode = make_packed_struct(this, str, attributes);
        // log_assert(current_ast_mod);
        current_ast_mod->children.push_back(wnode);
      }
      basic_prep = true;
    }
    break;

  case AST_STRUCT_ITEM:
    if (is_custom_type) {
      // log_assert(children.size() == 1);
      // log_assert(children[0]->type == AST_WIRETYPE);
      auto type_name = children[0]->str;
      if (!current_scope.count(type_name)) {
        // log_file_error(filename, location.first_line,
        //                "Unknown identifier `%s' used as type name\n",
        //                type_name.c_str());
        exit(1);
      }
      AstNode *resolved_type_node = current_scope.at(type_name);
      if (resolved_type_node->type != AST_TYPEDEF) {
        // log_file_error(filename, location.first_line,
        //                "`%s' does not name a type\n", type_name.c_str());
        exit(1);
      }
      // log_assert(resolved_type_node->children.size() == 1);
      AstNode *template_node = resolved_type_node->children[0];

      // Ensure typedef itself is fully simplified
      while (
          template_node->simplify(const_fold, stage, width_hint, sign_hint)) {
      };

      // Remove type reference
      delete children[0];
      children.pop_back();

      switch (template_node->type) {
      case AST_WIRE:
        type = AST_STRUCT_ITEM;
        break;
      case AST_STRUCT:
      case AST_UNION:
        type = template_node->type;
        break;
      default:
        // log_file_error(filename, location.first_line,
        //                "Invalid type for struct member: %s",
        //                type2str(template_node->type).c_str());
        exit(1);
      }

      is_reg = template_node->is_reg;
      is_logic = template_node->is_logic;
      is_signed = template_node->is_signed;
      is_string = template_node->is_string;
      is_custom_type = template_node->is_custom_type;

      range_valid = template_node->range_valid;
      range_swapped = template_node->range_swapped;
      range_left = template_node->range_left;
      range_right = template_node->range_right;

      set_attribute(ID::wiretype, mkconst_str(resolved_type_node->str));

      // Copy clones of children from template
      for (auto template_child : template_node->children) {
        children.push_back(template_child->clone());
      }

      did_something = true;
    }
    // log_assert(!is_custom_type);
    break;

  case AST_ENUM:
    // log("\nENUM %s: %d child %d\n", str.c_str(), basic_prep,
    // children[0]->basic_prep);
    if (!basic_prep) {
      for (auto item_node : children) {
        while (!item_node->basic_prep &&
               item_node->simplify(false, stage, -1, false))
          did_something = true;
      }
      // allocate values (called more than once)
      allocateDefaultEnumValues();
    }
    break;

  case AST_PARAMETER:
  case AST_LOCALPARAM:
    // if parameter is implicit type which is the typename of a struct or union,
    // save information about struct in wiretype attribute
    if (children[0]->type == AST_IDENTIFIER &&
        current_scope.count(children[0]->str) > 0) {
      auto item_node = current_scope[children[0]->str];
      if (item_node->type == AST_STRUCT || item_node->type == AST_UNION) {
        set_attribute(ID::wiretype, item_node->clone());
        size_packed_struct(attributes[ID::wiretype], 0);
        add_members_to_scope(attributes[ID::wiretype], str);
      }
    }
    while (!children[0]->basic_prep &&
           children[0]->simplify(false, stage, -1, false) == true)
      did_something = true;
    children[0]->detectSignWidth(width_hint, sign_hint);
    if (children.size() > 1 && children[1]->type == AST_RANGE) {
      while (!children[1]->basic_prep &&
             children[1]->simplify(false, stage, -1, false) == true)
        did_something = true;
      if (!children[1]->range_valid)
        input_error("Non-constant width range on parameter decl.\n");
      width_hint = std::max(width_hint, children[1]->range_left -
                                            children[1]->range_right + 1);
    }
    break;
  case AST_ENUM_ITEM:
    while (!children[0]->basic_prep &&
           children[0]->simplify(false, stage, -1, false))
      did_something = true;
    children[0]->detectSignWidth(width_hint, sign_hint);
    if (children.size() > 1 && children[1]->type == AST_RANGE) {
      while (!children[1]->basic_prep &&
             children[1]->simplify(false, stage, -1, false))
        did_something = true;
      if (!children[1]->range_valid)
        input_error("Non-constant width range on enum item decl.\n");
      width_hint = std::max(width_hint, children[1]->range_left -
                                            children[1]->range_right + 1);
    }
    break;

  case AST_TO_BITS:
  case AST_TO_SIGNED:
  case AST_TO_UNSIGNED:
  case AST_SELFSZ:
  case AST_CAST_SIZE:
  case AST_CONCAT:
  case AST_REPLICATE:
  case AST_REDUCE_AND:
  case AST_REDUCE_OR:
  case AST_REDUCE_XOR:
  case AST_REDUCE_XNOR:
  case AST_REDUCE_BOOL:
    detect_width_simple = true;
    children_are_self_determined = true;
    break;

  case AST_NEG:
  case AST_BIT_NOT:
  case AST_POS:
  case AST_BIT_AND:
  case AST_BIT_OR:
  case AST_BIT_XOR:
  case AST_BIT_XNOR:
  case AST_ADD:
  case AST_SUB:
  case AST_MUL:
  case AST_DIV:
  case AST_MOD:
    detect_width_simple = true;
    break;

  case AST_SHIFT_LEFT:
  case AST_SHIFT_RIGHT:
  case AST_SHIFT_SLEFT:
  case AST_SHIFT_SRIGHT:
  case AST_POW:
    detect_width_simple = true;
    child_1_is_self_determined = true;
    break;

  case AST_LT:
  case AST_LE:
  case AST_EQ:
  case AST_NE:
  case AST_EQX:
  case AST_NEX:
  case AST_GE:
  case AST_GT:
    width_hint = -1;
    sign_hint = true;
    for (auto child : children) {
      while (!child->basic_prep &&
             child->simplify(false, stage, -1, false) == true)
        did_something = true;
      child->detectSignWidthWorker(width_hint, sign_hint);
    }
    reset_width_after_children = true;
    break;

  case AST_LOGIC_AND:
  case AST_LOGIC_OR:
  case AST_LOGIC_NOT:
    detect_width_simple = true;
    children_are_self_determined = true;
    break;

  case AST_TERNARY:
    child_0_is_self_determined = true;
    break;

  case AST_MEMRD:
    detect_width_simple = true;
    children_are_self_determined = true;
    break;

  case AST_FCALL:
  case AST_TCALL:
    children_are_self_determined = true;
    break;

  default:
    width_hint = -1;
    sign_hint = false;
  }

  if (detect_width_simple && width_hint < 0) {
    if (type == AST_REPLICATE)
      while (children[0]->simplify(true, stage, -1, false) == true)
        did_something = true;
    for (auto child : children)
      while (!child->basic_prep &&
             child->simplify(false, stage, -1, false) == true)
        did_something = true;
    detectSignWidth(width_hint, sign_hint);
  }

  if (type == AST_FCALL && str == "\\$past")
    detectSignWidth(width_hint, sign_hint);

  if (type == AST_TERNARY) {
    if (width_hint < 0) {
      while (!children[0]->basic_prep &&
             children[0]->simplify(true, stage, -1, false))
        did_something = true;

      bool backup_unevaluated_tern_branch = unevaluated_tern_branch;
      AstNode *chosen = get_tern_choice().first;

      unevaluated_tern_branch =
          backup_unevaluated_tern_branch || chosen == children[2];
      while (!children[1]->basic_prep &&
             children[1]->simplify(false, stage, -1, false))
        did_something = true;

      unevaluated_tern_branch =
          backup_unevaluated_tern_branch || chosen == children[1];
      while (!children[2]->basic_prep &&
             children[2]->simplify(false, stage, -1, false))
        did_something = true;

      unevaluated_tern_branch = backup_unevaluated_tern_branch;
      detectSignWidth(width_hint, sign_hint);
    }
    int width_hint_left, width_hint_right;
    bool sign_hint_left, sign_hint_right;
    bool found_real_left, found_real_right;
    children[1]->detectSignWidth(width_hint_left, sign_hint_left,
                                 &found_real_left);
    children[2]->detectSignWidth(width_hint_right, sign_hint_right,
                                 &found_real_right);
    if (found_real_left || found_real_right) {
      child_1_is_self_determined = true;
      child_2_is_self_determined = true;
    }
  }

  if (type == AST_CONDX && children.size() > 0 &&
      children.at(0)->type == AST_CONSTANT) {
    for (auto &bit : children.at(0)->bits)
      if (bit == State::Sz || bit == State::Sx)
        bit = State::Sa;
  }

  if (type == AST_CONDZ && children.size() > 0 &&
      children.at(0)->type == AST_CONSTANT) {
    for (auto &bit : children.at(0)->bits)
      if (bit == State::Sz)
        bit = State::Sa;
  }

  if (const_fold && type == AST_CASE) {
    detectSignWidth(width_hint, sign_hint);
    while (children[0]->simplify(const_fold, stage, width_hint, sign_hint)) {
    }
    if (children[0]->type == AST_CONSTANT && children[0]->bits_only_01()) {
      children[0]->is_signed = sign_hint;
      RTLIL::Const case_expr = children[0]->bitsAsConst(width_hint, sign_hint);
      std::vector<AstNode *> new_children;
      new_children.push_back(children[0]);
      for (int i = 1; i < GetSize(children); i++) {
        AstNode *child = children[i];
        // log_assert(child->type == AST_COND || child->type == AST_CONDX ||
        //            child->type == AST_CONDZ);
        for (auto v : child->children) {
          if (v->type == AST_DEFAULT)
            goto keep_const_cond;
          if (v->type == AST_BLOCK)
            continue;
          while (v->simplify(const_fold, stage, width_hint, sign_hint)) {
          }
          if (v->type == AST_CONSTANT && v->bits_only_01()) {
            RTLIL::Const case_item_expr = v->bitsAsConst(width_hint, sign_hint);
            RTLIL::Const match =
                const_eq(case_expr, case_item_expr, sign_hint, sign_hint, 1);
            // log_assert(match.bits.size() == 1);
            if (match.bits.front() == RTLIL::State::S1) {
              while (i + 1 < GetSize(children))
                delete children[++i];
              goto keep_const_cond;
            }
            continue;
          }
          goto keep_const_cond;
        }
        if (0)
        keep_const_cond:
          new_children.push_back(child);
        else
          delete child;
      }
      new_children.swap(children);
    }
  }

  dict<std::string, pool<int>> backup_memwr_visible;
  dict<std::string, pool<int>> final_memwr_visible;

  if (type == AST_CASE && stage == 2) {
    backup_memwr_visible = current_memwr_visible;
    final_memwr_visible = current_memwr_visible;
  }

  // simplify all children first
  // (iterate by index as e.g. auto wires can add new children in the process)
  for (size_t i = 0; i < children.size(); i++) {
    bool did_something_here = true;
    bool backup_flag_autowire = flag_autowire;
    bool backup_unevaluated_tern_branch = unevaluated_tern_branch;
    if ((type == AST_GENFOR || type == AST_FOR) && i >= 3)
      break;
    if ((type == AST_GENIF || type == AST_GENCASE) && i >= 1)
      break;
    if (type == AST_GENBLOCK)
      break;
    if (type == AST_CELLARRAY && children[i]->type == AST_CELL)
      continue;
    if (type == AST_BLOCK && !str.empty())
      break;
    if (type == AST_PREFIX && i >= 1)
      break;
    if (type == AST_DEFPARAM && i == 0)
      flag_autowire = true;
    if (type == AST_TERNARY && i > 0 && !unevaluated_tern_branch) {
      AstNode *chosen = get_tern_choice().first;
      unevaluated_tern_branch = chosen && chosen != children[i];
    }
    while (did_something_here && i < children.size()) {
      bool const_fold_here = const_fold;
      int width_hint_here = width_hint;
      bool sign_hint_here = sign_hint;
      if (i == 0 && (type == AST_REPLICATE || type == AST_WIRE))
        const_fold_here = true;
      if (type == AST_PARAMETER || type == AST_LOCALPARAM)
        const_fold_here = true;
      if (type == AST_BLOCK) {
        current_block = this;
        current_block_child = children[i];
      }
      if ((type == AST_ALWAYS || type == AST_INITIAL) &&
          children[i]->type == AST_BLOCK)
        current_top_block = children[i];
      if (i == 0 && child_0_is_self_determined)
        width_hint_here = -1, sign_hint_here = false;
      if (i == 1 && child_1_is_self_determined)
        width_hint_here = -1, sign_hint_here = false;
      if (i == 2 && child_2_is_self_determined)
        width_hint_here = -1, sign_hint_here = false;
      if (children_are_self_determined)
        width_hint_here = -1, sign_hint_here = false;
      did_something_here = children[i]->simplify(
          const_fold_here, stage, width_hint_here, sign_hint_here);
      if (did_something_here)
        did_something = true;
    }
    if (stage == 2 && children[i]->type == AST_INITIAL &&
        current_ast_mod != this) {
      current_ast_mod->children.push_back(children[i]);
      children.erase(children.begin() + (i--));
      did_something = true;
    }
    flag_autowire = backup_flag_autowire;
    unevaluated_tern_branch = backup_unevaluated_tern_branch;
    if (stage == 2 && type == AST_CASE) {
      for (auto &x : current_memwr_visible) {
        for (int y : x.second)
          final_memwr_visible[x.first].insert(y);
      }
      current_memwr_visible = backup_memwr_visible;
    }
  }
  for (auto &attr : attributes) {
    while (attr.second->simplify(true, stage, -1, false))
      did_something = true;
  }
  if (type == AST_CASE && stage == 2) {
    current_memwr_visible = final_memwr_visible;
  }
  if (type == AST_ALWAYS && stage == 2) {
    current_memwr_visible.clear();
    current_memwr_count.clear();
  }

  if (reset_width_after_children) {
    width_hint = backup_width_hint;
    sign_hint = backup_sign_hint;
    if (width_hint < 0)
      detectSignWidth(width_hint, sign_hint);
  }

  current_block = backup_current_block;
  current_block_child = backup_current_block_child;
  current_top_block = backup_current_top_block;
  current_always = backup_current_always;
  current_always_clocked = backup_current_always_clocked;

  for (auto it = backup_scope.begin(); it != backup_scope.end(); it++) {
    if (it->second == NULL)
      current_scope.erase(it->first);
    else
      current_scope[it->first] = it->second;
  }

  current_filename = filename;

  if (type == AST_MODULE || type == AST_INTERFACE)
    current_scope.clear();

  // convert defparam nodes to cell parameters
  if (type == AST_DEFPARAM && !children.empty()) {
    if (children[0]->type != AST_IDENTIFIER)
      input_error(
          "Module name in defparam contains non-constant expressions!\n");

    std::string modname, paramname = children[0]->str;

    size_t pos = paramname.rfind('.');

    while (pos != 0 && pos != std::string::npos) {
      modname = paramname.substr(0, pos);

      if (current_scope.count(modname))
        break;

      pos = paramname.rfind('.', pos - 1);
    }

    if (pos == std::string::npos)
      input_error("Can't find object for defparam `%s`!\n",
                  RTLIL::unescape_id(paramname).c_str());

    paramname = "\\" + paramname.substr(pos + 1);

    if (current_scope.at(modname)->type != AST_CELL)
      input_error("Defparam argument `%s . %s` does not match a cell!\n",
                  RTLIL::unescape_id(modname).c_str(),
                  RTLIL::unescape_id(paramname).c_str());

    AstNode *paraset =
        new AstNode(AST_PARASET, children[1]->clone(),
                    GetSize(children) > 2 ? children[2]->clone() : NULL);
    paraset->str = paramname;

    AstNode *cell = current_scope.at(modname);
    cell->children.insert(cell->children.begin() + 1, paraset);
    delete_children();
  }

  // resolve typedefs
  if (type == AST_TYPEDEF) {
    // log_assert(children.size() == 1);
    auto type_node = children[0];
    // log_assert(type_node->type == AST_WIRE || type_node->type == AST_MEMORY
    // ||
    //            type_node->type == AST_STRUCT || type_node->type ==
    //            AST_UNION);
    while (type_node->simplify(const_fold, stage, width_hint, sign_hint)) {
      did_something = true;
    }
    // log_assert(!type_node->is_custom_type);
  }

  // resolve types of wires
  if (type == AST_WIRE || type == AST_MEMORY) {
    if (is_custom_type) {
      // log_assert(children.size() >= 1);
      // log_assert(children[0]->type == AST_WIRETYPE);
      auto type_name = children[0]->str;
      if (!current_scope.count(type_name)) {
        input_error("Unknown identifier `%s' used as type name\n",
                    type_name.c_str());
      }
      AstNode *resolved_type_node = current_scope.at(type_name);
      if (resolved_type_node->type != AST_TYPEDEF)
        input_error("`%s' does not name a type\n", type_name.c_str());
      // log_assert(resolved_type_node->children.size() == 1);
      AstNode *template_node = resolved_type_node->children[0];

      // Ensure typedef itself is fully simplified
      while (
          template_node->simplify(const_fold, stage, width_hint, sign_hint)) {
      };

      if (!str.empty() && str[0] == '\\' &&
          (template_node->type == AST_STRUCT ||
           template_node->type == AST_UNION)) {
        // replace instance with wire representing the packed structure
        newNode = make_packed_struct(template_node, str, attributes);
        newNode->set_attribute(ID::wiretype,
                               mkconst_str(resolved_type_node->str));
        // add original input/output attribute to resolved wire
        newNode->is_input = this->is_input;
        newNode->is_output = this->is_output;
        current_scope[str] = this;
        goto apply_newNode;
      }

      // Remove type reference
      delete children[0];
      children.erase(children.begin());

      if (type == AST_WIRE)
        type = template_node->type;
      is_reg = template_node->is_reg;
      is_logic = template_node->is_logic;
      is_signed = template_node->is_signed;
      is_string = template_node->is_string;
      is_custom_type = template_node->is_custom_type;

      range_valid = template_node->range_valid;
      range_swapped = template_node->range_swapped;
      range_left = template_node->range_left;
      range_right = template_node->range_right;

      set_attribute(ID::wiretype, mkconst_str(resolved_type_node->str));

      // if an enum then add attributes to support simulator tracing
      annotateTypedEnums(template_node);

      // Insert clones children from template at beginning
      for (int i = 0; i < GetSize(template_node->children); i++)
        children.insert(children.begin() + i,
                        template_node->children[i]->clone());

      if (type == AST_MEMORY && GetSize(children) == 1) {
        // Single-bit memories must have [0:0] range
        AstNode *rng = make_range(0, 0);
        children.insert(children.begin(), rng);
      }
      fixup_hierarchy_flags();
      did_something = true;
    }
    // log_assert(!is_custom_type);
  }

  // resolve types of parameters
  if (type == AST_LOCALPARAM || type == AST_PARAMETER) {
    if (is_custom_type) {
      // log_assert(children.size() == 2);
      // log_assert(children[1]->type == AST_WIRETYPE);
      auto type_name = children[1]->str;
      if (!current_scope.count(type_name)) {
        input_error("Unknown identifier `%s' used as type name\n",
                    type_name.c_str());
      }
      AstNode *resolved_type_node = current_scope.at(type_name);
      if (resolved_type_node->type != AST_TYPEDEF)
        input_error("`%s' does not name a type\n", type_name.c_str());
      // log_assert(resolved_type_node->children.size() == 1);
      AstNode *template_node = resolved_type_node->children[0];

      // Ensure typedef itself is fully simplified
      while (
          template_node->simplify(const_fold, stage, width_hint, sign_hint)) {
      };

      if (template_node->type == AST_STRUCT ||
          template_node->type == AST_UNION) {
        // replace with wire representing the packed structure
        newNode = make_packed_struct(template_node, str, attributes);
        newNode->set_attribute(ID::wiretype,
                               mkconst_str(resolved_type_node->str));
        newNode->type = type;
        current_scope[str] = this;
        // copy param value, it needs to be 1st value
        delete children[1];
        children.pop_back();
        newNode->children.insert(newNode->children.begin(),
                                 children[0]->clone());
        goto apply_newNode;
      }
      delete children[1];
      children.pop_back();

      if (template_node->type == AST_MEMORY)
        input_error("unpacked array type `%s' cannot be used for a parameter\n",
                    children[1]->str.c_str());
      is_signed = template_node->is_signed;
      is_string = template_node->is_string;
      is_custom_type = template_node->is_custom_type;

      range_valid = template_node->range_valid;
      range_swapped = template_node->range_swapped;
      range_left = template_node->range_left;
      range_right = template_node->range_right;
      set_attribute(ID::wiretype, mkconst_str(resolved_type_node->str));
      for (auto template_child : template_node->children)
        children.push_back(template_child->clone());
      fixup_hierarchy_flags();
      did_something = true;
    }
    // log_assert(!is_custom_type);
  }

  // resolve constant prefixes
  if (type == AST_PREFIX) {
    if (children[0]->type != AST_CONSTANT) {
      // dumpAst(NULL, ">   ");
      input_error("Index in generate block prefix syntax is not constant!\n");
    }
    if (children[1]->type == AST_PREFIX)
      children[1]->simplify(const_fold, stage, width_hint, sign_hint);
    // log_assert(children[1]->type == AST_IDENTIFIER);
    newNode = children[1]->clone();
    const char *second_part = children[1]->str.c_str();
    if (second_part[0] == '\\')
      second_part++;
    newNode->str =
        stringf("%s[%d].%s", str.c_str(), children[0]->integer, second_part);
    goto apply_newNode;
  }

  // evaluate TO_BITS nodes
  if (type == AST_TO_BITS) {
    if (children[0]->type != AST_CONSTANT)
      input_error("Left operand of to_bits expression is not constant!\n");
    if (children[1]->type != AST_CONSTANT)
      input_error("Right operand of to_bits expression is not constant!\n");
    RTLIL::Const new_value = children[1]->bitsAsConst(
        children[0]->bitsAsConst().as_int(), children[1]->is_signed);
    newNode = mkconst_bits(new_value.bits, children[1]->is_signed);
    goto apply_newNode;
  }

  // annotate constant ranges
  if (type == AST_RANGE) {
    bool old_range_valid = range_valid;
    range_valid = false;
    range_swapped = false;
    range_left = -1;
    range_right = 0;
    // log_assert(children.size() >= 1);
    if (children[0]->type == AST_CONSTANT) {
      range_valid = true;
      range_left = children[0]->integer;
      if (children.size() == 1)
        range_right = range_left;
    }
    if (children.size() >= 2) {
      if (children[1]->type == AST_CONSTANT)
        range_right = children[1]->integer;
      else
        range_valid = false;
    }
    if (old_range_valid != range_valid)
      did_something = true;
    if (range_valid && range_right > range_left) {
      int tmp = range_right;
      range_right = range_left;
      range_left = tmp;
      range_swapped = true;
    }
  }

  // annotate wires with their ranges
  if (type == AST_WIRE) {
    if (children.size() > 0) {
      if (children[0]->range_valid) {
        if (!range_valid)
          did_something = true;
        range_valid = true;
        range_swapped = children[0]->range_swapped;
        range_left = children[0]->range_left;
        range_right = children[0]->range_right;
        bool force_upto = false, force_downto = false;
        if (attributes.count(ID::force_upto)) {
          AstNode *val = attributes[ID::force_upto];
          if (val->type != AST_CONSTANT)
            input_error("Attribute `force_upto' with non-constant value!\n");
          force_upto = val->asAttrConst().as_bool();
        }
        if (attributes.count(ID::force_downto)) {
          AstNode *val = attributes[ID::force_downto];
          if (val->type != AST_CONSTANT)
            input_error("Attribute `force_downto' with non-constant value!\n");
          force_downto = val->asAttrConst().as_bool();
        }
        if (force_upto && force_downto)
          input_error("Attributes `force_downto' and `force_upto' cannot be "
                      "both set!\n");
        if ((force_upto && !range_swapped) || (force_downto && range_swapped)) {
          std::swap(range_left, range_right);
          range_swapped = force_upto;
        }
      }
    } else {
      if (!range_valid)
        did_something = true;
      range_valid = true;
      range_swapped = false;
      range_left = 0;
      range_right = 0;
    }
  }

  // resolve multiranges on memory decl
  if (type == AST_MEMORY && children.size() > 1 &&
      children[1]->type == AST_MULTIRANGE) {
    int total_size = 1;
    multirange_dimensions.clear();
    multirange_swapped.clear();
    for (auto range : children[1]->children) {
      if (!range->range_valid)
        input_error("Non-constant range on memory decl.\n");
      multirange_dimensions.push_back(
          std::min(range->range_left, range->range_right));
      multirange_dimensions.push_back(
          std::max(range->range_left, range->range_right) -
          std::min(range->range_left, range->range_right) + 1);
      multirange_swapped.push_back(range->range_swapped);
      total_size *= multirange_dimensions.back();
    }
    delete children[1];
    children[1] = new AstNode(AST_RANGE, AstNode::mkconst_int(0, true),
                              AstNode::mkconst_int(total_size - 1, true));
    fixup_hierarchy_flags();
    did_something = true;
  }

  // resolve multiranges on memory access
  if (type == AST_IDENTIFIER && id2ast && id2ast->type == AST_MEMORY &&
      children.size() > 0 && children[0]->type == AST_MULTIRANGE) {
    AstNode *index_expr = nullptr;

    integer = children[0]->children.size(); // save original number of
                                            // dimensions for $size() etc.
    for (int i = 0; 2 * i < GetSize(id2ast->multirange_dimensions); i++) {
      if (GetSize(children[0]->children) <= i)
        input_error("Insufficient number of array indices for %s.\n",
                    log_id(str));

      AstNode *new_index_expr =
          children[0]->children[i]->children.at(0)->clone();

      if (id2ast->multirange_dimensions[2 * i])
        new_index_expr = new AstNode(
            AST_SUB, new_index_expr,
            AstNode::mkconst_int(id2ast->multirange_dimensions[2 * i], true));

      if (i == 0)
        index_expr = new_index_expr;
      else
        index_expr = new AstNode(
            AST_ADD,
            new AstNode(AST_MUL, index_expr,
                        AstNode::mkconst_int(
                            id2ast->multirange_dimensions[2 * i + 1], true)),
            new_index_expr);
    }

    for (int i = GetSize(id2ast->multirange_dimensions) / 2;
         i < GetSize(children[0]->children); i++)
      children.push_back(children[0]->children[i]->clone());

    delete children[0];
    if (index_expr == nullptr)
      children.erase(children.begin());
    else
      children[0] = new AstNode(AST_RANGE, index_expr);

    fixup_hierarchy_flags();
    did_something = true;
  }

  // trim/extend parameters
  if (type == AST_PARAMETER || type == AST_LOCALPARAM ||
      type == AST_ENUM_ITEM) {
    if (children.size() > 1 && children[1]->type == AST_RANGE) {
      if (!children[1]->range_valid)
        input_error("Non-constant width range on parameter decl.\n");
      int width =
          std::abs(children[1]->range_left - children[1]->range_right) + 1;
      if (children[0]->type == AST_REALVALUE) {
        RTLIL::Const constvalue = children[0]->realAsConst(width);
        // log_file_warning(filename, location.first_line,
        //                  "converting real value %e to binary %s.\n",
        //                  children[0]->realvalue, log_signal(constvalue));
        delete children[0];
        children[0] = mkconst_bits(constvalue.bits, sign_hint);
        fixup_hierarchy_flags();
        did_something = true;
      }
      if (children[0]->type == AST_CONSTANT) {
        if (width != int(children[0]->bits.size())) {
          RTLIL::SigSpec sig(children[0]->bits);
          sig.extend_u0(width, children[0]->is_signed);
          AstNode *old_child_0 = children[0];
          children[0] = mkconst_bits(sig.as_const().bits, is_signed);
          delete old_child_0;
          fixup_hierarchy_flags();
        }
        children[0]->is_signed = is_signed;
      }
      range_valid = true;
      range_swapped = children[1]->range_swapped;
      range_left = children[1]->range_left;
      range_right = children[1]->range_right;
    } else if (children.size() > 1 && children[1]->type == AST_REALVALUE &&
               children[0]->type == AST_CONSTANT) {
      double as_realvalue = children[0]->asReal(sign_hint);
      delete children[0];
      children[0] = new AstNode(AST_REALVALUE);
      children[0]->realvalue = as_realvalue;
      fixup_hierarchy_flags();
      did_something = true;
    }
  }

  if (type == AST_IDENTIFIER && !basic_prep) {
    // check if a plausible struct member sss.mmmm
    std::string sname;
    if (name_has_dot(str, sname)) {
      if (current_scope.count(str) > 0) {
        auto item_node = current_scope[str];
        if (item_node->type == AST_STRUCT_ITEM ||
            item_node->type == AST_STRUCT || item_node->type == AST_UNION) {
          // structure member, rewrite this node to reference the packed struct
          // wire
          auto range = make_struct_member_range(this, item_node);
          newNode = new AstNode(AST_IDENTIFIER, range);
          newNode->str = sname;
          // save type and original number of dimensions for $size() etc.
          newNode->set_attribute(ID::wiretype, item_node->clone());
          if (!item_node->multirange_dimensions.empty() &&
              children.size() > 0) {
            if (children[0]->type == AST_RANGE)
              newNode->integer = 1;
            else if (children[0]->type == AST_MULTIRANGE)
              newNode->integer = children[0]->children.size();
          }
          newNode->basic_prep = true;
          if (item_node->is_signed)
            newNode = new AstNode(AST_TO_SIGNED, newNode);
          goto apply_newNode;
        }
      }
    }
  }
  // annotate identifiers using scope resolution and create auto-wires as needed
  if (type == AST_IDENTIFIER) {
    if (current_scope.count(str) == 0) {
      AstNode *current_scope_ast = (current_ast_mod == nullptr)
                                       ? VERILOG_FRONTEND::current_ast
                                       : current_ast_mod;
      str = try_pop_module_prefix();
      for (auto node : current_scope_ast->children) {
        // log("looking at mod scope child %s\n", type2str(node->type).c_str());
        switch (node->type) {
        case AST_PARAMETER:
        case AST_LOCALPARAM:
        case AST_WIRE:
        case AST_AUTOWIRE:
        case AST_GENVAR:
        case AST_MEMORY:
        case AST_FUNCTION:
        case AST_TASK:
        case AST_DPI_FUNCTION:
          // log("found child %s, %s\n", type2str(node->type).c_str(),
          // node->str.c_str());
          if (str == node->str) {
            // log("add %s, type %s to scope\n", str.c_str(),
            // type2str(node->type).c_str());
            current_scope[node->str] = node;
          }
          break;
        case AST_ENUM:
          current_scope[node->str] = node;
          for (auto enum_node : node->children) {
            // log_assert(enum_node->type == AST_ENUM_ITEM);
            if (str == enum_node->str) {
              // log("\nadding enum item %s to scope\n", str.c_str());
              current_scope[str] = enum_node;
            }
          }
          break;
        default:
          break;
        }
      }
    }
    if (current_scope.count(str) == 0) {
      if (current_ast_mod == nullptr) {
        input_error(
            "Identifier `%s' is implicitly declared outside of a module.\n",
            str.c_str());
      } else if (flag_autowire || str == "\\$global_clock") {
        AstNode *auto_wire = new AstNode(AST_AUTOWIRE);
        auto_wire->str = str;
        current_ast_mod->children.push_back(auto_wire);
        current_scope[str] = auto_wire;
        did_something = true;
      } else {
        input_error("Identifier `%s' is implicitly declared and "
                    "`default_nettype is set to none.\n",
                    str.c_str());
      }
    }
    if (id2ast != current_scope[str]) {
      id2ast = current_scope[str];
      did_something = true;
    }
  }

  // split memory access with bit select to individual statements
  if (type == AST_IDENTIFIER && children.size() == 2 &&
      children[0]->type == AST_RANGE && children[1]->type == AST_RANGE &&
      !in_lvalue && stage == 2) {
    if (id2ast == NULL || id2ast->type != AST_MEMORY ||
        children[0]->children.size() != 1)
      input_error("Invalid bit-select on memory access!\n");

    int mem_width, mem_size, addr_bits;
    id2ast->meminfo(mem_width, mem_size, addr_bits);

    int data_range_left = id2ast->children[0]->range_left;
    int data_range_right = id2ast->children[0]->range_right;

    if (id2ast->children[0]->range_swapped)
      std::swap(data_range_left, data_range_right);

    std::stringstream sstr;
    sstr << "$mem2bits$" << str << "$" << RTLIL::encode_filename(filename)
         << ":" << location.first_line << "$" << (autoidx++);
    std::string wire_id = sstr.str();

    AstNode *wire = new AstNode(
        AST_WIRE, new AstNode(AST_RANGE, mkconst_int(data_range_left, true),
                              mkconst_int(data_range_right, true)));
    wire->str = wire_id;
    if (current_block)
      wire->set_attribute(ID::nosync, AstNode::mkconst_int(1, false));
    current_ast_mod->children.push_back(wire);
    while (wire->simplify(true, 1, -1, false)) {
    }

    AstNode *data = clone();
    delete data->children[1];
    data->children.pop_back();

    AstNode *assign =
        new AstNode(AST_ASSIGN_EQ, new AstNode(AST_IDENTIFIER), data);
    assign->children[0]->str = wire_id;
    assign->children[0]->was_checked = true;

    if (current_block) {
      size_t assign_idx = 0;
      while (assign_idx < current_block->children.size() &&
             current_block->children[assign_idx] != current_block_child)
        assign_idx++;
      // log_assert(assign_idx < current_block->children.size());
      current_block->children.insert(
          current_block->children.begin() + assign_idx, assign);
      wire->is_reg = true;
    } else {
      AstNode *proc = new AstNode(AST_ALWAYS, new AstNode(AST_BLOCK));
      proc->children[0]->children.push_back(assign);
      current_ast_mod->children.push_back(proc);
    }

    newNode = new AstNode(AST_IDENTIFIER, children[1]->clone());
    newNode->str = wire_id;
    newNode->integer =
        integer; // save original number of dimensions for $size() etc.
    newNode->id2ast = wire;
    goto apply_newNode;
  }

  if (type == AST_WHILE)
    input_error("While loops are only allowed in constant functions!\n");

  if (type == AST_REPEAT) {
    AstNode *count = children[0];
    AstNode *body = children[1];

    // eval count expression
    while (count->simplify(true, stage, 32, true)) {
    }

    if (count->type != AST_CONSTANT)
      input_error("Repeat loops outside must have constant repeat counts!\n");

    // convert to a block with the body repeated n times
    type = AST_BLOCK;
    children.clear();
    for (int i = 0; i < count->bitsAsConst().as_int(); i++)
      children.insert(children.begin(), body->clone());

    delete count;
    delete body;
    did_something = true;
  }

  // unroll for loops and generate-for blocks
  if ((type == AST_GENFOR || type == AST_FOR) && children.size() != 0) {
    AstNode *init_ast = children[0];
    AstNode *while_ast = children[1];
    AstNode *next_ast = children[2];
    AstNode *body_ast = children[3];

    while (body_ast->type == AST_GENBLOCK && body_ast->str.empty() &&
           body_ast->children.size() == 1 &&
           body_ast->children.at(0)->type == AST_GENBLOCK)
      body_ast = body_ast->children.at(0);

    const char *loop_type_str = "procedural";
    const char *var_type_str = "register";
    AstNodeType var_type = AST_WIRE;
    if (type == AST_GENFOR) {
      loop_type_str = "generate";
      var_type_str = "genvar";
      var_type = AST_GENVAR;
    }

    if (init_ast->type != AST_ASSIGN_EQ)
      input_error("Unsupported 1st expression of %s for-loop!\n",
                  loop_type_str);
    if (next_ast->type != AST_ASSIGN_EQ)
      input_error("Unsupported 3rd expression of %s for-loop!\n",
                  loop_type_str);

    if (init_ast->children[0]->id2ast == NULL ||
        init_ast->children[0]->id2ast->type != var_type)
      input_error(
          "Left hand side of 1st expression of %s for-loop is not a %s!\n",
          loop_type_str, var_type_str);
    if (next_ast->children[0]->id2ast == NULL ||
        next_ast->children[0]->id2ast->type != var_type)
      input_error(
          "Left hand side of 3rd expression of %s for-loop is not a %s!\n",
          loop_type_str, var_type_str);

    if (init_ast->children[0]->id2ast != next_ast->children[0]->id2ast)
      input_error("Incompatible left-hand sides in 1st and 3rd expression of "
                  "%s for-loop!\n",
                  loop_type_str);

    // eval 1st expression
    AstNode *varbuf = init_ast->children[1]->clone();
    {
      int expr_width_hint = -1;
      bool expr_sign_hint = true;
      varbuf->detectSignWidth(expr_width_hint, expr_sign_hint);
      while (varbuf->simplify(true, stage, 32, true)) {
      }
    }

    if (varbuf->type != AST_CONSTANT)
      input_error(
          "Right hand side of 1st expression of %s for-loop is not constant!\n",
          loop_type_str);

    auto resolved = current_scope.at(init_ast->children[0]->str);
    if (resolved->range_valid) {
      int const_size = varbuf->range_left - varbuf->range_right;
      int resolved_size = resolved->range_left - resolved->range_right;
      if (const_size < resolved_size) {
        for (int i = const_size; i < resolved_size; i++)
          varbuf->bits.push_back(resolved->is_signed ? varbuf->bits.back()
                                                     : State::S0);
        varbuf->range_left = resolved->range_left;
        varbuf->range_right = resolved->range_right;
        varbuf->range_swapped = resolved->range_swapped;
        varbuf->range_valid = resolved->range_valid;
      }
    }

    varbuf = new AstNode(AST_LOCALPARAM, varbuf);
    varbuf->str = init_ast->children[0]->str;

    AstNode *backup_scope_varbuf = current_scope[varbuf->str];
    current_scope[varbuf->str] = varbuf;

    size_t current_block_idx = 0;
    if (type == AST_FOR) {
      while (current_block_idx < current_block->children.size() &&
             current_block->children[current_block_idx] != current_block_child)
        current_block_idx++;
    }

    while (1) {
      // eval 2nd expression
      AstNode *buf = while_ast->clone();
      {
        int expr_width_hint = -1;
        bool expr_sign_hint = true;
        buf->detectSignWidth(expr_width_hint, expr_sign_hint);
        while (buf->simplify(true, stage, expr_width_hint, expr_sign_hint)) {
        }
      }

      if (buf->type != AST_CONSTANT)
        input_error("2nd expression of %s for-loop is not constant!\n",
                    loop_type_str);

      if (buf->integer == 0) {
        delete buf;
        break;
      }
      delete buf;

      // expand body
      int index = varbuf->children[0]->integer;
      // log_assert(body_ast->type == AST_GENBLOCK || body_ast->type ==
      // AST_BLOCK); log_assert(!body_ast->str.empty());
      buf = body_ast->clone();

      std::stringstream sstr;
      sstr << buf->str << "[" << index << "].";
      std::string prefix = sstr.str();

      // create a scoped localparam for the current value of the loop variable
      AstNode *local_index = varbuf->clone();
      size_t pos = local_index->str.rfind('.');
      if (pos != std::string::npos) // remove outer prefix
        local_index->str = "\\" + local_index->str.substr(pos + 1);
      local_index->str = prefix_id(prefix, local_index->str);
      current_scope[local_index->str] = local_index;
      current_ast_mod->children.push_back(local_index);

      buf->expand_genblock(prefix);

      if (type == AST_GENFOR) {
        for (size_t i = 0; i < buf->children.size(); i++) {
          buf->children[i]->simplify(const_fold, stage, -1, false);
          current_ast_mod->children.push_back(buf->children[i]);
        }
      } else {
        for (size_t i = 0; i < buf->children.size(); i++)
          current_block->children.insert(current_block->children.begin() +
                                             current_block_idx++,
                                         buf->children[i]);
      }
      buf->children.clear();
      delete buf;

      // eval 3rd expression
      buf = next_ast->children[1]->clone();
      buf->set_in_param_flag(true);
      {
        int expr_width_hint = -1;
        bool expr_sign_hint = true;
        buf->detectSignWidth(expr_width_hint, expr_sign_hint);
        while (buf->simplify(true, stage, expr_width_hint, expr_sign_hint)) {
        }
      }

      if (buf->type != AST_CONSTANT)
        input_error("Right hand side of 3rd expression of %s for-loop is not "
                    "constant (%s)!\n",
                    loop_type_str, type2str(buf->type).c_str());

      delete varbuf->children[0];
      varbuf->children[0] = buf;
    }

    if (type == AST_FOR) {
      AstNode *buf = next_ast->clone();
      delete buf->children[1];
      buf->children[1] = varbuf->children[0]->clone();
      current_block->children.insert(
          current_block->children.begin() + current_block_idx++, buf);
    }

    current_scope[varbuf->str] = backup_scope_varbuf;
    delete varbuf;
    delete_children();
    did_something = true;
  }

  // check for local objects in unnamed block
  if (type == AST_BLOCK && str.empty()) {
    for (size_t i = 0; i < children.size(); i++)
      if (children[i]->type == AST_WIRE || children[i]->type == AST_MEMORY ||
          children[i]->type == AST_PARAMETER ||
          children[i]->type == AST_LOCALPARAM ||
          children[i]->type == AST_TYPEDEF) {
        // log_assert(!VERILOG_FRONTEND::sv_mode);
        children[i]->input_error("Local declaration in unnamed block is only "
                                 "supported in SystemVerilog mode!\n");
      }
  }

  // transform block with name
  if (type == AST_BLOCK && !str.empty()) {
    expand_genblock(str + ".");

    // if this is an autonamed block is in an always_comb
    if (current_always && current_always->attributes.count(ID::always_comb) &&
        is_autonamed_block(str))
      // track local variables in this block so we can consider adding
      // nosync once the block has been fully elaborated
      for (AstNode *child : children)
        if (child->type == AST_WIRE && !child->attributes.count(ID::nosync))
          mark_auto_nosync(this, child);

    std::vector<AstNode *> new_children;
    for (size_t i = 0; i < children.size(); i++)
      if (children[i]->type == AST_WIRE || children[i]->type == AST_MEMORY ||
          children[i]->type == AST_PARAMETER ||
          children[i]->type == AST_LOCALPARAM ||
          children[i]->type == AST_TYPEDEF) {
        children[i]->simplify(false, stage, -1, false);
        current_ast_mod->children.push_back(children[i]);
        current_scope[children[i]->str] = children[i];
      } else
        new_children.push_back(children[i]);

    children.swap(new_children);
    did_something = true;
    str.clear();
  }

  // simplify unconditional generate block
  if (type == AST_GENBLOCK && children.size() != 0) {
    if (!str.empty()) {
      expand_genblock(str + ".");
    }

    for (size_t i = 0; i < children.size(); i++) {
      children[i]->simplify(const_fold, stage, -1, false);
      current_ast_mod->children.push_back(children[i]);
    }

    children.clear();
    did_something = true;
  }

  // simplify generate-if blocks
  if (type == AST_GENIF && children.size() != 0) {
    AstNode *buf = children[0]->clone();
    while (buf->simplify(true, stage, width_hint, sign_hint)) {
    }
    if (buf->type != AST_CONSTANT) {
      // for (auto f : log_files)
      // 	dumpAst(f, "verilog-ast> ");
      input_error("Condition for generate if is not constant!\n");
    }
    if (buf->asBool() != 0) {
      delete buf;
      buf = children[1]->clone();
    } else {
      delete buf;
      buf = children.size() > 2 ? children[2]->clone() : NULL;
    }

    if (buf) {
      if (buf->type != AST_GENBLOCK)
        buf = new AstNode(AST_GENBLOCK, buf);

      if (!buf->str.empty()) {
        buf->expand_genblock(buf->str + ".");
      }

      for (size_t i = 0; i < buf->children.size(); i++) {
        buf->children[i]->simplify(const_fold, stage, -1, false);
        current_ast_mod->children.push_back(buf->children[i]);
      }

      buf->children.clear();
      delete buf;
    }

    delete_children();
    did_something = true;
  }

  // simplify generate-case blocks
  if (type == AST_GENCASE && children.size() != 0) {
    AstNode *buf = children[0]->clone();
    while (buf->simplify(true, stage, width_hint, sign_hint)) {
    }
    if (buf->type != AST_CONSTANT) {
      // for (auto f : log_files)
      // 	dumpAst(f, "verilog-ast> ");
      input_error("Condition for generate case is not constant!\n");
    }

    bool ref_signed = buf->is_signed;
    RTLIL::Const ref_value = buf->bitsAsConst();
    delete buf;

    AstNode *selected_case = NULL;
    for (size_t i = 1; i < children.size(); i++) {
      // log_assert(children.at(i)->type == AST_COND ||
      //            children.at(i)->type == AST_CONDX ||
      //            children.at(i)->type == AST_CONDZ);

      AstNode *this_genblock = NULL;
      for (auto child : children.at(i)->children) {
        // log_assert(this_genblock == NULL);
        if (child->type == AST_GENBLOCK)
          this_genblock = child;
      }

      for (auto child : children.at(i)->children) {
        if (child->type == AST_DEFAULT) {
          if (selected_case == NULL)
            selected_case = this_genblock;
          continue;
        }
        if (child->type == AST_GENBLOCK)
          continue;

        buf = child->clone();
        buf->set_in_param_flag(true);
        while (buf->simplify(true, stage, width_hint, sign_hint)) {
        }
        if (buf->type != AST_CONSTANT) {
          // for (auto f : log_files)
          // 	dumpAst(f, "verilog-ast> ");
          input_error("Expression in generate case is not constant!\n");
        }

        bool is_selected = RTLIL::const_eq(ref_value, buf->bitsAsConst(),
                                           ref_signed && buf->is_signed,
                                           ref_signed && buf->is_signed, 1)
                               .as_bool();
        delete buf;

        if (is_selected) {
          selected_case = this_genblock;
          i = children.size();
          break;
        }
      }
    }

    if (selected_case != NULL) {
      // log_assert(selected_case->type == AST_GENBLOCK);
      buf = selected_case->clone();

      if (!buf->str.empty()) {
        buf->expand_genblock(buf->str + ".");
      }

      for (size_t i = 0; i < buf->children.size(); i++) {
        buf->children[i]->simplify(const_fold, stage, -1, false);
        current_ast_mod->children.push_back(buf->children[i]);
      }

      buf->children.clear();
      delete buf;
    }

    delete_children();
    did_something = true;
  }

  // unroll cell arrays
  if (type == AST_CELLARRAY) {
    if (!children.at(0)->range_valid)
      input_error("Non-constant array range on cell array.\n");

    newNode = new AstNode(AST_GENBLOCK);
    int num =
        std::max(children.at(0)->range_left, children.at(0)->range_right) -
        std::min(children.at(0)->range_left, children.at(0)->range_right) + 1;

    for (int i = 0; i < num; i++) {
      int idx = children.at(0)->range_left > children.at(0)->range_right
                    ? children.at(0)->range_right + i
                    : children.at(0)->range_right - i;
      AstNode *new_cell = children.at(1)->clone();
      newNode->children.push_back(new_cell);
      new_cell->str += stringf("[%d]", idx);
      if (new_cell->type == AST_PRIMITIVE) {
        input_error("Cell arrays of primitives are currently not supported.\n");
      } else {
        // log_assert(new_cell->children.at(0)->type == AST_CELLTYPE);
        new_cell->children.at(0)->str = stringf(
            "$array:%d:%d:%s", i, num, new_cell->children.at(0)->str.c_str());
      }
    }

    goto apply_newNode;
  }

  // replace primitives with assignments
  if (type == AST_PRIMITIVE) {
    if (children.size() < 2)
      input_error("Insufficient number of arguments for primitive `%s'!\n",
                  str.c_str());

    std::vector<AstNode *> children_list;
    for (auto child : children) {
      // log_assert(child->type == AST_ARGUMENT);
      // log_assert(child->children.size() == 1);
      children_list.push_back(child->children[0]);
      child->children.clear();
      delete child;
    }
    children.clear();

    if (str == "bufif0" || str == "bufif1" || str == "notif0" ||
        str == "notif1") {
      if (children_list.size() != 3)
        input_error("Invalid number of arguments for primitive `%s'!\n",
                    str.c_str());

      std::vector<RTLIL::State> z_const(1, RTLIL::State::Sz);

      AstNode *mux_input = children_list.at(1);
      if (str == "notif0" || str == "notif1") {
        mux_input = new AstNode(AST_BIT_NOT, mux_input);
      }
      AstNode *node = new AstNode(AST_TERNARY, children_list.at(2));
      if (str == "bufif0") {
        node->children.push_back(AstNode::mkconst_bits(z_const, false));
        node->children.push_back(mux_input);
      } else {
        node->children.push_back(mux_input);
        node->children.push_back(AstNode::mkconst_bits(z_const, false));
      }

      str.clear();
      type = AST_ASSIGN;
      children.push_back(children_list.at(0));
      children.back()->was_checked = true;
      children.push_back(node);
      fixup_hierarchy_flags();
      did_something = true;
    } else if (str == "buf" || str == "not") {
      AstNode *input = children_list.back();
      if (str == "not")
        input = new AstNode(AST_BIT_NOT, input);

      newNode = new AstNode(AST_GENBLOCK);
      for (auto it = children_list.begin();
           it != std::prev(children_list.end()); it++) {
        newNode->children.push_back(
            new AstNode(AST_ASSIGN, *it, input->clone()));
        newNode->children.back()->was_checked = true;
      }
      delete input;

      did_something = true;
    } else {
      AstNodeType op_type = AST_NONE;
      bool invert_results = false;

      if (str == "and")
        op_type = AST_BIT_AND;
      if (str == "nand")
        op_type = AST_BIT_AND, invert_results = true;
      if (str == "or")
        op_type = AST_BIT_OR;
      if (str == "nor")
        op_type = AST_BIT_OR, invert_results = true;
      if (str == "xor")
        op_type = AST_BIT_XOR;
      if (str == "xnor")
        op_type = AST_BIT_XOR, invert_results = true;
      // log_assert(op_type != AST_NONE);

      AstNode *node = children_list[1];
      if (op_type != AST_POS)
        for (size_t i = 2; i < children_list.size(); i++) {
          node = new AstNode(op_type, node, children_list[i]);
          node->location = location;
        }
      if (invert_results)
        node = new AstNode(AST_BIT_NOT, node);

      str.clear();
      type = AST_ASSIGN;
      children.push_back(children_list[0]);
      children.back()->was_checked = true;
      children.push_back(node);
      fixup_hierarchy_flags();
      did_something = true;
    }
  }

  // replace dynamic ranges in left-hand side expressions (e.g. "foo[bar] <=
  // 1'b1;") with either a big case block that selects the correct single-bit
  // assignment, or mask and shift operations.
  if (type == AST_ASSIGN_EQ || type == AST_ASSIGN_LE) {
    if (children[0]->type != AST_IDENTIFIER ||
        children[0]->children.size() == 0)
      goto skip_dynamic_range_lvalue_expansion;
    if (children[0]->children[0]->range_valid || did_something)
      goto skip_dynamic_range_lvalue_expansion;
    if (children[0]->id2ast == NULL || children[0]->id2ast->type != AST_WIRE)
      goto skip_dynamic_range_lvalue_expansion;
    if (!children[0]->id2ast->range_valid)
      goto skip_dynamic_range_lvalue_expansion;

    int source_width =
        children[0]->id2ast->range_left - children[0]->id2ast->range_right + 1;
    int source_offset = children[0]->id2ast->range_right;
    int result_width = 1;
    int stride = 1;
    AST::AstNode *member_node = get_struct_member(children[0]);
    if (member_node) {
      // Clamp chunk to range of member within struct/union.
      // log_assert(!source_offset && !children[0]->id2ast->range_swapped);
      source_width = member_node->range_left - member_node->range_right + 1;

      // When the (* nowrshmsk *) attribute is set, a CASE block is generated
      // below to select the indexed bit slice. When a multirange array is
      // indexed, the start of each possible slice is separated by the bit
      // stride of the last index dimension, and we can optimize the CASE block
      // accordingly. The dimension of the original array expression is saved in
      // the 'integer' field.
      int dims = children[0]->integer;
      stride = source_width;
      for (int dim = 0; dim < dims; dim++) {
        stride /= get_struct_range_width(member_node, dim);
      }
    }

    AstNode *shift_expr = NULL;
    AstNode *range = children[0]->children[0];

    if (!try_determine_range_width(range, result_width))
      input_error(
          "Unsupported expression on dynamic range select on signal `%s'!\n",
          str.c_str());

    if (range->children.size() >= 2)
      shift_expr = range->children[1]->clone();
    else
      shift_expr = range->children[0]->clone();

    bool use_case_method = false;

    if (children[0]->id2ast->attributes.count(ID::nowrshmsk)) {
      AstNode *node = children[0]->id2ast->attributes.at(ID::nowrshmsk);
      while (node->simplify(true, stage, -1, false)) {
      }
      if (node->type != AST_CONSTANT)
        input_error("Non-constant value for `nowrshmsk' attribute on `%s'!\n",
                    children[0]->id2ast->str.c_str());
      if (node->asAttrConst().as_bool())
        use_case_method = true;
    }

    if (!use_case_method && current_always->detect_latch(children[0]->str))
      use_case_method = true;

    if (use_case_method) {
      // big case block

      did_something = true;
      newNode = new AstNode(AST_CASE, shift_expr);
      for (int i = 0; i < source_width; i += stride) {
        int start_bit = source_offset + i;
        int end_bit = std::min(start_bit + result_width, source_width) - 1;
        AstNode *cond = new AstNode(AST_COND, mkconst_int(start_bit, true));
        AstNode *lvalue = children[0]->clone();
        lvalue->delete_children();
        if (member_node)
          lvalue->set_attribute(ID::wiretype, member_node->clone());
        lvalue->children.push_back(new AstNode(AST_RANGE,
                                               mkconst_int(end_bit, true),
                                               mkconst_int(start_bit, true)));
        cond->children.push_back(new AstNode(
            AST_BLOCK, new AstNode(type, lvalue, children[1]->clone())));
        newNode->children.push_back(cond);
      }
    } else {
      // mask and shift operations, disabled for now

      AstNode *wire_mask = new AstNode(
          AST_WIRE, new AstNode(AST_RANGE, mkconst_int(source_width - 1, true),
                                mkconst_int(0, true)));
      wire_mask->str = stringf("$bitselwrite$mask$%s:%d$%d",
                               RTLIL::encode_filename(filename).c_str(),
                               location.first_line, autoidx++);
      wire_mask->set_attribute(ID::nosync, AstNode::mkconst_int(1, false));
      wire_mask->is_logic = true;
      while (wire_mask->simplify(true, 1, -1, false)) {
      }
      current_ast_mod->children.push_back(wire_mask);

      AstNode *wire_data = new AstNode(
          AST_WIRE, new AstNode(AST_RANGE, mkconst_int(source_width - 1, true),
                                mkconst_int(0, true)));
      wire_data->str = stringf("$bitselwrite$data$%s:%d$%d",
                               RTLIL::encode_filename(filename).c_str(),
                               location.first_line, autoidx++);
      wire_data->set_attribute(ID::nosync, AstNode::mkconst_int(1, false));
      wire_data->is_logic = true;
      while (wire_data->simplify(true, 1, -1, false)) {
      }
      current_ast_mod->children.push_back(wire_data);

      int shamt_width_hint = -1;
      bool shamt_sign_hint = true;
      shift_expr->detectSignWidth(shamt_width_hint, shamt_sign_hint);

      AstNode *wire_sel = new AstNode(
          AST_WIRE,
          new AstNode(AST_RANGE, mkconst_int(shamt_width_hint - 1, true),
                      mkconst_int(0, true)));
      wire_sel->str = stringf("$bitselwrite$sel$%s:%d$%d",
                              RTLIL::encode_filename(filename).c_str(),
                              location.first_line, autoidx++);
      wire_sel->set_attribute(ID::nosync, AstNode::mkconst_int(1, false));
      wire_sel->is_logic = true;
      wire_sel->is_signed = shamt_sign_hint;
      while (wire_sel->simplify(true, 1, -1, false)) {
      }
      current_ast_mod->children.push_back(wire_sel);

      did_something = true;
      newNode = new AstNode(AST_BLOCK);

      AstNode *lvalue = children[0]->clone();
      lvalue->delete_children();
      if (member_node)
        lvalue->set_attribute(ID::wiretype, member_node->clone());

      AstNode *ref_mask = new AstNode(AST_IDENTIFIER);
      ref_mask->str = wire_mask->str;
      ref_mask->id2ast = wire_mask;
      ref_mask->was_checked = true;

      AstNode *ref_data = new AstNode(AST_IDENTIFIER);
      ref_data->str = wire_data->str;
      ref_data->id2ast = wire_data;
      ref_data->was_checked = true;

      AstNode *ref_sel = new AstNode(AST_IDENTIFIER);
      ref_sel->str = wire_sel->str;
      ref_sel->id2ast = wire_sel;
      ref_sel->was_checked = true;

      AstNode *old_data = lvalue->clone();
      if (type == AST_ASSIGN_LE)
        old_data->lookahead = true;

      AstNode *s = new AstNode(AST_ASSIGN_EQ, ref_sel->clone(), shift_expr);
      newNode->children.push_back(s);

      AstNode *shamt = ref_sel;

      // convert to signed while preserving the sign and value
      shamt = new AstNode(AST_CAST_SIZE,
                          mkconst_int(shamt_width_hint + 1, true), shamt);
      shamt = new AstNode(AST_TO_SIGNED, shamt);

      // offset the shift amount by the lower bound of the dimension
      int start_bit = source_offset;
      shamt = new AstNode(AST_SUB, shamt, mkconst_int(start_bit, true));

      // reflect the shift amount if the dimension is swapped
      if (children[0]->id2ast->range_swapped)
        shamt = new AstNode(
            AST_SUB, mkconst_int(source_width - result_width, true), shamt);

      // AST_SHIFT uses negative amounts for shifting left
      shamt = new AstNode(AST_NEG, shamt);

      AstNode *t;

      t = mkconst_bits(std::vector<RTLIL::State>(result_width, State::S1),
                       false);
      t = new AstNode(AST_SHIFT, t, shamt->clone());
      t = new AstNode(AST_ASSIGN_EQ, ref_mask->clone(), t);
      newNode->children.push_back(t);

      t = new AstNode(
          AST_BIT_AND,
          mkconst_bits(std::vector<RTLIL::State>(result_width, State::S1),
                       false),
          children[1]->clone());
      t = new AstNode(AST_SHIFT, t, shamt);
      t = new AstNode(AST_ASSIGN_EQ, ref_data->clone(), t);
      newNode->children.push_back(t);

      t = new AstNode(AST_BIT_AND, old_data,
                      new AstNode(AST_BIT_NOT, ref_mask));
      t = new AstNode(AST_BIT_OR, t, ref_data);
      t = new AstNode(type, lvalue, t);
      newNode->children.push_back(t);

      newNode->fixup_hierarchy_flags(true);
    }

    goto apply_newNode;
  }
skip_dynamic_range_lvalue_expansion:;

  if (stage > 1 &&
      (type == AST_ASSERT || type == AST_ASSUME || type == AST_LIVE ||
       type == AST_FAIR || type == AST_COVER) &&
      current_block != NULL) {
    std::stringstream sstr;
    sstr << "$formal$" << RTLIL::encode_filename(filename) << ":"
         << location.first_line << "$" << (autoidx++);
    std::string id_check = sstr.str() + "_CHECK", id_en = sstr.str() + "_EN";

    AstNode *wire_check = new AstNode(AST_WIRE);
    wire_check->str = id_check;
    wire_check->was_checked = true;
    current_ast_mod->children.push_back(wire_check);
    current_scope[wire_check->str] = wire_check;
    while (wire_check->simplify(true, 1, -1, false)) {
    }

    AstNode *wire_en = new AstNode(AST_WIRE);
    wire_en->str = id_en;
    wire_en->was_checked = true;
    current_ast_mod->children.push_back(wire_en);
    if (current_always_clocked) {
      current_ast_mod->children.push_back(new AstNode(
          AST_INITIAL,
          new AstNode(AST_BLOCK,
                      new AstNode(AST_ASSIGN_LE, new AstNode(AST_IDENTIFIER),
                                  AstNode::mkconst_int(0, false, 1)))));
      current_ast_mod->children.back()
          ->children[0]
          ->children[0]
          ->children[0]
          ->str = id_en;
      current_ast_mod->children.back()
          ->children[0]
          ->children[0]
          ->children[0]
          ->was_checked = true;
    }
    current_scope[wire_en->str] = wire_en;
    while (wire_en->simplify(true, 1, -1, false)) {
    }

    AstNode *check_defval;
    if (type == AST_LIVE || type == AST_FAIR) {
      check_defval = new AstNode(AST_REDUCE_BOOL, children[0]->clone());
    } else {
      std::vector<RTLIL::State> x_bit;
      x_bit.push_back(RTLIL::State::Sx);
      check_defval = mkconst_bits(x_bit, false);
    }

    AstNode *assign_check =
        new AstNode(AST_ASSIGN_LE, new AstNode(AST_IDENTIFIER), check_defval);
    assign_check->children[0]->str = id_check;
    assign_check->children[0]->was_checked = true;

    AstNode *assign_en = new AstNode(AST_ASSIGN_LE, new AstNode(AST_IDENTIFIER),
                                     mkconst_int(0, false, 1));
    assign_en->children[0]->str = id_en;
    assign_en->children[0]->was_checked = true;

    AstNode *default_signals = new AstNode(AST_BLOCK);
    default_signals->children.push_back(assign_check);
    default_signals->children.push_back(assign_en);
    current_top_block->children.insert(current_top_block->children.begin(),
                                       default_signals);

    if (type == AST_LIVE || type == AST_FAIR) {
      assign_check = nullptr;
    } else {
      assign_check =
          new AstNode(AST_ASSIGN_LE, new AstNode(AST_IDENTIFIER),
                      new AstNode(AST_REDUCE_BOOL, children[0]->clone()));
      assign_check->children[0]->str = id_check;
      assign_check->children[0]->was_checked = true;
      assign_check->fixup_hierarchy_flags();
    }

    if (current_always == nullptr || current_always->type != AST_INITIAL) {
      assign_en = new AstNode(AST_ASSIGN_LE, new AstNode(AST_IDENTIFIER),
                              mkconst_int(1, false, 1));
    } else {
      assign_en = new AstNode(AST_ASSIGN_LE, new AstNode(AST_IDENTIFIER),
                              new AstNode(AST_FCALL));
      assign_en->children[1]->str = "\\$initstate";
    }
    assign_en->children[0]->str = id_en;
    assign_en->children[0]->was_checked = true;
    assign_en->fixup_hierarchy_flags();

    newNode = new AstNode(AST_BLOCK);
    if (assign_check != nullptr)
      newNode->children.push_back(assign_check);
    newNode->children.push_back(assign_en);

    AstNode *assertnode = new AstNode(type);
    assertnode->location = location;
    assertnode->str = str;
    assertnode->children.push_back(new AstNode(AST_IDENTIFIER));
    assertnode->children.push_back(new AstNode(AST_IDENTIFIER));
    assertnode->children[0]->str = id_check;
    assertnode->children[1]->str = id_en;
    assertnode->attributes.swap(attributes);
    current_ast_mod->children.push_back(assertnode);

    goto apply_newNode;
  }

  if (stage > 1 &&
      (type == AST_ASSERT || type == AST_ASSUME || type == AST_LIVE ||
       type == AST_FAIR || type == AST_COVER) &&
      children.size() == 1) {
    children.push_back(mkconst_int(1, false, 1));
    fixup_hierarchy_flags();
    did_something = true;
  }

  // found right-hand side identifier for memory -> replace with memory read
  // port
  if (stage > 1 && type == AST_IDENTIFIER && id2ast != NULL &&
      id2ast->type == AST_MEMORY && !in_lvalue && children.size() == 1 &&
      children[0]->type == AST_RANGE && children[0]->children.size() == 1) {
    newNode = new AstNode(AST_MEMRD, children[0]->children[0]->clone());
    newNode->str = str;
    newNode->id2ast = id2ast;
    goto apply_newNode;
  }

  // assignment with nontrivial member in left-hand concat expression -> split
  // assignment
  if ((type == AST_ASSIGN_EQ || type == AST_ASSIGN_LE) &&
      children[0]->type == AST_CONCAT && width_hint > 0) {
    bool found_nontrivial_member = false;

    for (auto child : children[0]->children) {
      if (child->type == AST_IDENTIFIER && child->id2ast != NULL &&
          child->id2ast->type == AST_MEMORY)
        found_nontrivial_member = true;
    }

    if (found_nontrivial_member) {
      newNode = new AstNode(AST_BLOCK);

      AstNode *wire_tmp = new AstNode(
          AST_WIRE, new AstNode(AST_RANGE, mkconst_int(width_hint - 1, true),
                                mkconst_int(0, true)));
      wire_tmp->str = stringf("$splitcmplxassign$%s:%d$%d",
                              RTLIL::encode_filename(filename).c_str(),
                              location.first_line, autoidx++);
      current_ast_mod->children.push_back(wire_tmp);
      current_scope[wire_tmp->str] = wire_tmp;
      wire_tmp->set_attribute(ID::nosync, AstNode::mkconst_int(1, false));
      while (wire_tmp->simplify(true, 1, -1, false)) {
      }
      wire_tmp->is_logic = true;

      AstNode *wire_tmp_id = new AstNode(AST_IDENTIFIER);
      wire_tmp_id->str = wire_tmp->str;

      newNode->children.push_back(
          new AstNode(AST_ASSIGN_EQ, wire_tmp_id, children[1]->clone()));
      newNode->children.back()->was_checked = true;

      int cursor = 0;
      for (auto child : children[0]->children) {
        int child_width_hint = -1;
        bool child_sign_hint = true;
        child->detectSignWidth(child_width_hint, child_sign_hint);

        AstNode *rhs = wire_tmp_id->clone();
        rhs->children.push_back(new AstNode(
            AST_RANGE,
            AstNode::mkconst_int(cursor + child_width_hint - 1, true),
            AstNode::mkconst_int(cursor, true)));
        newNode->children.push_back(new AstNode(type, child->clone(), rhs));

        cursor += child_width_hint;
      }

      goto apply_newNode;
    }
  }

  // assignment with memory in left-hand side expression -> replace with memory
  // write port
  if (stage > 1 && (type == AST_ASSIGN_EQ || type == AST_ASSIGN_LE) &&
      children[0]->type == AST_IDENTIFIER && children[0]->id2ast &&
      children[0]->id2ast->type == AST_MEMORY &&
      children[0]->id2ast->children.size() >= 2 &&
      children[0]->id2ast->children[0]->range_valid &&
      children[0]->id2ast->children[1]->range_valid &&
      (children[0]->children.size() == 1 ||
       children[0]->children.size() == 2) &&
      children[0]->children[0]->type == AST_RANGE) {
    std::stringstream sstr;
    sstr << "$memwr$" << children[0]->str << "$"
         << RTLIL::encode_filename(filename) << ":" << location.first_line
         << "$" << (autoidx++);
    std::string id_addr = sstr.str() + "_ADDR", id_data = sstr.str() + "_DATA",
                id_en = sstr.str() + "_EN";

    int mem_width, mem_size, addr_bits;
    bool mem_signed = children[0]->id2ast->is_signed;
    children[0]->id2ast->meminfo(mem_width, mem_size, addr_bits);

    newNode = new AstNode(AST_BLOCK);
    AstNode *defNode = new AstNode(AST_BLOCK);

    int data_range_left = children[0]->id2ast->children[0]->range_left;
    int data_range_right = children[0]->id2ast->children[0]->range_right;
    int mem_data_range_offset = std::min(data_range_left, data_range_right);

    int addr_width_hint = -1;
    bool addr_sign_hint = true;
    children[0]->children[0]->children[0]->detectSignWidthWorker(
        addr_width_hint, addr_sign_hint);
    addr_bits = std::max(addr_bits, addr_width_hint);

    std::vector<RTLIL::State> x_bits_addr, x_bits_data, set_bits_en;
    for (int i = 0; i < addr_bits; i++)
      x_bits_addr.push_back(RTLIL::State::Sx);
    for (int i = 0; i < mem_width; i++)
      x_bits_data.push_back(RTLIL::State::Sx);
    for (int i = 0; i < mem_width; i++)
      set_bits_en.push_back(RTLIL::State::S1);

    AstNode *node_addr = nullptr;
    if (children[0]->children[0]->children[0]->isConst()) {
      node_addr = children[0]->children[0]->children[0]->clone();
    } else {
      AstNode *wire_addr = new AstNode(
          AST_WIRE, new AstNode(AST_RANGE, mkconst_int(addr_bits - 1, true),
                                mkconst_int(0, true)));
      wire_addr->str = id_addr;
      wire_addr->was_checked = true;
      current_ast_mod->children.push_back(wire_addr);
      current_scope[wire_addr->str] = wire_addr;
      while (wire_addr->simplify(true, 1, -1, false)) {
      }

      AstNode *assign_addr =
          new AstNode(AST_ASSIGN_EQ, new AstNode(AST_IDENTIFIER),
                      mkconst_bits(x_bits_addr, false));
      assign_addr->children[0]->str = id_addr;
      assign_addr->children[0]->was_checked = true;
      defNode->children.push_back(assign_addr);

      assign_addr = new AstNode(AST_ASSIGN_EQ, new AstNode(AST_IDENTIFIER),
                                children[0]->children[0]->children[0]->clone());
      assign_addr->children[0]->str = id_addr;
      assign_addr->children[0]->was_checked = true;
      newNode->children.push_back(assign_addr);

      node_addr = new AstNode(AST_IDENTIFIER);
      node_addr->str = id_addr;
    }

    AstNode *node_data = nullptr;
    if (children[0]->children.size() == 1 && children[1]->isConst()) {
      node_data = children[1]->clone();
    } else {
      AstNode *wire_data = new AstNode(
          AST_WIRE, new AstNode(AST_RANGE, mkconst_int(mem_width - 1, true),
                                mkconst_int(0, true)));
      wire_data->str = id_data;
      wire_data->was_checked = true;
      wire_data->is_signed = mem_signed;
      current_ast_mod->children.push_back(wire_data);
      current_scope[wire_data->str] = wire_data;
      while (wire_data->simplify(true, 1, -1, false)) {
      }

      AstNode *assign_data =
          new AstNode(AST_ASSIGN_EQ, new AstNode(AST_IDENTIFIER),
                      mkconst_bits(x_bits_data, false));
      assign_data->children[0]->str = id_data;
      assign_data->children[0]->was_checked = true;
      defNode->children.push_back(assign_data);

      node_data = new AstNode(AST_IDENTIFIER);
      node_data->str = id_data;
    }

    AstNode *wire_en = new AstNode(
        AST_WIRE, new AstNode(AST_RANGE, mkconst_int(mem_width - 1, true),
                              mkconst_int(0, true)));
    wire_en->str = id_en;
    wire_en->was_checked = true;
    current_ast_mod->children.push_back(wire_en);
    current_scope[wire_en->str] = wire_en;
    while (wire_en->simplify(true, 1, -1, false)) {
    }

    AstNode *assign_en_first =
        new AstNode(AST_ASSIGN_EQ, new AstNode(AST_IDENTIFIER),
                    mkconst_int(0, false, mem_width));
    assign_en_first->children[0]->str = id_en;
    assign_en_first->children[0]->was_checked = true;
    defNode->children.push_back(assign_en_first);

    AstNode *node_en = new AstNode(AST_IDENTIFIER);
    node_en->str = id_en;

    if (!defNode->children.empty())
      current_top_block->children.insert(current_top_block->children.begin(),
                                         defNode);
    else
      delete defNode;

    AstNode *assign_data = nullptr;
    AstNode *assign_en = nullptr;
    if (children[0]->children.size() == 2) {
      if (children[0]->children[1]->range_valid) {
        int offset = children[0]->children[1]->range_right;
        int width = children[0]->children[1]->range_left - offset + 1;
        offset -= mem_data_range_offset;

        std::vector<RTLIL::State> padding_x(offset, RTLIL::State::Sx);

        assign_data =
            new AstNode(AST_ASSIGN_EQ, new AstNode(AST_IDENTIFIER),
                        new AstNode(AST_CONCAT, mkconst_bits(padding_x, false),
                                    children[1]->clone()));
        assign_data->children[0]->str = id_data;
        assign_data->children[0]->was_checked = true;

        for (int i = 0; i < mem_width; i++)
          set_bits_en[i] = offset <= i && i < offset + width ? RTLIL::State::S1
                                                             : RTLIL::State::S0;
        assign_en = new AstNode(AST_ASSIGN_EQ, new AstNode(AST_IDENTIFIER),
                                mkconst_bits(set_bits_en, false));
        assign_en->children[0]->str = id_en;
        assign_en->children[0]->was_checked = true;
      } else {
        AstNode *the_range = children[0]->children[1];
        AstNode *offset_ast;
        int width;

        if (!try_determine_range_width(the_range, width))
          input_error("Unsupported expression on dynamic range select on "
                      "signal `%s'!\n",
                      str.c_str());

        if (the_range->children.size() >= 2)
          offset_ast = the_range->children[1]->clone();
        else
          offset_ast = the_range->children[0]->clone();

        if (mem_data_range_offset)
          offset_ast = new AstNode(AST_SUB, offset_ast,
                                   mkconst_int(mem_data_range_offset, true));

        assign_data =
            new AstNode(AST_ASSIGN_EQ, new AstNode(AST_IDENTIFIER),
                        new AstNode(AST_SHIFT_LEFT, children[1]->clone(),
                                    offset_ast->clone()));
        assign_data->children[0]->str = id_data;
        assign_data->children[0]->was_checked = true;

        for (int i = 0; i < mem_width; i++)
          set_bits_en[i] = i < width ? RTLIL::State::S1 : RTLIL::State::S0;
        assign_en = new AstNode(AST_ASSIGN_EQ, new AstNode(AST_IDENTIFIER),
                                new AstNode(AST_SHIFT_LEFT,
                                            mkconst_bits(set_bits_en, false),
                                            offset_ast->clone()));
        assign_en->children[0]->str = id_en;
        assign_en->children[0]->was_checked = true;
        delete offset_ast;
      }
    } else {
      if (!(children[0]->children.size() == 1 && children[1]->isConst())) {
        assign_data = new AstNode(AST_ASSIGN_EQ, new AstNode(AST_IDENTIFIER),
                                  children[1]->clone());
        assign_data->children[0]->str = id_data;
        assign_data->children[0]->was_checked = true;
      }

      assign_en = new AstNode(AST_ASSIGN_EQ, new AstNode(AST_IDENTIFIER),
                              mkconst_bits(set_bits_en, false));
      assign_en->children[0]->str = id_en;
      assign_en->children[0]->was_checked = true;
    }
    if (assign_data)
      newNode->children.push_back(assign_data);
    if (assign_en)
      newNode->children.push_back(assign_en);

    AstNode *wrnode;
    if (current_always->type == AST_INITIAL)
      wrnode = new AstNode(AST_MEMINIT, node_addr, node_data, node_en,
                           mkconst_int(1, false));
    else
      wrnode = new AstNode(AST_MEMWR, node_addr, node_data, node_en);
    wrnode->str = children[0]->str;
    wrnode->id2ast = children[0]->id2ast;
    wrnode->location = location;
    if (wrnode->type == AST_MEMWR) {
      int portid = current_memwr_count[wrnode->str]++;
      wrnode->children.push_back(mkconst_int(portid, false));
      std::vector<RTLIL::State> priority_mask;
      for (int i = 0; i < portid; i++) {
        bool has_prio = current_memwr_visible[wrnode->str].count(i);
        priority_mask.push_back(State(has_prio));
      }
      wrnode->children.push_back(mkconst_bits(priority_mask, false));
      current_memwr_visible[wrnode->str].insert(portid);
      current_always->children.push_back(wrnode);
    } else {
      current_ast_mod->children.push_back(wrnode);
    }

    if (newNode->children.empty()) {
      delete newNode;
      newNode = new AstNode();
    }
    goto apply_newNode;
  }

  // replace function and task calls with the code from the function or task
  if ((type == AST_FCALL || type == AST_TCALL) && !str.empty()) {
    if (type == AST_FCALL) {
      if (str == "\\$initstate") {
        int myidx = autoidx++;

        AstNode *wire = new AstNode(AST_WIRE);
        wire->str = stringf("$initstate$%d_wire", myidx);
        current_ast_mod->children.push_back(wire);
        while (wire->simplify(true, 1, -1, false)) {
        }

        AstNode *cell =
            new AstNode(AST_CELL, new AstNode(AST_CELLTYPE),
                        new AstNode(AST_ARGUMENT, new AstNode(AST_IDENTIFIER)));
        cell->str = stringf("$initstate$%d", myidx);
        cell->children[0]->str = "$initstate";
        cell->children[1]->str = "\\Y";
        cell->children[1]->children[0]->str = wire->str;
        cell->children[1]->children[0]->id2ast = wire;
        current_ast_mod->children.push_back(cell);
        while (cell->simplify(true, 1, -1, false)) {
        }

        newNode = new AstNode(AST_IDENTIFIER);
        newNode->str = wire->str;
        newNode->id2ast = wire;
        goto apply_newNode;
      }

      if (str == "\\$past") {
        if (width_hint < 0)
          goto replace_fcall_later;

        int num_steps = 1;

        if (GetSize(children) != 1 && GetSize(children) != 2)
          input_error("System function %s got %d arguments, expected 1 or 2.\n",
                      RTLIL::unescape_id(str).c_str(), int(children.size()));

        if (!current_always_clocked)
          input_error("System function %s is only allowed in clocked blocks.\n",
                      RTLIL::unescape_id(str).c_str());

        if (GetSize(children) == 2) {
          AstNode *buf = children[1]->clone();
          while (buf->simplify(true, stage, -1, false)) {
          }
          if (buf->type != AST_CONSTANT)
            input_error("Failed to evaluate system function `%s' with "
                        "non-constant value.\n",
                        str.c_str());

          num_steps = buf->asInt(true);
          delete buf;
        }

        AstNode *block = nullptr;

        for (auto child : current_always->children)
          if (child->type == AST_BLOCK)
            block = child;

        // log_assert(block != nullptr);

        if (num_steps == 0) {
          newNode = children[0]->clone();
          goto apply_newNode;
        }

        int myidx = autoidx++;
        AstNode *outreg = nullptr;

        for (int i = 0; i < num_steps; i++) {
          AstNode *reg = new AstNode(
              AST_WIRE,
              new AstNode(AST_RANGE, mkconst_int(width_hint - 1, true),
                          mkconst_int(0, true)));

          reg->str = stringf("$past$%s:%d$%d$%d",
                             RTLIL::encode_filename(filename).c_str(),
                             location.first_line, myidx, i);
          reg->is_reg = true;
          reg->is_signed = sign_hint;

          current_ast_mod->children.push_back(reg);

          while (reg->simplify(true, 1, -1, false)) {
          }

          AstNode *regid = new AstNode(AST_IDENTIFIER);
          regid->str = reg->str;
          regid->id2ast = reg;
          regid->was_checked = true;

          AstNode *rhs = nullptr;

          if (outreg == nullptr) {
            rhs = children.at(0)->clone();
          } else {
            rhs = new AstNode(AST_IDENTIFIER);
            rhs->str = outreg->str;
            rhs->id2ast = outreg;
          }

          block->children.push_back(new AstNode(AST_ASSIGN_LE, regid, rhs));
          outreg = reg;
        }

        newNode = new AstNode(AST_IDENTIFIER);
        newNode->str = outreg->str;
        newNode->id2ast = outreg;
        goto apply_newNode;
      }

      if (str == "\\$stable" || str == "\\$rose" || str == "\\$fell" ||
          str == "\\$changed") {
        if (GetSize(children) != 1)
          input_error("System function %s got %d arguments, expected 1.\n",
                      RTLIL::unescape_id(str).c_str(), int(children.size()));

        if (!current_always_clocked)
          input_error("System function %s is only allowed in clocked blocks.\n",
                      RTLIL::unescape_id(str).c_str());

        AstNode *present = children.at(0)->clone();
        AstNode *past = clone();
        past->str = "\\$past";

        if (str == "\\$stable")
          newNode = new AstNode(AST_EQ, past, present);

        else if (str == "\\$changed")
          newNode = new AstNode(AST_NE, past, present);

        else if (str == "\\$rose")
          newNode = new AstNode(
              AST_LOGIC_AND,
              new AstNode(AST_LOGIC_NOT, new AstNode(AST_BIT_AND, past,
                                                     mkconst_int(1, false))),
              new AstNode(AST_BIT_AND, present, mkconst_int(1, false)));

        else if (str == "\\$fell")
          newNode = new AstNode(
              AST_LOGIC_AND,
              new AstNode(AST_BIT_AND, past, mkconst_int(1, false)),
              new AstNode(AST_LOGIC_NOT, new AstNode(AST_BIT_AND, present,
                                                     mkconst_int(1, false))));

        else {
          // log_abort();
          exit(1);
        }

        goto apply_newNode;
      }

      // $anyconst and $anyseq are mapped in AstNode::genRTLIL()
      if (str == "\\$anyconst" || str == "\\$anyseq" || str == "\\$allconst" ||
          str == "\\$allseq") {
        recursion_counter--;
        return false;
      }

      if (str == "\\$clog2") {
        if (children.size() != 1)
          input_error("System function %s got %d arguments, expected 1.\n",
                      RTLIL::unescape_id(str).c_str(), int(children.size()));

        AstNode *buf = children[0]->clone();
        while (buf->simplify(true, stage, width_hint, sign_hint)) {
        }
        if (buf->type != AST_CONSTANT)
          input_error("Failed to evaluate system function `%s' with "
                      "non-constant value.\n",
                      str.c_str());

        RTLIL::Const arg_value = buf->bitsAsConst();
        if (arg_value.as_bool())
          arg_value = const_sub(arg_value, 1, false, false, GetSize(arg_value));
        delete buf;

        uint32_t result = 0;
        for (size_t i = 0; i < arg_value.bits.size(); i++)
          if (arg_value.bits.at(i) == RTLIL::State::S1)
            result = i + 1;

        newNode = mkconst_int(result, true);
        goto apply_newNode;
      }

      if (str == "\\$size" || str == "\\$bits" || str == "\\$high" ||
          str == "\\$low" || str == "\\$left" || str == "\\$right") {
        int dim = 1;
        if (str == "\\$bits") {
          if (children.size() != 1)
            input_error("System function %s got %d arguments, expected 1.\n",
                        RTLIL::unescape_id(str).c_str(), int(children.size()));
        } else {
          if (children.size() != 1 && children.size() != 2)
            input_error(
                "System function %s got %d arguments, expected 1 or 2.\n",
                RTLIL::unescape_id(str).c_str(), int(children.size()));
          if (children.size() == 2) {
            AstNode *buf = children[1]->clone();
            // Evaluate constant expression
            while (buf->simplify(true, stage, width_hint, sign_hint)) {
            }
            dim = buf->asInt(false);
            delete buf;
          }
        }
        AstNode *buf = children[0]->clone();
        int mem_depth = 1;
        int result, high = 0, low = 0, left = 0, right = 0,
                    width = 1; // defaults for a simple wire
        AstNode *id_ast = NULL;

        // Is this needed?
        // while (buf->simplify(true, false, stage, width_hint, sign_hint,
        // false)) { }
        buf->detectSignWidth(width_hint, sign_hint);

        if (buf->type == AST_IDENTIFIER) {
          id_ast = buf->id2ast;
          if (id_ast == NULL && current_scope.count(buf->str))
            id_ast = current_scope.at(buf->str);
          if (!id_ast)
            input_error(
                "Failed to resolve identifier %s for width detection!\n",
                buf->str.c_str());

          // Check for item in packed struct / union
          AST::AstNode *item_node = get_struct_member(buf);
          if (id_ast->type == AST_WIRE && item_node) {
            // The dimension of the original array expression is saved in the
            // 'integer' field
            dim += buf->integer;
            if (item_node->multirange_dimensions.empty()) {
              if (dim != 1)
                input_error("Dimension %d out of range in `%s', as it only has "
                            "one dimension!\n",
                            dim, item_node->str.c_str());
              left = high = item_node->range_left;
              right = low = item_node->range_right;
            } else {
              int dims = GetSize(item_node->multirange_dimensions) / 2;
              if (dim < 1 || dim > dims)
                input_error("Dimension %d out of range in `%s', as it only has "
                            "dimensions 1..%d!\n",
                            dim, item_node->str.c_str(), dims);
              right = low = get_struct_range_offset(item_node, dim - 1);
              left = high =
                  low + get_struct_range_width(item_node, dim - 1) - 1;
              if (item_node->multirange_swapped[dim - 1]) {
                std::swap(left, right);
              }
              for (int i = dim; i < dims; i++) {
                mem_depth *= get_struct_range_width(item_node, i);
              }
            }
          }
          // Otherwise, we have 4 cases:
          // wire x;                ==> AST_WIRE, no AST_RANGE children
          // wire [1:0]x;           ==> AST_WIRE, AST_RANGE children
          // wire [1:0]x[1:0];      ==> AST_MEMORY, two AST_RANGE children (1st
          // for packed, 2nd for unpacked) wire [1:0]x[1:0][1:0]; ==>
          // AST_MEMORY, one AST_RANGE child (0) for packed, then AST_MULTIRANGE
          // child (1) for unpacked (updated: actually by the time we are here,
          // AST_MULTIRANGE is converted into one big AST_RANGE) case 0 handled
          // by default
          else if ((id_ast->type == AST_WIRE || id_ast->type == AST_MEMORY) &&
                   id_ast->children.size() > 0) {
            // handle packed array left/right for case 1, and cases 2/3 when
            // requesting the last dimension (packed side)
            AstNode *wire_range = id_ast->children[0];
            left = wire_range->children[0]->integer;
            right = wire_range->children[1]->integer;
            high = std::max(left, right);
            low = std::min(left, right);
          }
          if (id_ast->type == AST_MEMORY) {
            // a slice of our identifier means we advance to the next dimension,
            // e.g. $size(a[3])
            if (buf->children.size() > 0) {
              // something is hanging below this identifier
              if (buf->children[0]->type == AST_RANGE && buf->integer == 0)
                // if integer == 0, this node was originally created as
                // AST_RANGE so it's dimension is 1
                dim++;
              // more than one range, e.g. $size(a[3][2])
              else // created an AST_MULTIRANGE, converted to AST_RANGE, but
                   // original dimension saved in 'integer' field
                dim += buf->integer; // increment by multirange size
            }

            // We got here only if the argument is a memory
            // Otherwise $size() and $bits() return the expression width
            AstNode *mem_range = id_ast->children[1];
            if (str == "\\$bits") {
              if (mem_range->type == AST_RANGE) {
                if (!mem_range->range_valid)
                  input_error("Failed to detect width of memory access `%s'!\n",
                              buf->str.c_str());
                mem_depth = mem_range->range_left - mem_range->range_right + 1;
              } else
                input_error("Unknown memory depth AST type in `%s'!\n",
                            buf->str.c_str());
            } else {
              // $size(), $left(), $right(), $high(), $low()
              int dims = 1;
              if (mem_range->type == AST_RANGE) {
                if (id_ast->multirange_dimensions.empty()) {
                  if (!mem_range->range_valid)
                    input_error(
                        "Failed to detect width of memory access `%s'!\n",
                        buf->str.c_str());
                  if (dim == 1) {
                    left = mem_range->range_right;
                    right = mem_range->range_left;
                    high = std::max(left, right);
                    low = std::min(left, right);
                  }
                } else {
                  dims = GetSize(id_ast->multirange_dimensions) / 2;
                  if (dim <= dims) {
                    width_hint = id_ast->multirange_dimensions[2 * dim - 1];
                    high = id_ast->multirange_dimensions[2 * dim - 2] +
                           id_ast->multirange_dimensions[2 * dim - 1] - 1;
                    low = id_ast->multirange_dimensions[2 * dim - 2];
                    if (id_ast->multirange_swapped[dim - 1]) {
                      left = low;
                      right = high;
                    } else {
                      right = low;
                      left = high;
                    }
                  } else if ((dim > dims + 1) || (dim < 0))
                    input_error("Dimension %d out of range in `%s', as it only "
                                "has dimensions 1..%d!\n",
                                dim, buf->str.c_str(), dims + 1);
                }
              } else {
                input_error("Unknown memory depth AST type in `%s'!\n",
                            buf->str.c_str());
              }
            }
          }
          width = high - low + 1;
        } else {
          width = width_hint;
        }
        delete buf;
        if (str == "\\$high")
          result = high;
        else if (str == "\\$low")
          result = low;
        else if (str == "\\$left")
          result = left;
        else if (str == "\\$right")
          result = right;
        else if (str == "\\$size")
          result = width;
        else { // str == "\\$bits"
          result = width * mem_depth;
        }
        newNode = mkconst_int(result, true);
        goto apply_newNode;
      }

      if (str == "\\$ln" || str == "\\$log10" || str == "\\$exp" ||
          str == "\\$sqrt" || str == "\\$pow" || str == "\\$floor" ||
          str == "\\$ceil" || str == "\\$sin" || str == "\\$cos" ||
          str == "\\$tan" || str == "\\$asin" || str == "\\$acos" ||
          str == "\\$atan" || str == "\\$atan2" || str == "\\$hypot" ||
          str == "\\$sinh" || str == "\\$cosh" || str == "\\$tanh" ||
          str == "\\$asinh" || str == "\\$acosh" || str == "\\$atanh" ||
          str == "\\$rtoi" || str == "\\$itor") {
        bool func_with_two_arguments =
            str == "\\$pow" || str == "\\$atan2" || str == "\\$hypot";
        double x = 0, y = 0;

        if (func_with_two_arguments) {
          if (children.size() != 2)
            input_error("System function %s got %d arguments, expected 2.\n",
                        RTLIL::unescape_id(str).c_str(), int(children.size()));
        } else {
          if (children.size() != 1)
            input_error("System function %s got %d arguments, expected 1.\n",
                        RTLIL::unescape_id(str).c_str(), int(children.size()));
        }

        if (children.size() >= 1) {
          while (children[0]->simplify(true, stage, width_hint, sign_hint)) {
          }
          if (!children[0]->isConst())
            input_error("Failed to evaluate system function `%s' with "
                        "non-constant argument.\n",
                        RTLIL::unescape_id(str).c_str());
          int child_width_hint = width_hint;
          bool child_sign_hint = sign_hint;
          children[0]->detectSignWidth(child_width_hint, child_sign_hint);
          x = children[0]->asReal(child_sign_hint);
        }

        if (children.size() >= 2) {
          while (children[1]->simplify(true, stage, width_hint, sign_hint)) {
          }
          if (!children[1]->isConst())
            input_error("Failed to evaluate system function `%s' with "
                        "non-constant argument.\n",
                        RTLIL::unescape_id(str).c_str());
          int child_width_hint = width_hint;
          bool child_sign_hint = sign_hint;
          children[1]->detectSignWidth(child_width_hint, child_sign_hint);
          y = children[1]->asReal(child_sign_hint);
        }

        if (str == "\\$rtoi") {
          newNode = AstNode::mkconst_int(x, true);
        } else {
          newNode = new AstNode(AST_REALVALUE);
          if (str == "\\$ln")
            newNode->realvalue = ::log(x);
          else if (str == "\\$log10")
            newNode->realvalue = ::log10(x);
          else if (str == "\\$exp")
            newNode->realvalue = ::exp(x);
          else if (str == "\\$sqrt")
            newNode->realvalue = ::sqrt(x);
          else if (str == "\\$pow")
            newNode->realvalue = ::pow(x, y);
          else if (str == "\\$floor")
            newNode->realvalue = ::floor(x);
          else if (str == "\\$ceil")
            newNode->realvalue = ::ceil(x);
          else if (str == "\\$sin")
            newNode->realvalue = ::sin(x);
          else if (str == "\\$cos")
            newNode->realvalue = ::cos(x);
          else if (str == "\\$tan")
            newNode->realvalue = ::tan(x);
          else if (str == "\\$asin")
            newNode->realvalue = ::asin(x);
          else if (str == "\\$acos")
            newNode->realvalue = ::acos(x);
          else if (str == "\\$atan")
            newNode->realvalue = ::atan(x);
          else if (str == "\\$atan2")
            newNode->realvalue = ::atan2(x, y);
          else if (str == "\\$hypot")
            newNode->realvalue = ::hypot(x, y);
          else if (str == "\\$sinh")
            newNode->realvalue = ::sinh(x);
          else if (str == "\\$cosh")
            newNode->realvalue = ::cosh(x);
          else if (str == "\\$tanh")
            newNode->realvalue = ::tanh(x);
          else if (str == "\\$asinh")
            newNode->realvalue = ::asinh(x);
          else if (str == "\\$acosh")
            newNode->realvalue = ::acosh(x);
          else if (str == "\\$atanh")
            newNode->realvalue = ::atanh(x);
          else if (str == "\\$itor")
            newNode->realvalue = x;
          else {
            // log_abort();
            exit(1);
          }
        }
        goto apply_newNode;
      }

      if (str == "\\$sformatf") {
        Fmt fmt = processFormat(stage, /*sformat_like=*/true);
        newNode = AstNode::mkconst_str(fmt.render());
        goto apply_newNode;
      }

      if (str == "\\$countbits") {
        if (children.size() < 2)
          input_error(
              "System function %s got %d arguments, expected at least 2.\n",
              RTLIL::unescape_id(str).c_str(), int(children.size()));

        std::vector<RTLIL::State> control_bits;

        // Determine which bits to count
        for (size_t i = 1; i < children.size(); i++) {
          AstNode *node = children[i];
          while (node->simplify(true, stage, -1, false)) {
          }
          if (node->type != AST_CONSTANT)
            input_error("Failed to evaluate system function `%s' with "
                        "non-constant control bit argument.\n",
                        str.c_str());
          if (node->bits.size() != 1)
            input_error("Failed to evaluate system function `%s' with control "
                        "bit width != 1.\n",
                        str.c_str());
          control_bits.push_back(node->bits[0]);
        }

        // Detect width of exp (first argument of $countbits)
        int exp_width = -1;
        bool exp_sign = false;
        AstNode *exp = children[0];
        exp->detectSignWidth(exp_width, exp_sign, NULL);

        newNode = mkconst_int(0, false);

        for (int i = 0; i < exp_width; i++) {
          // Generate nodes for:  exp << i >> ($size(exp) - 1)
          //                          ^^   ^^
          AstNode *lsh_node =
              new AstNode(AST_SHIFT_LEFT, exp->clone(), mkconst_int(i, false));
          AstNode *rsh_node = new AstNode(AST_SHIFT_RIGHT, lsh_node,
                                          mkconst_int(exp_width - 1, false));

          AstNode *or_node = nullptr;

          for (RTLIL::State control_bit : control_bits) {
            // Generate node for:  (exp << i >> ($size(exp) - 1)) ===
            // control_bit
            //                                                    ^^^
            AstNode *eq_node = new AstNode(AST_EQX, rsh_node->clone(),
                                           mkconst_bits({control_bit}, false));

            // Or the result for each checked bit value
            if (or_node)
              or_node = new AstNode(AST_LOGIC_OR, or_node, eq_node);
            else
              or_node = eq_node;
          }

          // We should have at least one element in control_bits,
          // because we checked for the number of arguments above
          // log_assert(or_node != nullptr);

          delete rsh_node;

          // Generate node for adding with result of previous bit
          newNode = new AstNode(AST_ADD, newNode, or_node);
        }

        goto apply_newNode;
      }

      if (str == "\\$countones" || str == "\\$isunknown" ||
          str == "\\$onehot" || str == "\\$onehot0") {
        if (children.size() != 1)
          input_error("System function %s got %d arguments, expected 1.\n",
                      RTLIL::unescape_id(str).c_str(), int(children.size()));

        AstNode *countbits = clone();
        countbits->str = "\\$countbits";

        if (str == "\\$countones") {
          countbits->children.push_back(
              mkconst_bits({RTLIL::State::S1}, false));
          newNode = countbits;
        } else if (str == "\\$isunknown") {
          countbits->children.push_back(mkconst_bits({RTLIL::Sx}, false));
          countbits->children.push_back(mkconst_bits({RTLIL::Sz}, false));
          newNode = new AstNode(AST_GT, countbits, mkconst_int(0, false));
        } else if (str == "\\$onehot") {
          countbits->children.push_back(
              mkconst_bits({RTLIL::State::S1}, false));
          newNode = new AstNode(AST_EQ, countbits, mkconst_int(1, false));
        } else if (str == "\\$onehot0") {
          countbits->children.push_back(
              mkconst_bits({RTLIL::State::S1}, false));
          newNode = new AstNode(AST_LE, countbits, mkconst_int(1, false));
        } else {
          // log_abort();
          exit(1);
        }

        goto apply_newNode;
      }

      if (current_scope.count(str) != 0 &&
          current_scope[str]->type == AST_DPI_FUNCTION) {
        AstNode *dpi_decl = current_scope[str];

        std::string rtype, fname;
        std::vector<std::string> argtypes;
        std::vector<AstNode *> args;

        rtype = RTLIL::unescape_id(dpi_decl->children.at(0)->str);
        fname = RTLIL::unescape_id(dpi_decl->children.at(1)->str);

        for (int i = 2; i < GetSize(dpi_decl->children); i++) {
          if (i - 2 >= GetSize(children))
            input_error(
                "Insufficient number of arguments in DPI function call.\n");

          argtypes.push_back(RTLIL::unescape_id(dpi_decl->children.at(i)->str));
          args.push_back(children.at(i - 2)->clone());
          while (args.back()->simplify(true, stage, -1, false)) {
          }

          if (args.back()->type != AST_CONSTANT &&
              args.back()->type != AST_REALVALUE)
            input_error("Failed to evaluate DPI function with non-constant "
                        "argument.\n");
        }

        newNode = dpi_call(rtype, fname, argtypes, args);

        for (auto arg : args)
          delete arg;

        goto apply_newNode;
      }

      if (current_scope.count(str) == 0)
        str = try_pop_module_prefix();
      if (current_scope.count(str) == 0 ||
          current_scope[str]->type != AST_FUNCTION)
        input_error("Can't resolve function name `%s'.\n", str.c_str());
    }

    if (type == AST_TCALL) {
      if (str == "$finish" || str == "$stop") {
        if (!current_always || current_always->type != AST_INITIAL)
          input_error(
              "System task `%s' outside initial block is unsupported.\n",
              str.c_str());

        input_error("System task `%s' executed.\n", str.c_str());
      }

      if (str == "\\$readmemh" || str == "\\$readmemb") {
        if (GetSize(children) < 2 || GetSize(children) > 4)
          input_error("System function %s got %d arguments, expected 2-4.\n",
                      RTLIL::unescape_id(str).c_str(), int(children.size()));

        AstNode *node_filename = children[0]->clone();
        while (node_filename->simplify(true, stage, width_hint, sign_hint)) {
        }
        if (node_filename->type != AST_CONSTANT)
          input_error("Failed to evaluate system function `%s' with "
                      "non-constant 1st argument.\n",
                      str.c_str());

        AstNode *node_memory = children[1]->clone();
        while (node_memory->simplify(true, stage, width_hint, sign_hint)) {
        }
        if (node_memory->type != AST_IDENTIFIER ||
            node_memory->id2ast == nullptr ||
            node_memory->id2ast->type != AST_MEMORY)
          input_error("Failed to evaluate system function `%s' with non-memory "
                      "2nd argument.\n",
                      str.c_str());

        int start_addr = -1, finish_addr = -1;

        if (GetSize(children) > 2) {
          AstNode *node_addr = children[2]->clone();
          while (node_addr->simplify(true, stage, width_hint, sign_hint)) {
          }
          if (node_addr->type != AST_CONSTANT)
            input_error("Failed to evaluate system function `%s' with "
                        "non-constant 3rd argument.\n",
                        str.c_str());
          start_addr = int(node_addr->asInt(false));
        }

        if (GetSize(children) > 3) {
          AstNode *node_addr = children[3]->clone();
          while (node_addr->simplify(true, stage, width_hint, sign_hint)) {
          }
          if (node_addr->type != AST_CONSTANT)
            input_error("Failed to evaluate system function `%s' with "
                        "non-constant 4th argument.\n",
                        str.c_str());
          finish_addr = int(node_addr->asInt(false));
        }

        bool unconditional_init = false;
        if (current_always->type == AST_INITIAL) {
          pool<AstNode *> queue;
          // log_assert(current_always->children[0]->type == AST_BLOCK);
          queue.insert(current_always->children[0]);
          while (!unconditional_init && !queue.empty()) {
            pool<AstNode *> next_queue;
            for (auto n : queue)
              for (auto c : n->children) {
                if (c == this)
                  unconditional_init = true;
                next_queue.insert(c);
              }
            next_queue.swap(queue);
          }
        }

        newNode = readmem(
            str == "\\$readmemh", node_filename->bitsAsConst().decode_string(),
            node_memory->id2ast, start_addr, finish_addr, unconditional_init);
        delete node_filename;
        delete node_memory;
        goto apply_newNode;
      }

      if (current_scope.count(str) == 0)
        str = try_pop_module_prefix();
      if (current_scope.count(str) == 0 || current_scope[str]->type != AST_TASK)
        input_error("Can't resolve task name `%s'.\n", str.c_str());
    }

    std::stringstream sstr;
    sstr << str << "$func$" << RTLIL::encode_filename(filename) << ":"
         << location.first_line << "$" << (autoidx++) << '.';
    std::string prefix = sstr.str();

    AstNode *decl = current_scope[str];
    if (unevaluated_tern_branch && decl->is_recursive_function())
      goto replace_fcall_later;
    decl = decl->clone();
    decl->replace_result_wire_name_in_function(str,
                                               "$result"); // enables recursion
    decl->expand_genblock(prefix);

    if (decl->type == AST_FUNCTION &&
        !decl->attributes.count(ID::via_celltype)) {
      bool require_const_eval = decl->has_const_only_constructs();
      bool all_args_const = true;
      for (auto child : children) {
        while (child->simplify(true, 1, -1, false)) {
        }
        if (child->type != AST_CONSTANT && child->type != AST_REALVALUE)
          all_args_const = false;
      }

      if (all_args_const) {
        AstNode *func_workspace = decl->clone();
        func_workspace->set_in_param_flag(true);
        func_workspace->str = prefix_id(prefix, "$result");
        newNode = func_workspace->eval_const_function(
            this, in_param || require_const_eval);
        delete func_workspace;
        if (newNode) {
          delete decl;
          goto apply_newNode;
        }
      }

      if (in_param)
        input_error("Non-constant function call in constant expression.\n");
      if (require_const_eval)
        input_error("Function %s can only be called with constant arguments.\n",
                    str.c_str());
    }

    size_t arg_count = 0;
    dict<std::string, AstNode *> wire_cache;
    std::vector<AstNode *> new_stmts;
    std::vector<AstNode *> output_assignments;

    if (current_block == NULL) {
      // log_assert(type == AST_FCALL);

      AstNode *wire = NULL;
      std::string res_name = prefix_id(prefix, "$result");
      for (auto child : decl->children)
        if (child->type == AST_WIRE && child->str == res_name)
          wire = child->clone();
      // log_assert(wire != NULL);

      wire->port_id = 0;
      wire->is_input = false;
      wire->is_output = false;

      current_scope[wire->str] = wire;
      current_ast_mod->children.push_back(wire);
      while (wire->simplify(true, 1, -1, false)) {
      }

      AstNode *lvalue = new AstNode(AST_IDENTIFIER);
      lvalue->str = wire->str;

      AstNode *always = new AstNode(
          AST_ALWAYS,
          new AstNode(AST_BLOCK, new AstNode(AST_ASSIGN_EQ, lvalue, clone())));
      always->children[0]->children[0]->was_checked = true;

      current_ast_mod->children.push_back(always);

      goto replace_fcall_with_id;
    }

    if (decl->attributes.count(ID::via_celltype)) {
      std::string celltype =
          decl->attributes.at(ID::via_celltype)->asAttrConst().decode_string();
      std::string outport = str;

      if (celltype.find(' ') != std::string::npos) {
        int pos = celltype.find(' ');
        outport = RTLIL::escape_id(celltype.substr(pos + 1));
        celltype = RTLIL::escape_id(celltype.substr(0, pos));
      } else
        celltype = RTLIL::escape_id(celltype);

      AstNode *cell = new AstNode(AST_CELL, new AstNode(AST_CELLTYPE));
      cell->str = prefix.substr(0, GetSize(prefix) - 1);
      cell->children[0]->str = celltype;

      for (auto attr : decl->attributes)
        if (attr.first.str().rfind("\\via_celltype_defparam_", 0) == 0) {
          AstNode *cell_arg = new AstNode(AST_PARASET, attr.second->clone());
          cell_arg->str = RTLIL::escape_id(
              attr.first.substr(strlen("\\via_celltype_defparam_")));
          cell->children.push_back(cell_arg);
        }

      for (auto child : decl->children)
        if (child->type == AST_WIRE &&
            (child->is_input || child->is_output ||
             (type == AST_FCALL && child->str == str))) {
          AstNode *wire = child->clone();
          wire->port_id = 0;
          wire->is_input = false;
          wire->is_output = false;
          current_ast_mod->children.push_back(wire);
          while (wire->simplify(true, 1, -1, false)) {
          }

          AstNode *wire_id = new AstNode(AST_IDENTIFIER);
          wire_id->str = wire->str;

          if ((child->is_input || child->is_output) &&
              arg_count < children.size()) {
            AstNode *arg = children[arg_count++]->clone();
            AstNode *assign =
                child->is_input
                    ? new AstNode(AST_ASSIGN_EQ, wire_id->clone(), arg)
                    : new AstNode(AST_ASSIGN_EQ, arg, wire_id->clone());
            assign->children[0]->was_checked = true;

            for (auto it = current_block->children.begin();
                 it != current_block->children.end(); it++) {
              if (*it != current_block_child)
                continue;
              current_block->children.insert(it, assign);
              break;
            }
          }

          AstNode *cell_arg = new AstNode(AST_ARGUMENT, wire_id);
          cell_arg->str = child->str == str ? outport : child->str;
          cell->children.push_back(cell_arg);
        }

      current_ast_mod->children.push_back(cell);
      goto replace_fcall_with_id;
    }

    for (auto child : decl->children)
      if (child->type == AST_WIRE || child->type == AST_MEMORY ||
          child->type == AST_PARAMETER || child->type == AST_LOCALPARAM ||
          child->type == AST_ENUM_ITEM) {
        AstNode *wire = nullptr;

        if (wire_cache.count(child->str)) {
          wire = wire_cache.at(child->str);
          bool contains_value = wire->type == AST_LOCALPARAM;
          if (wire->children.size() == contains_value) {
            for (auto c : child->children)
              wire->children.push_back(c->clone());
          } else if (!child->children.empty()) {
            while (child->simplify(true, stage, -1, false)) {
            }
            if (GetSize(child->children) ==
                GetSize(wire->children) - contains_value) {
              for (int i = 0; i < GetSize(child->children); i++)
                if (*child->children.at(i) !=
                    *wire->children.at(i + contains_value))
                  goto tcall_incompatible_wires;
            } else {
            tcall_incompatible_wires:
              input_error("Incompatible re-declaration of wire %s.\n",
                          child->str.c_str());
            }
          }
        } else {
          wire = child->clone();
          wire->port_id = 0;
          wire->is_input = false;
          wire->is_output = false;
          wire->is_reg = true;
          wire->set_attribute(ID::nosync, AstNode::mkconst_int(1, false));
          if (child->type == AST_ENUM_ITEM)
            wire->set_attribute(ID::enum_base_type,
                                child->attributes[ID::enum_base_type]);

          wire_cache[child->str] = wire;

          current_scope[wire->str] = wire;
          current_ast_mod->children.push_back(wire);
        }

        while (wire->simplify(true, 1, -1, false)) {
        }

        if ((child->is_input || child->is_output) &&
            arg_count < children.size()) {
          AstNode *arg = children[arg_count++]->clone();
          // convert purely constant arguments into localparams
          if (child->is_input && child->type == AST_WIRE &&
              arg->type == AST_CONSTANT &&
              node_contains_assignment_to(decl, child)) {
            wire->type = AST_LOCALPARAM;
            if (wire->attributes.count(ID::nosync))
              delete wire->attributes.at(ID::nosync);
            wire->attributes.erase(ID::nosync);
            wire->children.insert(wire->children.begin(), arg->clone());
            // args without a range implicitly have width 1
            if (wire->children.back()->type != AST_RANGE) {
              // check if this wire is redeclared with an explicit size
              bool uses_explicit_size = false;
              for (const AstNode *other_child : decl->children)
                if (other_child->type == AST_WIRE &&
                    child->str == other_child->str &&
                    !other_child->children.empty() &&
                    other_child->children.back()->type == AST_RANGE) {
                  uses_explicit_size = true;
                  break;
                }
              if (!uses_explicit_size) {
                AstNode *range = new AstNode();
                range->type = AST_RANGE;
                wire->children.push_back(range);
                range->children.push_back(mkconst_int(0, true));
                range->children.push_back(mkconst_int(0, true));
              }
            }
            wire->fixup_hierarchy_flags();
            // updates the sizing
            while (wire->simplify(true, 1, -1, false)) {
            }
            delete arg;
            continue;
          }
          AstNode *wire_id = new AstNode(AST_IDENTIFIER);
          wire_id->str = wire->str;
          AstNode *assign = child->is_input
                                ? new AstNode(AST_ASSIGN_EQ, wire_id, arg)
                                : new AstNode(AST_ASSIGN_EQ, arg, wire_id);
          assign->children[0]->was_checked = true;
          if (child->is_input)
            new_stmts.push_back(assign);
          else
            output_assignments.push_back(assign);
        }
      }

    for (auto child : decl->children)
      if (child->type != AST_WIRE && child->type != AST_MEMORY &&
          child->type != AST_PARAMETER && child->type != AST_LOCALPARAM)
        new_stmts.push_back(child->clone());

    new_stmts.insert(new_stmts.end(), output_assignments.begin(),
                     output_assignments.end());

    for (auto it = current_block->children.begin();; it++) {
      // log_assert(it != current_block->children.end());
      if (*it == current_block_child) {
        current_block->children.insert(it, new_stmts.begin(), new_stmts.end());
        break;
      }
    }

  replace_fcall_with_id:
    delete decl;
    if (type == AST_FCALL) {
      delete_children();
      type = AST_IDENTIFIER;
      str = prefix_id(prefix, "$result");
    }
    if (type == AST_TCALL)
      str = "";
    did_something = true;
  }

replace_fcall_later:;

  // perform const folding when activated
  if (const_fold) {
    bool string_op;
    std::vector<RTLIL::State> tmp_bits;
    RTLIL::Const (*const_func)(const RTLIL::Const &, const RTLIL::Const &, bool,
                               bool, int);
    RTLIL::Const dummy_arg;

    switch (type) {
    case AST_IDENTIFIER:
      if (current_scope.count(str) > 0 &&
          (current_scope[str]->type == AST_PARAMETER ||
           current_scope[str]->type == AST_LOCALPARAM ||
           current_scope[str]->type == AST_ENUM_ITEM)) {
        if (current_scope[str]->children[0]->type == AST_CONSTANT) {
          if (children.size() != 0 && children[0]->type == AST_RANGE &&
              children[0]->range_valid) {
            std::vector<RTLIL::State> data;
            bool param_upto = current_scope[str]->range_valid &&
                              current_scope[str]->range_swapped;
            int param_offset = current_scope[str]->range_valid
                                   ? current_scope[str]->range_right
                                   : 0;
            int param_width =
                current_scope[str]->range_valid
                    ? current_scope[str]->range_left -
                          current_scope[str]->range_right + 1
                    : GetSize(current_scope[str]->children[0]->bits);
            int tmp_range_left = children[0]->range_left,
                tmp_range_right = children[0]->range_right;
            if (param_upto) {
              tmp_range_left = (param_width + 2 * param_offset) -
                               children[0]->range_right - 1;
              tmp_range_right = (param_width + 2 * param_offset) -
                                children[0]->range_left - 1;
            }
            AST::AstNode *member_node = get_struct_member(this);
            int chunk_offset = member_node ? member_node->range_right : 0;
            // log_assert(!(chunk_offset && param_upto));
            for (int i = tmp_range_right; i <= tmp_range_left; i++) {
              int index = i - param_offset;
              if (0 <= index && index < param_width)
                data.push_back(current_scope[str]
                                   ->children[0]
                                   ->bits[chunk_offset + index]);
              else
                data.push_back(RTLIL::State::Sx);
            }
            newNode = mkconst_bits(data, false);
          } else if (children.size() == 0)
            newNode = current_scope[str]->children[0]->clone();
        } else if (current_scope[str]->children[0]->isConst())
          newNode = current_scope[str]->children[0]->clone();
      }
      break;
    case AST_BIT_NOT:
      if (children[0]->type == AST_CONSTANT) {
        RTLIL::Const y =
            RTLIL::const_not(children[0]->bitsAsConst(width_hint, sign_hint),
                             dummy_arg, sign_hint, false, width_hint);
        newNode = mkconst_bits(y.bits, sign_hint);
      }
      break;
    case AST_TO_SIGNED:
    case AST_TO_UNSIGNED:
      if (children[0]->type == AST_CONSTANT) {
        RTLIL::Const y = children[0]->bitsAsConst(width_hint, sign_hint);
        newNode = mkconst_bits(y.bits, type == AST_TO_SIGNED);
      }
      break;
      if (0) {
      case AST_BIT_AND:
        const_func = RTLIL::const_and;
      }
      if (0) {
      case AST_BIT_OR:
        const_func = RTLIL::const_or;
      }
      if (0) {
      case AST_BIT_XOR:
        const_func = RTLIL::const_xor;
      }
      if (0) {
      case AST_BIT_XNOR:
        const_func = RTLIL::const_xnor;
      }
      if (children[0]->type == AST_CONSTANT &&
          children[1]->type == AST_CONSTANT) {
        RTLIL::Const y =
            const_func(children[0]->bitsAsConst(width_hint, sign_hint),
                       children[1]->bitsAsConst(width_hint, sign_hint),
                       sign_hint, sign_hint, width_hint);
        newNode = mkconst_bits(y.bits, sign_hint);
      }
      break;
      if (0) {
      case AST_REDUCE_AND:
        const_func = RTLIL::const_reduce_and;
      }
      if (0) {
      case AST_REDUCE_OR:
        const_func = RTLIL::const_reduce_or;
      }
      if (0) {
      case AST_REDUCE_XOR:
        const_func = RTLIL::const_reduce_xor;
      }
      if (0) {
      case AST_REDUCE_XNOR:
        const_func = RTLIL::const_reduce_xnor;
      }
      if (0) {
      case AST_REDUCE_BOOL:
        const_func = RTLIL::const_reduce_bool;
      }
      if (children[0]->type == AST_CONSTANT) {
        RTLIL::Const y = const_func(RTLIL::Const(children[0]->bits), dummy_arg,
                                    false, false, -1);
        newNode = mkconst_bits(y.bits, false);
      }
      break;
    case AST_LOGIC_NOT:
      if (children[0]->type == AST_CONSTANT) {
        RTLIL::Const y =
            RTLIL::const_logic_not(RTLIL::Const(children[0]->bits), dummy_arg,
                                   children[0]->is_signed, false, -1);
        newNode = mkconst_bits(y.bits, false);
      } else if (children[0]->isConst()) {
        newNode = mkconst_int(children[0]->asReal(sign_hint) == 0, false, 1);
      }
      break;
      if (0) {
      case AST_LOGIC_AND:
        const_func = RTLIL::const_logic_and;
      }
      if (0) {
      case AST_LOGIC_OR:
        const_func = RTLIL::const_logic_or;
      }
      if (children[0]->type == AST_CONSTANT &&
          children[1]->type == AST_CONSTANT) {
        RTLIL::Const y = const_func(
            RTLIL::Const(children[0]->bits), RTLIL::Const(children[1]->bits),
            children[0]->is_signed, children[1]->is_signed, -1);
        newNode = mkconst_bits(y.bits, false);
      } else if (children[0]->isConst() && children[1]->isConst()) {
        if (type == AST_LOGIC_AND)
          newNode = mkconst_int((children[0]->asReal(sign_hint) != 0) &&
                                    (children[1]->asReal(sign_hint) != 0),
                                false, 1);
        else
          newNode = mkconst_int((children[0]->asReal(sign_hint) != 0) ||
                                    (children[1]->asReal(sign_hint) != 0),
                                false, 1);
      }
      break;
      if (0) {
      case AST_SHIFT_LEFT:
        const_func = RTLIL::const_shl;
      }
      if (0) {
      case AST_SHIFT_RIGHT:
        const_func = RTLIL::const_shr;
      }
      if (0) {
      case AST_SHIFT_SLEFT:
        const_func = RTLIL::const_sshl;
      }
      if (0) {
      case AST_SHIFT_SRIGHT:
        const_func = RTLIL::const_sshr;
      }
      if (0) {
      case AST_POW:
        const_func = RTLIL::const_pow;
      }
      if (children[0]->type == AST_CONSTANT &&
          children[1]->type == AST_CONSTANT) {
        RTLIL::Const y = const_func(
            children[0]->bitsAsConst(width_hint, sign_hint),
            RTLIL::Const(children[1]->bits), sign_hint,
            type == AST_POW ? children[1]->is_signed : false, width_hint);
        newNode = mkconst_bits(y.bits, sign_hint);
      } else if (type == AST_POW && children[0]->isConst() &&
                 children[1]->isConst()) {
        newNode = new AstNode(AST_REALVALUE);
        newNode->realvalue =
            pow(children[0]->asReal(sign_hint), children[1]->asReal(sign_hint));
      }
      break;
      if (0) {
      case AST_LT:
        const_func = RTLIL::const_lt;
      }
      if (0) {
      case AST_LE:
        const_func = RTLIL::const_le;
      }
      if (0) {
      case AST_EQ:
        const_func = RTLIL::const_eq;
      }
      if (0) {
      case AST_NE:
        const_func = RTLIL::const_ne;
      }
      if (0) {
      case AST_EQX:
        const_func = RTLIL::const_eqx;
      }
      if (0) {
      case AST_NEX:
        const_func = RTLIL::const_nex;
      }
      if (0) {
      case AST_GE:
        const_func = RTLIL::const_ge;
      }
      if (0) {
      case AST_GT:
        const_func = RTLIL::const_gt;
      }
      if (children[0]->type == AST_CONSTANT &&
          children[1]->type == AST_CONSTANT) {
        int cmp_width =
            std::max(children[0]->bits.size(), children[1]->bits.size());
        bool cmp_signed = children[0]->is_signed && children[1]->is_signed;
        RTLIL::Const y =
            const_func(children[0]->bitsAsConst(cmp_width, cmp_signed),
                       children[1]->bitsAsConst(cmp_width, cmp_signed),
                       cmp_signed, cmp_signed, 1);
        newNode = mkconst_bits(y.bits, false);
      } else if (children[0]->isConst() && children[1]->isConst()) {
        bool cmp_signed =
            (children[0]->type == AST_REALVALUE || children[0]->is_signed) &&
            (children[1]->type == AST_REALVALUE || children[1]->is_signed);
        switch (type) {
        case AST_LT:
          newNode = mkconst_int(children[0]->asReal(cmp_signed) <
                                    children[1]->asReal(cmp_signed),
                                false, 1);
          break;
        case AST_LE:
          newNode = mkconst_int(children[0]->asReal(cmp_signed) <=
                                    children[1]->asReal(cmp_signed),
                                false, 1);
          break;
        case AST_EQ:
          newNode = mkconst_int(children[0]->asReal(cmp_signed) ==
                                    children[1]->asReal(cmp_signed),
                                false, 1);
          break;
        case AST_NE:
          newNode = mkconst_int(children[0]->asReal(cmp_signed) !=
                                    children[1]->asReal(cmp_signed),
                                false, 1);
          break;
        case AST_EQX:
          newNode = mkconst_int(children[0]->asReal(cmp_signed) ==
                                    children[1]->asReal(cmp_signed),
                                false, 1);
          break;
        case AST_NEX:
          newNode = mkconst_int(children[0]->asReal(cmp_signed) !=
                                    children[1]->asReal(cmp_signed),
                                false, 1);
          break;
        case AST_GE:
          newNode = mkconst_int(children[0]->asReal(cmp_signed) >=
                                    children[1]->asReal(cmp_signed),
                                false, 1);
          break;
        case AST_GT:
          newNode = mkconst_int(children[0]->asReal(cmp_signed) >
                                    children[1]->asReal(cmp_signed),
                                false, 1);
          break;
        default:
          // log_abort();
          exit(1);
        }
      }
      break;
      if (0) {
      case AST_ADD:
        const_func = RTLIL::const_add;
      }
      if (0) {
      case AST_SUB:
        const_func = RTLIL::const_sub;
      }
      if (0) {
      case AST_MUL:
        const_func = RTLIL::const_mul;
      }
      if (0) {
      case AST_DIV:
        const_func = RTLIL::const_div;
      }
      if (0) {
      case AST_MOD:
        const_func = RTLIL::const_mod;
      }
      if (children[0]->type == AST_CONSTANT &&
          children[1]->type == AST_CONSTANT) {
        RTLIL::Const y =
            const_func(children[0]->bitsAsConst(width_hint, sign_hint),
                       children[1]->bitsAsConst(width_hint, sign_hint),
                       sign_hint, sign_hint, width_hint);
        newNode = mkconst_bits(y.bits, sign_hint);
      } else if (children[0]->isConst() && children[1]->isConst()) {
        newNode = new AstNode(AST_REALVALUE);
        switch (type) {
        case AST_ADD:
          newNode->realvalue =
              children[0]->asReal(sign_hint) + children[1]->asReal(sign_hint);
          break;
        case AST_SUB:
          newNode->realvalue =
              children[0]->asReal(sign_hint) - children[1]->asReal(sign_hint);
          break;
        case AST_MUL:
          newNode->realvalue =
              children[0]->asReal(sign_hint) * children[1]->asReal(sign_hint);
          break;
        case AST_DIV:
          newNode->realvalue =
              children[0]->asReal(sign_hint) / children[1]->asReal(sign_hint);
          break;
        case AST_MOD:
          newNode->realvalue = fmod(children[0]->asReal(sign_hint),
                                    children[1]->asReal(sign_hint));
          break;
        default:
          // log_abort();
          exit(1);
        }
      }
      break;
      if (0) {
      case AST_SELFSZ:
        const_func = RTLIL::const_pos;
      }
      if (0) {
      case AST_POS:
        const_func = RTLIL::const_pos;
      }
      if (0) {
      case AST_NEG:
        const_func = RTLIL::const_neg;
      }
      if (children[0]->type == AST_CONSTANT) {
        RTLIL::Const y =
            const_func(children[0]->bitsAsConst(width_hint, sign_hint),
                       dummy_arg, sign_hint, false, width_hint);
        newNode = mkconst_bits(y.bits, sign_hint);
      } else if (children[0]->isConst()) {
        newNode = new AstNode(AST_REALVALUE);
        if (type == AST_NEG)
          newNode->realvalue = -children[0]->asReal(sign_hint);
        else
          newNode->realvalue = +children[0]->asReal(sign_hint);
      }
      break;
    case AST_TERNARY:
      if (children[0]->isConst()) {
        auto pair = get_tern_choice();
        AstNode *choice = pair.first;
        AstNode *not_choice = pair.second;

        if (choice != NULL) {
          if (choice->type == AST_CONSTANT) {
            int other_width_hint = width_hint;
            bool other_sign_hint = sign_hint, other_real = false;
            not_choice->detectSignWidth(other_width_hint, other_sign_hint,
                                        &other_real);
            if (other_real) {
              newNode = new AstNode(AST_REALVALUE);
              choice->detectSignWidth(width_hint, sign_hint);
              newNode->realvalue = choice->asReal(sign_hint);
            } else {
              RTLIL::Const y = choice->bitsAsConst(width_hint, sign_hint);
              if (choice->is_string && y.bits.size() % 8 == 0 &&
                  sign_hint == false)
                newNode = mkconst_str(y.bits);
              else
                newNode = mkconst_bits(y.bits, sign_hint);
            }
          } else if (choice->isConst()) {
            newNode = choice->clone();
          }
        } else if (children[1]->type == AST_CONSTANT &&
                   children[2]->type == AST_CONSTANT) {
          RTLIL::Const a = children[1]->bitsAsConst(width_hint, sign_hint);
          RTLIL::Const b = children[2]->bitsAsConst(width_hint, sign_hint);
          // log_assert(a.bits.size() == b.bits.size());
          for (size_t i = 0; i < a.bits.size(); i++)
            if (a.bits[i] != b.bits[i])
              a.bits[i] = RTLIL::State::Sx;
          newNode = mkconst_bits(a.bits, sign_hint);
        } else if (children[1]->isConst() && children[2]->isConst()) {
          newNode = new AstNode(AST_REALVALUE);
          if (children[1]->asReal(sign_hint) == children[2]->asReal(sign_hint))
            newNode->realvalue = children[1]->asReal(sign_hint);
          else
            // IEEE Std 1800-2012 Sec. 11.4.11 states that the entry in Table
            // 7-1 for the data type in question should be returned if the ?: is
            // ambiguous. The value in Table 7-1 for the 'real' type is 0.0.
            newNode->realvalue = 0.0;
        }
      }
      break;
    case AST_CAST_SIZE:
      if (children.at(0)->type == AST_CONSTANT &&
          children.at(1)->type == AST_CONSTANT) {
        int width = children[0]->bitsAsConst().as_int();
        RTLIL::Const val;
        if (children[1]->is_unsized)
          val = children[1]->bitsAsUnsizedConst(width);
        else
          val = children[1]->bitsAsConst(width);
        newNode = mkconst_bits(val.bits, children[1]->is_signed);
      }
      break;
    case AST_CONCAT:
      string_op = !children.empty();
      for (auto it = children.begin(); it != children.end(); it++) {
        if ((*it)->type != AST_CONSTANT)
          goto not_const;
        if (!(*it)->is_string)
          string_op = false;
        tmp_bits.insert(tmp_bits.end(), (*it)->bits.begin(), (*it)->bits.end());
      }
      newNode =
          string_op ? mkconst_str(tmp_bits) : mkconst_bits(tmp_bits, false);
      break;
    case AST_REPLICATE:
      if (children.at(0)->type != AST_CONSTANT ||
          children.at(1)->type != AST_CONSTANT)
        goto not_const;
      for (int i = 0; i < children[0]->bitsAsConst().as_int(); i++)
        tmp_bits.insert(tmp_bits.end(), children.at(1)->bits.begin(),
                        children.at(1)->bits.end());
      newNode = children.at(1)->is_string ? mkconst_str(tmp_bits)
                                          : mkconst_bits(tmp_bits, false);
      break;
    default:
    not_const:
      break;
    }
  }

  // if any of the above set 'newNode' -> use 'newNode' as template to update
  // 'this'
  if (newNode) {
  apply_newNode:
    // fprintf(stderr, "----\n");
    // dumpAst(stderr, "- ");
    // newNode->dumpAst(stderr, "+ ");
    // log_assert(newNode != NULL);
    newNode->filename = filename;
    newNode->location = location;
    newNode->cloneInto(this);
    fixup_hierarchy_flags();
    delete newNode;
    did_something = true;
  }

  if (!did_something)
    basic_prep = true;

  recursion_counter--;
  return did_something;
}

void AstNode::replace_result_wire_name_in_function(const std::string &from,
                                                   const std::string &to) {
  for (AstNode *child : children)
    child->replace_result_wire_name_in_function(from, to);
  if (str == from && type != AST_FCALL && type != AST_TCALL)
    str = to;
}

// replace a readmem[bh] TCALL ast node with a block of memory assignments
AstNode *AstNode::readmem(bool is_readmemh, std::string mem_filename,
                          AstNode *memory, int start_addr, int finish_addr,
                          bool unconditional_init) {
  int mem_width, mem_size, addr_bits;
  memory->meminfo(mem_width, mem_size, addr_bits);

  AstNode *block = new AstNode(AST_BLOCK);

  AstNode *meminit = nullptr;
  int next_meminit_cursor = 0;
  std::vector<State> meminit_bits;
  std::vector<State> en_bits;
  int meminit_size = 0;

  for (int i = 0; i < mem_width; i++)
    en_bits.push_back(State::S1);

  std::ifstream f;
  f.open(mem_filename.c_str());
  if (f.fail()) {
#ifdef _WIN32
    char slash = '\\';
#else
    char slash = '/';
#endif
    std::string path = filename.substr(0, filename.find_last_of(slash) + 1);
    f.open(path + mem_filename.c_str());
    yosys_input_files.insert(path + mem_filename);
  } else {
    yosys_input_files.insert(mem_filename);
  }
  if (f.fail() || GetSize(mem_filename) == 0)
    input_error("Can not open file `%s` for %s.\n", mem_filename.c_str(),
                str.c_str());

  // log_assert(GetSize(memory->children) == 2 &&
  //            memory->children[1]->type == AST_RANGE &&
  //            memory->children[1]->range_valid);
  int range_left = memory->children[1]->range_left,
      range_right = memory->children[1]->range_right;
  int range_min = std::min(range_left, range_right),
      range_max = std::max(range_left, range_right);

  if (start_addr < 0)
    start_addr = range_min;

  if (finish_addr < 0)
    finish_addr = range_max + 1;

  bool in_comment = false;
  int increment = start_addr <= finish_addr ? +1 : -1;
  int cursor = start_addr;

  while (!f.eof()) {
    std::string line, token;
    std::getline(f, line);

    for (int i = 0; i < GetSize(line); i++) {
      if (in_comment && line.compare(i, 2, "*/") == 0) {
        line[i] = ' ';
        line[i + 1] = ' ';
        in_comment = false;
        continue;
      }
      if (!in_comment && line.compare(i, 2, "/*") == 0)
        in_comment = true;
      if (in_comment)
        line[i] = ' ';
    }

    while (1) {
      token = next_token(line, " \t\r\n");
      if (token.empty() || token.compare(0, 2, "//") == 0)
        break;

      if (token[0] == '@') {
        token = token.substr(1);
        const char *nptr = token.c_str();
        char *endptr;
        cursor = strtol(nptr, &endptr, 16);
        if (!*nptr || *endptr)
          input_error("Can not parse address `%s` for %s.\n", nptr,
                      str.c_str());
        continue;
      }

      AstNode *value = VERILOG_FRONTEND::const2ast(
          stringf("%d'%c", mem_width, is_readmemh ? 'h' : 'b') + token);

      if (unconditional_init) {
        if (meminit == nullptr || cursor != next_meminit_cursor) {
          if (meminit != nullptr) {
            meminit->children[1] = AstNode::mkconst_bits(meminit_bits, false);
            meminit->children[3] = AstNode::mkconst_int(meminit_size, false);
          }

          meminit = new AstNode(AST_MEMINIT);
          meminit->children.push_back(AstNode::mkconst_int(cursor, false));
          meminit->children.push_back(nullptr);
          meminit->children.push_back(AstNode::mkconst_bits(en_bits, false));
          meminit->children.push_back(nullptr);
          meminit->str = memory->str;
          meminit->id2ast = memory;
          meminit_bits.clear();
          meminit_size = 0;

          current_ast_mod->children.push_back(meminit);
          next_meminit_cursor = cursor;
        }

        meminit_size++;
        next_meminit_cursor++;
        meminit_bits.insert(meminit_bits.end(), value->bits.begin(),
                            value->bits.end());
        delete value;
      } else {
        block->children.push_back(new AstNode(
            AST_ASSIGN_EQ,
            new AstNode(
                AST_IDENTIFIER,
                new AstNode(AST_RANGE, AstNode::mkconst_int(cursor, false))),
            value));
        block->children.back()->children[0]->str = memory->str;
        block->children.back()->children[0]->id2ast = memory;
        block->children.back()->children[0]->was_checked = true;
      }

      cursor += increment;
      if ((cursor == finish_addr + increment) ||
          (increment > 0 && cursor > range_max) ||
          (increment < 0 && cursor < range_min))
        break;
    }

    if ((cursor == finish_addr + increment) ||
        (increment > 0 && cursor > range_max) ||
        (increment < 0 && cursor < range_min))
      break;
  }

  if (meminit != nullptr) {
    meminit->children[1] = AstNode::mkconst_bits(meminit_bits, false);
    meminit->children[3] = AstNode::mkconst_int(meminit_size, false);
  }

  return block;
}

// annotate the names of all wires and other named objects in a named generate
// or procedural block; nested blocks are themselves annotated such that the
// prefix is carried forward, but resolution of their children is deferred
void AstNode::expand_genblock(const std::string &prefix) {
  if (type == AST_IDENTIFIER || type == AST_FCALL || type == AST_TCALL ||
      type == AST_WIRETYPE || type == AST_PREFIX) {
    // log_assert(!str.empty());

    // search starting in the innermost scope and then stepping outward
    for (size_t ppos = prefix.size() - 1; ppos; --ppos) {
      if (prefix.at(ppos) != '.')
        continue;

      std::string new_prefix = prefix.substr(0, ppos + 1);
      auto attempt_resolve =
          [&new_prefix](const std::string &ident) -> std::string {
        std::string new_name = prefix_id(new_prefix, ident);
        if (current_scope.count(new_name))
          return new_name;
        return {};
      };

      // attempt to resolve the full identifier
      std::string resolved = attempt_resolve(str);
      if (!resolved.empty()) {
        str = resolved;
        break;
      }

      // attempt to resolve hierarchical prefixes within the identifier,
      // as the prefix could refer to a local scope which exists but
      // hasn't yet been elaborated
      for (size_t spos = str.size() - 1; spos; --spos) {
        if (str.at(spos) != '.')
          continue;
        resolved = attempt_resolve(str.substr(0, spos));
        if (!resolved.empty()) {
          str = resolved + str.substr(spos);
          ppos = 1; // break outer loop
          break;
        }
      }
    }
  }

  auto prefix_node = [&prefix](AstNode *child) {
    if (child->str.empty())
      return;
    std::string new_name = prefix_id(prefix, child->str);
    if (child->type == AST_FUNCTION)
      child->replace_result_wire_name_in_function(child->str, new_name);
    else
      child->str = new_name;
    current_scope[new_name] = child;
  };

  for (size_t i = 0; i < children.size(); i++) {
    AstNode *child = children[i];

    switch (child->type) {
    case AST_WIRE:
    case AST_MEMORY:
    case AST_PARAMETER:
    case AST_LOCALPARAM:
    case AST_FUNCTION:
    case AST_TASK:
    case AST_CELL:
    case AST_TYPEDEF:
    case AST_ENUM_ITEM:
    case AST_GENVAR:
      prefix_node(child);
      break;

    case AST_BLOCK:
    case AST_GENBLOCK:
      if (!child->str.empty())
        prefix_node(child);
      break;

    case AST_ENUM:
      current_scope[child->str] = child;
      for (auto enode : child->children) {
        // log_assert(enode->type == AST_ENUM_ITEM);
        prefix_node(enode);
      }
      break;

    default:
      break;
    }
  }

  for (size_t i = 0; i < children.size(); i++) {
    AstNode *child = children[i];
    // AST_PREFIX member names should not be prefixed; we recurse into them
    // as normal to ensure indices and ranges are properly resolved, and
    // then restore the previous string
    if (type == AST_PREFIX && i == 1) {
      std::string backup_scope_name = child->str;
      child->expand_genblock(prefix);
      child->str = backup_scope_name;
      continue;
    }
    // functions/tasks may reference wires, constants, etc. in this scope
    if (child->type == AST_FUNCTION || child->type == AST_TASK)
      continue;
    // named blocks pick up the current prefix and will expanded later
    if ((child->type == AST_GENBLOCK || child->type == AST_BLOCK) &&
        !child->str.empty())
      continue;

    child->expand_genblock(prefix);
  }
}

// add implicit AST_GENBLOCK names according to IEEE 1364-2005 Section 12.4.3 or
// IEEE 1800-2017 Section 27.6
void AstNode::label_genblks(std::set<std::string> &existing, int &counter) {
  switch (type) {
  case AST_GENIF:
  case AST_GENFOR:
  case AST_GENCASE:
    // seeing a proper generate control flow construct increments the
    // counter once
    ++counter;
    for (AstNode *child : children)
      child->label_genblks(existing, counter);
    break;

  case AST_GENBLOCK: {
    // if this block is unlabeled, generate its corresponding unique name
    for (int padding = 0; str.empty(); ++padding) {
      std::string candidate = "\\genblk";
      for (int i = 0; i < padding; ++i)
        candidate += '0';
      candidate += std::to_string(counter);
      if (!existing.count(candidate))
        str = candidate;
    }
    // within a genblk, the counter starts fresh
    std::set<std::string> existing_local = existing;
    int counter_local = 0;
    for (AstNode *child : children)
      child->label_genblks(existing_local, counter_local);
    break;
  }

  default:
    // track names which could conflict with implicit genblk names
    if (str.rfind("\\genblk", 0) == 0)
      existing.insert(str);
    for (AstNode *child : children)
      child->label_genblks(existing, counter);
    break;
  }
}

// helper function for mem2reg_as_needed_pass1
static void mark_memories_assign_lhs_complex(
    dict<AstNode *, pool<std::string>> &mem2reg_places,
    dict<AstNode *, uint32_t> &mem2reg_candidates, AstNode *that) {
  for (auto &child : that->children)
    mark_memories_assign_lhs_complex(mem2reg_places, mem2reg_candidates, child);

  if (that->type == AST_IDENTIFIER && that->id2ast &&
      that->id2ast->type == AST_MEMORY) {
    AstNode *mem = that->id2ast;
    if (!(mem2reg_candidates[mem] & AstNode::MEM2REG_FL_CMPLX_LHS))
      mem2reg_places[mem].insert(
          stringf("%s:%d", RTLIL::encode_filename(that->filename).c_str(),
                  that->location.first_line));
    mem2reg_candidates[mem] |= AstNode::MEM2REG_FL_CMPLX_LHS;
  }
}

// find memories that should be replaced by registers
void AstNode::mem2reg_as_needed_pass1(
    dict<AstNode *, pool<std::string>> &mem2reg_places,
    dict<AstNode *, uint32_t> &mem2reg_candidates,
    dict<AstNode *, uint32_t> &proc_flags, uint32_t &flags) {
  uint32_t children_flags = 0;
  int lhs_children_counter = 0;

  if (type == AST_TYPEDEF)
    return; // don't touch content of typedefs

  if (type == AST_ASSIGN || type == AST_ASSIGN_LE || type == AST_ASSIGN_EQ) {
    // mark all memories that are used in a complex expression on the left side
    // of an assignment
    for (auto &lhs_child : children[0]->children)
      mark_memories_assign_lhs_complex(mem2reg_places, mem2reg_candidates,
                                       lhs_child);

    if (children[0]->type == AST_IDENTIFIER && children[0]->id2ast &&
        children[0]->id2ast->type == AST_MEMORY) {
      AstNode *mem = children[0]->id2ast;

      // activate mem2reg if this is assigned in an async proc
      if (flags & AstNode::MEM2REG_FL_ASYNC) {
        if (!(mem2reg_candidates[mem] & AstNode::MEM2REG_FL_SET_ASYNC))
          mem2reg_places[mem].insert(
              stringf("%s:%d", RTLIL::encode_filename(filename).c_str(),
                      location.first_line));
        mem2reg_candidates[mem] |= AstNode::MEM2REG_FL_SET_ASYNC;
      }

      // remember if this is assigned blocking (=)
      if (type == AST_ASSIGN_EQ) {
        if (!(proc_flags[mem] & AstNode::MEM2REG_FL_EQ1))
          mem2reg_places[mem].insert(
              stringf("%s:%d", RTLIL::encode_filename(filename).c_str(),
                      location.first_line));
        proc_flags[mem] |= AstNode::MEM2REG_FL_EQ1;
      }

      // for proper (non-init) writes: remember if this is a constant index or
      // not
      if ((flags & MEM2REG_FL_INIT) == 0) {
        if (children[0]->children.size() &&
            children[0]->children[0]->type == AST_RANGE &&
            children[0]->children[0]->children.size()) {
          if (children[0]->children[0]->children[0]->type == AST_CONSTANT)
            mem2reg_candidates[mem] |= AstNode::MEM2REG_FL_CONST_LHS;
          else
            mem2reg_candidates[mem] |= AstNode::MEM2REG_FL_VAR_LHS;
        }
      }

      // remember where this is
      if (flags & MEM2REG_FL_INIT) {
        if (!(mem2reg_candidates[mem] & AstNode::MEM2REG_FL_SET_INIT))
          mem2reg_places[mem].insert(
              stringf("%s:%d", RTLIL::encode_filename(filename).c_str(),
                      location.first_line));
        mem2reg_candidates[mem] |= AstNode::MEM2REG_FL_SET_INIT;
      } else {
        if (!(mem2reg_candidates[mem] & AstNode::MEM2REG_FL_SET_ELSE))
          mem2reg_places[mem].insert(
              stringf("%s:%d", RTLIL::encode_filename(filename).c_str(),
                      location.first_line));
        mem2reg_candidates[mem] |= AstNode::MEM2REG_FL_SET_ELSE;
      }
    }

    lhs_children_counter = 1;
  }

  if (type == AST_IDENTIFIER && id2ast && id2ast->type == AST_MEMORY) {
    AstNode *mem = id2ast;

    // flag if used after blocking assignment (in same proc)
    if ((proc_flags[mem] & AstNode::MEM2REG_FL_EQ1) &&
        !(mem2reg_candidates[mem] & AstNode::MEM2REG_FL_EQ2)) {
      mem2reg_places[mem].insert(
          stringf("%s:%d", RTLIL::encode_filename(filename).c_str(),
                  location.first_line));
      mem2reg_candidates[mem] |= AstNode::MEM2REG_FL_EQ2;
    }
  }

  // also activate if requested, either by using mem2reg attribute or by
  // declaring array as 'wire' instead of 'reg' or 'logic'
  if (type == AST_MEMORY &&
      (get_bool_attribute(ID::mem2reg) || (flags & AstNode::MEM2REG_FL_ALL) ||
       !(is_reg || is_logic)))
    mem2reg_candidates[this] |= AstNode::MEM2REG_FL_FORCED;

  if ((type == AST_MODULE || type == AST_INTERFACE) &&
      get_bool_attribute(ID::mem2reg))
    children_flags |= AstNode::MEM2REG_FL_ALL;

  dict<AstNode *, uint32_t> *proc_flags_p = NULL;

  if (type == AST_ALWAYS) {
    int count_edge_events = 0;
    for (auto child : children)
      if (child->type == AST_POSEDGE || child->type == AST_NEGEDGE)
        count_edge_events++;
    if (count_edge_events != 1)
      children_flags |= AstNode::MEM2REG_FL_ASYNC;
    proc_flags_p = new dict<AstNode *, uint32_t>;
  } else if (type == AST_INITIAL) {
    children_flags |= AstNode::MEM2REG_FL_INIT;
    proc_flags_p = new dict<AstNode *, uint32_t>;
  }

  uint32_t backup_flags = flags;
  flags |= children_flags;
  // log_assert((flags & ~0x000000ff) == 0);

  for (auto child : children) {
    if (lhs_children_counter > 0) {
      lhs_children_counter--;
      if (child->children.size() && child->children[0]->type == AST_RANGE &&
          child->children[0]->children.size()) {
        for (auto c : child->children[0]->children) {
          if (proc_flags_p)
            c->mem2reg_as_needed_pass1(mem2reg_places, mem2reg_candidates,
                                       *proc_flags_p, flags);
          else
            c->mem2reg_as_needed_pass1(mem2reg_places, mem2reg_candidates,
                                       proc_flags, flags);
        }
      }
    } else if (proc_flags_p)
      child->mem2reg_as_needed_pass1(mem2reg_places, mem2reg_candidates,
                                     *proc_flags_p, flags);
    else
      child->mem2reg_as_needed_pass1(mem2reg_places, mem2reg_candidates,
                                     proc_flags, flags);
  }

  flags &= ~children_flags | backup_flags;

  if (proc_flags_p) {
#ifndef NDEBUG
    // for (auto it : *proc_flags_p)
    //   log_assert((it.second & ~0xff000000) == 0);
#endif
    delete proc_flags_p;
  }
}

bool AstNode::mem2reg_check(pool<AstNode *> &mem2reg_set) {
  if (type != AST_IDENTIFIER || !id2ast || !mem2reg_set.count(id2ast))
    return false;

  if (children.empty() || children[0]->type != AST_RANGE ||
      GetSize(children[0]->children) != 1)
    input_error("Invalid array access.\n");

  return true;
}

void AstNode::mem2reg_remove(pool<AstNode *> &mem2reg_set,
                             std::vector<AstNode *> &delnodes) {
  // log_assert(mem2reg_set.count(this) == 0);

  if (mem2reg_set.count(id2ast))
    id2ast = nullptr;

  for (size_t i = 0; i < children.size(); i++) {
    if (mem2reg_set.count(children[i]) > 0) {
      delnodes.push_back(children[i]);
      children.erase(children.begin() + (i--));
    } else {
      children[i]->mem2reg_remove(mem2reg_set, delnodes);
    }
  }
}

// actually replace memories with registers
bool AstNode::mem2reg_as_needed_pass2(pool<AstNode *> &mem2reg_set,
                                      AstNode *mod, AstNode *block,
                                      AstNode *&async_block) {
  bool did_something = false;

  if (type == AST_BLOCK)
    block = this;

  if (type == AST_FUNCTION || type == AST_TASK)
    return false;

  if (type == AST_TYPEDEF)
    return false;

  if (type == AST_MEMINIT && id2ast && mem2reg_set.count(id2ast)) {
    // log_assert(children[0]->type == AST_CONSTANT);
    // log_assert(children[1]->type == AST_CONSTANT);
    // log_assert(children[2]->type == AST_CONSTANT);
    // log_assert(children[3]->type == AST_CONSTANT);

    int cursor = children[0]->asInt(false);
    Const data = children[1]->bitsAsConst();
    Const en = children[2]->bitsAsConst();
    int length = children[3]->asInt(false);

    if (length != 0) {
      AstNode *block = new AstNode(AST_INITIAL, new AstNode(AST_BLOCK));
      mod->children.push_back(block);
      block = block->children[0];

      int wordsz = GetSize(data) / length;

      for (int i = 0; i < length; i++) {
        int pos = 0;
        while (pos < wordsz) {
          if (en[pos] != State::S1) {
            pos++;
          } else {
            int epos = pos + 1;
            while (epos < wordsz && en[epos] == State::S1)
              epos++;
            int clen = epos - pos;
            AstNode *range =
                new AstNode(AST_RANGE, AstNode::mkconst_int(cursor + i, false));
            if (pos != 0 || epos != wordsz) {
              int left;
              int right;
              AstNode *mrange = id2ast->children[0];
              if (mrange->range_left < mrange->range_right) {
                right = mrange->range_right - pos;
                left = mrange->range_right - epos + 1;
              } else {
                right = mrange->range_right + pos;
                left = mrange->range_right + epos - 1;
              }
              range = new AstNode(
                  AST_MULTIRANGE, range,
                  new AstNode(AST_RANGE, AstNode::mkconst_int(left, true),
                              AstNode::mkconst_int(right, true)));
            }
            AstNode *target = new AstNode(AST_IDENTIFIER, range);
            target->str = str;
            target->id2ast = id2ast;
            target->was_checked = true;
            block->children.push_back(new AstNode(
                AST_ASSIGN_EQ, target,
                mkconst_bits(data.extract(i * wordsz + pos, clen).bits,
                             false)));
            pos = epos;
          }
        }
      }
    }

    AstNode *newNode = new AstNode(AST_NONE);
    newNode->cloneInto(this);
    delete newNode;

    did_something = true;
  }

  if (type == AST_ASSIGN && block == NULL &&
      children[0]->mem2reg_check(mem2reg_set)) {
    if (async_block == NULL) {
      async_block = new AstNode(AST_ALWAYS, new AstNode(AST_BLOCK));
      mod->children.push_back(async_block);
    }

    AstNode *newNode = clone();
    newNode->type = AST_ASSIGN_EQ;
    newNode->children[0]->was_checked = true;
    async_block->children[0]->children.push_back(newNode);

    newNode = new AstNode(AST_NONE);
    newNode->cloneInto(this);
    delete newNode;

    did_something = true;
  }

  if ((type == AST_ASSIGN_LE || type == AST_ASSIGN_EQ) &&
      children[0]->mem2reg_check(mem2reg_set) &&
      children[0]->children[0]->children[0]->type != AST_CONSTANT) {
    std::stringstream sstr;
    sstr << "$mem2reg_wr$" << children[0]->str << "$"
         << RTLIL::encode_filename(filename) << ":" << location.first_line
         << "$" << (autoidx++);
    std::string id_addr = sstr.str() + "_ADDR", id_data = sstr.str() + "_DATA";

    int mem_width, mem_size, addr_bits;
    bool mem_signed = children[0]->id2ast->is_signed;
    children[0]->id2ast->meminfo(mem_width, mem_size, addr_bits);

    AstNode *wire_addr = new AstNode(
        AST_WIRE, new AstNode(AST_RANGE, mkconst_int(addr_bits - 1, true),
                              mkconst_int(0, true)));
    wire_addr->str = id_addr;
    wire_addr->is_reg = true;
    wire_addr->was_checked = true;
    wire_addr->set_attribute(ID::nosync, AstNode::mkconst_int(1, false));
    mod->children.push_back(wire_addr);
    while (wire_addr->simplify(true, 1, -1, false)) {
    }

    AstNode *wire_data = new AstNode(
        AST_WIRE, new AstNode(AST_RANGE, mkconst_int(mem_width - 1, true),
                              mkconst_int(0, true)));
    wire_data->str = id_data;
    wire_data->is_reg = true;
    wire_data->was_checked = true;
    wire_data->is_signed = mem_signed;
    wire_data->set_attribute(ID::nosync, AstNode::mkconst_int(1, false));
    mod->children.push_back(wire_data);
    while (wire_data->simplify(true, 1, -1, false)) {
    }

    // log_assert(block != NULL);
    size_t assign_idx = 0;
    while (assign_idx < block->children.size() &&
           block->children[assign_idx] != this)
      assign_idx++;
    // log_assert(assign_idx < block->children.size());

    AstNode *assign_addr =
        new AstNode(AST_ASSIGN_EQ, new AstNode(AST_IDENTIFIER),
                    children[0]->children[0]->children[0]->clone());
    assign_addr->children[0]->str = id_addr;
    assign_addr->children[0]->was_checked = true;
    block->children.insert(block->children.begin() + assign_idx + 1,
                           assign_addr);

    AstNode *case_node = new AstNode(AST_CASE, new AstNode(AST_IDENTIFIER));
    case_node->children[0]->str = id_addr;
    for (int i = 0; i < mem_size; i++) {
      if (children[0]->children[0]->children[0]->type == AST_CONSTANT &&
          int(children[0]->children[0]->children[0]->integer) != i)
        continue;
      AstNode *cond_node =
          new AstNode(AST_COND, AstNode::mkconst_int(i, false, addr_bits),
                      new AstNode(AST_BLOCK));
      AstNode *assign_reg = new AstNode(type, new AstNode(AST_IDENTIFIER),
                                        new AstNode(AST_IDENTIFIER));
      if (children[0]->children.size() == 2)
        assign_reg->children[0]->children.push_back(
            children[0]->children[1]->clone());
      assign_reg->children[0]->str =
          stringf("%s[%d]", children[0]->str.c_str(), i);
      assign_reg->children[1]->str = id_data;
      cond_node->children[1]->children.push_back(assign_reg);
      case_node->children.push_back(cond_node);
    }

    // fixup on the full hierarchy below case_node
    case_node->fixup_hierarchy_flags(true);

    block->children.insert(block->children.begin() + assign_idx + 2, case_node);

    children[0]->delete_children();
    children[0]->range_valid = false;
    children[0]->id2ast = NULL;
    children[0]->str = id_data;
    type = AST_ASSIGN_EQ;
    children[0]->was_checked = true;

    fixup_hierarchy_flags();
    did_something = true;
  }

  if (mem2reg_check(mem2reg_set)) {
    AstNode *bit_part_sel = NULL;
    if (children.size() == 2)
      bit_part_sel = children[1]->clone();

    if (children[0]->children[0]->type == AST_CONSTANT) {
      int id = children[0]->children[0]->integer;
      int left = id2ast->children[1]->children[0]->integer;
      int right = id2ast->children[1]->children[1]->integer;
      bool valid_const_access =
          (left <= id && id <= right) || (right <= id && id <= left);
      if (valid_const_access) {
        str = stringf("%s[%d]", str.c_str(), id);
        delete_children();
        range_valid = false;
        id2ast = NULL;
      } else {
        int width;
        if (bit_part_sel) {
          bit_part_sel->dumpAst(nullptr, "? ");
          if (bit_part_sel->children.size() == 1)
            width = 0;
          else
            width = bit_part_sel->children[0]->integer -
                    bit_part_sel->children[1]->integer;
          delete bit_part_sel;
          bit_part_sel = nullptr;
        } else {
          width = id2ast->children[0]->children[0]->integer -
                  id2ast->children[0]->children[1]->integer;
        }
        width = abs(width) + 1;

        delete_children();

        std::vector<RTLIL::State> x_bits;
        for (int i = 0; i < width; i++)
          x_bits.push_back(RTLIL::State::Sx);
        AstNode *constant = AstNode::mkconst_bits(x_bits, false);
        constant->cloneInto(this);
        delete constant;
      }
    } else {
      std::stringstream sstr;
      sstr << "$mem2reg_rd$" << str << "$" << RTLIL::encode_filename(filename)
           << ":" << location.first_line << "$" << (autoidx++);
      std::string id_addr = sstr.str() + "_ADDR",
                  id_data = sstr.str() + "_DATA";

      int mem_width, mem_size, addr_bits;
      bool mem_signed = id2ast->is_signed;
      id2ast->meminfo(mem_width, mem_size, addr_bits);

      AstNode *wire_addr = new AstNode(
          AST_WIRE, new AstNode(AST_RANGE, mkconst_int(addr_bits - 1, true),
                                mkconst_int(0, true)));
      wire_addr->str = id_addr;
      wire_addr->is_reg = true;
      wire_addr->was_checked = true;
      if (block)
        wire_addr->set_attribute(ID::nosync, AstNode::mkconst_int(1, false));
      mod->children.push_back(wire_addr);
      while (wire_addr->simplify(true, 1, -1, false)) {
      }

      AstNode *wire_data = new AstNode(
          AST_WIRE, new AstNode(AST_RANGE, mkconst_int(mem_width - 1, true),
                                mkconst_int(0, true)));
      wire_data->str = id_data;
      wire_data->is_reg = true;
      wire_data->was_checked = true;
      wire_data->is_signed = mem_signed;
      if (block)
        wire_data->set_attribute(ID::nosync, AstNode::mkconst_int(1, false));
      mod->children.push_back(wire_data);
      while (wire_data->simplify(true, 1, -1, false)) {
      }

      AstNode *assign_addr = new AstNode(block ? AST_ASSIGN_EQ : AST_ASSIGN,
                                         new AstNode(AST_IDENTIFIER),
                                         children[0]->children[0]->clone());
      assign_addr->children[0]->str = id_addr;
      assign_addr->children[0]->was_checked = true;

      AstNode *case_node = new AstNode(AST_CASE, new AstNode(AST_IDENTIFIER));
      case_node->children[0]->str = id_addr;

      for (int i = 0; i < mem_size; i++) {
        if (children[0]->children[0]->type == AST_CONSTANT &&
            int(children[0]->children[0]->integer) != i)
          continue;
        AstNode *cond_node =
            new AstNode(AST_COND, AstNode::mkconst_int(i, false, addr_bits),
                        new AstNode(AST_BLOCK));
        AstNode *assign_reg =
            new AstNode(AST_ASSIGN_EQ, new AstNode(AST_IDENTIFIER),
                        new AstNode(AST_IDENTIFIER));
        assign_reg->children[0]->str = id_data;
        assign_reg->children[0]->was_checked = true;
        assign_reg->children[1]->str = stringf("%s[%d]", str.c_str(), i);
        cond_node->children[1]->children.push_back(assign_reg);
        case_node->children.push_back(cond_node);
      }

      std::vector<RTLIL::State> x_bits;
      for (int i = 0; i < mem_width; i++)
        x_bits.push_back(RTLIL::State::Sx);

      AstNode *cond_node = new AstNode(AST_COND, new AstNode(AST_DEFAULT),
                                       new AstNode(AST_BLOCK));
      AstNode *assign_reg =
          new AstNode(AST_ASSIGN_EQ, new AstNode(AST_IDENTIFIER),
                      AstNode::mkconst_bits(x_bits, false));
      assign_reg->children[0]->str = id_data;
      assign_reg->children[0]->was_checked = true;
      cond_node->children[1]->children.push_back(assign_reg);
      case_node->children.push_back(cond_node);

      // fixup on the full hierarchy below case_node
      case_node->fixup_hierarchy_flags(true);

      if (block) {
        size_t assign_idx = 0;
        while (assign_idx < block->children.size() &&
               !block->children[assign_idx]->contains(this))
          assign_idx++;
        // log_assert(assign_idx < block->children.size());
        block->children.insert(block->children.begin() + assign_idx, case_node);
        block->children.insert(block->children.begin() + assign_idx,
                               assign_addr);
      } else {
        AstNode *proc =
            new AstNode(AST_ALWAYS, new AstNode(AST_BLOCK, case_node));
        mod->children.push_back(proc);
        mod->children.push_back(assign_addr);
        mod->fixup_hierarchy_flags();
      }

      delete_children();
      range_valid = false;
      id2ast = NULL;
      str = id_data;
    }

    if (bit_part_sel) {
      children.push_back(bit_part_sel);
      fixup_hierarchy_flags();
    }

    did_something = true;
  }

  // log_assert(id2ast == NULL || mem2reg_set.count(id2ast) == 0);

  auto children_list = children;
  for (size_t i = 0; i < children_list.size(); i++)
    if (children_list[i]->mem2reg_as_needed_pass2(mem2reg_set, mod, block,
                                                  async_block))
      did_something = true;

  return did_something;
}

// calculate memory dimensions
void AstNode::meminfo(int &mem_width, int &mem_size, int &addr_bits) {
  // log_assert(type == AST_MEMORY);

  mem_width = children[0]->range_left - children[0]->range_right + 1;
  mem_size = children[1]->range_left - children[1]->range_right;

  if (mem_size < 0)
    mem_size *= -1;
  mem_size += std::min(children[1]->range_left, children[1]->range_right) + 1;

  addr_bits = 1;
  while ((1 << addr_bits) < mem_size)
    addr_bits++;
}

bool AstNode::detect_latch(const std::string &var) {
  switch (type) {
  case AST_ALWAYS:
    for (auto &c : children) {
      switch (c->type) {
      case AST_POSEDGE:
      case AST_NEGEDGE:
        return false;
      case AST_EDGE:
        break;
      case AST_BLOCK:
        if (!c->detect_latch(var))
          return false;
        break;
      default:
        // log_abort();
        exit(1);
      }
    }
    return true;
  case AST_BLOCK:
    for (auto &c : children)
      if (!c->detect_latch(var))
        return false;
    return true;
  case AST_CASE: {
    bool r = true;
    for (auto &c : children) {
      if (c->type == AST_COND) {
        if (c->children.at(1)->detect_latch(var))
          return true;
        r = false;
      }
      if (c->type == AST_DEFAULT) {
        if (c->children.at(0)->detect_latch(var))
          return true;
        r = false;
      }
    }
    return r;
  }
  case AST_ASSIGN_EQ:
  case AST_ASSIGN_LE:
    if (children.at(0)->type == AST_IDENTIFIER &&
        children.at(0)->children.empty() && children.at(0)->str == var)
      return false;
    return true;
  default:
    return true;
  }
}

bool AstNode::has_const_only_constructs() {
  if (type == AST_WHILE || type == AST_REPEAT)
    return true;
  for (auto child : children)
    if (child->has_const_only_constructs())
      return true;
  return false;
}

bool AstNode::is_simple_const_expr() {
  if (type == AST_IDENTIFIER)
    return false;
  for (auto child : children)
    if (!child->is_simple_const_expr())
      return false;
  return true;
}

// helper function for AstNode::eval_const_function()
bool AstNode::replace_variables(
    std::map<std::string, AstNode::varinfo_t> &variables, AstNode *fcall,
    bool must_succeed) {
  if (type == AST_IDENTIFIER && variables.count(str)) {
    int offset = variables.at(str).offset,
        width = variables.at(str).val.bits.size();
    if (!children.empty()) {
      if (children.size() != 1 || children.at(0)->type != AST_RANGE) {
        if (!must_succeed)
          return false;
        input_error("Memory access in constant function is not supported\n%s: "
                    "...called from here.\n",
                    fcall->loc_string().c_str());
      }
      if (!children.at(0)->replace_variables(variables, fcall, must_succeed))
        return false;
      while (simplify(true, 1, -1, false)) {
      }
      if (!children.at(0)->range_valid) {
        if (!must_succeed)
          return false;
        input_error("Non-constant range\n%s: ... called from here.\n",
                    fcall->loc_string().c_str());
      }
      offset =
          std::min(children.at(0)->range_left, children.at(0)->range_right);
      width = std::min(
          std::abs(children.at(0)->range_left - children.at(0)->range_right) +
              1,
          width);
    }
    offset -= variables.at(str).offset;
    if (variables.at(str).range_swapped)
      offset = -offset;
    std::vector<RTLIL::State> &var_bits = variables.at(str).val.bits;
    std::vector<RTLIL::State> new_bits(var_bits.begin() + offset,
                                       var_bits.begin() + offset + width);
    AstNode *newNode = mkconst_bits(new_bits, variables.at(str).is_signed);
    newNode->cloneInto(this);
    delete newNode;
    return true;
  }

  for (auto &child : children)
    if (!child->replace_variables(variables, fcall, must_succeed))
      return false;
  return true;
}

// attempt to statically evaluate a functions with all-const arguments
AstNode *AstNode::eval_const_function(AstNode *fcall, bool must_succeed) {
  std::map<std::string, AstNode *> backup_scope = current_scope;
  std::map<std::string, AstNode::varinfo_t> variables;
  std::vector<AstNode *> to_delete;
  AstNode *block = new AstNode(AST_BLOCK);
  AstNode *result = nullptr;

  size_t argidx = 0;
  for (auto child : children) {
    block->children.push_back(child->clone());
  }
  block->set_in_param_flag(true);

  while (!block->children.empty()) {
    AstNode *stmt = block->children.front();

#if 0
		log("-----------------------------------\n");
		for (auto &it : variables)
			log("%20s %40s\n", it.first.c_str(), log_signal(it.second.val));
		stmt->dumpAst(NULL, "stmt> ");
#endif

    if (stmt->type == AST_WIRE) {
      while (stmt->simplify(true, 1, -1, false)) {
      }
      if (!stmt->range_valid) {
        if (!must_succeed)
          goto finished;
        stmt->input_error(
            "Can't determine size of variable %s\n%s: ... called from here.\n",
            stmt->str.c_str(), fcall->loc_string().c_str());
      }
      AstNode::varinfo_t &variable = variables[stmt->str];
      int width = abs(stmt->range_left - stmt->range_right) + 1;
      // if this variable has already been declared as an input, check the
      // sizes match if it already had an explicit size
      if (variable.arg && variable.explicitly_sized &&
          variable.val.size() != width) {
        input_error(
            "Incompatible re-declaration of constant function wire %s.\n",
            stmt->str.c_str());
      }
      variable.val = RTLIL::Const(RTLIL::State::Sx, width);
      variable.offset =
          stmt->range_swapped ? stmt->range_left : stmt->range_right;
      variable.range_swapped = stmt->range_swapped;
      variable.is_signed = stmt->is_signed;
      variable.explicitly_sized =
          stmt->children.size() && stmt->children.back()->type == AST_RANGE;
      // identify the argument corresponding to this wire, if applicable
      if (stmt->is_input && argidx < fcall->children.size()) {
        variable.arg = fcall->children.at(argidx++);
      }
      // load the constant arg's value into this variable
      if (variable.arg) {
        if (variable.arg->type == AST_CONSTANT) {
          variable.val = variable.arg->bitsAsConst(width);
        } else {
          // log_assert(variable.arg->type == AST_REALVALUE);
          variable.val = variable.arg->realAsConst(width);
        }
      }
      current_scope[stmt->str] = stmt;

      block->children.erase(block->children.begin());
      to_delete.push_back(stmt);
      continue;
    }

    // log_assert(variables.count(str) != 0);

    if (stmt->type == AST_LOCALPARAM) {
      while (stmt->simplify(true, 1, -1, false)) {
      }

      current_scope[stmt->str] = stmt;

      block->children.erase(block->children.begin());
      to_delete.push_back(stmt);
      continue;
    }

    if (stmt->type == AST_ASSIGN_EQ) {
      if (stmt->children.at(0)->type == AST_IDENTIFIER &&
          stmt->children.at(0)->children.size() != 0 &&
          stmt->children.at(0)->children.at(0)->type == AST_RANGE)
        if (!stmt->children.at(0)->children.at(0)->replace_variables(
                variables, fcall, must_succeed))
          goto finished;
      if (!stmt->children.at(1)->replace_variables(variables, fcall,
                                                   must_succeed))
        goto finished;
      while (stmt->simplify(true, 1, -1, false)) {
      }

      if (stmt->type != AST_ASSIGN_EQ)
        continue;

      if (stmt->children.at(1)->type != AST_CONSTANT) {
        if (!must_succeed)
          goto finished;
        stmt->input_error("Non-constant expression in constant function\n%s: "
                          "... called from here. X\n",
                          fcall->loc_string().c_str());
      }

      if (stmt->children.at(0)->type != AST_IDENTIFIER) {
        if (!must_succeed)
          goto finished;
        stmt->input_error("Unsupported composite left hand side in constant "
                          "function\n%s: ... called from here.\n",
                          fcall->loc_string().c_str());
      }

      if (!variables.count(stmt->children.at(0)->str)) {
        if (!must_succeed)
          goto finished;
        stmt->input_error("Assignment to non-local variable in constant "
                          "function\n%s: ... called from here.\n",
                          fcall->loc_string().c_str());
      }

      if (stmt->children.at(0)->children.empty()) {
        variables[stmt->children.at(0)->str].val =
            stmt->children.at(1)->bitsAsConst(
                variables[stmt->children.at(0)->str].val.bits.size());
      } else {
        AstNode *range = stmt->children.at(0)->children.at(0);
        if (!range->range_valid) {
          if (!must_succeed)
            goto finished;
          range->input_error("Non-constant range\n%s: ... called from here.\n",
                             fcall->loc_string().c_str());
        }
        int offset = std::min(range->range_left, range->range_right);
        int width = std::abs(range->range_left - range->range_right) + 1;
        varinfo_t &v = variables[stmt->children.at(0)->str];
        RTLIL::Const r = stmt->children.at(1)->bitsAsConst(v.val.bits.size());
        for (int i = 0; i < width; i++) {
          int index = i + offset - v.offset;
          if (v.range_swapped)
            index = -index;
          v.val.bits.at(index) = r.bits.at(i);
        }
      }

      delete block->children.front();
      block->children.erase(block->children.begin());
      continue;
    }

    if (stmt->type == AST_FOR) {
      block->children.insert(block->children.begin(), stmt->children.at(0));
      stmt->children.at(3)->children.push_back(stmt->children.at(2));
      stmt->children.erase(stmt->children.begin() + 2);
      stmt->children.erase(stmt->children.begin());
      stmt->type = AST_WHILE;
      continue;
    }

    if (stmt->type == AST_WHILE) {
      AstNode *cond = stmt->children.at(0)->clone();
      if (!cond->replace_variables(variables, fcall, must_succeed))
        goto finished;
      cond->set_in_param_flag(true);
      while (cond->simplify(true, 1, -1, false)) {
      }

      if (cond->type != AST_CONSTANT) {
        if (!must_succeed)
          goto finished;
        stmt->input_error("Non-constant expression in constant function\n%s: "
                          "... called from here.\n",
                          fcall->loc_string().c_str());
      }

      if (cond->asBool()) {
        block->children.insert(block->children.begin(),
                               stmt->children.at(1)->clone());
      } else {
        delete block->children.front();
        block->children.erase(block->children.begin());
      }

      delete cond;
      continue;
    }

    if (stmt->type == AST_REPEAT) {
      AstNode *num = stmt->children.at(0)->clone();
      if (!num->replace_variables(variables, fcall, must_succeed))
        goto finished;
      num->set_in_param_flag(true);
      while (num->simplify(true, 1, -1, false)) {
      }

      if (num->type != AST_CONSTANT) {
        if (!must_succeed)
          goto finished;
        stmt->input_error("Non-constant expression in constant function\n%s: "
                          "... called from here.\n",
                          fcall->loc_string().c_str());
      }

      block->children.erase(block->children.begin());
      for (int i = 0; i < num->bitsAsConst().as_int(); i++)
        block->children.insert(block->children.begin(),
                               stmt->children.at(1)->clone());

      delete stmt;
      delete num;
      continue;
    }

    if (stmt->type == AST_CASE) {
      AstNode *expr = stmt->children.at(0)->clone();
      if (!expr->replace_variables(variables, fcall, must_succeed))
        goto finished;
      expr->set_in_param_flag(true);
      while (expr->simplify(true, 1, -1, false)) {
      }

      AstNode *sel_case = NULL;
      for (size_t i = 1; i < stmt->children.size(); i++) {
        bool found_match = false;
        // log_assert(stmt->children.at(i)->type == AST_COND ||
        //            stmt->children.at(i)->type == AST_CONDX ||
        //            stmt->children.at(i)->type == AST_CONDZ);

        if (stmt->children.at(i)->children.front()->type == AST_DEFAULT) {
          sel_case = stmt->children.at(i)->children.back();
          continue;
        }

        for (size_t j = 0;
             j + 1 < stmt->children.at(i)->children.size() && !found_match;
             j++) {
          AstNode *cond = stmt->children.at(i)->children.at(j)->clone();
          if (!cond->replace_variables(variables, fcall, must_succeed))
            goto finished;

          cond = new AstNode(AST_EQ, expr->clone(), cond);
          cond->set_in_param_flag(true);
          while (cond->simplify(true, 1, -1, false)) {
          }

          if (cond->type != AST_CONSTANT) {
            if (!must_succeed)
              goto finished;
            stmt->input_error("Non-constant expression in constant "
                              "function\n%s: ... called from here.\n",
                              fcall->loc_string().c_str());
          }

          found_match = cond->asBool();
          delete cond;
        }

        if (found_match) {
          sel_case = stmt->children.at(i)->children.back();
          break;
        }
      }

      block->children.erase(block->children.begin());
      if (sel_case)
        block->children.insert(block->children.begin(), sel_case->clone());
      delete stmt;
      delete expr;
      continue;
    }

    if (stmt->type == AST_BLOCK) {
      if (!stmt->str.empty())
        stmt->expand_genblock(stmt->str + ".");

      block->children.erase(block->children.begin());
      block->children.insert(block->children.begin(), stmt->children.begin(),
                             stmt->children.end());
      stmt->children.clear();
      block->fixup_hierarchy_flags();
      delete stmt;
      continue;
    }

    if (!must_succeed)
      goto finished;
    stmt->input_error("Unsupported language construct in constant "
                      "function\n%s: ... called from here.\n",
                      fcall->loc_string().c_str());
    // log_abort();
    exit(1);
  }

  result = AstNode::mkconst_bits(variables.at(str).val.bits,
                                 variables.at(str).is_signed);

finished:
  delete block;
  current_scope = backup_scope;

  for (auto it : to_delete) {
    delete it;
  }
  to_delete.clear();

  return result;
}

void AstNode::allocateDefaultEnumValues() {
  // log_assert(type == AST_ENUM);
  // log_assert(children.size() > 0);
  if (children.front()->attributes.count(ID::enum_base_type))
    return; // already elaborated
  int last_enum_int = -1;
  for (auto node : children) {
    // log_assert(node->type == AST_ENUM_ITEM);
    node->set_attribute(ID::enum_base_type, mkconst_str(str));
    for (size_t i = 0; i < node->children.size(); i++) {
      switch (node->children[i]->type) {
      case AST_NONE:
        // replace with auto-incremented constant
        delete node->children[i];
        node->children[i] = AstNode::mkconst_int(++last_enum_int, true);
        break;
      case AST_CONSTANT:
        // explicit constant (or folded expression)
        // TODO: can't extend 'x or 'z item
        last_enum_int = node->children[i]->integer;
        break;
      default:
        // ignore ranges
        break;
      }
      // TODO: range check
    }
  }
}

bool AstNode::is_recursive_function() const {
  std::set<const AstNode *> visited;
  std::function<bool(const AstNode *node)> visit = [&](const AstNode *node) {
    if (visited.count(node))
      return node == this;
    visited.insert(node);
    if (node->type == AST_FCALL) {
      auto it = current_scope.find(node->str);
      if (it != current_scope.end() && visit(it->second))
        return true;
    }
    for (const AstNode *child : node->children) {
      if (visit(child))
        return true;
    }
    return false;
  };

  // log_assert(type == AST_FUNCTION);
  return visit(this);
}

std::pair<AstNode *, AstNode *> AstNode::get_tern_choice() {
  if (!children[0]->isConst())
    return {};

  bool found_sure_true = false;
  bool found_maybe_true = false;

  if (children[0]->type == AST_CONSTANT)
    for (auto &bit : children[0]->bits) {
      if (bit == RTLIL::State::S1)
        found_sure_true = true;
      if (bit > RTLIL::State::S1)
        found_maybe_true = true;
    }
  else
    found_sure_true = children[0]->asReal(true) != 0;

  AstNode *choice = nullptr, *not_choice = nullptr;
  if (found_sure_true)
    choice = children[1], not_choice = children[2];
  else if (!found_maybe_true)
    choice = children[2], not_choice = children[1];

  return {choice, not_choice};
}

std::string AstNode::try_pop_module_prefix() const {
  AstNode *current_scope_ast = (current_ast_mod == nullptr)
                                   ? VERILOG_FRONTEND::current_ast
                                   : current_ast_mod;
  size_t pos = str.find('.', 1);
  if (str[0] == '\\' && pos != std::string::npos) {
    std::string new_str = "\\" + str.substr(pos + 1);
    if (current_scope.count(new_str)) {
      std::string prefix = str.substr(0, pos);
      auto it = current_scope_ast->attributes.find(ID::hdlname);
      if ((it != current_scope_ast->attributes.end() &&
           it->second->str == prefix) ||
          prefix == current_scope_ast->str)
        return new_str;
    }
  }
  return str;
}

using namespace AST;
using namespace AST_INTERNAL;

// helper function for creating RTLIL code for unary operations
static RTLIL::SigSpec uniop2rtlil(AstNode *that, RTLIL::IdString type,
                                  int result_width, const RTLIL::SigSpec &arg,
                                  bool gen_attributes = true) {
  RTLIL::IdString name = stringf("%s$%s:%d$%d", type.c_str(),
                                 RTLIL::encode_filename(that->filename).c_str(),
                                 that->location.first_line, autoidx++);
  RTLIL::Cell *cell = current_module->addCell(name, type);
  set_src_attr(cell, that);

  RTLIL::Wire *wire =
      current_module->addWire(cell->name.str() + "_Y", result_width);
  set_src_attr(wire, that);
  wire->is_signed = that->is_signed;

  if (gen_attributes)
    for (auto &attr : that->attributes) {
      if (attr.second->type != AST_CONSTANT)
        that->input_error("Attribute `%s' with non-constant value!\n",
                          attr.first.c_str());
      cell->attributes[attr.first] = attr.second->asAttrConst();
    }

  cell->parameters[ID::A_SIGNED] = RTLIL::Const(that->children[0]->is_signed);
  cell->parameters[ID::A_WIDTH] = RTLIL::Const(arg.size());
  cell->setPort(ID::A, arg);

  cell->parameters[ID::Y_WIDTH] = result_width;
  cell->setPort(ID::Y, wire);
  return wire;
}

// helper function for extending bit width (preferred over SigSpec::extend()
// because of correct undef propagation in ConstEval)
static void widthExtend(AstNode *that, RTLIL::SigSpec &sig, int width,
                        bool is_signed) {
  if (width <= sig.size()) {
    sig.extend_u0(width, is_signed);
    return;
  }

  RTLIL::IdString name = stringf("$extend$%s:%d$%d",
                                 RTLIL::encode_filename(that->filename).c_str(),
                                 that->location.first_line, autoidx++);
  RTLIL::Cell *cell = current_module->addCell(name, ID($pos));
  set_src_attr(cell, that);

  RTLIL::Wire *wire = current_module->addWire(cell->name.str() + "_Y", width);
  set_src_attr(wire, that);
  wire->is_signed = that->is_signed;

  if (that != NULL)
    for (auto &attr : that->attributes) {
      if (attr.second->type != AST_CONSTANT)
        that->input_error("Attribute `%s' with non-constant value!\n",
                          attr.first.c_str());
      cell->attributes[attr.first] = attr.second->asAttrConst();
    }

  cell->parameters[ID::A_SIGNED] = RTLIL::Const(is_signed);
  cell->parameters[ID::A_WIDTH] = RTLIL::Const(sig.size());
  cell->setPort(ID::A, sig);

  cell->parameters[ID::Y_WIDTH] = width;
  cell->setPort(ID::Y, wire);
  sig = wire;
}

// helper function for creating RTLIL code for binary operations
static RTLIL::SigSpec binop2rtlil(AstNode *that, RTLIL::IdString type,
                                  int result_width, const RTLIL::SigSpec &left,
                                  const RTLIL::SigSpec &right) {
  RTLIL::IdString name = stringf("%s$%s:%d$%d", type.c_str(),
                                 RTLIL::encode_filename(that->filename).c_str(),
                                 that->location.first_line, autoidx++);
  RTLIL::Cell *cell = current_module->addCell(name, type);
  set_src_attr(cell, that);

  RTLIL::Wire *wire =
      current_module->addWire(cell->name.str() + "_Y", result_width);
  set_src_attr(wire, that);
  wire->is_signed = that->is_signed;

  for (auto &attr : that->attributes) {
    if (attr.second->type != AST_CONSTANT)
      that->input_error("Attribute `%s' with non-constant value!\n",
                        attr.first.c_str());
    cell->attributes[attr.first] = attr.second->asAttrConst();
  }

  cell->parameters[ID::A_SIGNED] = RTLIL::Const(that->children[0]->is_signed);
  cell->parameters[ID::B_SIGNED] = RTLIL::Const(that->children[1]->is_signed);

  cell->parameters[ID::A_WIDTH] = RTLIL::Const(left.size());
  cell->parameters[ID::B_WIDTH] = RTLIL::Const(right.size());

  cell->setPort(ID::A, left);
  cell->setPort(ID::B, right);

  cell->parameters[ID::Y_WIDTH] = result_width;
  cell->setPort(ID::Y, wire);
  return wire;
}

// helper function for creating RTLIL code for multiplexers
static RTLIL::SigSpec mux2rtlil(AstNode *that, const RTLIL::SigSpec &cond,
                                const RTLIL::SigSpec &left,
                                const RTLIL::SigSpec &right) {
  // log_assert(cond.size() == 1);

  std::stringstream sstr;
  sstr << "$ternary$" << RTLIL::encode_filename(that->filename) << ":"
       << that->location.first_line << "$" << (autoidx++);

  RTLIL::Cell *cell = current_module->addCell(sstr.str(), ID($mux));
  set_src_attr(cell, that);

  RTLIL::Wire *wire =
      current_module->addWire(cell->name.str() + "_Y", left.size());
  set_src_attr(wire, that);
  wire->is_signed = that->is_signed;

  for (auto &attr : that->attributes) {
    if (attr.second->type != AST_CONSTANT)
      that->input_error("Attribute `%s' with non-constant value!\n",
                        attr.first.c_str());
    cell->attributes[attr.first] = attr.second->asAttrConst();
  }

  cell->parameters[ID::WIDTH] = RTLIL::Const(left.size());

  cell->setPort(ID::A, right);
  cell->setPort(ID::B, left);
  cell->setPort(ID::S, cond);
  cell->setPort(ID::Y, wire);

  return wire;
}

// helper class for rewriting simple lookahead references in AST always blocks
struct AST_INTERNAL::LookaheadRewriter {
  dict<RTLIL::IdString, std::pair<AstNode *, AstNode *>> lookaheadids;

  void collect_lookaheadids(AstNode *node) {
    if (node->lookahead) {
      // log_assert(node->type == AST_IDENTIFIER);
      if (!lookaheadids.count(node->str)) {
        AstNode *wire = new AstNode(AST_WIRE);
        for (auto c : node->id2ast->children)
          wire->children.push_back(c->clone());
        wire->fixup_hierarchy_flags();
        wire->str = stringf("$lookahead%s$%d", node->str.c_str(), autoidx++);
        wire->set_attribute(ID::nosync, AstNode::mkconst_int(1, false));
        wire->is_logic = true;
        while (wire->simplify(true, 1, -1, false)) {
        }
        current_ast_mod->children.push_back(wire);
        lookaheadids[node->str] = std::make_pair(node->id2ast, wire);
        wire->genRTLIL();
      }
    }

    for (auto child : node->children)
      collect_lookaheadids(child);
  }

  bool has_lookaheadids(AstNode *node) {
    if (node->type == AST_IDENTIFIER && lookaheadids.count(node->str) != 0)
      return true;

    for (auto child : node->children)
      if (has_lookaheadids(child))
        return true;

    return false;
  }

  bool has_nonlookaheadids(AstNode *node) {
    if (node->type == AST_IDENTIFIER && lookaheadids.count(node->str) == 0)
      return true;

    for (auto child : node->children)
      if (has_nonlookaheadids(child))
        return true;

    return false;
  }

  void rewrite_lookaheadids(AstNode *node, bool lhs = false) {
    if (node->type == AST_ASSIGN_LE) {
      if (has_lookaheadids(node->children[0])) {
        if (has_nonlookaheadids(node->children[0])) {
          // log_error("incompatible mix of lookahead and non-lookahead IDs in "
          //           "LHS expression.\n");
          exit(1);
        }

        rewrite_lookaheadids(node->children[0], true);
        node->type = AST_ASSIGN_EQ;
      }

      rewrite_lookaheadids(node->children[1], lhs);
      return;
    }

    if (node->type == AST_IDENTIFIER && (node->lookahead || lhs)) {
      AstNode *newwire = lookaheadids.at(node->str).second;
      node->str = newwire->str;
      node->id2ast = newwire;
      lhs = false;
    }

    for (auto child : node->children)
      rewrite_lookaheadids(child, lhs);
  }

  LookaheadRewriter(AstNode *top) {
    // top->dumpAst(NULL, "REWRITE-BEFORE> ");
    // top->dumpVlog(NULL, "REWRITE-BEFORE> ");

    AstNode *block = nullptr;

    for (auto c : top->children)
      if (c->type == AST_BLOCK) {
        // log_assert(block == nullptr);
        block = c;
      }
    // log_assert(block != nullptr);

    collect_lookaheadids(block);
    rewrite_lookaheadids(block);

    for (auto it : lookaheadids) {
      AstNode *ref_orig = new AstNode(AST_IDENTIFIER);
      ref_orig->str = it.second.first->str;
      ref_orig->id2ast = it.second.first;
      ref_orig->was_checked = true;

      AstNode *ref_temp = new AstNode(AST_IDENTIFIER);
      ref_temp->str = it.second.second->str;
      ref_temp->id2ast = it.second.second;
      ref_temp->was_checked = true;

      AstNode *init_assign =
          new AstNode(AST_ASSIGN_EQ, ref_temp->clone(), ref_orig->clone());
      AstNode *final_assign = new AstNode(AST_ASSIGN_LE, ref_orig, ref_temp);

      block->children.insert(block->children.begin(), init_assign);
      block->children.push_back(final_assign);
    }

    // top->dumpAst(NULL, "REWRITE-AFTER> ");
    // top->dumpVlog(NULL, "REWRITE-AFTER> ");
  }
};

// ------------------------------------------------
// A map-like container, but you can save and restore the state
// ------------------------------------------------

template <typename Key, typename T, typename OPS = hash_ops<Key>>
struct stackmap {
private:
  std::vector<dict<Key, T *, OPS>> backup_state;
  dict<Key, T, OPS> current_state;
  static T empty_tuple;

public:
  stackmap() {}
  stackmap(const dict<Key, T, OPS> &other) : current_state(other) {}

  template <typename Other> void operator=(const Other &other) {
    for (auto &it : current_state)
      if (!backup_state.empty() && backup_state.back().count(it.first) == 0)
        backup_state.back()[it.first] = new T(it.second);
    current_state.clear();

    for (auto &it : other)
      set(it.first, it.second);
  }

  bool has(const Key &k) { return current_state.count(k) != 0; }

  void set(const Key &k, const T &v) {
    if (!backup_state.empty() && backup_state.back().count(k) == 0)
      backup_state.back()[k] =
          current_state.count(k) ? new T(current_state.at(k)) : nullptr;
    current_state[k] = v;
  }

  void unset(const Key &k) {
    if (!backup_state.empty() && backup_state.back().count(k) == 0)
      backup_state.back()[k] =
          current_state.count(k) ? new T(current_state.at(k)) : nullptr;
    current_state.erase(k);
  }

  const T &get(const Key &k) {
    if (current_state.count(k) == 0)
      return empty_tuple;
    return current_state.at(k);
  }

  void reset(const Key &k) {
    for (int i = GetSize(backup_state) - 1; i >= 0; i--)
      if (backup_state[i].count(k) != 0) {
        if (backup_state[i].at(k) == nullptr)
          current_state.erase(k);
        else
          current_state[k] = *backup_state[i].at(k);
        return;
      }
    current_state.erase(k);
  }

  const dict<Key, T, OPS> &stdmap() { return current_state; }

  void save() { backup_state.resize(backup_state.size() + 1); }

  void restore() {
    // log_assert(!backup_state.empty());
    for (auto &it : backup_state.back())
      if (it.second != nullptr) {
        current_state[it.first] = *it.second;
        delete it.second;
      } else
        current_state.erase(it.first);
    backup_state.pop_back();
  }

  ~stackmap() {
    while (!backup_state.empty())
      restore();
  }
};

// helper class for converting AST always nodes to RTLIL processes
struct AST_INTERNAL::ProcessGenerator {
  // input and output structures
  AstNode *always;
  RTLIL::SigSpec initSyncSignals;
  RTLIL::Process *proc;
  RTLIL::SigSpec outputSignals;

  // This always points to the RTLIL::CaseRule being filled at the moment
  RTLIL::CaseRule *current_case;

  // This map contains the replacement pattern to be used in the right hand side
  // of an assignment. E.g. in the code "foo = bar; foo = func(foo);" the foo in
  // the right hand side of the 2nd assignment needs to be replace with the
  // temporary signal holding the value assigned in the first assignment. So
  // when the first assignment is processed the according information is
  // appended to subst_rvalue_from and subst_rvalue_to.
  stackmap<RTLIL::SigBit, RTLIL::SigBit> subst_rvalue_map;

  // This map contains the replacement pattern to be used in the left hand side
  // of an assignment. E.g. in the code "always @(posedge clk) foo <= bar" the
  // signal bar should not be connected to the signal foo. Instead it must be
  // connected to the temporary signal that is used as input for the register
  // that drives the signal foo.
  stackmap<RTLIL::SigBit, RTLIL::SigBit> subst_lvalue_map;

  // The code here generates a number of temporary signal for each output
  // register. This map helps generating nice numbered names for all this
  // temporary signals.
  std::map<RTLIL::Wire *, int> new_temp_count;

  // Buffer for generating the init action
  RTLIL::SigSpec init_lvalue, init_rvalue;

  // The most recently assigned $print cell \PRIORITY.
  int last_print_priority;

  ProcessGenerator(AstNode *always,
                   RTLIL::SigSpec initSyncSignalsArg = RTLIL::SigSpec())
      : always(always), initSyncSignals(initSyncSignalsArg),
        last_print_priority(0) {
    // rewrite lookahead references
    LookaheadRewriter la_rewriter(always);

    // generate process and simple root case
    proc = current_module->addProcess(stringf(
        "$proc$%s:%d$%d", RTLIL::encode_filename(always->filename).c_str(),
        always->location.first_line, autoidx++));
    set_src_attr(proc, always);
    for (auto &attr : always->attributes) {
      if (attr.second->type != AST_CONSTANT)
        always->input_error("Attribute `%s' with non-constant value!\n",
                            attr.first.c_str());
      proc->attributes[attr.first] = attr.second->asAttrConst();
    }
    current_case = &proc->root_case;

    // create initial temporary signal for all output registers
    RTLIL::SigSpec subst_lvalue_from, subst_lvalue_to;
    collect_lvalues(subst_lvalue_from, always, true, true);
    subst_lvalue_to = new_temp_signal(subst_lvalue_from);
    subst_lvalue_map = subst_lvalue_from.to_sigbit_map(subst_lvalue_to);

    bool found_global_syncs = false;
    bool found_anyedge_syncs = false;
    for (auto child : always->children) {
      if ((child->type == AST_POSEDGE || child->type == AST_NEGEDGE) &&
          GetSize(child->children) == 1 &&
          child->children.at(0)->type == AST_IDENTIFIER &&
          child->children.at(0)->id2ast &&
          child->children.at(0)->id2ast->type == AST_WIRE &&
          child->children.at(0)->id2ast->get_bool_attribute(ID::gclk)) {
        found_global_syncs = true;
      }
      if (child->type == AST_EDGE) {
        if (GetSize(child->children) == 1 &&
            child->children.at(0)->type == AST_IDENTIFIER &&
            child->children.at(0)->str == "\\$global_clock")
          found_global_syncs = true;
        else
          found_anyedge_syncs = true;
      }
    }

    if (found_anyedge_syncs) {
      if (found_global_syncs)
        always->input_error("Found non-synthesizable event list!\n");
      // log("Note: Assuming pure combinatorial block at %s in\n",
      //     always->loc_string().c_str());
      // log("compliance with IEC 62142(E):2005 / IEEE Std. 1364.1(E):2002. "
      //     "Recommending\n");
      // log("use of @* instead of @(...) for better match of synthesis and "
      //     "simulation.\n");
    }

    // create syncs for the process
    bool found_clocked_sync = false;
    for (auto child : always->children)
      if (child->type == AST_POSEDGE || child->type == AST_NEGEDGE) {
        if (GetSize(child->children) == 1 &&
            child->children.at(0)->type == AST_IDENTIFIER &&
            child->children.at(0)->id2ast &&
            child->children.at(0)->id2ast->type == AST_WIRE &&
            child->children.at(0)->id2ast->get_bool_attribute(ID::gclk))
          continue;
        found_clocked_sync = true;
        if (found_global_syncs || found_anyedge_syncs)
          always->input_error("Found non-synthesizable event list!\n");
        RTLIL::SyncRule *syncrule = new RTLIL::SyncRule;
        syncrule->type = child->type == AST_POSEDGE ? RTLIL::STp : RTLIL::STn;
        syncrule->signal = child->children[0]->genRTLIL();
        if (GetSize(syncrule->signal) != 1)
          always->input_error("Found posedge/negedge event on a signal that is "
                              "not 1 bit wide!\n");
        addChunkActions(syncrule->actions, subst_lvalue_from, subst_lvalue_to,
                        true);
        proc->syncs.push_back(syncrule);
      }
    if (proc->syncs.empty()) {
      RTLIL::SyncRule *syncrule = new RTLIL::SyncRule;
      syncrule->type = found_global_syncs ? RTLIL::STg : RTLIL::STa;
      syncrule->signal = RTLIL::SigSpec();
      addChunkActions(syncrule->actions, subst_lvalue_from, subst_lvalue_to,
                      true);
      proc->syncs.push_back(syncrule);
    }

    // create initial assignments for the temporary signals
    if ((flag_nolatches || always->get_bool_attribute(ID::nolatches) ||
         current_module->get_bool_attribute(ID::nolatches)) &&
        !found_clocked_sync) {
      subst_rvalue_map = subst_lvalue_from.to_sigbit_dict(
          RTLIL::SigSpec(RTLIL::State::Sx, GetSize(subst_lvalue_from)));
    } else {
      addChunkActions(current_case->actions, subst_lvalue_to,
                      subst_lvalue_from);
    }

    // process the AST
    for (auto child : always->children)
      if (child->type == AST_BLOCK)
        processAst(child);

    for (auto sync : proc->syncs)
      processMemWrites(sync);

    if (initSyncSignals.size() > 0) {
      RTLIL::SyncRule *sync = new RTLIL::SyncRule;
      sync->type = RTLIL::SyncType::STi;
      proc->syncs.push_back(sync);

      // log_assert(init_lvalue.size() == init_rvalue.size());

      int offset = 0;
      for (auto &init_lvalue_c : init_lvalue.chunks()) {
        RTLIL::SigSpec lhs = init_lvalue_c;
        RTLIL::SigSpec rhs = init_rvalue.extract(offset, init_lvalue_c.width);
        remove_unwanted_lvalue_bits(lhs, rhs);
        sync->actions.push_back(RTLIL::SigSig(lhs, rhs));
        offset += lhs.size();
      }
    }

    outputSignals = RTLIL::SigSpec(subst_lvalue_from);
  }

  void remove_unwanted_lvalue_bits(RTLIL::SigSpec &lhs, RTLIL::SigSpec &rhs) {
    RTLIL::SigSpec new_lhs, new_rhs;

    // log_assert(GetSize(lhs) == GetSize(rhs));
    for (int i = 0; i < GetSize(lhs); i++) {
      if (lhs[i].wire == nullptr)
        continue;
      new_lhs.append(lhs[i]);
      new_rhs.append(rhs[i]);
    }

    lhs = new_lhs;
    rhs = new_rhs;
  }

  // create new temporary signals
  RTLIL::SigSpec new_temp_signal(RTLIL::SigSpec sig) {
    std::vector<RTLIL::SigChunk> chunks = sig.chunks();

    for (int i = 0; i < GetSize(chunks); i++) {
      RTLIL::SigChunk &chunk = chunks[i];
      if (chunk.wire == NULL)
        continue;

      std::string wire_name;
      do {
        wire_name = stringf("$%d%s[%d:%d]", new_temp_count[chunk.wire]++,
                            chunk.wire->name.c_str(),
                            chunk.width + chunk.offset - 1, chunk.offset);
        ;
        if (chunk.wire->name.str().find('$') != std::string::npos)
          wire_name += stringf("$%d", autoidx++);
      } while (current_module->wires_.count(wire_name) > 0);

      RTLIL::Wire *wire = current_module->addWire(wire_name, chunk.width);
      set_src_attr(wire, always);

      chunk.wire = wire;
      chunk.offset = 0;
    }

    return chunks;
  }

  // recursively traverse the AST and collect all assigned signals
  void collect_lvalues(RTLIL::SigSpec &reg, AstNode *ast, bool type_eq,
                       bool type_le, bool run_sort_and_unify = true) {
    switch (ast->type) {
    case AST_CASE:
      for (auto child : ast->children)
        if (child != ast->children[0]) {
          // log_assert(child->type == AST_COND || child->type == AST_CONDX ||
          //            child->type == AST_CONDZ);
          collect_lvalues(reg, child, type_eq, type_le, false);
        }
      break;

    case AST_COND:
    case AST_CONDX:
    case AST_CONDZ:
    case AST_ALWAYS:
    case AST_INITIAL:
      for (auto child : ast->children)
        if (child->type == AST_BLOCK)
          collect_lvalues(reg, child, type_eq, type_le, false);
      break;

    case AST_BLOCK:
      for (auto child : ast->children) {
        if (child->type == AST_ASSIGN_EQ && type_eq)
          reg.append(child->children[0]->genRTLIL());
        if (child->type == AST_ASSIGN_LE && type_le)
          reg.append(child->children[0]->genRTLIL());
        if (child->type == AST_CASE || child->type == AST_BLOCK)
          collect_lvalues(reg, child, type_eq, type_le, false);
      }
      break;

    default:
      // log_abort();
      exit(1);
    }

    if (run_sort_and_unify) {
      std::set<RTLIL::SigBit> sorted_reg;
      for (auto bit : reg)
        if (bit.wire)
          sorted_reg.insert(bit);
      reg = RTLIL::SigSpec(sorted_reg);
    }
  }

  // remove all assignments to the given signal pattern in a case and all its
  // children. e.g. when the last statement in the code "a = 23; if (b) a = 42;
  // a = 0;" is processed this function is called to clean up the first two
  // assignments as they are overwritten by the third assignment.
  void removeSignalFromCaseTree(const RTLIL::SigSpec &pattern,
                                RTLIL::CaseRule *cs) {
    for (auto it = cs->actions.begin(); it != cs->actions.end(); it++)
      it->first.remove2(pattern, &it->second);

    for (auto it = cs->switches.begin(); it != cs->switches.end(); it++)
      for (auto it2 = (*it)->cases.begin(); it2 != (*it)->cases.end(); it2++)
        removeSignalFromCaseTree(pattern, *it2);
  }

  // add an assignment (aka "action") but split it up in chunks. this way huge
  // assignments are avoided and the generated $mux cells have a more "natural"
  // size.
  void addChunkActions(std::vector<RTLIL::SigSig> &actions,
                       RTLIL::SigSpec lvalue, RTLIL::SigSpec rvalue,
                       bool inSyncRule = false) {
    if (inSyncRule && initSyncSignals.size() > 0) {
      init_lvalue.append(lvalue.extract(initSyncSignals));
      init_rvalue.append(lvalue.extract(initSyncSignals, &rvalue));
      lvalue.remove2(initSyncSignals, &rvalue);
    }
    // log_assert(lvalue.size() == rvalue.size());

    int offset = 0;
    for (auto &lvalue_c : lvalue.chunks()) {
      RTLIL::SigSpec lhs = lvalue_c;
      RTLIL::SigSpec rhs = rvalue.extract(offset, lvalue_c.width);
      if (inSyncRule && lvalue_c.wire &&
          lvalue_c.wire->get_bool_attribute(ID::nosync))
        rhs = RTLIL::SigSpec(RTLIL::State::Sx, rhs.size());
      remove_unwanted_lvalue_bits(lhs, rhs);
      actions.push_back(RTLIL::SigSig(lhs, rhs));
      offset += lhs.size();
    }
  }

  // recursively process the AST and fill the RTLIL::Process
  void processAst(AstNode *ast) {
    switch (ast->type) {
    case AST_BLOCK:
      for (auto child : ast->children)
        processAst(child);
      break;

    case AST_ASSIGN_EQ:
    case AST_ASSIGN_LE: {
      RTLIL::SigSpec unmapped_lvalue = ast->children[0]->genRTLIL(),
                     lvalue = unmapped_lvalue;
      RTLIL::SigSpec rvalue = ast->children[1]->genWidthRTLIL(
          lvalue.size(), true, &subst_rvalue_map.stdmap());

      pool<RTLIL::SigBit> lvalue_sigbits;
      for (int i = 0; i < GetSize(lvalue); i++) {
        if (lvalue_sigbits.count(lvalue[i]) > 0) {
          unmapped_lvalue.remove(i);
          lvalue.remove(i);
          rvalue.remove(i--);
        } else
          lvalue_sigbits.insert(lvalue[i]);
      }

      lvalue.replace(subst_lvalue_map.stdmap());

      if (ast->type == AST_ASSIGN_EQ) {
        for (int i = 0; i < GetSize(unmapped_lvalue); i++)
          subst_rvalue_map.set(unmapped_lvalue[i], rvalue[i]);
      }

      removeSignalFromCaseTree(lvalue, current_case);
      remove_unwanted_lvalue_bits(lvalue, rvalue);
      current_case->actions.push_back(RTLIL::SigSig(lvalue, rvalue));
    } break;

    case AST_CASE: {
      int width_hint;
      bool sign_hint;
      ast->detectSignWidth(width_hint, sign_hint);

      RTLIL::SwitchRule *sw = new RTLIL::SwitchRule;
      set_src_attr(sw, ast);
      sw->signal = ast->children[0]->genWidthRTLIL(width_hint, sign_hint,
                                                   &subst_rvalue_map.stdmap());
      current_case->switches.push_back(sw);

      for (auto &attr : ast->attributes) {
        if (attr.second->type != AST_CONSTANT)
          ast->input_error("Attribute `%s' with non-constant value!\n",
                           attr.first.c_str());
        sw->attributes[attr.first] = attr.second->asAttrConst();
      }

      RTLIL::SigSpec this_case_eq_lvalue;
      collect_lvalues(this_case_eq_lvalue, ast, true, false);

      RTLIL::SigSpec this_case_eq_ltemp = new_temp_signal(this_case_eq_lvalue);

      RTLIL::SigSpec this_case_eq_rvalue = this_case_eq_lvalue;
      this_case_eq_rvalue.replace(subst_rvalue_map.stdmap());

      RTLIL::CaseRule *default_case = NULL;
      RTLIL::CaseRule *last_generated_case = NULL;
      for (auto child : ast->children) {
        if (child == ast->children[0])
          continue;
        // log_assert(child->type == AST_COND || child->type == AST_CONDX ||
        //            child->type == AST_CONDZ);

        subst_lvalue_map.save();
        subst_rvalue_map.save();

        for (int i = 0; i < GetSize(this_case_eq_lvalue); i++)
          subst_lvalue_map.set(this_case_eq_lvalue[i], this_case_eq_ltemp[i]);

        RTLIL::CaseRule *backup_case = current_case;
        current_case = new RTLIL::CaseRule;
        set_src_attr(current_case, child);
        last_generated_case = current_case;
        addChunkActions(current_case->actions, this_case_eq_ltemp,
                        this_case_eq_rvalue);
        for (auto node : child->children) {
          if (node->type == AST_DEFAULT)
            default_case = current_case;
          else if (node->type == AST_BLOCK)
            processAst(node);
          else
            current_case->compare.push_back(node->genWidthRTLIL(
                width_hint, sign_hint, &subst_rvalue_map.stdmap()));
        }
        if (default_case != current_case)
          sw->cases.push_back(current_case);
        else {
          // log_assert(current_case->compare.size() == 0);
        }
        current_case = backup_case;

        subst_lvalue_map.restore();
        subst_rvalue_map.restore();
      }

      if (last_generated_case != NULL &&
          ast->get_bool_attribute(ID::full_case) && default_case == NULL) {
#if 0
					// this is a valid transformation, but as optimization it is premature.
					// better: add a default case that assigns 'x' to everything, and let later
					// optimizations take care of the rest
					last_generated_case->compare.clear();
#else
        default_case = new RTLIL::CaseRule;
        addChunkActions(
            default_case->actions, this_case_eq_ltemp,
            RTLIL::SigSpec(State::Sx, GetSize(this_case_eq_rvalue)));
        sw->cases.push_back(default_case);
#endif
      } else {
        if (default_case == NULL) {
          default_case = new RTLIL::CaseRule;
          addChunkActions(default_case->actions, this_case_eq_ltemp,
                          this_case_eq_rvalue);
        }
        sw->cases.push_back(default_case);
      }

      for (int i = 0; i < GetSize(this_case_eq_lvalue); i++)
        subst_rvalue_map.set(this_case_eq_lvalue[i], this_case_eq_ltemp[i]);

      this_case_eq_lvalue.replace(subst_lvalue_map.stdmap());
      removeSignalFromCaseTree(this_case_eq_lvalue, current_case);
      addChunkActions(current_case->actions, this_case_eq_lvalue,
                      this_case_eq_ltemp);
    } break;

    case AST_WIRE:
      ast->input_error("Found reg declaration in block without label!\n");
      break;

    case AST_ASSIGN:
      ast->input_error("Found continous assignment in always/initial block!\n");
      break;

    case AST_PARAMETER:
    case AST_LOCALPARAM:
      ast->input_error("Found parameter declaration in block without label!\n");
      break;

    case AST_TCALL:
      if (ast->str == "$display" || ast->str == "$displayb" ||
          ast->str == "$displayh" || ast->str == "$displayo" ||
          ast->str == "$write" || ast->str == "$writeb" ||
          ast->str == "$writeh" || ast->str == "$writeo") {
        std::stringstream sstr;
        sstr << ast->str << "$" << ast->filename << ":"
             << ast->location.first_line << "$" << (autoidx++);

        RTLIL::Cell *cell = current_module->addCell(sstr.str(), ID($print));
        set_src_attr(cell, ast);

        RTLIL::SigSpec triggers;
        RTLIL::Const polarity;
        for (auto sync : proc->syncs) {
          if (sync->type == RTLIL::STp) {
            triggers.append(sync->signal);
            polarity.bits.push_back(RTLIL::S1);
          } else if (sync->type == RTLIL::STn) {
            triggers.append(sync->signal);
            polarity.bits.push_back(RTLIL::S0);
          }
        }
        cell->parameters[ID::TRG_WIDTH] = triggers.size();
        cell->parameters[ID::TRG_ENABLE] = !triggers.empty();
        cell->parameters[ID::TRG_POLARITY] = polarity;
        cell->parameters[ID::PRIORITY] = --last_print_priority;
        cell->setPort(ID::TRG, triggers);

        RTLIL::Wire *wire = current_module->addWire(sstr.str() + "_EN", 1);
        set_src_attr(wire, ast);
        cell->setPort(ID::EN, wire);

        proc->root_case.actions.push_back(RTLIL::SigSig(wire, false));
        current_case->actions.push_back(RTLIL::SigSig(wire, true));

        int default_base = 10;
        if (ast->str.back() == 'b')
          default_base = 2;
        else if (ast->str.back() == 'o')
          default_base = 8;
        else if (ast->str.back() == 'h')
          default_base = 16;

        std::vector<VerilogFmtArg> args;
        for (auto node : ast->children) {
          int width;
          bool is_signed;
          node->detectSignWidth(width, is_signed, nullptr);

          VerilogFmtArg arg = {};
          arg.filename = node->filename;
          arg.first_line = node->location.first_line;
          if (node->type == AST_CONSTANT && node->is_string) {
            arg.type = VerilogFmtArg::STRING;
            arg.str = node->bitsAsConst().decode_string();
            // and in case this will be used as an argument...
            arg.sig = node->bitsAsConst();
            arg.signed_ = false;
          } else if (node->type == AST_IDENTIFIER && node->str == "$time") {
            arg.type = VerilogFmtArg::TIME;
          } else if (node->type == AST_IDENTIFIER && node->str == "$realtime") {
            arg.type = VerilogFmtArg::TIME;
            arg.realtime = true;
          } else {
            arg.type = VerilogFmtArg::INTEGER;
            arg.sig =
                node->genWidthRTLIL(-1, false, &subst_rvalue_map.stdmap());
            arg.signed_ = is_signed;
          }
          args.push_back(arg);
        }

        Fmt fmt = {};
        fmt.parse_verilog(args, /*sformat_like=*/false, default_base,
                          /*task_name=*/ast->str, current_module->name);
        if (ast->str.substr(0, 8) == "$display")
          fmt.append_string("\n");
        fmt.emit_rtlil(cell);
      } else if (!ast->str.empty()) {
        // log_file_error(ast->filename, ast->location.first_line,
        //                "Found unsupported invocation of system task `%s'!\n",
        //                ast->str.c_str());
        exit(1);
      }
      break;

    case AST_NONE:
    case AST_FOR:
      break;

    default:
      // ast->dumpAst(NULL, "ast> ");
      // current_ast_mod->dumpAst(NULL, "mod> ");
      // log_abort();
      exit(1);
    }
  }

  void processMemWrites(RTLIL::SyncRule *sync) {
    // Maps per-memid AST_MEMWR IDs to indices in the mem_write_actions array.
    dict<std::pair<std::string, int>, int> port_map;
    for (auto child : always->children)
      if (child->type == AST_MEMWR) {
        std::string memid = child->str;
        int portid = child->children[3]->asInt(false);
        int cur_idx = GetSize(sync->mem_write_actions);
        RTLIL::MemWriteAction action;
        set_src_attr(&action, child);
        action.memid = memid;
        action.address = child->children[0]->genWidthRTLIL(
            -1, true, &subst_rvalue_map.stdmap());
        action.data = child->children[1]->genWidthRTLIL(
            current_module->memories[memid]->width, true,
            &subst_rvalue_map.stdmap());
        action.enable = child->children[2]->genWidthRTLIL(
            -1, true, &subst_rvalue_map.stdmap());
        RTLIL::Const orig_priority_mask = child->children[4]->bitsAsConst();
        RTLIL::Const priority_mask = RTLIL::Const(0, cur_idx);
        for (int i = 0; i < portid; i++) {
          int new_bit = port_map[std::make_pair(memid, i)];
          priority_mask.bits[new_bit] = orig_priority_mask.bits[i];
        }
        action.priority_mask = priority_mask;
        sync->mem_write_actions.push_back(action);
        port_map[std::make_pair(memid, portid)] = cur_idx;
      }
  }
};

// Generate RTLIL for a bind construct
//
// The AST node will have one or more AST_IDENTIFIER children, which were added
// by bind_target_instance in the parser. After these, it will have one or more
// cells, as parsed by single_cell. These have type AST_CELL.
//
// If there is more than one AST_IDENTIFIER, the first one should be considered
// a module identifier. If there is only one AST_IDENTIFIER, we can't tell at
// this point whether it's a module/interface name or the name of an instance
// because the correct interpretation depends on what's visible at elaboration
// time. For now, we just treat it as a target instance with unknown type, and
// we'll deal with the corner case in the hierarchy pass.
//
// To simplify downstream code, RTLIL::Binding only has a single target and
// single bound instance. If we see the syntax that allows more than one of
// either, we split it into multiple Binding objects.
std::vector<RTLIL::Binding *> AstNode::genBindings() const {
  // Partition children into identifiers and cells
  int num_ids = 0;
  for (int i = 0; i < GetSize(children); ++i) {
    if (children[i]->type != AST_IDENTIFIER) {
      // log_assert(i > 0);
      num_ids = i;
      break;
    }
  }

  // We should have found at least one child that's not an identifier
  // log_assert(num_ids > 0);

  // Make sense of the identifiers, extracting a possible type name and a
  // list of hierarchical IDs. We represent an unknown type with an empty
  // string.
  RTLIL::IdString tgt_type;
  int first_tgt_inst = 0;
  if (num_ids > 1) {
    tgt_type = children[0]->str;
    first_tgt_inst = 1;
  }

  std::vector<RTLIL::Binding *> ret;

  // At this point, we know that children with index >= first_tgt_inst and
  // index < num_ids are (hierarchical?) names of target instances. Make a
  // binding object for each of them, and fill in the generated instance
  // cells each time.
  for (int i = first_tgt_inst; i < num_ids; ++i) {
    const AstNode &tgt_child = *children[i];

    for (int j = num_ids; j < GetSize(children); ++j) {
      const AstNode &cell_child = *children[j];

      // log_assert(cell_child.type == AST_CELL);

      ret.push_back(new AST::Binding(tgt_type, tgt_child.str, cell_child));
    }
  }

  return ret;
}

// detect sign and width of an expression
void AstNode::detectSignWidthWorker(int &width_hint, bool &sign_hint,
                                    bool *found_real) {
  std::string type_name;
  bool sub_sign_hint = true;
  int sub_width_hint = -1;
  int this_width = 0;
  AstNode *range = NULL;
  AstNode *id_ast = NULL;

  bool local_found_real = false;
  if (found_real == NULL)
    found_real = &local_found_real;

  switch (type) {
  case AST_NONE:
    // unallocated enum, ignore
    break;
  case AST_CONSTANT:
    width_hint = std::max(width_hint, int(bits.size()));
    if (!is_signed)
      sign_hint = false;
    break;

  case AST_REALVALUE:
    *found_real = true;
    width_hint = std::max(width_hint, 32);
    break;

  case AST_IDENTIFIER:
    id_ast = id2ast;
    if (!id_ast) {
      if (current_scope.count(str))
        id_ast = current_scope[str];
      else {
        std::string alt = try_pop_module_prefix();
        if (current_scope.count(alt))
          id_ast = current_scope[alt];
      }
    }
    if (!id_ast)
      input_error("Failed to resolve identifier %s for width detection!\n",
                  str.c_str());
    if (id_ast->type == AST_PARAMETER || id_ast->type == AST_LOCALPARAM ||
        id_ast->type == AST_ENUM_ITEM) {
      if (id_ast->children.size() > 1 && id_ast->children[1]->range_valid) {
        this_width = id_ast->children[1]->range_left -
                     id_ast->children[1]->range_right + 1;
      } else {
        if (id_ast->children[0]->type != AST_CONSTANT)
          while (id_ast->simplify(true, 1, -1, false)) {
          }
        if (id_ast->children[0]->type == AST_CONSTANT)
          this_width = id_ast->children[0]->bits.size();
        else
          input_error("Failed to detect width for parameter %s!\n",
                      str.c_str());
      }
      if (children.size() != 0)
        range = children[0];
    } else if (id_ast->type == AST_WIRE || id_ast->type == AST_AUTOWIRE) {
      if (!id_ast->range_valid) {
        if (id_ast->type == AST_AUTOWIRE)
          this_width = 1;
        else {
          // current_ast_mod->dumpAst(NULL, "mod> ");
          // log("---\n");
          // id_ast->dumpAst(NULL, "decl> ");
          // dumpAst(NULL, "ref> ");
          input_error("Failed to detect width of signal access `%s'!\n",
                      str.c_str());
        }
      } else {
        this_width = id_ast->range_left - id_ast->range_right + 1;
        if (children.size() != 0)
          range = children[0];
      }
    } else if (id_ast->type == AST_GENVAR) {
      this_width = 32;
    } else if (id_ast->type == AST_MEMORY) {
      if (!id_ast->children[0]->range_valid)
        input_error("Failed to detect width of memory access `%s'!\n",
                    str.c_str());
      this_width = id_ast->children[0]->range_left -
                   id_ast->children[0]->range_right + 1;
      if (children.size() > 1)
        range = children[1];
    } else if (id_ast->type == AST_STRUCT_ITEM || id_ast->type == AST_STRUCT ||
               id_ast->type == AST_UNION) {
      AstNode *tmp_range = make_struct_member_range(this, id_ast);
      this_width = tmp_range->range_left - tmp_range->range_right + 1;
      delete tmp_range;
    } else
      input_error("Failed to detect width for identifier %s!\n", str.c_str());
    if (range) {
      if (range->children.size() == 1)
        this_width = 1;
      else if (!range->range_valid) {
        AstNode *left_at_zero_ast = children[0]->children[0]->clone_at_zero();
        AstNode *right_at_zero_ast =
            children[0]->children.size() >= 2
                ? children[0]->children[1]->clone_at_zero()
                : left_at_zero_ast->clone();
        while (left_at_zero_ast->simplify(true, 1, -1, false)) {
        }
        while (right_at_zero_ast->simplify(true, 1, -1, false)) {
        }
        if (left_at_zero_ast->type != AST_CONSTANT ||
            right_at_zero_ast->type != AST_CONSTANT)
          input_error("Unsupported expression on dynamic range select on "
                      "signal `%s'!\n",
                      str.c_str());
        this_width =
            abs(int(left_at_zero_ast->integer - right_at_zero_ast->integer)) +
            1;
        delete left_at_zero_ast;
        delete right_at_zero_ast;
      } else
        this_width = range->range_left - range->range_right + 1;
      sign_hint = false;
    }
    width_hint = std::max(width_hint, this_width);
    if (!id_ast->is_signed)
      sign_hint = false;
    break;

  case AST_TO_BITS:
    while (children[0]->simplify(true, 1, -1, false) == true) {
    }
    if (children[0]->type != AST_CONSTANT)
      input_error("Left operand of tobits expression is not constant!\n");
    children[1]->detectSignWidthWorker(sub_width_hint, sign_hint);
    width_hint = std::max(width_hint, children[0]->bitsAsConst().as_int());
    break;

  case AST_TO_SIGNED:
    children.at(0)->detectSignWidthWorker(width_hint, sub_sign_hint);
    break;

  case AST_TO_UNSIGNED:
    children.at(0)->detectSignWidthWorker(width_hint, sub_sign_hint);
    sign_hint = false;
    break;

  case AST_SELFSZ:
    sub_width_hint = 0;
    children.at(0)->detectSignWidthWorker(sub_width_hint, sign_hint);
    break;

  case AST_CAST_SIZE:
    while (children.at(0)->simplify(true, 1, -1, false)) {
    }
    if (children.at(0)->type != AST_CONSTANT)
      input_error("Static cast with non constant expression!\n");
    children.at(1)->detectSignWidthWorker(width_hint, sign_hint);
    this_width = children.at(0)->bitsAsConst().as_int();
    width_hint = std::max(width_hint, this_width);
    if (width_hint <= 0)
      input_error("Static cast with zero or negative size!\n");
    break;

  case AST_CONCAT:
    for (auto child : children) {
      sub_width_hint = 0;
      sub_sign_hint = true;
      child->detectSignWidthWorker(sub_width_hint, sub_sign_hint);
      this_width += sub_width_hint;
    }
    width_hint = std::max(width_hint, this_width);
    sign_hint = false;
    break;

  case AST_REPLICATE:
    while (children[0]->simplify(true, 1, -1, false) == true) {
    }
    if (children[0]->type != AST_CONSTANT)
      input_error("Left operand of replicate expression is not constant!\n");
    children[1]->detectSignWidthWorker(sub_width_hint, sub_sign_hint);
    width_hint = std::max(width_hint,
                          children[0]->bitsAsConst().as_int() * sub_width_hint);
    sign_hint = false;
    break;

  case AST_NEG:
  case AST_BIT_NOT:
  case AST_POS:
    children[0]->detectSignWidthWorker(width_hint, sign_hint, found_real);
    break;

  case AST_BIT_AND:
  case AST_BIT_OR:
  case AST_BIT_XOR:
  case AST_BIT_XNOR:
    for (auto child : children)
      child->detectSignWidthWorker(width_hint, sign_hint, found_real);
    break;

  case AST_REDUCE_AND:
  case AST_REDUCE_OR:
  case AST_REDUCE_XOR:
  case AST_REDUCE_XNOR:
  case AST_REDUCE_BOOL:
    width_hint = std::max(width_hint, 1);
    sign_hint = false;
    break;

  case AST_SHIFT_LEFT:
  case AST_SHIFT_RIGHT:
  case AST_SHIFT_SLEFT:
  case AST_SHIFT_SRIGHT:
  case AST_SHIFTX:
  case AST_SHIFT:
  case AST_POW:
    children[0]->detectSignWidthWorker(width_hint, sign_hint, found_real);
    break;

  case AST_LT:
  case AST_LE:
  case AST_EQ:
  case AST_NE:
  case AST_EQX:
  case AST_NEX:
  case AST_GE:
  case AST_GT:
    width_hint = std::max(width_hint, 1);
    sign_hint = false;
    break;

  case AST_ADD:
  case AST_SUB:
  case AST_MUL:
  case AST_DIV:
  case AST_MOD:
    for (auto child : children)
      child->detectSignWidthWorker(width_hint, sign_hint, found_real);
    break;

  case AST_LOGIC_AND:
  case AST_LOGIC_OR:
  case AST_LOGIC_NOT:
    width_hint = std::max(width_hint, 1);
    sign_hint = false;
    break;

  case AST_TERNARY:
    children.at(1)->detectSignWidthWorker(width_hint, sign_hint, found_real);
    children.at(2)->detectSignWidthWorker(width_hint, sign_hint, found_real);
    break;

  case AST_MEMRD:
    if (!id2ast->is_signed)
      sign_hint = false;
    if (!id2ast->children[0]->range_valid)
      input_error("Failed to detect width of memory access `%s'!\n",
                  str.c_str());
    this_width =
        id2ast->children[0]->range_left - id2ast->children[0]->range_right + 1;
    width_hint = std::max(width_hint, this_width);
    break;

  case AST_CASE: {
    // This detects the _overall_ sign and width to be used for comparing
    // the case expression with the case item expressions. The case
    // expression and case item expressions are extended to the maximum
    // width among them, and are only interpreted as signed if all of them
    // are signed.
    width_hint = -1;
    sign_hint = true;
    auto visit_case_expr = [&width_hint, &sign_hint](AstNode *node) {
      int sub_width_hint = -1;
      bool sub_sign_hint = true;
      node->detectSignWidth(sub_width_hint, sub_sign_hint);
      width_hint = std::max(width_hint, sub_width_hint);
      sign_hint &= sub_sign_hint;
    };
    visit_case_expr(children[0]);
    for (size_t i = 1; i < children.size(); i++) {
      AstNode *child = children[i];
      for (AstNode *v : child->children)
        if (v->type != AST_DEFAULT && v->type != AST_BLOCK)
          visit_case_expr(v);
    }
    break;
  }

  case AST_PREFIX:
    // Prefix nodes always resolve to identifiers in generate loops, so we
    // can simply perform the resolution to determine the sign and width.
    simplify(true, 1, -1, false);
    // log_assert(type == AST_IDENTIFIER);
    detectSignWidthWorker(width_hint, sign_hint, found_real);
    break;

  case AST_FCALL:
    if (str == "\\$anyconst" || str == "\\$anyseq" || str == "\\$allconst" ||
        str == "\\$allseq") {
      if (GetSize(children) == 1) {
        while (children[0]->simplify(true, 1, -1, false) == true) {
        }
        if (children[0]->type != AST_CONSTANT)
          input_error("System function %s called with non-const argument!\n",
                      RTLIL::unescape_id(str).c_str());
        width_hint = std::max(width_hint, int(children[0]->asInt(true)));
      }
      break;
    }
    if (str == "\\$past") {
      if (GetSize(children) > 0) {
        sub_width_hint = 0;
        sub_sign_hint = true;
        children.at(0)->detectSignWidthWorker(sub_width_hint, sub_sign_hint);
        width_hint = std::max(width_hint, sub_width_hint);
        sign_hint &= sub_sign_hint;
      }
      break;
    }
    if (str == "\\$size" || str == "\\$bits" || str == "\\$high" ||
        str == "\\$low" || str == "\\$left" || str == "\\$right") {
      width_hint = std::max(width_hint, 32);
      break;
    }
    if (current_scope.count(str)) {
      // This width detection is needed for function calls which are
      // unelaborated, which currently applies to calls to functions
      // reached via unevaluated ternary branches or used in case or case
      // item expressions.
      const AstNode *func = current_scope.at(str);
      if (func->type != AST_FUNCTION)
        input_error("Function call to %s resolved to something that isn't a "
                    "function!\n",
                    RTLIL::unescape_id(str).c_str());
      const AstNode *wire = nullptr;
      for (const AstNode *child : func->children)
        if (child->str == func->str) {
          wire = child;
          break;
        }
      // log_assert(wire && wire->type == AST_WIRE);
      sign_hint &= wire->is_signed;
      int result_width = 1;
      if (!wire->children.empty()) {
        // log_assert(wire->children.size() == 1);
        const AstNode *range = wire->children.at(0);
        // log_assert(range->type == AST_RANGE && range->children.size() == 2);
        AstNode *left = range->children.at(0)->clone();
        AstNode *right = range->children.at(1)->clone();
        left->set_in_param_flag(true);
        right->set_in_param_flag(true);
        while (left->simplify(true, 1, -1, false)) {
        }
        while (right->simplify(true, 1, -1, false)) {
        }
        if (left->type != AST_CONSTANT || right->type != AST_CONSTANT)
          input_error("Function %s has non-constant width!",
                      RTLIL::unescape_id(str).c_str());
        result_width = abs(int(left->asInt(true) - right->asInt(true)));
        delete left;
        delete right;
      }
      width_hint = std::max(width_hint, result_width);
      break;
    }
    YS_FALLTHROUGH

  // everything should have been handled above -> print error if not.
  default:
    AstNode *current_scope_ast = current_ast_mod == nullptr
                                     ? VERILOG_FRONTEND::current_ast
                                     : current_ast_mod;
    // for (auto f : log_files)
    //   current_scope_ast->dumpAst(f, "verilog-ast> ");
    input_error("Don't know how to detect sign and width for %s node!\n",
                type2str(type).c_str());
  }

  if (*found_real)
    sign_hint = true;
}

// detect sign and width of an expression
void AstNode::detectSignWidth(int &width_hint, bool &sign_hint,
                              bool *found_real) {
  width_hint = -1;
  sign_hint = true;
  if (found_real)
    *found_real = false;
  detectSignWidthWorker(width_hint, sign_hint, found_real);

  constexpr int kWidthLimit = 1 << 24;
  if (width_hint >= kWidthLimit)
    input_error("Expression width %d exceeds implementation limit of %d!\n",
                width_hint, kWidthLimit);
}

static void check_unique_id(RTLIL::Module *module, RTLIL::IdString id,
                            const AstNode *node, const char *to_add_kind) {
  auto already_exists = [&](const RTLIL::AttrObject *existing,
                            const char *existing_kind) {
    std::string src = existing->get_string_attribute(ID::src);
    std::string location_str = "earlier";
    if (!src.empty())
      location_str = "at " + src;
    node->input_error("Cannot add %s `%s' because a %s with the same name was "
                      "already created %s!\n",
                      to_add_kind, id.c_str(), existing_kind,
                      location_str.c_str());
  };

  if (const RTLIL::Wire *wire = module->wire(id))
    already_exists(wire, "signal");
  if (const RTLIL::Cell *cell = module->cell(id))
    already_exists(cell, "cell");
  if (module->processes.count(id))
    already_exists(module->processes.at(id), "process");
  if (module->memories.count(id))
    already_exists(module->memories.at(id), "memory");
}

// create RTLIL from an AST node
// all generated cells, wires and processes are added to the module pointed to
// by 'current_module' when the AST node is an expression (AST_ADD, AST_BIT_XOR,
// etc.), the result signal is returned.
//
// note that this function is influenced by a number of global variables that
// might be set when called from genWidthRTLIL(). also note that this function
// recursively calls itself to transform larger expressions into a netlist of
// cells.
RTLIL::SigSpec AstNode::genRTLIL(int width_hint, bool sign_hint) {
  // in the following big switch() statement there are some uses of
  // Clifford's Device (http://www.clifford.at/cfun/cliffdev/). In this
  // cases this variable is used to hold the type of the cell that should
  // be instantiated for this type of AST node.
  RTLIL::IdString type_name;

  current_filename = filename;

  switch (type) {
  // simply ignore this nodes.
  // they are either leftovers from simplify() or are referenced by other nodes
  // and are only accessed here thru this references
  case AST_NONE:
  case AST_TASK:
  case AST_FUNCTION:
  case AST_DPI_FUNCTION:
  case AST_AUTOWIRE:
  case AST_DEFPARAM:
  case AST_GENVAR:
  case AST_GENFOR:
  case AST_GENBLOCK:
  case AST_GENIF:
  case AST_GENCASE:
  case AST_PACKAGE:
  case AST_ENUM:
  case AST_MODPORT:
  case AST_MODPORTMEMBER:
  case AST_TYPEDEF:
  case AST_STRUCT:
  case AST_UNION:
    break;
  case AST_INTERFACEPORT: {
    // If a port in a module with unknown type is found, mark it with the
    // attribute 'is_interface' This is used by the hierarchy pass to know when
    // it can replace interface connection with the individual signals.
    RTLIL::IdString id = str;
    check_unique_id(current_module, id, this, "interface port");
    RTLIL::Wire *wire = current_module->addWire(id, 1);
    set_src_attr(wire, this);
    wire->start_offset = 0;
    wire->port_id = port_id;
    wire->port_input = true;
    wire->port_output = true;
    wire->set_bool_attribute(ID::is_interface);
    if (children.size() > 0) {
      for (size_t i = 0; i < children.size(); i++) {
        if (children[i]->type == AST_INTERFACEPORTTYPE) {
          std::pair<std::string, std::string> res =
              AST::split_modport_from_type(children[i]->str);
          wire->attributes[ID::interface_type] = res.first;
          if (res.second != "")
            wire->attributes[ID::interface_modport] = res.second;
          break;
        }
      }
    }
    wire->upto = 0;
  } break;
  case AST_INTERFACEPORTTYPE:
    break;

  // remember the parameter, needed for example in techmap
  case AST_PARAMETER:
    current_module->avail_parameters(str);
    if (GetSize(children) >= 1 && children[0]->type == AST_CONSTANT) {
      current_module->parameter_default_values[str] =
          children[0]->asParaConst();
    }
    YS_FALLTHROUGH
  case AST_LOCALPARAM:
    if (flag_pwires) {
      if (GetSize(children) < 1 || children[0]->type != AST_CONSTANT)
        input_error("Parameter `%s' with non-constant value!\n", str.c_str());

      RTLIL::Const val = children[0]->bitsAsConst();
      RTLIL::IdString id = str;
      check_unique_id(current_module, id, this, "pwire");
      RTLIL::Wire *wire = current_module->addWire(id, GetSize(val));
      current_module->connect(wire, val);
      wire->is_signed = children[0]->is_signed;

      set_src_attr(wire, this);
      wire->attributes[type == AST_PARAMETER ? ID::parameter : ID::localparam] =
          1;

      for (auto &attr : attributes) {
        if (attr.second->type != AST_CONSTANT)
          input_error("Attribute `%s' with non-constant value!\n",
                      attr.first.c_str());
        wire->attributes[attr.first] = attr.second->asAttrConst();
      }
    }
    break;

  // create an RTLIL::Wire for an AST_WIRE node
  case AST_WIRE: {
    if (!range_valid)
      input_error("Signal `%s' with non-constant width!\n", str.c_str());

    if (!(range_left + 1 >= range_right))
      input_error("Signal `%s' with invalid width range %d!\n", str.c_str(),
                  range_left - range_right + 1);

    RTLIL::IdString id = str;
    check_unique_id(current_module, id, this, "signal");
    RTLIL::Wire *wire =
        current_module->addWire(id, range_left - range_right + 1);
    set_src_attr(wire, this);
    wire->start_offset = range_right;
    wire->port_id = port_id;
    wire->port_input = is_input;
    wire->port_output = is_output;
    wire->upto = range_swapped;
    wire->is_signed = is_signed;

    for (auto &attr : attributes) {
      if (attr.second->type != AST_CONSTANT)
        input_error("Attribute `%s' with non-constant value!\n",
                    attr.first.c_str());
      wire->attributes[attr.first] = attr.second->asAttrConst();
    }

    if (is_wand)
      wire->set_bool_attribute(ID::wand);
    if (is_wor)
      wire->set_bool_attribute(ID::wor);
  } break;

  // create an RTLIL::Memory for an AST_MEMORY node
  case AST_MEMORY: {
    // log_assert(children.size() >= 2);
    // log_assert(children[0]->type == AST_RANGE);
    // log_assert(children[1]->type == AST_RANGE);

    if (!children[0]->range_valid || !children[1]->range_valid)
      input_error("Memory `%s' with non-constant width or size!\n",
                  str.c_str());

    RTLIL::Memory *memory = new RTLIL::Memory;
    set_src_attr(memory, this);
    memory->name = str;
    memory->width = children[0]->range_left - children[0]->range_right + 1;
    if (children[1]->range_right < children[1]->range_left) {
      memory->start_offset = children[1]->range_right;
      memory->size = children[1]->range_left - children[1]->range_right + 1;
    } else {
      memory->start_offset = children[1]->range_left;
      memory->size = children[1]->range_right - children[1]->range_left + 1;
    }
    check_unique_id(current_module, memory->name, this, "memory");
    current_module->memories[memory->name] = memory;

    for (auto &attr : attributes) {
      if (attr.second->type != AST_CONSTANT)
        input_error("Attribute `%s' with non-constant value!\n",
                    attr.first.c_str());
      memory->attributes[attr.first] = attr.second->asAttrConst();
    }
  } break;

  // simply return the corresponding RTLIL::SigSpec for an AST_CONSTANT node
  case AST_CONSTANT:
  case AST_REALVALUE: {
    if (width_hint < 0)
      detectSignWidth(width_hint, sign_hint);
    is_signed = sign_hint;

    if (type == AST_CONSTANT) {
      if (is_unsized) {
        return RTLIL::SigSpec(bitsAsUnsizedConst(width_hint));
      } else {
        return RTLIL::SigSpec(bitsAsConst());
      }
    }

    RTLIL::SigSpec sig = realAsConst(width_hint);
    // log_file_warning(filename, location.first_line,
    //                  "converting real value %e to binary %s.\n", realvalue,
    //                  log_signal(sig));
    return sig;
  }

  // simply return the corresponding RTLIL::SigSpec for an AST_IDENTIFIER node
  // for identifiers with dynamic bit ranges (e.g. "foo[bar]" or
  // "foo[bar+3:bar]") a shifter cell is created and the output signal of this
  // cell is returned
  case AST_IDENTIFIER: {
    RTLIL::Wire *wire = NULL;
    RTLIL::SigChunk chunk;
    bool is_interface = false;

    AST::AstNode *member_node = NULL;
    int add_undef_bits_msb = 0;
    int add_undef_bits_lsb = 0;

    // log_assert(id2ast != nullptr);

    if (id2ast->type == AST_AUTOWIRE &&
        current_module->wires_.count(str) == 0) {
      RTLIL::Wire *wire = current_module->addWire(str);
      set_src_attr(wire, this);
      wire->name = str;

      // If we are currently processing a bind directive which wires up
      // signals or parameters explicitly, rather than with .*, then
      // current_module will start out empty and we don't want to warn the
      // user about it: we'll spot broken wiring later, when we run the
      // hierarchy pass.
      if (dynamic_cast<RTLIL::Binding *>(current_module)) {
        /* nothing to do here */
      } else if (flag_autowire) {
        // log_file_warning(filename, location.first_line,
        //                  "Identifier `%s' is implicitly declared.\n",
        //                  str.c_str());
      } else
        input_error("Identifier `%s' is implicitly declared and "
                    "`default_nettype is set to none.\n",
                    str.c_str());
    } else if (id2ast->type == AST_PARAMETER ||
               id2ast->type == AST_LOCALPARAM ||
               id2ast->type == AST_ENUM_ITEM) {
      if (id2ast->children[0]->type != AST_CONSTANT)
        input_error("Parameter %s does not evaluate to constant value!\n",
                    str.c_str());
      chunk = RTLIL::Const(id2ast->children[0]->bits);
      goto use_const_chunk;
    } else if ((id2ast->type == AST_WIRE || id2ast->type == AST_AUTOWIRE ||
                id2ast->type == AST_MEMORY) &&
               current_module->wires_.count(str) != 0) {
      RTLIL::Wire *current_wire = current_module->wire(str);
      if (current_wire->get_bool_attribute(ID::is_interface))
        is_interface = true;
      // Ignore
    }
    // If an identifier is found that is not already known, assume that it is an
    // interface:
    else if (1) { // FIXME: Check if sv_mode first?
      is_interface = true;
    } else {
      input_error("Identifier `%s' doesn't map to any signal!\n", str.c_str());
    }

    if (id2ast->type == AST_MEMORY)
      input_error("Identifier `%s' does map to an unexpanded memory!\n",
                  str.c_str());

    // If identifier is an interface, create a RTLIL::SigSpec with a dummy wire
    // with a attribute called 'is_interface' This makes it possible for the
    // hierarchy pass to see what are interface connections and then replace
    // them with the individual signals:
    if (is_interface) {
      RTLIL::IdString dummy_wire_name =
          stringf("$dummywireforinterface%s", str.c_str());
      RTLIL::Wire *dummy_wire = current_module->wire(dummy_wire_name);
      if (!dummy_wire) {
        dummy_wire = current_module->addWire(dummy_wire_name);
        dummy_wire->set_bool_attribute(ID::is_interface);
      }
      return dummy_wire;
    }

    wire = current_module->wires_[str];
    chunk.wire = wire;
    chunk.width = wire->width;
    chunk.offset = 0;

    if ((member_node = get_struct_member(this))) {
      // Clamp wire chunk to range of member within struct/union.
      chunk.width = member_node->range_left - member_node->range_right + 1;
      chunk.offset = member_node->range_right;
    }

  use_const_chunk:
    if (children.size() != 0) {
      if (children[0]->type != AST_RANGE)
        input_error("Single range expected.\n");
      int source_width = id2ast->range_left - id2ast->range_right + 1;
      int source_offset = id2ast->range_right;
      int chunk_left = source_width - 1;
      int chunk_right = 0;

      if (member_node) {
        // Clamp wire chunk to range of member within struct/union.
        // log_assert(!source_offset && !id2ast->range_swapped);
        chunk_left = chunk.offset + chunk.width - 1;
        chunk_right = chunk.offset;
      }

      if (!children[0]->range_valid) {
        AstNode *left_at_zero_ast = children[0]->children[0]->clone_at_zero();
        AstNode *right_at_zero_ast =
            children[0]->children.size() >= 2
                ? children[0]->children[1]->clone_at_zero()
                : left_at_zero_ast->clone();
        while (left_at_zero_ast->simplify(true, 1, -1, false)) {
        }
        while (right_at_zero_ast->simplify(true, 1, -1, false)) {
        }
        if (left_at_zero_ast->type != AST_CONSTANT ||
            right_at_zero_ast->type != AST_CONSTANT)
          input_error("Unsupported expression on dynamic range select on "
                      "signal `%s'!\n",
                      str.c_str());
        int width =
            abs(int(left_at_zero_ast->integer - right_at_zero_ast->integer)) +
            1;
        AstNode *fake_ast =
            new AstNode(AST_NONE, clone(),
                        children[0]->children.size() >= 2
                            ? children[0]->children[1]->clone()
                            : children[0]->children[0]->clone());
        fake_ast->children[0]->delete_children();
        if (member_node)
          fake_ast->children[0]->set_attribute(ID::wiretype,
                                               member_node->clone());

        int fake_ast_width = 0;
        bool fake_ast_sign = true;
        fake_ast->children[1]->detectSignWidth(fake_ast_width, fake_ast_sign);
        RTLIL::SigSpec shift_val =
            fake_ast->children[1]->genRTLIL(fake_ast_width, fake_ast_sign);

        if (source_offset != 0) {
          shift_val = current_module->Sub(NEW_ID, shift_val, source_offset,
                                          fake_ast_sign);
          fake_ast->children[1]->is_signed = true;
        }
        if (id2ast->range_swapped) {
          shift_val =
              current_module->Sub(NEW_ID, RTLIL::SigSpec(source_width - width),
                                  shift_val, fake_ast_sign);
          fake_ast->children[1]->is_signed = true;
        }
        if (GetSize(shift_val) >= 32)
          fake_ast->children[1]->is_signed = true;
        RTLIL::SigSpec sig =
            binop2rtlil(fake_ast, ID($shiftx), width,
                        fake_ast->children[0]->genRTLIL(), shift_val);
        delete left_at_zero_ast;
        delete right_at_zero_ast;
        delete fake_ast;
        return sig;
      } else {
        chunk.width = children[0]->range_left - children[0]->range_right + 1;
        chunk.offset += children[0]->range_right - source_offset;
        if (id2ast->range_swapped)
          chunk.offset = source_width - (chunk.offset + chunk.width);
        if (chunk.offset > chunk_left ||
            chunk.offset + chunk.width < chunk_right) {
          // if (chunk.width == 1)
          //   log_file_warning(filename, location.first_line,
          //                    "Range select out of bounds on signal `%s': "
          //                    "Setting result bit to undef.\n",
          //                    str.c_str());
          // else
          //   log_file_warning(filename, location.first_line,
          //                    "Range select [%d:%d] out of bounds on signal "
          //                    "`%s': Setting all %d result bits to undef.\n",
          //                    children[0]->range_left,
          //                    children[0]->range_right, str.c_str(),
          //                    chunk.width);
          chunk = RTLIL::SigChunk(RTLIL::State::Sx, chunk.width);
        } else {
          if (chunk.offset + chunk.width - 1 > chunk_left) {
            add_undef_bits_msb = (chunk.offset + chunk.width - 1) - chunk_left;
            chunk.width -= add_undef_bits_msb;
          }
          if (chunk.offset < chunk_right) {
            add_undef_bits_lsb = chunk_right - chunk.offset;
            chunk.width -= add_undef_bits_lsb;
            chunk.offset += add_undef_bits_lsb;
          }
          // if (add_undef_bits_lsb)
          //   log_file_warning(filename, location.first_line,
          //                    "Range [%d:%d] select out of bounds on signal "
          //                    "`%s': Setting %d LSB bits to undef.\n",
          //                    children[0]->range_left,
          //                    children[0]->range_right, str.c_str(),
          //                    add_undef_bits_lsb);
          // if (add_undef_bits_msb)
          //   log_file_warning(filename, location.first_line,
          //                    "Range [%d:%d] select out of bounds on signal "
          //                    "`%s': Setting %d MSB bits to undef.\n",
          //                    children[0]->range_left,
          //                    children[0]->range_right, str.c_str(),
          //                    add_undef_bits_msb);
        }
      }
    }

    RTLIL::SigSpec sig = {RTLIL::SigSpec(RTLIL::State::Sx, add_undef_bits_msb),
                          chunk,
                          RTLIL::SigSpec(RTLIL::State::Sx, add_undef_bits_lsb)};

    if (genRTLIL_subst_ptr)
      sig.replace(*genRTLIL_subst_ptr);

    is_signed = children.size() > 0 ? false : id2ast->is_signed && sign_hint;
    return sig;
  }

  // just pass thru the signal. the parent will evaluate the is_signed property
  // and interpret the SigSpec accordingly
  case AST_TO_SIGNED:
  case AST_TO_UNSIGNED:
  case AST_SELFSZ: {
    RTLIL::SigSpec sig = children[0]->genRTLIL();
    if (sig.size() < width_hint)
      sig.extend_u0(width_hint, sign_hint);
    is_signed = sign_hint;
    return sig;
  }

  // changing the size of signal can be done directly using RTLIL::SigSpec
  case AST_CAST_SIZE: {
    RTLIL::SigSpec size = children[0]->genRTLIL();
    if (!size.is_fully_const())
      input_error("Static cast with non constant expression!\n");
    int width = size.as_int();
    if (width <= 0)
      input_error("Static cast with zero or negative size!\n");
    // determine the *signedness* of the expression
    int sub_width_hint = -1;
    bool sub_sign_hint = true;
    children[1]->detectSignWidth(sub_width_hint, sub_sign_hint);
    // generate the signal given the *cast's* size and the
    // *expression's* signedness
    RTLIL::SigSpec sig = children[1]->genWidthRTLIL(width, sub_sign_hint);
    // context may effect this node's signedness, but not that of the
    // casted expression
    is_signed = sign_hint;
    return sig;
  }

  // concatenation of signals can be done directly using RTLIL::SigSpec
  case AST_CONCAT: {
    RTLIL::SigSpec sig;
    for (auto it = children.begin(); it != children.end(); it++)
      sig.append((*it)->genRTLIL());
    if (sig.size() < width_hint)
      sig.extend_u0(width_hint, false);
    return sig;
  }

  // replication of signals can be done directly using RTLIL::SigSpec
  case AST_REPLICATE: {
    RTLIL::SigSpec left = children[0]->genRTLIL();
    RTLIL::SigSpec right = children[1]->genRTLIL();
    if (!left.is_fully_const())
      input_error("Left operand of replicate expression is not constant!\n");
    int count = left.as_int();
    RTLIL::SigSpec sig;
    for (int i = 0; i < count; i++)
      sig.append(right);
    if (sig.size() < width_hint)
      sig.extend_u0(width_hint, false);
    is_signed = false;
    return sig;
  }

    // generate cells for unary operations: $not, $pos, $neg
    if (0) {
    case AST_BIT_NOT:
      type_name = ID($not);
    }
    if (0) {
    case AST_POS:
      type_name = ID($pos);
    }
    if (0) {
    case AST_NEG:
      type_name = ID($neg);
    }
    {
      RTLIL::SigSpec arg = children[0]->genRTLIL(width_hint, sign_hint);
      is_signed = children[0]->is_signed;
      int width = arg.size();
      if (width_hint > 0) {
        width = width_hint;
        widthExtend(this, arg, width, is_signed);
      }
      return uniop2rtlil(this, type_name, width, arg);
    }

    // generate cells for binary operations: $and, $or, $xor, $xnor
    if (0) {
    case AST_BIT_AND:
      type_name = ID($and);
    }
    if (0) {
    case AST_BIT_OR:
      type_name = ID($or);
    }
    if (0) {
    case AST_BIT_XOR:
      type_name = ID($xor);
    }
    if (0) {
    case AST_BIT_XNOR:
      type_name = ID($xnor);
    }
    {
      if (width_hint < 0)
        detectSignWidth(width_hint, sign_hint);
      RTLIL::SigSpec left = children[0]->genRTLIL(width_hint, sign_hint);
      RTLIL::SigSpec right = children[1]->genRTLIL(width_hint, sign_hint);
      int width = std::max(left.size(), right.size());
      if (width_hint > 0)
        width = width_hint;
      is_signed = children[0]->is_signed && children[1]->is_signed;
      return binop2rtlil(this, type_name, width, left, right);
    }

    // generate cells for unary operations: $reduce_and, $reduce_or,
    // $reduce_xor, $reduce_xnor
    if (0) {
    case AST_REDUCE_AND:
      type_name = ID($reduce_and);
    }
    if (0) {
    case AST_REDUCE_OR:
      type_name = ID($reduce_or);
    }
    if (0) {
    case AST_REDUCE_XOR:
      type_name = ID($reduce_xor);
    }
    if (0) {
    case AST_REDUCE_XNOR:
      type_name = ID($reduce_xnor);
    }
    {
      RTLIL::SigSpec arg = children[0]->genRTLIL();
      RTLIL::SigSpec sig =
          uniop2rtlil(this, type_name, std::max(width_hint, 1), arg);
      return sig;
    }

    // generate cells for unary operations: $reduce_bool
    // (this is actually just an $reduce_or, but for clarity a different cell
    // type is used)
    if (0) {
    case AST_REDUCE_BOOL:
      type_name = ID($reduce_bool);
    }
    {
      RTLIL::SigSpec arg = children[0]->genRTLIL();
      RTLIL::SigSpec sig =
          arg.size() > 1
              ? uniop2rtlil(this, type_name, std::max(width_hint, 1), arg)
              : arg;
      return sig;
    }

    // generate cells for binary operations: $shl, $shr, $sshl, $sshr
    if (0) {
    case AST_SHIFT_LEFT:
      type_name = ID($shl);
    }
    if (0) {
    case AST_SHIFT_RIGHT:
      type_name = ID($shr);
    }
    if (0) {
    case AST_SHIFT_SLEFT:
      type_name = ID($sshl);
    }
    if (0) {
    case AST_SHIFT_SRIGHT:
      type_name = ID($sshr);
    }
    if (0) {
    case AST_SHIFTX:
      type_name = ID($shiftx);
    }
    if (0) {
    case AST_SHIFT:
      type_name = ID($shift);
    }
    {
      if (width_hint < 0)
        detectSignWidth(width_hint, sign_hint);
      RTLIL::SigSpec left = children[0]->genRTLIL(width_hint, sign_hint);
      RTLIL::SigSpec right = children[1]->genRTLIL();
      int width = width_hint > 0 ? width_hint : left.size();
      is_signed = children[0]->is_signed;
      return binop2rtlil(this, type_name, width, left, right);
    }

  // generate cells for binary operations: $pow
  case AST_POW: {
    int right_width;
    bool right_signed;
    children[1]->detectSignWidth(right_width, right_signed);
    if (width_hint < 0)
      detectSignWidth(width_hint, sign_hint);
    RTLIL::SigSpec left = children[0]->genRTLIL(width_hint, sign_hint);
    RTLIL::SigSpec right = children[1]->genRTLIL(right_width, right_signed);
    int width = width_hint > 0 ? width_hint : left.size();
    is_signed = children[0]->is_signed;
    if (!flag_noopt && left.is_fully_const() && left.as_int() == 2 &&
        !right_signed)
      return binop2rtlil(this, ID($shl), width, RTLIL::SigSpec(1, left.size()),
                         right);
    return binop2rtlil(this, ID($pow), width, left, right);
  }

    // generate cells for binary operations: $lt, $le, $eq, $ne, $ge, $gt
    if (0) {
    case AST_LT:
      type_name = ID($lt);
    }
    if (0) {
    case AST_LE:
      type_name = ID($le);
    }
    if (0) {
    case AST_EQ:
      type_name = ID($eq);
    }
    if (0) {
    case AST_NE:
      type_name = ID($ne);
    }
    if (0) {
    case AST_EQX:
      type_name = ID($eqx);
    }
    if (0) {
    case AST_NEX:
      type_name = ID($nex);
    }
    if (0) {
    case AST_GE:
      type_name = ID($ge);
    }
    if (0) {
    case AST_GT:
      type_name = ID($gt);
    }
    {
      int width = std::max(width_hint, 1);
      width_hint = -1, sign_hint = true;
      children[0]->detectSignWidthWorker(width_hint, sign_hint);
      children[1]->detectSignWidthWorker(width_hint, sign_hint);
      RTLIL::SigSpec left = children[0]->genRTLIL(width_hint, sign_hint);
      RTLIL::SigSpec right = children[1]->genRTLIL(width_hint, sign_hint);
      RTLIL::SigSpec sig = binop2rtlil(this, type_name, width, left, right);
      return sig;
    }

    // generate cells for binary operations: $add, $sub, $mul, $div, $mod
    if (0) {
    case AST_ADD:
      type_name = ID($add);
    }
    if (0) {
    case AST_SUB:
      type_name = ID($sub);
    }
    if (0) {
    case AST_MUL:
      type_name = ID($mul);
    }
    if (0) {
    case AST_DIV:
      type_name = ID($div);
    }
    if (0) {
    case AST_MOD:
      type_name = ID($mod);
    }
    {
      if (width_hint < 0)
        detectSignWidth(width_hint, sign_hint);
      RTLIL::SigSpec left = children[0]->genRTLIL(width_hint, sign_hint);
      RTLIL::SigSpec right = children[1]->genRTLIL(width_hint, sign_hint);
#if 0
			int width = max(left.size(), right.size());
			if (width > width_hint && width_hint > 0)
				width = width_hint;
			if (width < width_hint) {
				if (type == AST_ADD || type == AST_SUB || type == AST_DIV)
					width++;
				if (type == AST_SUB && (!children[0]->is_signed || !children[1]->is_signed))
					width = width_hint;
				if (type == AST_MUL)
					width = min(left.size() + right.size(), width_hint);
			}
#else
      int width = std::max(std::max(left.size(), right.size()), width_hint);
#endif
      is_signed = children[0]->is_signed && children[1]->is_signed;
      return binop2rtlil(this, type_name, width, left, right);
    }

    // generate cells for binary operations: $logic_and, $logic_or
    if (0) {
    case AST_LOGIC_AND:
      type_name = ID($logic_and);
    }
    if (0) {
    case AST_LOGIC_OR:
      type_name = ID($logic_or);
    }
    {
      RTLIL::SigSpec left = children[0]->genRTLIL();
      RTLIL::SigSpec right = children[1]->genRTLIL();
      return binop2rtlil(this, type_name, std::max(width_hint, 1), left, right);
    }

  // generate cells for unary operations: $logic_not
  case AST_LOGIC_NOT: {
    RTLIL::SigSpec arg = children[0]->genRTLIL();
    return uniop2rtlil(this, ID($logic_not), std::max(width_hint, 1), arg);
  }

  // generate multiplexer for ternary operator (aka ?:-operator)
  case AST_TERNARY: {
    if (width_hint < 0)
      detectSignWidth(width_hint, sign_hint);
    is_signed = sign_hint;

    RTLIL::SigSpec cond = children[0]->genRTLIL();
    RTLIL::SigSpec sig;

    if (cond.is_fully_def()) {
      if (cond.as_bool()) {
        sig = children[1]->genRTLIL(width_hint, sign_hint);
        // log_assert(is_signed == children[1]->is_signed);
      } else {
        sig = children[2]->genRTLIL(width_hint, sign_hint);
        // log_assert(is_signed == children[2]->is_signed);
      }

      widthExtend(this, sig, sig.size(), is_signed);
    } else {
      RTLIL::SigSpec val1 = children[1]->genRTLIL(width_hint, sign_hint);
      RTLIL::SigSpec val2 = children[2]->genRTLIL(width_hint, sign_hint);

      if (cond.size() > 1)
        cond = uniop2rtlil(this, ID($reduce_bool), 1, cond, false);

      int width = std::max(val1.size(), val2.size());
      // log_assert(is_signed == children[1]->is_signed);
      // log_assert(is_signed == children[2]->is_signed);
      widthExtend(this, val1, width, is_signed);
      widthExtend(this, val2, width, is_signed);

      sig = mux2rtlil(this, cond, val1, val2);
    }

    if (sig.size() < width_hint)
      sig.extend_u0(width_hint, sign_hint);
    return sig;
  }

  // generate $memrd cells for memory read ports
  case AST_MEMRD: {
    std::stringstream sstr;
    sstr << "$memrd$" << str << "$" << RTLIL::encode_filename(filename) << ":"
         << location.first_line << "$" << (autoidx++);

    RTLIL::Cell *cell = current_module->addCell(sstr.str(), ID($memrd));
    set_src_attr(cell, this);

    RTLIL::Wire *wire = current_module->addWire(
        cell->name.str() + "_DATA", current_module->memories[str]->width);
    set_src_attr(wire, this);

    int mem_width, mem_size, addr_bits;
    is_signed = id2ast->is_signed;
    wire->is_signed = is_signed;
    id2ast->meminfo(mem_width, mem_size, addr_bits);

    RTLIL::SigSpec addr_sig = children[0]->genRTLIL();

    cell->setPort(ID::CLK, RTLIL::SigSpec(RTLIL::State::Sx, 1));
    cell->setPort(ID::EN, RTLIL::SigSpec(RTLIL::State::Sx, 1));
    cell->setPort(ID::ADDR, addr_sig);
    cell->setPort(ID::DATA, RTLIL::SigSpec(wire));

    cell->parameters[ID::MEMID] = RTLIL::Const(str);
    cell->parameters[ID::ABITS] = RTLIL::Const(GetSize(addr_sig));
    cell->parameters[ID::WIDTH] = RTLIL::Const(wire->width);

    cell->parameters[ID::CLK_ENABLE] = RTLIL::Const(0);
    cell->parameters[ID::CLK_POLARITY] = RTLIL::Const(0);
    cell->parameters[ID::TRANSPARENT] = RTLIL::Const(0);

    if (!sign_hint)
      is_signed = false;

    return RTLIL::SigSpec(wire);
  }

  // generate $meminit cells
  case AST_MEMINIT: {
    std::stringstream sstr;
    sstr << "$meminit$" << str << "$" << RTLIL::encode_filename(filename) << ":"
         << location.first_line << "$" << (autoidx++);

    RTLIL::SigSpec en_sig = children[2]->genRTLIL();

    RTLIL::Cell *cell = current_module->addCell(sstr.str(), ID($meminit_v2));
    set_src_attr(cell, this);

    int mem_width, mem_size, addr_bits;
    id2ast->meminfo(mem_width, mem_size, addr_bits);

    if (children[3]->type != AST_CONSTANT)
      input_error("Memory init with non-constant word count!\n");
    int num_words = int(children[3]->asInt(false));
    cell->parameters[ID::WORDS] = RTLIL::Const(num_words);

    RTLIL::SigSpec addr_sig = children[0]->genRTLIL();

    cell->setPort(ID::ADDR, addr_sig);
    cell->setPort(ID::DATA,
                  children[1]->genWidthRTLIL(
                      current_module->memories[str]->width * num_words, true));
    cell->setPort(ID::EN, en_sig);

    cell->parameters[ID::MEMID] = RTLIL::Const(str);
    cell->parameters[ID::ABITS] = RTLIL::Const(GetSize(addr_sig));
    cell->parameters[ID::WIDTH] =
        RTLIL::Const(current_module->memories[str]->width);

    cell->parameters[ID::PRIORITY] = RTLIL::Const(autoidx - 1);
  } break;

  // generate $assert cells
  case AST_ASSERT:
  case AST_ASSUME:
  case AST_LIVE:
  case AST_FAIR:
  case AST_COVER: {
    RTLIL::IdString celltype;
    if (type == AST_ASSERT)
      celltype = ID($assert);
    if (type == AST_ASSUME)
      celltype = ID($assume);
    if (type == AST_LIVE)
      celltype = ID($live);
    if (type == AST_FAIR)
      celltype = ID($fair);
    if (type == AST_COVER)
      celltype = ID($cover);

    // log_assert(children.size() == 2);

    RTLIL::SigSpec check = children[0]->genRTLIL();
    if (GetSize(check) != 1)
      check = current_module->ReduceBool(NEW_ID, check);

    RTLIL::SigSpec en = children[1]->genRTLIL();
    if (GetSize(en) != 1)
      en = current_module->ReduceBool(NEW_ID, en);

    RTLIL::IdString cellname;
    if (str.empty())
      cellname = stringf("%s$%s:%d$%d", celltype.c_str(),
                         RTLIL::encode_filename(filename).c_str(),
                         location.first_line, autoidx++);
    else
      cellname = str;

    check_unique_id(current_module, cellname, this, "procedural assertion");
    RTLIL::Cell *cell = current_module->addCell(cellname, celltype);
    set_src_attr(cell, this);

    for (auto &attr : attributes) {
      if (attr.second->type != AST_CONSTANT)
        input_error("Attribute `%s' with non-constant value!\n",
                    attr.first.c_str());
      cell->attributes[attr.first] = attr.second->asAttrConst();
    }

    cell->setPort(ID::A, check);
    cell->setPort(ID::EN, en);
  } break;

  // add entries to current_module->connections for assignments (outside of
  // always blocks)
  case AST_ASSIGN: {
    RTLIL::SigSpec left = children[0]->genRTLIL();
    RTLIL::SigSpec right = children[1]->genWidthRTLIL(left.size(), true);
    if (left.has_const()) {
      RTLIL::SigSpec new_left, new_right;
      for (int i = 0; i < GetSize(left); i++)
        if (left[i].wire) {
          new_left.append(left[i]);
          new_right.append(right[i]);
        }
      // log_file_warning(
      //     filename, location.first_line,
      //     "Ignoring assignment to constant bits:\n"
      //     "    old assignment: %s = %s\n    new assignment: %s = %s.\n",
      //     log_signal(left), log_signal(right), log_signal(new_left),
      //     log_signal(new_right));
      left = new_left;
      right = new_right;
    }
    current_module->connect(RTLIL::SigSig(left, right));
  } break;

  // create an RTLIL::Cell for an AST_CELL
  case AST_CELL: {
    int port_counter = 0, para_counter = 0;

    RTLIL::IdString id = str;
    check_unique_id(current_module, id, this, "cell");
    RTLIL::Cell *cell = current_module->addCell(id, "");
    set_src_attr(cell, this);
    // Set attribute 'module_not_derived' which will be cleared again after the
    // hierarchy pass
    cell->set_bool_attribute(ID::module_not_derived);

    for (auto it = children.begin(); it != children.end(); it++) {
      AstNode *child = *it;
      if (child->type == AST_CELLTYPE) {
        cell->type = child->str;
        if (flag_icells && cell->type.begins_with("\\$"))
          cell->type = cell->type.substr(1);
        continue;
      }
      if (child->type == AST_PARASET) {
        RTLIL::IdString paraname =
            child->str.empty() ? stringf("$%d", ++para_counter) : child->str;
        const AstNode *value = child->children[0];
        if (value->type == AST_REALVALUE) {
          // log_file_warning(
          //     filename, location.first_line,
          //     "Replacing floating point parameter %s.%s = %f with string.\n",
          //     log_id(cell), log_id(paraname), value->realvalue);
        } else if (value->type != AST_CONSTANT) {
          // input_error("Parameter %s.%s with non-constant value!\n",
          //             log_id(cell), log_id(paraname));
          exit(1);
        }
        cell->parameters[paraname] = value->asParaConst();
        continue;
      }
      if (child->type == AST_ARGUMENT) {
        RTLIL::SigSpec sig;
        if (child->children.size() > 0) {
          AstNode *arg = child->children[0];
          int local_width_hint = -1;
          bool local_sign_hint = false;
          // don't inadvertently attempt to detect the width of interfaces
          if (arg->type != AST_IDENTIFIER || !arg->id2ast ||
              arg->id2ast->type != AST_CELL)
            arg->detectSignWidth(local_width_hint, local_sign_hint);
          sig = arg->genRTLIL(local_width_hint, local_sign_hint);
          // log_assert(local_sign_hint == arg->is_signed);
          if (sig.is_wire()) {
            // if the resulting SigSpec is a wire, its
            // signedness should match that of the AstNode
            if (arg->type == AST_IDENTIFIER && arg->id2ast &&
                arg->id2ast->is_signed && !arg->is_signed) {
              // fully-sliced signed wire will be resolved
              // once the module becomes available
              // log_assert(attributes.count(ID::reprocess_after));
            } else {
              // log_assert(arg->is_signed == sig.as_wire()->is_signed);
            }
          } else if (arg->is_signed) {
            // non-trivial signed nodes are indirected through
            // signed wires to enable sign extension
            RTLIL::IdString wire_name = NEW_ID;
            RTLIL::Wire *wire =
                current_module->addWire(wire_name, GetSize(sig));
            wire->is_signed = true;
            current_module->connect(wire, sig);
            sig = wire;
          }
        }
        if (child->str.size() == 0) {
          char buf[100];
          snprintf(buf, 100, "$%d", ++port_counter);
          cell->setPort(buf, sig);
        } else {
          cell->setPort(child->str, sig);
        }
        continue;
      }
      // log_abort();
      exit(1);
    }
    for (auto &attr : attributes) {
      if (attr.second->type != AST_CONSTANT)
        input_error("Attribute `%s' with non-constant value.\n",
                    attr.first.c_str());
      cell->attributes[attr.first] = attr.second->asAttrConst();
    }
    if (cell->type == ID($specify2)) {
      int src_width = GetSize(cell->getPort(ID::SRC));
      int dst_width = GetSize(cell->getPort(ID::DST));
      bool full = cell->getParam(ID::FULL).as_bool();
      if (!full && src_width != dst_width)
        input_error("Parallel specify SRC width does not match DST width.\n");
      cell->setParam(ID::SRC_WIDTH, Const(src_width));
      cell->setParam(ID::DST_WIDTH, Const(dst_width));
    } else if (cell->type == ID($specify3)) {
      int dat_width = GetSize(cell->getPort(ID::DAT));
      int dst_width = GetSize(cell->getPort(ID::DST));
      if (dat_width != dst_width)
        input_error("Specify DAT width does not match DST width.\n");
      int src_width = GetSize(cell->getPort(ID::SRC));
      cell->setParam(ID::SRC_WIDTH, Const(src_width));
      cell->setParam(ID::DST_WIDTH, Const(dst_width));
    } else if (cell->type == ID($specrule)) {
      int src_width = GetSize(cell->getPort(ID::SRC));
      int dst_width = GetSize(cell->getPort(ID::DST));
      cell->setParam(ID::SRC_WIDTH, Const(src_width));
      cell->setParam(ID::DST_WIDTH, Const(dst_width));
    }
  } break;

  // use ProcessGenerator for always blocks
  case AST_ALWAYS: {
    AstNode *always = this->clone();
    ProcessGenerator generator(always);
    ignoreThisSignalsInInitial.append(generator.outputSignals);
    delete always;
  } break;

  case AST_INITIAL: {
    AstNode *always = this->clone();
    ProcessGenerator generator(always, ignoreThisSignalsInInitial);
    delete always;
  } break;

  case AST_TECALL: {
    int sz = children.size();
    if (str == "$info") {
      // if (sz > 0)
      //   log_file_info(filename, location.first_line, "%s.\n",
      //                 children[0]->str.c_str());
      // else
      //   log_file_info(filename, location.first_line, "\n");
    } else if (str == "$warning") {
      // if (sz > 0)
      //   log_file_warning(filename, location.first_line, "%s.\n",
      //                    children[0]->str.c_str());
      // else
      //   log_file_warning(filename, location.first_line, "\n");
    } else if (str == "$error") {
      if (sz > 0)
        input_error("%s.\n", children[0]->str.c_str());
      else
        input_error("\n");
    } else if (str == "$fatal") {
      // TODO: 1st parameter, if exists, is 0,1 or 2, and passed to $finish()
      // if no parameter is given, default value is 1
      // dollar_finish(sz ? children[0] : 1);
      // perhaps create & use log_file_fatal()
      if (sz > 0)
        input_error("FATAL: %s.\n", children[0]->str.c_str());
      else
        input_error("FATAL.\n");
    } else {
      input_error("Unknown elabortoon system task '%s'.\n", str.c_str());
    }
  } break;

  case AST_BIND: {
    // Read a bind construct. This should have one or more cells as children.
    for (RTLIL::Binding *binding : genBindings())
      current_module->add(binding);
    break;
  }

  case AST_FCALL: {
    if (str == "\\$anyconst" || str == "\\$anyseq" || str == "\\$allconst" ||
        str == "\\$allseq") {
      std::string myid = stringf("%s$%d", str.c_str() + 1, autoidx++);
      int width = width_hint;

      if (GetSize(children) > 1)
        input_error("System function %s got %d arguments, expected 1 or 0.\n",
                    RTLIL::unescape_id(str).c_str(), GetSize(children));

      if (GetSize(children) == 1) {
        if (children[0]->type != AST_CONSTANT)
          input_error("System function %s called with non-const argument!\n",
                      RTLIL::unescape_id(str).c_str());
        width = children[0]->asInt(true);
      }

      if (width <= 0)
        input_error("Failed to detect width of %s!\n",
                    RTLIL::unescape_id(str).c_str());

      RTLIL::Cell *cell = current_module->addCell(myid, str.substr(1));
      set_src_attr(cell, this);
      cell->parameters[ID::WIDTH] = width;

      if (attributes.count(ID::reg)) {
        auto &attr = attributes.at(ID::reg);
        if (attr->type != AST_CONSTANT)
          input_error("Attribute `reg' with non-constant value!\n");
        cell->attributes[ID::reg] = attr->asAttrConst();
      }

      RTLIL::Wire *wire = current_module->addWire(myid + "_wire", width);
      set_src_attr(wire, this);
      cell->setPort(ID::Y, wire);

      is_signed = sign_hint;
      return RTLIL::SigSpec(wire);
    }
  }
    YS_FALLTHROUGH

  // everything should have been handled above -> print error if not.
  default:
    // for (auto f : log_files)
    //   current_ast_mod->dumpAst(f, "verilog-ast> ");
    // input_error("Don't know how to generate RTLIL code for %s node!\n",
    //             type2str(type).c_str());
    exit(1);
  }

  return RTLIL::SigSpec();
}

// this is a wrapper for AstNode::genRTLIL() when a specific signal width is
// requested and/or signals must be substituted before being used as input
// values (used by ProcessGenerator) note that this is using some global
// variables to communicate this special settings to AstNode::genRTLIL().
RTLIL::SigSpec AstNode::genWidthRTLIL(
    int width, bool sgn,
    const dict<RTLIL::SigBit, RTLIL::SigBit> *new_subst_ptr) {
  const dict<RTLIL::SigBit, RTLIL::SigBit> *backup_subst_ptr =
      genRTLIL_subst_ptr;

  if (new_subst_ptr)
    genRTLIL_subst_ptr = new_subst_ptr;

  bool sign_hint = sgn;
  int width_hint = width;
  detectSignWidthWorker(width_hint, sign_hint);
  RTLIL::SigSpec sig = genRTLIL(width_hint, sign_hint);

  genRTLIL_subst_ptr = backup_subst_ptr;

  if (width >= 0)
    sig.extend_u0(width, is_signed);

  return sig;
}

void Fmt::append_string(const std::string &str) {
  FmtPart part = {};
  part.type = FmtPart::STRING;
  part.str = str;
  parts.push_back(part);
}

void Fmt::parse_rtlil(const RTLIL::Cell *cell) {
  std::string fmt = cell->getParam(ID(FORMAT)).decode_string();
  RTLIL::SigSpec args = cell->getPort(ID(ARGS));
  parts.clear();

  FmtPart part;
  for (size_t i = 0; i < fmt.size(); i++) {
    if (fmt.substr(i, 2) == "}}") {
      part.str += '}';
      ++i;
    } else if (fmt.substr(i, 2) == "{{") {
      part.str += '{';
      ++i;
    } else if (fmt[i] == '}') {
      // log_assert(false && "Unexpected '}' in format string");
      exit(1);
    } else if (fmt[i] == '{') {
      if (!part.str.empty()) {
        part.type = FmtPart::STRING;
        parts.push_back(part);
        part = {};
      }

      if (++i == fmt.size()) {
        // log_assert(false && "Unexpected end in format substitution");
        exit(1);
      }

      size_t arg_size = 0;
      for (; i < fmt.size(); i++) {
        if (fmt[i] >= '0' && fmt[i] <= '9') {
          arg_size *= 10;
          arg_size += fmt[i] - '0';
        } else if (fmt[i] == ':') {
          ++i;
          break;
        } else {
          // log_assert(false && "Unexpected character in format substitution");
          exit(1);
        }
      }
      if (i == fmt.size()) {
        // log_assert(false && "Unexpected end in format substitution");
        exit(1);
      }

      if ((size_t)args.size() < arg_size) {
        // log_assert(false && "Format part overruns arguments");
        exit(1);
      }
      part.sig = args.extract(0, arg_size);
      args.remove(0, arg_size);

      if (fmt[i] == '>')
        part.justify = FmtPart::RIGHT;
      else if (fmt[i] == '<')
        part.justify = FmtPart::LEFT;
      else {
        // log_assert(false && "Unexpected justification in format
        // substitution");
        exit(1);
      }
      if (++i == fmt.size()) {
        // log_assert(false && "Unexpected end in format substitution");
        exit(1);
      }

      if (fmt[i] == '0' || fmt[i] == ' ')
        part.padding = fmt[i];
      else {
        // log_assert(false && "Unexpected padding in format substitution");
        exit(1);
      }
      if (++i == fmt.size()) {
        // log_assert(false && "Unexpected end in format substitution");
        exit(1);
      }

      for (; i < fmt.size(); i++) {
        if (fmt[i] >= '0' && fmt[i] <= '9') {
          part.width *= 10;
          part.width += fmt[i] - '0';
          continue;
        } else if (fmt[i] == 'b') {
          part.type = FmtPart::INTEGER;
          part.base = 2;
        } else if (fmt[i] == 'o') {
          part.type = FmtPart::INTEGER;
          part.base = 8;
        } else if (fmt[i] == 'd') {
          part.type = FmtPart::INTEGER;
          part.base = 10;
        } else if (fmt[i] == 'h') {
          part.type = FmtPart::INTEGER;
          part.base = 16;
        } else if (fmt[i] == 'c') {
          part.type = FmtPart::CHARACTER;
        } else if (fmt[i] == 't') {
          part.type = FmtPart::TIME;
        } else if (fmt[i] == 'r') {
          part.type = FmtPart::TIME;
          part.realtime = true;
        } else {
          // log_assert(false && "Unexpected character in format substitution");
          exit(1);
        }
        ++i;
        break;
      }
      if (i == fmt.size()) {
        // log_assert(false && "Unexpected end in format substitution");
        exit(1);
      }

      if (part.type == FmtPart::INTEGER) {
        if (fmt[i] == '+') {
          part.plus = true;
          if (++i == fmt.size()) {
            // log_assert(false && "Unexpected end in format substitution");
            exit(1);
          }
        }

        if (fmt[i] == 'u')
          part.signed_ = false;
        else if (fmt[i] == 's')
          part.signed_ = true;
        else {
          // log_assert(false && "Unexpected character in format substitution");
          exit(1);
        }
        if (++i == fmt.size()) {
          // log_assert(false && "Unexpected end in format substitution");
          exit(1);
        }
      }

      if (fmt[i] != '}') {
        // log_assert(false && "Expected '}' after format substitution");
        exit(1);
      }

      parts.push_back(part);
      part = {};
    } else {
      part.str += fmt[i];
    }
  }
  if (!part.str.empty()) {
    part.type = FmtPart::STRING;
    parts.push_back(part);
  }
}

void Fmt::emit_rtlil(RTLIL::Cell *cell) const {
  std::string fmt;
  RTLIL::SigSpec args;

  for (auto &part : parts) {
    switch (part.type) {
    case FmtPart::STRING:
      for (char c : part.str) {
        if (c == '{')
          fmt += "{{";
        else if (c == '}')
          fmt += "}}";
        else
          fmt += c;
      }
      break;

    case FmtPart::TIME:
      // log_assert(part.sig.size() == 0);
      YS_FALLTHROUGH
    case FmtPart::CHARACTER:
      // log_assert(part.sig.size() % 8 == 0);
      YS_FALLTHROUGH
    case FmtPart::INTEGER:
      args.append(part.sig);
      fmt += '{';
      fmt += std::to_string(part.sig.size());
      fmt += ':';
      if (part.justify == FmtPart::RIGHT)
        fmt += '>';
      else if (part.justify == FmtPart::LEFT)
        fmt += '<';
      else {
        // log_abort();
        exit(1);
      }
      // log_assert(part.width == 0 || part.padding != '\0');
      fmt += part.padding != '\0' ? part.padding : ' ';
      if (part.width > 0)
        fmt += std::to_string(part.width);
      if (part.type == FmtPart::INTEGER) {
        switch (part.base) {
        case 2:
          fmt += 'b';
          break;
        case 8:
          fmt += 'o';
          break;
        case 10:
          fmt += 'd';
          break;
        case 16:
          fmt += 'h';
          break;
        default:
          // log_abort();
          exit(1);
        }
        if (part.plus)
          fmt += '+';
        fmt += part.signed_ ? 's' : 'u';
      } else if (part.type == FmtPart::CHARACTER) {
        fmt += 'c';
      } else if (part.type == FmtPart::TIME) {
        if (part.realtime)
          fmt += 'r';
        else
          fmt += 't';
      } else {
        // log_abort();
        exit(1);
      }
      fmt += '}';
      break;

    default:
      // log_abort();
      exit(1);
    }
  }

  cell->setParam(ID(FORMAT), fmt);
  cell->setParam(ID(ARGS_WIDTH), args.size());
  cell->setPort(ID(ARGS), args);
}

static size_t compute_required_decimal_places(size_t size, bool signed_) {
  BigUnsigned max;
  if (!signed_)
    max.setBit(size, true);
  else
    max.setBit(size - 1, true);
  size_t places = 0;
  while (!max.isZero()) {
    places++;
    max /= 10;
  }
  if (signed_)
    places++;
  return places;
}

static size_t compute_required_nondecimal_places(size_t size, unsigned base) {
  // log_assert(base != 10);
  BigUnsigned max;
  max.setBit(size - 1, true);
  size_t places = 0;
  while (!max.isZero()) {
    places++;
    max /= base;
  }
  return places;
}

// Only called for integers, either when:
//
// (a) passed without a format string (e.g. "$display(a);"), or
//
// (b) the corresponding format specifier has no leading zero, e.g. "%b",
// "%20h", "%-10d".
//
// In these cases, for binary/octal/hex, we always zero-pad to the size of the
// signal; i.e. whether "%h" or "%10h" or "%-20h" is used, if the corresponding
// signal is 32'h1234, "00001234" will always be a substring of the output.
//
// For case (a), we have no specified width, so there is nothing more to do.
//
// For case (b), because we are only called with no leading zero on the
// specifier, any specified width beyond the signal size is therefore space
// padding, whatever the justification.
//
// For decimal, we do no zero-padding, instead space-padding to the size
// required for the signal's largest value.  This is per other Verilog
// implementations, and intuitively makes sense as decimal representations lack
// a discrete mapping of digits to bit groups.  Decimals may also show sign and
// must accommodate this, whereas other representations do not.
void Fmt::apply_verilog_automatic_sizing_and_add(FmtPart &part) {
  if (part.base == 10) {
    size_t places =
        compute_required_decimal_places(part.sig.size(), part.signed_);
    part.padding = ' ';
    part.width = std::max(part.width, places);
    parts.push_back(part);
    return;
  }

  part.padding = '0';

  size_t places =
      compute_required_nondecimal_places(part.sig.size(), part.base);
  if (part.width < places) {
    part.justify = FmtPart::RIGHT;
    part.width = places;
    parts.push_back(part);
  } else if (part.width == places) {
    parts.push_back(part);
  } else if (part.width > places) {
    auto gap = std::string(part.width - places, ' ');
    part.width = places;

    if (part.justify == FmtPart::RIGHT) {
      append_string(gap);
      parts.push_back(part);
    } else {
      part.justify = FmtPart::RIGHT;
      parts.push_back(part);
      append_string(gap);
    }
  }
}

void Fmt::parse_verilog(const std::vector<VerilogFmtArg> &args,
                        bool sformat_like, int default_base,
                        RTLIL::IdString task_name,
                        RTLIL::IdString module_name) {
  parts.clear();

  auto arg = args.begin();
  for (; arg != args.end(); ++arg) {
    switch (arg->type) {
    case VerilogFmtArg::INTEGER: {
      FmtPart part = {};
      part.type = FmtPart::INTEGER;
      part.sig = arg->sig;
      part.base = default_base;
      part.signed_ = arg->signed_;
      apply_verilog_automatic_sizing_and_add(part);
      break;
    }

    case VerilogFmtArg::STRING: {
      if (arg == args.begin() || !sformat_like) {
        const auto fmtarg = arg;
        const std::string &fmt = fmtarg->str;
        FmtPart part = {};
        for (size_t i = 0; i < fmt.size(); i++) {
          if (fmt[i] != '%') {
            part.str += fmt[i];
          } else if (fmt.substr(i, 2) == "%%") {
            i++;
            part.str += '%';
          } else if (fmt.substr(i, 2) == "%l" || fmt.substr(i, 2) == "%L") {
            i++;
            part.str += module_name.str();
          } else if (fmt.substr(i, 2) == "%m" || fmt.substr(i, 2) == "%M") {
            i++;
            part.str += module_name.str();
          } else {
            if (!part.str.empty()) {
              part.type = FmtPart::STRING;
              parts.push_back(part);
              part = {};
            }
            if (++i == fmt.size()) {
              // log_file_error(fmtarg->filename, fmtarg->first_line,
              //                "System task `%s' called with incomplete format
              //                " "specifier in argument %zu.\n",
              //                task_name.c_str(), fmtarg - args.begin() + 1);
              exit(1);
            }

            if (++arg == args.end()) {
              // log_file_error(
              //     fmtarg->filename, fmtarg->first_line,
              //     "System task `%s' called with fewer arguments than the "
              //     "format specifiers in argument %zu require.\n",
              //     task_name.c_str(), fmtarg - args.begin() + 1);
              exit(1);
            }
            part.sig = arg->sig;
            part.signed_ = arg->signed_;

            for (; i < fmt.size(); i++) {
              if (fmt[i] == '-') {
                // left justify; not in IEEE 1800-2017 or verilator but iverilog
                // has it
                part.justify = FmtPart::LEFT;
              } else if (fmt[i] == '+') {
                // always show sign; not in IEEE 1800-2017 or verilator but
                // iverilog has it
                part.plus = true;
              } else
                break;
            }
            if (i == fmt.size()) {
              // log_file_error(fmtarg->filename, fmtarg->first_line,
              //                "System task `%s' called with incomplete format
              //                " "specifier in argument %zu.\n",
              //                task_name.c_str(), fmtarg - args.begin() + 1);
              exit(1);
            }

            bool has_leading_zero = false, has_width = false;
            for (; i < fmt.size(); i++) {
              if (fmt[i] >= '0' && fmt[i] <= '9') {
                if (fmt[i] == '0' && !has_width) {
                  has_leading_zero = true;
                } else {
                  has_width = true;
                  part.width *= 10;
                  part.width += fmt[i] - '0';
                }
                continue;
              } else if (fmt[i] == 'b' || fmt[i] == 'B') {
                part.type = FmtPart::INTEGER;
                part.base = 2;
              } else if (fmt[i] == 'o' || fmt[i] == 'O') {
                part.type = FmtPart::INTEGER;
                part.base = 8;
              } else if (fmt[i] == 'd' || fmt[i] == 'D') {
                part.type = FmtPart::INTEGER;
                part.base = 10;
              } else if (fmt[i] == 'h' || fmt[i] == 'H' || fmt[i] == 'x' ||
                         fmt[i] == 'X') {
                // hex digits always printed in lowercase for %h%x as well as
                // %H%X
                part.type = FmtPart::INTEGER;
                part.base = 16;
              } else if (fmt[i] == 'c' || fmt[i] == 'C') {
                part.type = FmtPart::CHARACTER;
                part.sig.extend_u0(8);
                // %10c and %010c not fully defined in IEEE 1800-2017 and do
                // different things in iverilog
              } else if (fmt[i] == 's' || fmt[i] == 'S') {
                part.type = FmtPart::CHARACTER;
                if ((part.sig.size() % 8) != 0)
                  part.sig.extend_u0((part.sig.size() + 7) / 8 * 8);
                // %10s and %010s not fully defined in IEEE 1800-2017 and do the
                // same thing in iverilog
                part.padding = ' ';
              } else if (fmt[i] == 't' || fmt[i] == 'T') {
                if (arg->type == VerilogFmtArg::TIME) {
                  part.type = FmtPart::TIME;
                  part.realtime = arg->realtime;
                  if (!has_width && !has_leading_zero)
                    part.width = 20;
                } else {
                  // log_file_error(fmtarg->filename, fmtarg->first_line,
                  //                "System task `%s' called with format "
                  //                "character `%c' in argument %zu, but the "
                  //                "argument is not $time or $realtime.\n",
                  //                task_name.c_str(), fmt[i],
                  //                fmtarg - args.begin() + 1);
                  exit(1);
                }
              } else {
                // log_file_error(fmtarg->filename, fmtarg->first_line,
                //                "System task `%s' called with unrecognized "
                //                "format character `%c' in argument %zu.\n",
                //                task_name.c_str(), fmt[i],
                //                fmtarg - args.begin() + 1);
                exit(1);
              }
              break;
            }
            if (i == fmt.size()) {
              // log_file_error(fmtarg->filename, fmtarg->first_line,
              //                "System task `%s' called with incomplete format
              //                " "specifier in argument %zu.\n",
              //                task_name.c_str(), fmtarg - args.begin() + 1);
              exit(1);
            }

            if (part.padding == '\0')
              part.padding =
                  (has_leading_zero && part.justify == FmtPart::RIGHT) ? '0'
                                                                       : ' ';

            if (part.type == FmtPart::INTEGER && part.base != 10 && part.plus) {
              // log_file_error(fmtarg->filename, fmtarg->first_line,
              //                "System task `%s' called with invalid format "
              //                "specifier in argument %zu.\n",
              //                task_name.c_str(), fmtarg - args.begin() + 1);
              exit(1);
            }

            if (part.type == FmtPart::INTEGER && !has_leading_zero)
              apply_verilog_automatic_sizing_and_add(part);
            else
              parts.push_back(part);
            part = {};
          }
        }
        if (!part.str.empty()) {
          part.type = FmtPart::STRING;
          parts.push_back(part);
        }
      } else {
        FmtPart part = {};
        part.type = FmtPart::STRING;
        part.str = arg->str;
        parts.push_back(part);
      }
      break;
    }

    default:
      // log_abort();
      exit(1);
    }
  }
}

std::vector<VerilogFmtArg> Fmt::emit_verilog() const {
  std::vector<VerilogFmtArg> args;
  VerilogFmtArg fmt = {};
  fmt.type = VerilogFmtArg::STRING;

  for (auto &part : parts) {
    switch (part.type) {
    case FmtPart::STRING:
      for (char c : part.str) {
        if (c == '%')
          fmt.str += "%%";
        else
          fmt.str += c;
      }
      break;

    case FmtPart::INTEGER: {
      VerilogFmtArg arg = {};
      arg.type = VerilogFmtArg::INTEGER;
      arg.sig = part.sig;
      arg.signed_ = part.signed_;
      args.push_back(arg);

      fmt.str += '%';
      if (part.plus)
        fmt.str += '+';
      if (part.justify == FmtPart::LEFT)
        fmt.str += '-';
      if (part.width == 0) {
        fmt.str += '0';
      } else if (part.width > 0) {
        // log_assert(part.padding == ' ' || part.padding == '0');
        if (part.base != 10 || part.padding == '0')
          fmt.str += '0';
        fmt.str += std::to_string(part.width);
      }
      switch (part.base) {
      case 2:
        fmt.str += 'b';
        break;
      case 8:
        fmt.str += 'o';
        break;
      case 10:
        fmt.str += 'd';
        break;
      case 16:
        fmt.str += 'h';
        break;
      default:
        // log_abort();
        exit(1);
      }
      break;
    }

    case FmtPart::CHARACTER: {
      VerilogFmtArg arg;
      arg.type = VerilogFmtArg::INTEGER;
      arg.sig = part.sig;
      args.push_back(arg);

      fmt.str += '%';
      if (part.justify == FmtPart::LEFT)
        fmt.str += '-';
      if (part.sig.size() == 8) {
        if (part.width > 0) {
          // log_assert(part.padding == '0' || part.padding == ' ');
          if (part.padding == '0')
            fmt.str += part.padding;
          fmt.str += std::to_string(part.width);
        }
        fmt.str += 'c';
      } else {
        // log_assert(part.sig.size() % 8 == 0);
        if (part.width > 0) {
          // log_assert(part.padding == ' '); // no zero padding
          fmt.str += std::to_string(part.width);
        }
        fmt.str += 's';
      }
      break;
    }

    case FmtPart::TIME: {
      VerilogFmtArg arg;
      arg.type = VerilogFmtArg::TIME;
      if (part.realtime)
        arg.realtime = true;
      args.push_back(arg);

      fmt.str += '%';
      if (part.plus)
        fmt.str += '+';
      if (part.justify == FmtPart::LEFT)
        fmt.str += '-';
      // log_assert(part.padding == ' ' || part.padding == '0');
      if (part.padding == '0' && part.width > 0)
        fmt.str += '0';
      fmt.str += std::to_string(part.width);
      fmt.str += 't';
      break;
    }

    default:
      // log_abort();
      exit(1);
    }
  }

  args.insert(args.begin(), fmt);
  return args;
}

void Fmt::emit_cxxrtl(
    std::ostream &f,
    std::function<void(const RTLIL::SigSpec &)> emit_sig) const {
  for (auto &part : parts) {
    switch (part.type) {
    case FmtPart::STRING:
      f << " << \"";
      for (char c : part.str) {
        switch (c) {
        case '\\':
          YS_FALLTHROUGH
        case '"':
          f << '\\' << c;
          break;
        case '\a':
          f << "\\a";
          break;
        case '\b':
          f << "\\b";
          break;
        case '\f':
          f << "\\f";
          break;
        case '\n':
          f << "\\n";
          break;
        case '\r':
          f << "\\r";
          break;
        case '\t':
          f << "\\t";
          break;
        case '\v':
          f << "\\v";
          break;
        default:
          f << c;
          break;
        }
      }
      f << '"';
      break;

    case FmtPart::INTEGER:
    case FmtPart::CHARACTER: {
      f << " << value_formatted<" << part.sig.size() << ">(";
      emit_sig(part.sig);
      f << ", " << (part.type == FmtPart::CHARACTER);
      f << ", " << (part.justify == FmtPart::LEFT);
      f << ", (char)" << (int)part.padding;
      f << ", " << part.width;
      f << ", " << part.base;
      f << ", " << part.signed_;
      f << ", " << part.plus;
      f << ')';
      break;
    }

    case FmtPart::TIME: {
      // CXXRTL only records steps taken, so there's no difference between
      // the values taken by $time and $realtime.
      f << " << value_formatted<64>(";
      f << "value<64>{steps}";
      f << ", " << (part.type == FmtPart::CHARACTER);
      f << ", " << (part.justify == FmtPart::LEFT);
      f << ", (char)" << (int)part.padding;
      f << ", " << part.width;
      f << ", " << part.base;
      f << ", " << part.signed_;
      f << ", " << part.plus;
      f << ')';
      break;
    }

    default:
      // log_abort();
      exit(1);
    }
  }
}

std::string Fmt::render() const {
  std::string str;

  for (auto &part : parts) {
    switch (part.type) {
    case FmtPart::STRING:
      str += part.str;
      break;

    case FmtPart::INTEGER:
    case FmtPart::TIME:
    case FmtPart::CHARACTER: {
      std::string buf;
      if (part.type == FmtPart::INTEGER) {
        RTLIL::Const value = part.sig.as_const();

        if (part.base != 10) {
          size_t minimum_size = 0;
          for (size_t index = 0; index < (size_t)value.size(); index++)
            if (value[index] != State::S0)
              minimum_size = index + 1;
          value = value.extract(0, minimum_size);
        }

        if (part.base == 2) {
          buf = value.as_string();
        } else if (part.base == 8 || part.base == 16) {
          size_t step = (part.base == 16) ? 4 : 3;
          for (size_t index = 0; index < (size_t)value.size(); index += step) {
            RTLIL::Const subvalue =
                value.extract(index, std::min(step, value.size() - index));
            bool has_x = false, all_x = true, has_z = false, all_z = true;
            for (State bit : subvalue) {
              if (bit == State::Sx)
                has_x = true;
              else
                all_x = false;
              if (bit == State::Sz)
                has_z = true;
              else
                all_z = false;
            }
            if (all_x)
              buf += 'x';
            else if (all_z)
              buf += 'z';
            else if (has_x)
              buf += 'X';
            else if (has_z)
              buf += 'Z';
            else
              buf += "0123456789abcdef"[subvalue.as_int()];
          }
          std::reverse(buf.begin(), buf.end());
        } else if (part.base == 10) {
          bool has_x = false, all_x = true, has_z = false, all_z = true;
          for (State bit : value) {
            if (bit == State::Sx)
              has_x = true;
            else
              all_x = false;
            if (bit == State::Sz)
              has_z = true;
            else
              all_z = false;
          }
          if (all_x)
            buf += 'x';
          else if (all_z)
            buf += 'z';
          else if (has_x)
            buf += 'X';
          else if (has_z)
            buf += 'Z';
          else {
            bool negative = part.signed_ && value[value.size() - 1];
            RTLIL::Const absvalue;
            if (negative)
              absvalue = RTLIL::const_neg(value, {}, part.signed_, {},
                                          value.size() + 1);
            else
              absvalue = value;
            // log_assert(absvalue.is_fully_def());
            if (absvalue.is_fully_zero())
              buf += '0';
            while (!absvalue.is_fully_zero()) {
              buf += '0' +
                     RTLIL::const_mod(absvalue, 10, false, false, 4).as_int();
              absvalue =
                  RTLIL::const_div(absvalue, 10, false, false, absvalue.size());
            }
            if (negative || part.plus)
              buf += negative ? '-' : '+';
            std::reverse(buf.begin(), buf.end());
          }
        } else {
          // log_abort();
          exit(1);
        }
      } else if (part.type == FmtPart::CHARACTER) {
        buf = part.sig.as_const().decode_string();
      } else if (part.type == FmtPart::TIME) {
        // We only render() during initial, so time is always zero.
        buf = "0";
      }

      // log_assert(part.width == 0 || part.padding != '\0');
      if (part.justify == FmtPart::RIGHT && buf.size() < part.width) {
        size_t pad_width = part.width - buf.size();
        if (part.padding == '0' &&
            (!buf.empty() && (buf.front() == '+' || buf.front() == '-'))) {
          str += buf.front();
          buf.erase(0, 1);
        }
        str += std::string(pad_width, part.padding);
      }
      str += buf;
      if (part.justify == FmtPart::LEFT && buf.size() < part.width)
        str += std::string(part.width - buf.size(), part.padding);
      break;
    }
    }
  }

  return str;
}

void frontend_verilog_yyerror(char const *fmt, ...) {
  va_list ap;
  char buffer[1024];
  char *p = buffer;
  va_start(ap, fmt);
  p += vsnprintf(p, buffer + sizeof(buffer) - p, fmt, ap);
  va_end(ap);
  p += snprintf(p, buffer + sizeof(buffer) - p, "\n");
  // yosys_mini::log_file_error(
  //     yosys_mini::AST::current_filename,
  //     frontend_verilog_yyget_lineno(), "%s", buffer);
  exit(1);
}
