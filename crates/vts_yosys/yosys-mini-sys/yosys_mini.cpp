#include "yosys_mini.h"
#include "rtlil.h"

#include <string.h>

int yosys_mini::autoidx = 0;
yosys_mini::CellTypes yosys_celltypes;
std::set<std::string> yosys_mini::yosys_input_files, yosys_output_files;

using namespace yosys_mini;

std::vector<std::string> yosys_mini::split_tokens(const std::string &text,
                                                  const char *sep) {
  std::vector<std::string> tokens;
  std::string current_token;
  for (char c : text) {
    if (strchr(sep, c)) {
      if (!current_token.empty()) {
        tokens.push_back(current_token);
        current_token.clear();
      }
    } else
      current_token += c;
  }
  if (!current_token.empty()) {
    tokens.push_back(current_token);
    current_token.clear();
  }
  return tokens;
}

std::string yosys_mini::next_token(std::string &text, const char *sep, bool long_strings)
{
	size_t pos_begin = text.find_first_not_of(sep);

	if (pos_begin == std::string::npos)
		pos_begin = text.size();

	if (long_strings && pos_begin != text.size() && text[pos_begin] == '"') {
		std::string sep_string = sep;
		for (size_t i = pos_begin+1; i < text.size(); i++) {
			if (text[i] == '"' && (i+1 == text.size() || sep_string.find(text[i+1]) != std::string::npos)) {
				std::string token = text.substr(pos_begin, i-pos_begin+1);
				text = text.substr(i+1);
				return token;
			}
			if (i+1 < text.size() && text[i] == '"' && text[i+1] == ';' && (i+2 == text.size() || sep_string.find(text[i+2]) != std::string::npos)) {
				std::string token = text.substr(pos_begin, i-pos_begin+1);
				text = text.substr(i+2);
				return token + ";";
			}
		}
	}

	size_t pos_end = text.find_first_of(sep, pos_begin);

	if (pos_end == std::string::npos)
		pos_end = text.size();

	std::string token = text.substr(pos_begin, pos_end-pos_begin);
	text = text.substr(pos_end);
	return token;
}

int yosys_mini::readsome(std::istream &f, char *s, int n) {
  int rc = int(f.readsome(s, n));

  // f.readsome() sometimes returns 0 on a non-empty stream..
  if (rc == 0) {
    int c = f.get();
    if (c != EOF) {
      *s = c;
      rc = 1;
    }
  }

  return rc;
}
