cmake_minimum_required(VERSION 3.20)

project(mini-yosys)

set(yosys_sources
  frontends/ast/ast.cc
  frontends/ast/ast_binding.cc
  frontends/ast/genrtlil.cc
  frontends/ast/simplify.cc
  frontends/verilog/const2ast.cc
  frontends/verilog/preproc.cc
  frontends/verilog/verilog_frontend.cc
  frontends/verilog/verilog_lexer.cc
  frontends/verilog/verilog_parser.tab.cc
  kernel/binding.cc
  kernel/calc.cc
  kernel/fmt.cc
  kernel/register.cc
  kernel/rtlil.cc
  kernel/yosys.cc
  libs/bigint/BigInteger.cc
  libs/bigint/BigIntegerAlgorithms.cc
  libs/bigint/BigIntegerUtils.cc
  libs/bigint/BigUnsigned.cc
  libs/bigint/BigUnsignedInABase.cc
  libs/json11/json11.cpp
  libs/sha1/sha1.cpp
  passes/cmds/select.cc
  wrapper.cpp
)

set(yosys_headers
  kernel/rtlil.h
  kernel/yosys.h
)

add_library(yosys STATIC)
target_sources(yosys PUBLIC ${yosys_headers})
target_sources(yosys PRIVATE ${yosys_sources})
target_include_directories(yosys PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
