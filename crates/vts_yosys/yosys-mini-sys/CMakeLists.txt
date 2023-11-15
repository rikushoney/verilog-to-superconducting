cmake_minimum_required(VERSION 3.20)

project(mini-yosys)

set(yosys_mini_sources
  bigint.cpp
  sha1.cpp
  rtlil.cpp
  verilog.cpp
  verilog_parser.cpp
  verilog_lexer.cpp
  wrapper.cpp
  yosys_mini.cpp
)

set(yosys_mini_headers
  bigint.h
  sha1.h
  rtlil.h
  verilog.h
  verilog_parser.h
  wrapper.h
  yosys_mini.h
)

add_library(yosys-mini STATIC)
target_sources(yosys-mini PRIVATE ${yosys_mini_sources})
target_sources(yosys-mini PUBLIC ${yosys_mini_headers})

add_executable(smoke_test smoke_test.cpp)
target_link_libraries(smoke_test PRIVATE yosys-mini)
add_test(NAME smoke_test COMMAND smoke_test)
