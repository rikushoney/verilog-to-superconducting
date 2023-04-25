`timescale 1ns/1ns

`include "alu4.sv"

module testbench;

logic [3:0] add_input1;
logic [3:0] add_input2;
logic [3:0] add_result;
logic add_overflow;
alu4 dut_ADD(add_input1, add_input2, ADD, add_result, add_overflow);
logic [3:0] sub_input1;
logic [3:0] sub_input2;
logic [3:0] sub_result;
logic sub_overflow;
alu4 dut_SUB(sub_input1, sub_input2, SUB, sub_result, sub_overflow);
logic [3:0] and_input1;
logic [3:0] and_input2;
logic [3:0] and_result;
logic and_overflow;
alu4 dut_AND(and_input1, and_input2, AND, and_result, and_overflow);
logic [3:0] or_input1;
logic [3:0] or_input2;
logic [3:0] or_result;
logic or_overflow;
alu4 dut_OR(or_input1, or_input2, OR, or_result, or_overflow);

initial begin
  $dumpfile("alu4_testbench_dump.vcd");
  $dumpvars(0, testbench);
  // TODO: test all possible 4-bit combinations
  add_input1 = 4'b0100;
  add_input2 = 4'b0011;
  sub_input1 = 4'b1010;
  sub_input2 = 4'b0100;
  and_input1 = 4'b1010;
  and_input2 = 4'b1100;
  or_input1 = 4'b1010;
  or_input2 = 4'b1100;
  #1;
  assert(add_result == 4'b0111);
  assert(add_overflow == 0);
  assert(sub_result == 4'b0110);
  // TODO: detect overflow/underflow for sub?
  // assert(sub_overflow == 0);
  assert(and_result == 4'b1000);
  assert(and_overflow == 0);
  assert(or_result == 4'b1110);
  assert(or_overflow == 0);
  $finish;
end

endmodule: testbench
