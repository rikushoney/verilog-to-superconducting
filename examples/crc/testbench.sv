`timescale 1ns/1ns

`include "crc8.sv"
`include "crc16.sv"

module testbench;
  logic clk = 0;
  logic reset = 0;
  string test_input = "123456789";
  byte test_input_index;
  logic [7:0] current_input;
  logic [7:0] crc8_value;
  logic [7:0] crc8_remainder;
  logic [15:0] crc16_value;
  logic [15:0] crc16_remainder;
  crc8_07 crc8_dut (crc8_remainder, current_input, crc8_value);
  crc16_1021 crc16_dut (crc16_remainder, current_input, crc16_value);

  always begin
    #5 clk = ~clk;
  end

  always_ff @(posedge clk, posedge reset) begin
    if (reset) begin
      crc8_remainder = 0;
      crc16_remainder = 0;
      test_input_index = 0;
      current_input = test_input[test_input_index];
    end else if (test_input_index < test_input.len() - 1) begin
      crc8_remainder = crc8_value;
      crc16_remainder = crc16_value;
      test_input_index = test_input_index + 1;
      current_input = test_input[test_input_index];
    end
  end

  initial begin
    $dumpfile("crc_testbench_dump.vcd");
    $dumpvars(0, testbench);
    #10 reset = 1;
    #15 reset = 0;
    #100 $finish;
    assert(crc8_value == 8'hF4);
    assert(crc16_value == 16'h31C3);
  end

endmodule: testbench
