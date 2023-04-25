`include "add4.sv"

// operations that the ALU supports
typedef enum logic [1:0] {ADD, SUB, AND, OR} ALU_OP;

// 4-bit ALU capable of +, -, & and |
module alu4(
  input  logic [3:0] A,  // first input
  input  logic [3:0] B,  // second input
  input  ALU_OP      op, // ALU operation
  output logic [3:0] S,  // result of ALU operation
  output logic       V   // overflow flag
);

// intermediate logic for working with adder
logic Cin;
logic [3:0] Bin;
logic [3:0] Sout;
logic Cout;

// take 2's compliment of B if op is subtract
always_comb begin
  if (op == SUB) begin
    Cin = 1;
    Bin = ~B;
  end else begin
    Cin = 0;
    Bin = B;
  end
end

add4 adder(A, Bin, Cin, Sout, Cout);

// assign outputs
always_comb begin
  case(op)
    ADD: begin
      S = Sout;
      V = Cout;
    end
    SUB: begin
      S = Sout;
      V = Cout;
    end
    AND: begin
      S = A & B;
      V = 0;
    end
    OR: begin
      S = A | B;
      V = 0;
    end
  endcase
end

endmodule: alu4
