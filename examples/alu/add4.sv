// 4-bit adder with carry-in and carry-out
module add4(
  input logic [3:0] A,
  input logic [3:0] B,
  input logic Cin,
  output logic [3:0] S,
  output logic Cout
);

logic [3:0] G; // generator
logic [3:0] P; // propagator
logic [3:0] C; // carries

assign G = A & B;
assign P = A ^ B;

// generate carries
genvar i;
assign C[0] = Cin;
for (i = 1; i < 4; i++) begin
  assign C[i] = G[i - 1] | (C[i - 1] & P[i - 1]);
end

// calculate sum from inputs and carries
assign S = A ^ B ^ C;
assign Cout = G[3] | (C[3] & P[3]);

endmodule: add4
