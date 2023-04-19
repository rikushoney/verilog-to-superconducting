module crc8_07(
  input [7:0] remainder,
  input [7:0] crc_input,
  output [7:0] crc_output
);

// (poly = 0x07, left shift, input not reversed)
assign crc_output[0] = remainder[0] ^ remainder[6] ^ remainder[7] ^
  crc_input[0] ^ crc_input[6] ^ crc_input[7];
assign crc_output[1] = remainder[0] ^ remainder[1] ^ remainder[6] ^
  crc_input[0] ^ crc_input[1] ^ crc_input[6];
assign crc_output[2] = remainder[0] ^ remainder[1] ^ remainder[2] ^ remainder[6] ^
  crc_input[0] ^ crc_input[1] ^ crc_input[2] ^ crc_input[6];
assign crc_output[3] = remainder[1] ^ remainder[2] ^ remainder[3] ^ remainder[7] ^
  crc_input[1] ^ crc_input[2] ^ crc_input[3] ^ crc_input[7];
assign crc_output[4] = remainder[2] ^ remainder[3] ^ remainder[4] ^
  crc_input[2] ^ crc_input[3] ^ crc_input[4];
assign crc_output[5] = remainder[3] ^ remainder[4] ^ remainder[5] ^
  crc_input[3] ^ crc_input[4] ^ crc_input[5];
assign crc_output[6] = remainder[4] ^ remainder[5] ^ remainder[6] ^
  crc_input[4] ^ crc_input[5] ^ crc_input[6];
assign crc_output[7] = remainder[5] ^ remainder[6] ^ remainder[7] ^
  crc_input[5] ^ crc_input[6] ^ crc_input[7];

endmodule: crc8_07
