module crc16_1021(
  input [15:0] remainder,
  input [7:0] crc_input,
  output [15:0] crc_output
);

// (poly = 0x1021, left shift, input not reversed)
assign crc_output[0] = remainder[8] ^ remainder[12] ^
  crc_input[0] ^ crc_input[4];
assign crc_output[1] = remainder[9] ^ remainder[13] ^
  crc_input[1] ^ crc_input[5];
assign crc_output[2] = remainder[10] ^ remainder[14] ^
  crc_input[2] ^ crc_input[6];
assign crc_output[3] = remainder[11] ^ remainder[15] ^
  crc_input[3] ^ crc_input[7];
assign crc_output[4] = remainder[12] ^
  crc_input[4];
assign crc_output[5] = remainder[8] ^ remainder[12] ^ remainder[13] ^
  crc_input[0] ^ crc_input[4] ^ crc_input[5];
assign crc_output[6] = remainder[9] ^ remainder[13] ^ remainder[14] ^
  crc_input[1] ^ crc_input[5] ^ crc_input[6];
assign crc_output[7] = remainder[10] ^ remainder[14] ^ remainder[15] ^
  crc_input[2] ^ crc_input[6] ^ crc_input[7];
assign crc_output[8] = remainder[0] ^ remainder[11] ^ remainder[15] ^
  crc_input[3] ^ crc_input[7];
assign crc_output[9] = remainder[1] ^ remainder[12] ^
  crc_input[4];
assign crc_output[10] = remainder[2] ^ remainder[13] ^
  crc_input[5];
assign crc_output[11] = remainder[3] ^ remainder[14] ^
  crc_input[6];
assign crc_output[12] = remainder[4] ^ remainder[8] ^ remainder[12] ^ remainder[15] ^
  crc_input[0] ^ crc_input[4] ^ crc_input[7];
assign crc_output[13] = remainder[5] ^ remainder[9] ^ remainder[13] ^
  crc_input[1] ^ crc_input[5];
assign crc_output[14] = remainder[6] ^ remainder[10] ^ remainder[14] ^
  crc_input[2] ^ crc_input[6];
assign crc_output[15] = remainder[7] ^ remainder[11] ^ remainder[15] ^
  crc_input[3] ^ crc_input[7];

endmodule: crc16_1021
