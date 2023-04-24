VERILOG_FLAGS ?= -g2012

_build:
	mkdir $@

testbench: testbench.sv _build alu4.sv
	iverilog $(VERILOG_FLAGS) -o _build/$@ $(<F)

simulate: testbench
	vvp _build/$(<F)

clean:
	rm -r _build crc_testbench_dump.vcd

.PHONY: clean
