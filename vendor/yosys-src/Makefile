all: backends frontends kernel passes libs

build_dir := build

$(build_dir)/%.o: %.cc
	@mkdir -p $(dir $@)
	clang++ -std=c++11 -D_YOSYS_ -I. -c -o $@ $(filter-out $(build_dir)/,$<)

$(build_dir)/%.o: %.cpp
	@mkdir -p $(dir $@)
	clang++ -std=c++11 -D_YOSYS_ -I. -c -o $@ $(filter-out $(build_dir)/,$<)

backend_targets := \
	backends/blif/blif.o \
	backends/rtlil/rtlil_backend.o \
	backends/verilog/verilog_backend.o

backends: $(foreach obj,$(backend_targets),$(build_dir)/$(obj))

ast_targets := \
	ast.o \
	ast_binding.o \
	dpicall.o \
	genrtlil.o \
	simplify.o

ast_frontend: $(foreach obj,$(ast_targets),$(build_dir)/frontends/ast/$(obj))

rtlil_targets := \
	rtlil_frontend.o \
	rtlil_lexer.o \
	rtlil_parser.tab.o

rtlil_frontend: $(foreach obj,$(rtlil_targets),$(build_dir)/frontends/rtlil/$(obj))

verilog_targets := \
	const2ast.o \
	preproc.o \
	verilog_frontend.o \
	verilog_lexer.o \
	verilog_parser.tab.o

verilog_frontend: $(foreach obj,$(verilog_targets),$(build_dir)/frontends/verilog/$(obj))

frontends: ast_frontend $(build_dir)/frontends/blif/blifparse.o rtlil_frontend verilog_frontend

kernel_targets := \
	binding.o \
	calc.o \
	cellaigs.o \
	celledges.o \
	ff.o \
	ffmerge.o \
	fmt.o \
	json.o\
	log.o \
	mem.o \
	qcsat.o \
	register.o \
	rtlil.o \
	satgen.o \
	yosys.o \
	yw.o 

kernel: $(foreach obj,$(kernel_targets),$(build_dir)/kernel/$(obj))

proc_targets := \
	proc.o \
	proc_arst.o \
	proc_clean.o \
	proc_dff.o \
	proc_dlatch.o \
	proc_init.o \
	proc_memwr.o\
	proc_mux.o \
	proc_prune.o \
	proc_rmdead.o \
	proc_rom.o 

proc_pass: $(foreach obj,$(proc_targets),$(build_dir)/passes/proc/$(obj))

opt_targets := \
	muxpack.o\
	opt.o \
	opt_clean.o \
	opt_demorgan.o \
	opt_dff.o \
	opt_expr.o \
	opt_ffinv.o \
	opt_lut_ins.o \
	opt_mem.o \
	opt_mem_feedback.o \
	opt_mem_widen.o \
	opt_merge.o \
	opt_muxtree.o \
	opt_reduce.o \
	opt_share.o \
	pmux2shiftx.o \
	rmports.o \
	share.o \
	wreduce.o 

opt_pass: $(foreach obj,$(opt_targets),$(build_dir)/passes/opt/$(obj))

techmap_targets := \
	abc.o \
	abc9.o \
	abc9_exe.o \
	aigmap.o \
	alumacc.o \
	attrmap.o \
	attrmvcp.o \
	bmuxmap.o \
	bwmuxmap.o \
	clkbufmap.o \
	deminout.o \
	demuxmap.o \
	dffinit.o \
	dfflegalize.o \
	dfflibmap.o \
	dffunmap.o \
	extract.o \
	extract_counter.o \
	extract_fa.o \
	extract_reduce.o \
	extractinv.o\
	flatten.o \
	flowmap.o \
	hilomap.o \
	insbuf.o \
	iopadmap.o \
	libparse.o \
	lut2mux.o \
	maccmap.o \
	muxcover.o \
	nlutmap.o \
	pmuxtree.o \
	shregmap.o \
	simplemap.o \
	techmap.o \
	tribuf.o \
	zinit.o 
	
techmap_pass: $(foreach obj,$(techmap_targets),$(build_dir)/passes/techmap/$(obj))

passes: proc_pass opt_pass techmap_pass

bigint_targets := \
	BigInteger.o \
	BigIntegerAlgorithms.o \
	BigIntegerUtils.o \
	BigUnsigned.o \
	BigUnsignedInABase.o

bigint: $(foreach obj,$(bigint_targets),$(build_dir)/libs/bigint/$(obj))

sha1: $(build_dir)/libs/sha1/sha1.o

subcircuit: $(build_dir)/libs/subcircuit/subcircuit.o

ezsat_targets := \
	ezsat.o \
	ezminisat.o

ezsat: $(foreach obj,$(ezsat_targets),$(build_dir)/libs/ezsat/$(obj))

minisat_targets := \
	Options.o \
	SimpSolver.o \
	Solver.o \
	System.o

minisat: $(foreach obj,$(minisat_targets),$(build_dir)/libs/minisat/$(obj))

libs: bigint sha1 subcircuit ezsat minisat

clean:
	rm -r $(build_dir)

.PHONY: clean