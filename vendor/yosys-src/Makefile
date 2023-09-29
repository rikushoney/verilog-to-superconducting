all: backends frontends kernel passes

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
	log.o \
	mem.o \
	rtlil.o

kernel: $(foreach obj,$(kernel_targets),$(build_dir)/kernel/$(obj))

proc_targets := \
	proc.o \
	proc_prune.o \
	proc_clean.o \
	proc_rmdead.o \
	proc_init.o \
	proc_arst.o \
	proc_rom.o \
	proc_mux.o \
	proc_dlatch.o \
	proc_dff.o \
	proc_memwr.o

proc_pass: $(foreach obj,$(proc_targets),$(build_dir)/passes/proc/$(obj))

opt_targets := \
	opt.o \
	opt_merge.o \
	opt_mem.o \
	opt_mem_feedback.o \
	opt_mem_widen.o \
	opt_muxtree.o \
	opt_reduce.o \
	opt_share.o \
	opt_clean.o \
	opt_expr.o \
	wreduce.o \
	opt_demorgan.o \
	rmports.o \
	opt_lut_ins.o \
	opt_ffinv.o \
	pmux2shiftx.o \
	muxpack.o

opt_pass: $(foreach obj,$(opt_targets),$(build_dir)/passes/opt/$(obj))

techmap_targets := \
	flatten.o \
	techmap.o \
	simplemap.o \
	dfflibmap.o \
	maccmap.o \
	libparse.o \
	iopadmap.o \
	clkbufmap.o \
	hilomap.o \
	extract_counter.o \
	extract_reduce.o \
	alumacc.o \
	dffinit.o \
	pmuxtree.o \
	bmuxmap.o \
	bwmuxmap.o \
	muxcover.o \
	aigmap.o \
	tribuf.o \
	lut2mux.o \
	nlutmap.o \
	shregmap.o \
	insbuf.o \
	attrmvcp.o \
	attrmap.o \
	zinit.o \
	dfflegalize.o \
	dffunmap.o
	
techmap_pass: $(foreach obj,$(techmap_targets),$(build_dir)/passes/techmap/$(obj))

passes: proc_pass opt_pass techmap_pass

clean:
	rm -r $(build_dir)

.PHONY: clean