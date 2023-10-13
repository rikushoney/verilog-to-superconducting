use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let target = env::var("TARGET").unwrap();

    let dst = PathBuf::from(env::var_os("OUT_DIR").unwrap());

    let mut cfg = cc::Build::new();

    let yosys_dir = PathBuf::from("src/yosys").canonicalize().unwrap();
    cfg.cpp(true)
        .warnings(false)
        .out_dir(&dst)
        .include("src")
        .include("src/yosys")
        .define("_YOSYS_", "1")
        .define("YOSYS_SRC", format!("\"{}\"", yosys_dir.display()).as_str());

    if target.contains("msvc") {
        cfg.std("c++14");
    } else {
        cfg.std("c++11");
    }

    fs::create_dir_all(&dst).unwrap();
    cfg.include(dst.to_str().unwrap());

    // TODO: automatically scrape version from git
    fs::create_dir_all(dst.join("kernel")).unwrap();
    fs::copy(
        "src/version_417871e83.cc",
        dst.join("kernel/version_417871e83.cc"),
    )
    .unwrap();

    cfg.file(dst.join("kernel/version_417871e83.cc"));

    // TODO: automatically generate on hosts with python
    let techlibs_dst = dst.join("techlibs/common");
    fs::create_dir_all(&techlibs_dst).unwrap();
    fs::copy("src/simlib_help.inc", techlibs_dst.join("simlib_help.inc")).unwrap();
    fs::copy(
        "src/simcells_help.inc",
        techlibs_dst.join("simcells_help.inc"),
    )
    .unwrap();

    // TODO: automatically generate on hosts with bison/flex
    let rtlil_dst = dst.join("frontends/rtlil");
    fs::create_dir_all(&rtlil_dst).unwrap();
    fs::copy(
        "src/rtlil_parser.tab.cc",
        rtlil_dst.join("rtlil_parser.tab.cc"),
    )
    .unwrap();
    fs::copy(
        "src/rtlil_parser.tab.hh",
        rtlil_dst.join("rtlil_parser.tab.hh"),
    )
    .unwrap();
    fs::copy("src/rtlil_lexer.cc", rtlil_dst.join("rtlil_lexer.cc")).unwrap();

    cfg.file(rtlil_dst.join("rtlil_parser.tab.cc"));
    cfg.file(rtlil_dst.join("rtlil_lexer.cc"));

    let verilog_dst = dst.join("frontends/verilog");
    fs::create_dir_all(&verilog_dst).unwrap();
    fs::copy(
        "src/verilog_parser.tab.cc",
        verilog_dst.join("verilog_parser.tab.cc"),
    )
    .unwrap();
    fs::copy(
        "src/verilog_parser.tab.hh",
        verilog_dst.join("verilog_parser.tab.hh"),
    )
    .unwrap();
    fs::copy("src/verilog_lexer.cc", verilog_dst.join("verilog_lexer.cc")).unwrap();

    cfg.file(verilog_dst.join("verilog_parser.tab.cc"));
    cfg.file(verilog_dst.join("verilog_lexer.cc"));

    // https://github.com/YosysHQ/yosys/issues/906
    // https://github.com/YosysHQ/yosys/pull/1017
    cfg.define("YYMAXDEPTH", "10000000");

    macro_rules! add_srcs {
        ($dir:literal, [$($srcs:literal),* $(,)?]) => {
            $(
                cfg.file(concat!($dir, $srcs));
            )*
        }
    }

    add_srcs!(
        "src/yosys/kernel/",
        [
            "binding.cc",
            "calc.cc",
            "cellaigs.cc",
            "celledges.cc",
            "ff.cc",
            "ffmerge.cc",
            "fmt.cc",
            "json.cc",
            "log.cc",
            "mem.cc",
            "qcsat.cc",
            "register.cc",
            "rtlil.cc",
            "satgen.cc",
            "yosys.cc",
            "yw.cc",
        ]
    );

    add_srcs!(
        "src/yosys/libs/bigint/",
        [
            "BigInteger.cc",
            "BigIntegerAlgorithms.cc",
            "BigIntegerUtils.cc",
            "BigUnsigned.cc",
            "BigUnsignedInABase.cc",
        ]
    );

    add_srcs!(
        "src/yosys/libs/",
        [
            "ezsat/ezminisat.cc",
            "ezsat/ezsat.cc",
            "json11/json11.cpp",
            "sha1/sha1.cpp",
            "subcircuit/subcircuit.cc",
        ]
    );

    add_srcs!(
        "src/yosys/libs/minisat/",
        [
            "Options.cc",
            "SimpSolver.cc",
            "Solver.cc",
            "System.cc"]
    );

    add_srcs!(
        "src/yosys/frontends/ast/",
        [
            "ast.cc",
            "ast_binding.cc",
            "dpicall.cc",
            "genrtlil.cc",
            "simplify.cc",
        ]
    );

    add_srcs!(
        "src/yosys/frontends/",
        [
            "aiger/aigerparse.cc",
            "blif/blifparse.cc",
            "json/jsonparse.cc",
            "liberty/liberty.cc",
            "rpc/rpc_frontend.cc",
            "rtlil/rtlil_frontend.cc",
            "verific/verific.cc",
        ]
    );

    add_srcs!(
        "src/yosys/frontends/verilog/",
        ["const2ast.cc", "preproc.cc", "verilog_frontend.cc"]
    );

    add_srcs!(
        "src/yosys/passes/cmds/",
        [
            "add.cc",
            "autoname.cc",
            "blackbox.cc",
            "check.cc",
            "chformal.cc",
            "chtype.cc",
            "clean_zerowidth.cc",
            "connect.cc",
            "connwrappers.cc",
            "copy.cc",
            "cover.cc",
            "delete.cc",
            "design.cc",
            "dft_tag.cc",
            "edgetypes.cc",
            "future.cc",
            "glift.cc",
            "logcmd.cc",
            "logger.cc",
            "ltp.cc",
            "plugin.cc",
            "portlist.cc",
            "printattrs.cc",
            "qwp.cc",
            "rename.cc",
            "scatter.cc",
            "scc.cc",
            "scratchpad.cc",
            "select.cc",
            "setattr.cc",
            "setundef.cc",
            "show.cc",
            "splice.cc",
            "splitcells.cc",
            "splitnets.cc",
            "sta.cc",
            "stat.cc",
            "tee.cc",
            "torder.cc",
            "trace.cc",
            "viz.cc",
            "write_file.cc",
            "xprop.cc",
        ]
    );

    add_srcs!(
        "src/yosys/passes/equiv/",
        [
            "equiv_add.cc",
            "equiv_induct.cc",
            "equiv_make.cc",
            "equiv_mark.cc",
            "equiv_miter.cc",
            "equiv_opt.cc",
            "equiv_purge.cc",
            "equiv_remove.cc",
            "equiv_simple.cc",
            "equiv_status.cc",
            "equiv_struct.cc",
        ]
    );

    add_srcs!(
        "src/yosys/passes/fsm/",
        [
            "fsm.cc",
            "fsm_detect.cc",
            "fsm_expand.cc",
            "fsm_export.cc",
            "fsm_extract.cc",
            "fsm_info.cc",
            "fsm_map.cc",
            "fsm_opt.cc",
            "fsm_recode.cc",
        ]
    );

    add_srcs!(
        "src/yosys/passes/hierarchy/",
        ["hierarchy.cc", "submod.cc", "uniquify.cc",]
    );

    add_srcs!(
        "src/yosys/passes/memory/",
        [
            "memlib.cc",
            "memory.cc",
            "memory_bmux2rom.cc",
            "memory_bram.cc",
            "memory_collect.cc",
            "memory_dff.cc",
            "memory_libmap.cc",
            "memory_map.cc",
            "memory_memx.cc",
            "memory_narrow.cc",
            "memory_nordff.cc",
            "memory_share.cc",
            "memory_unpack.cc",
        ]
    );

    add_srcs!(
        "src/yosys/passes/opt/",
        [
            "muxpack.cc",
            "opt.cc",
            "opt_clean.cc",
            "opt_demorgan.cc",
            "opt_dff.cc",
            "opt_expr.cc",
            "opt_ffinv.cc",
            "opt_lut.cc",
            "opt_lut_ins.cc",
            "opt_mem.cc",
            "opt_mem_feedback.cc",
            "opt_mem_priority.cc",
            "opt_mem_widen.cc",
            "opt_merge.cc",
            "opt_muxtree.cc",
            "opt_reduce.cc",
            "opt_share.cc",
            "pmux2shiftx.cc",
            "rmports.cc",
            "share.cc",
            "wreduce.cc",
        ]
    );

    add_srcs!(
        "src/yosys/passes/proc/",
        [
            "proc.cc",
            "proc_arst.cc",
            "proc_clean.cc",
            "proc_dff.cc",
            "proc_dlatch.cc",
            "proc_init.cc",
            "proc_memwr.cc",
            "proc_mux.cc",
            "proc_prune.cc",
            "proc_rmdead.cc",
            "proc_rom.cc",
        ]
    );

    add_srcs!(
        "src/yosys/passes/sat/",
        [
            "assertpmux.cc",
            "async2sync.cc",
            "clk2fflogic.cc",
            "cutpoint.cc",
            "eval.cc",
            "expose.cc",
            "fmcombine.cc",
            "fminit.cc",
            "formalff.cc",
            "freduce.cc",
            "miter.cc",
            "mutate.cc",
            "recover_names.cc",
            "sat.cc",
            "supercover.cc",
            "synthprop.cc",
        ]
    );

    add_srcs!(
        "src/yosys/passes/techmap/",
        [
            "abc.cc",
            "abc9.cc",
            "abc9_exe.cc",
            "abc9_ops.cc",
            "aigmap.cc",
            "alumacc.cc",
            "attrmap.cc",
            "attrmvcp.cc",
            "bmuxmap.cc",
            "booth.cc",
            "bwmuxmap.cc",
            "clkbufmap.cc",
            "deminout.cc",
            "demuxmap.cc",
            "dffinit.cc",
            "dfflegalize.cc",
            "dfflibmap.cc",
            "dffunmap.cc",
            "extract.cc",
            "extract_counter.cc",
            "extract_fa.cc",
            "extract_reduce.cc",
            "extractinv.cc",
            "flatten.cc",
            "flowmap.cc",
            "hilomap.cc",
            "insbuf.cc",
            "iopadmap.cc",
            "libparse.cc",
            "lut2mux.cc",
            "maccmap.cc",
            "muxcover.cc",
            "nlutmap.cc",
            "pmuxtree.cc",
            "shregmap.cc",
            "simplemap.cc",
            "techmap.cc",
            "tribuf.cc",
            "zinit.cc",
        ]
    );

    add_srcs!(
        "src/yosys/passes/tests/",
        ["test_abcloop.cc", "test_autotb.cc", "test_cell.cc",]
    );

    add_srcs!(
        "src/yosys/backends/",
        [
            "aiger/aiger.cc",
            "aiger/xaiger.cc",
            "blif/blif.cc",
            "btor/btor.cc",
            "cxxrtl/cxxrtl_backend.cc",
            "edif/edif.cc",
            "firrtl/firrtl.cc",
            "intersynth/intersynth.cc",
            "jny/jny.cc",
            "json/json.cc",
            "rtlil/rtlil_backend.cc",
            "simplec/simplec.cc",
            "smt2/smt2.cc",
            "smv/smv.cc",
            "spice/spice.cc",
            "table/table.cc",
            "verilog/verilog_backend.cc",
        ]
    );

    add_srcs!("src/yosys/techlibs/common/", ["prep.cc", "synth.cc"]);

    cfg.compile("yosys");
}
