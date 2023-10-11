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
        .include("src/yosys")
        .define("YOSYS_SRC", format!("\"{}\"", yosys_dir.display()).as_str());

    if target.contains("msvc") {
        cfg.std("c++14");
    } else {
        cfg.std("c++11");
    }

    fs::create_dir_all(dst.join("techlibs/common")).unwrap();
    fs::copy(
        "src/simlib_help.inc",
        dst.join("techlibs/common/simlib_help.inc"),
    )
    .unwrap();
    fs::copy(
        "src/simcells_help.inc",
        dst.join("techlibs/common/simcells_help.inc"),
    )
    .unwrap();
    cfg.include(dst.to_str().unwrap());

    cfg.define("_YOSYS_", "1")
        .define("YOSYS_ENABLE_COVER", "0")
        .define("YOSYS_ENABLE_GLOB", "0")
        .define("YOSYS_ENABLE_PLUGINS", "0")
        .define("YOSYS_ENABLE_READLINE", "0")
        .define("YOSYS_ENABLE_TCL", "0")
        .define("YOSYS_ENABLE_ZLIB", "0");

    cfg.file("src/yosys/kernel/binding.cc")
        .file("src/yosys/kernel/calc.cc")
        .file("src/yosys/kernel/cellaigs.cc")
        .file("src/yosys/kernel/celledges.cc")
        .file("src/yosys/kernel/ff.cc")
        .file("src/yosys/kernel/ffmerge.cc")
        .file("src/yosys/kernel/fmt.cc")
        .file("src/yosys/kernel/json.cc")
        .file("src/yosys/kernel/log.cc")
        .file("src/yosys/kernel/mem.cc")
        .file("src/yosys/kernel/qcsat.cc")
        .file("src/yosys/kernel/register.cc")
        .file("src/yosys/kernel/rtlil.cc")
        .file("src/yosys/kernel/satgen.cc")
        .file("src/yosys/kernel/yosys.cc")
        .file("src/yosys/kernel/yw.cc");

    cfg.compile("yosys");
}
