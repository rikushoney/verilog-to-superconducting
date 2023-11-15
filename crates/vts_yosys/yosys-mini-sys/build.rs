use std::env;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=bigint.cpp");
    println!("cargo:rerun-if-changed=sha1.cpp");
    println!("cargo:rerun-if-changed=rtlil.cpp");
    println!("cargo:rerun-if-changed=verilog.cpp");
    println!("cargo:rerun-if-changed=verilog_parser.cpp");
    println!("cargo:rerun-if-changed=verilog_lexer.cpp");
    println!("cargo:rerun-if-changed=wrapper.cpp");
    println!("cargo:rerun-if-changed=yosys_mini.cpp");

    let current_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let yosys_includes = [current_dir.display().to_string()];
    let yosys_srcs = [
        current_dir.join("bigint.cpp"),
        current_dir.join("sha1.cpp"),
        current_dir.join("rtlil.cpp"),
        current_dir.join("verilog.cpp"),
        current_dir.join("verilog_parser.cpp"),
        current_dir.join("verilog_lexer.cpp"),
        current_dir.join("wrapper.cpp"),
        current_dir.join("yosys_mini.cpp"),
    ];

    cc::Build::new()
        .cpp(true)
        .files(&yosys_srcs)
        .includes(&yosys_includes)
        .warnings(false)
        .extra_warnings(false)
        .compile("yosys-mini");
}
