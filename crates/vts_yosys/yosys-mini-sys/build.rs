use std::env;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=wrapper.h");
    println!("cargo:rerun-if-changed=wrapper.cpp");
    println!("cargo:rerun-if-changed=yosys_mini.h");
    println!("cargo:rerun-if-changed=yosys_mini.cpp");

    let current_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let yosys_includes = [current_dir.display().to_string()];
    let yosys_srcs = [
        current_dir.join("yosys_mini.cpp"),
        current_dir.join("wrapper.cpp"),
    ];

    cc::Build::new()
        .cpp(true)
        .files(&yosys_srcs)
        .includes(&yosys_includes)
        .warnings(false)
        .extra_warnings(false)
        .compile("yosys-mini");
}
