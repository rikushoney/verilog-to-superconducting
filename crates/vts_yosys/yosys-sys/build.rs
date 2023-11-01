fn main() {
    let dst = cmake::build("yosys_cmake");

    println!("cargo:rustc-link-search=native={}", dst.display());
    println!("cargo:rustc-link-lib=dylib=yosys");
}
