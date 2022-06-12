# LLVM
1. `sudo apt install libllvm-13-ocaml-dev libllvm13 llvm-13 llvm-13-dev llvm-13-doc llvm-13-examples llvm-13-runtime`
# Zig
1. `cd /tmp`
1. Look for the latest tagged version: https://github.com/ziglang/zig/tags
1. `wget https://ziglang.org/download/0.9.1/zig-linux-x86_64-0.9.1.tar.xz`, or latest tagged version
1. `tar -xvf zig-linux-x86_64-0.9.1.tar.xz`
1. `sudo mv zig-linux-x86_64-0.9.1 /opt/zig-0.9.1`
1. Add to "~/.bashrc": &nbsp; `export PATH="$PATH:/opt/zig-0.9.1"`
1. `source ~/.bashrc`
1. `zig version`