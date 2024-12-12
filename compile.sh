#usr/bin/bash
set -x
base_name=$(basename "$1" '.c')
asm_file="${base_name}.s"  
gcc -S -masm=intel  -O0 -fno-builtin -fno-tree-fre -fno-tree-dce -fno-tree-cselim -fno-asynchronous-unwind-tables -fcf-protection=none $1 -o $asm_file