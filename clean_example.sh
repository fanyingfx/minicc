#/usr/bin/bash
find ./examples/ -maxdepth 1 -type f ! -name "*.c" -exec rm -f {} \;
