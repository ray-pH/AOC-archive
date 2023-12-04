#!/usr/bin/env bash
set -xe

nasm -f elf64 -g readfile.asm
gcc -no-pie -z noexecstack readfile.o -o readfile
