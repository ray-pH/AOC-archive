#!/usr/bin/env bash
set -xe

nasm -f elf64 -o printint.o printint.asm
gcc -no-pie -z noexecstack printint.o -o printint
