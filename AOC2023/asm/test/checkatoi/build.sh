#!/usr/bin/env bash
set -xe

nasm -f elf64 checkatoi.asm
gcc -no-pie -z noexecstack checkatoi.o ../../utils/atoi.o -o checkatoi
