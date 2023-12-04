; check the functionality of atoi

section .data
    formatstr db "int is %d", 10, 0
    numstr db "1234", 0

section .text
    extern printf
    extern atoi
    extern main

main:
    ;setup stack frame
    push rbp

    ;convert string to int
    mov rdi, numstr
    call atoi

    ;print the int
    mov rdi, formatstr
    mov rsi, rax
    xor rax, rax
    call printf

    ;teardown stack frame
    pop rbp
    
    ;return
    xor rax, rax
    ret
