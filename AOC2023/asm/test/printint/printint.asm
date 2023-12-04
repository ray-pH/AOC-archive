section .data
    formatstr db "int is %d", 10, 0
    num db 223

; section .bss
    ; num resq 1

section .text
    extern printf
    global main

main:
    ; setup stack frame
    push rbp

    mov rdi, formatstr
    mov rsi, [num]
    call printf

    pop rbp
    ; exit
    mov rax, 0
    ret


