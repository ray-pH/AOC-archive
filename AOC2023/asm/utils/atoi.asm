; convert cstring into i64

section .text
    global myatoi

; param:
;   rdi : *char (pointer to start of string)
; ret:
;   rax : i64
myatoi:
    ;setup stack frame
    push rbp
    mov rbp, rsp

    ; rdx, rax <- 0
    xor rax, rax
    xor rdx, rdx

.next_char:
    mov dl, byte [rdi]      ; get next byte [dl is the smallest part of rdx]
    cmp rdx, 0              ; check rdx == \0 ?
    je .exit
    sub rdx, 0x30           ; sub 0x30 to map ASCII to 0-9
    imul rax, 0xA           ; mul by 10
    add rax, rdx            ; add to accumulator
    inc rdi                 ; increment pointer
    jmp .next_char

.exit:
    leave
    ret

    
