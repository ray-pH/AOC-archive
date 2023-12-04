section .data
    filename db "inp", 0
    buffer   times 100 db 100     ; buffer to store the input
    buf_size equ $-buffer ; size of the buffer

section .bss
    inpfile_handle resq 1 ; file handle for the input file

section .text
    ; extern open, read, write, close
    extern main

main:
    ; Open the file
    mov rdi, filename
    mov rax, 2 ; open
    mov rsi, 0 ; read only
    syscall

    ; Save the file handle
    mov qword [inpfile_handle], rax

    ; Read the file
    mov rdi, qword [inpfile_handle]
    mov rax, 0 ; read
    mov rsi, buffer
    mov rdx, buf_size
    syscall

    ; use lseek to get the file size
    mov rdi, qword [inpfile_handle]
    mov rax, 8 ; lseek
    mov rsi, 0 ; offset
    mov rdx, 2 ; SEEK_CUR
    syscall

    ; Print the content line by line
    mov rdi, 1 ; stdout
    mov rax, 1 ; write
    mov rsi, buffer

    mov rcx, buf_size
print_line:
    ; Loop until we find a newline
    mov rdx, 0 ; char count in the current line

next_char:
    ; Check if we reached the end of the buffer
    cmp rdx, rcx
    je end_of_buffer

    ; Check if we reached the end of file
    cmp byte [rsi + rdx], 0
    je end_of_buffer

    ; Check if we reached the end of the line
    mov al, [rsi + rdx]
    cmp al, 10 ; newline
    je end_of_line

    ; Increment the char count
    inc rdx
    jmp next_char

end_of_line:
    ; Print the entire line
    mov rdi, 1 ; stdout
    mov rax, 1 ; write
    mov rdx, rcx ; line length
    syscall

    ; Move to the next line
    add rsi, rdx ; buffer + char count
    sub rcx, rdx ; buffer size - char count
    jmp print_line


end_of_buffer:

    ; Close the file
    mov rdi, qword [inpfile_handle]
    mov rax, 3 ; close
    syscall

    ; Return
    xor rdi, rdi
    ret
