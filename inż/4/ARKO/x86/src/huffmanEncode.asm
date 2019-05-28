global huffmanEncode

SYMBOLS                     equ     256
SIZE_OF_FILE_LENGTH         equ     4
SIZE_OF_SYMBOL              equ     1
SIZE_OF_SYMBOL_FREQUENCY    equ     4

section .bss
	input:			        resq    1       ; char*
    inputSize:              resq    1       ; long
    output:                 resq    1       ; char*
    maxOutputSize:          resq    1       ; long

    frequencies:            resq    SYMBOLS

section .text

huffmanEncode:
    push rbp
    mov rbp, rsp

    push r8
    push r9

    mov [input], rdi
    mov [inputSize], rsi
    mov [output], rdx
    mov [maxOutputSize], rcx

    mov rcx, [inputSize]                    ; counter for loop
    mov r8, [input]                         ; start of input
countFrequenciesLoop:
    movzx r9, byte[r8]                      ; load character from input
    inc qword[r9 + frequencies]             ; increase its frequency
    inc r8                                  ; go to the next character
    loop countFrequenciesLoop



    movzx rax, byte[frequencies + 10]

    pop r9
    pop r8
    pop rbp 
    ret