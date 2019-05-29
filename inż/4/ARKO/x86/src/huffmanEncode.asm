global huffmanEncode

BYTE_SIZE                   equ     1
WORD_SIZE                   equ     2
DWORD_SIZE                  equ     4
QWORD_SIZE                  equ     8

SYMBOLS                     equ     256
SIZE_OF_FILE_LENGTH         equ     4
SIZE_OF_SYMBOL              equ     1
SIZE_OF_SYMBOL_FREQUENCY    equ     4

SIZE_OF_TREENODE            equ     8*5                 ; 1 for symbol, 8 for: frequency, left child address, right child address, parent address
MAX_TREE_NODES              equ     2*SYMBOLS-1

section .bss
	input:			        resq    1                   ; char*
    inputSize:              resq    1                   ; long
    output:                 resq    1                   ; char*
    maxOutputSize:          resq    1                   ; long

    frequencies:            resq    SYMBOLS
    treenodes:              resb    SIZE_OF_TREENODE*MAX_TREE_NODES
    treenodesLeaves:        resb    1

section .text

huffmanEncode:
    push rbp
    mov rbp, rsp

    push r8
    push r9
    push r10
    push r11

    mov [input], rdi
    mov [inputSize], rsi
    mov [output], rdx
    mov [maxOutputSize], rcx

    mov rcx, [inputSize]                                ; counter for loop
    mov r8, [input]                                     ; start of input
countFrequenciesLoop:
    movzx r9, byte[r8]                                  ; load character from input
    inc qword[r9*QWORD_SIZE + frequencies]              ; increase its frequency
    inc r8                                              ; go to the next character
    loop countFrequenciesLoop

    mov rcx, SYMBOLS                                    ; counter for lopp
    mov r8, frequencies                                 ; start of frequencies
    mov r10, treenodes                                  ; start of treenodes
pushLeavesLoop:
    mov r9, qword[r8]                                   ; load frequency
    cmp r9, 0
    je continuePushLeavesLoop

    mov r11, r8                                         ; current freq position
    sub r11, frequencies                                ; difference/current freq = index*qword
    shr r11, 3                                          ; index = symbol
    mov byte[r10], r11b                                 ; save symbol
    mov qword[r10 + BYTE_SIZE], r9                      ; save frequency
    mov qword[r10 + BYTE_SIZE + QWORD_SIZE], 0          ; save left child = null
    mov qword[r10 + BYTE_SIZE + QWORD_SIZE*2], 0        ; save right child = null
    mov qword[r10 + BYTE_SIZE + QWORD_SIZE*3], 0        ; save parent = null

    add r10, BYTE_SIZE + 4*QWORD_SIZE                   ; next tree node
    inc byte[treenodesLeaves]                           ; count how many nodes there are

continuePushLeavesLoop:
    add r8, QWORD_SIZE                                  ; go to the next frequency
    loop pushLeavesLoop

    movzx rax, byte[treenodes]

    pop r11
    pop r10
    pop r9
    pop r8
    pop rbp 
    ret