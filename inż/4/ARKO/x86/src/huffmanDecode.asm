global huffmanDecode

BITS_IN_BYTE                equ     8

BYTE_SIZE                   equ     1
WORD_SIZE                   equ     2
DWORD_SIZE                  equ     4
QWORD_SIZE                  equ     8

BYTE_SIZE_POWER             equ     0
WORD_SIZE_POWER             equ     1
DWORD_SIZE_POWER            equ     2
QWORD_SIZE_POWER            equ     3

SYMBOLS                     equ     256
FILE_LENGTH_SIZE            equ     DWORD_SIZE
SYMBOL_SIZE                 equ     BYTE_SIZE
FREQUENCY_SIZE              equ     QWORD_SIZE
FREQUENCY_SIZE_POWER        equ     QWORD_SIZE_POWER

TREENODE_SIZE               equ     SYMBOL_SIZE + FREQUENCY_SIZE + 3*QWORD_SIZE + BYTE_SIZE ; 1 for symbol, 8 for: frequency, left child address, right child address, parent address, 1 for flag
MAX_TREE_NODES              equ     2*SYMBOLS-1

TREENODES_SIZE              equ     TREENODE_SIZE*MAX_TREE_NODES

section .bss
	input:			        resq    1                                                       ; char*
    inputSize:              resq    1                                                       ; long
    output:                 resq    1                                                       ; char*
    maxOutputSize:          resq    1                                                       ; long

    originalSize:           resq    1

    treenodes:              resb    TREENODES_SIZE
    treenodesLeaves:        resb    1
    treenodesNodes:         resw    1

section .text

huffmanDecode:
    push rbp
    mov rbp, rsp

    push r8
    push r9
    push r10
    push r11
    push r12
    push r13
    push r14
    push r15

    mov [input], rdi
    mov [inputSize], rsi
    mov [output], rdx
    mov [maxOutputSize], rcx

    mov r8, qword[rdi]
    mov qword[originalSize], r8                                                             ; load original file size

    add rdi, QWORD_SIZE
    mov r8b, byte[rdi]
    mov byte[treenodesLeaves], r8b                                                          ; load how many symbols were used
    movzx rcx, r8b

    add rdi, SYMBOL_SIZE
    mov r10, treenodes
pushLeavesLoop:                                 
    mov r8b, byte[rdi]                                                                      ; load symbol
    mov r9, qword[rdi + SYMBOL_SIZE]                                                        ; load frequency

    mov byte[r10], r8b
    mov qword[r10 + SYMBOL_SIZE], r9

    add r10, TREENODE_SIZE
    add rdi, SYMBOL_SIZE + FREQUENCY_SIZE
    loop pushLeavesLoop

    mov rax, 0

    pop r15
    pop r14
    pop r13
    pop r12
    pop r11
    pop r10
    pop r9
    pop r8
    pop rbp 
    ret