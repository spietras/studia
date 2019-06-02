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
    treenodesLeaves:        resw    1
    treenodesNodes:         resw    1

    currentInputPos:        resq    1

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
    mov r8w, word[rdi]
    mov word[treenodesLeaves], r8w                                                          ; load how many symbols were used
    movzx rcx, r8w

    add rdi, WORD_SIZE
    mov r10, treenodes
pushLeavesLoop:                                 
    mov r8b, byte[rdi]                                                                      ; load symbol
    mov r9, qword[rdi + SYMBOL_SIZE]                                                        ; load frequency

    mov byte[r10], r8b
    mov qword[r10 + SYMBOL_SIZE], r9

    add r10, TREENODE_SIZE
    add rdi, SYMBOL_SIZE + FREQUENCY_SIZE
    loop pushLeavesLoop

    mov qword[currentInputPos], rdi

    mov r8w, word[treenodesLeaves]
    mov word[treenodesNodes], r8w

    mov r13, r10                                                                            ; end of treenodes - add new nodes here
buildTreeLoop:
    mov rcx, treenodes
    mov r8, 0                                                                               ; first node found
    mov r9, 0                                                                               ; second node found
    mov r12w, 0                                                                             ; node counter
    mov r11, 0xFFFFFFFF                                                                     ; smallest found ("infinity" at first)
findTwoNodesLoop:

    cmp r12w, word[treenodesNodes]
    jge endFindTwoNodesLoop                                                                 ; if all nodes searched, end

    mov r10b, byte[rcx + SYMBOL_SIZE + FREQUENCY_SIZE + 3*QWORD_SIZE]                       ; used flag
    cmp r10b, 1
    je repeatTwoNodesLoop                                                                   ; if used, continue

    cmp r8, 0
    je afterFreqCheck                                                                       ; if two nodes not found yet, ignore frequency check

    mov r10, qword[rcx + SYMBOL_SIZE]                                                       ; frequency
    cmp r10, r11
    jg repeatTwoNodesLoop                                                                   ; if frequenct greater than the smallest frequency, continue

afterFreqCheck:
    jmp nodeFound
repeatTwoNodesLoop:
    add rcx, TREENODE_SIZE
    inc r12w
    jmp findTwoNodesLoop                                                                    ; go to next node and loop
nodeFound:

    cmp r8, 0
    je checkSecondEmpty
    jmp bothUsed
checkSecondEmpty:
    cmp r9, 0
    je bothEmpty
    jmp oneEmpty

bothEmpty:
    mov r9, rcx                                                                             ; if no nodes foud before, set found to r9
    jmp afterSort
oneEmpty:
    mov r14, qword[r9 + SYMBOL_SIZE]
    cmp r10, r14
    jl shiftWhenOneEmpty
    mov r8, rcx                                                                             ; if one node found before but it's weight is bigger than found, set found to r8
    jmp afterSort
shiftWhenOneEmpty:
    mov r8, r9                                                                              ; if weight is smaller, shift r9 to r8 and set found to r9
    mov r9, rcx
    jmp afterSort
bothUsed:
    mov r8, r9                                                                              ; if both used, they are sorted, so shift r9 to r8 and set found to r8
    mov r9, rcx
    jmp afterSort
afterSort:

    cmp r8, 0
    je afterSetSmallest                                                                     ; don't set smallest until two nodes found
setSmallest:
    mov r11, qword[r8 + SYMBOL_SIZE]
afterSetSmallest:
    inc r12w

    add rcx, TREENODE_SIZE
    jmp findTwoNodesLoop                                                                    ; go to next node and loop
endFindTwoNodesLoop:

    cmp r8, 0
    je endBuildTreeLoop                                                                     ; if end and only one node found, tree is complete

    mov r14, qword[r8 + SYMBOL_SIZE]
    mov r15, qword[r9 + SYMBOL_SIZE]
    add r14, r15
    mov qword[r13 + SYMBOL_SIZE], r14                                                       ; set new node frequency to sum of found nodes' frequencies

    mov r14, qword[r8 + SYMBOL_SIZE]
    cmp r14, r15
    jg firstNodeGreater
    jmp secondNodeGreater

firstNodeGreater:
    mov qword[r13 + SYMBOL_SIZE + FREQUENCY_SIZE], r9                                       ; set left child
    mov qword[r13 + SYMBOL_SIZE + FREQUENCY_SIZE + QWORD_SIZE], r8                          ; set right child
    jmp afterSetChildren
secondNodeGreater:
    mov qword[r13 + SYMBOL_SIZE + FREQUENCY_SIZE], r8                                       ; set left child
    mov qword[r13 + SYMBOL_SIZE + FREQUENCY_SIZE + QWORD_SIZE], r9                          ; set right child
    jmp afterSetChildren

afterSetChildren:
    mov qword[r8 + SYMBOL_SIZE + FREQUENCY_SIZE + QWORD_SIZE*2], r13                        ; set parent in first found
    mov byte[r8 + SYMBOL_SIZE + FREQUENCY_SIZE + QWORD_SIZE*3], 1                           ; set used in first found
    mov qword[r9 + SYMBOL_SIZE + FREQUENCY_SIZE + QWORD_SIZE*2], r13                        ; set parent in second found
    mov byte[r9 + SYMBOL_SIZE + FREQUENCY_SIZE + QWORD_SIZE*3], 1                           ; set used in second found

    inc word[treenodesNodes]
    add r13, TREENODE_SIZE
    jmp buildTreeLoop                                                                       ; increase nodes and loop

endBuildTreeLoop:

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