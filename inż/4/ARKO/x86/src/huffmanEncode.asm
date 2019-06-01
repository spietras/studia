global huffmanEncode

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

CODE_ENTRY_SIZE             equ     BYTE_SIZE + SYMBOLS*BYTE_SIZE                           ; code length + byte for each bit
CODES_SIZE                  equ     SYMBOLS*CODE_ENTRY_SIZE

section .bss
	input:			        resq    1                                                       ; char*
    inputSize:              resq    1                                                       ; long
    output:                 resq    1                                                       ; char*
    maxOutputSize:          resq    1                                                       ; long

    frequencies:            resq    SYMBOLS
    treenodes:              resb    TREENODES_SIZE
    treenodesLeaves:        resb    1
    treenodesNodes:         resw    1

    codes:                  resb    CODES_SIZE

    currentOutPos           resq    1

section .text

huffmanEncode:
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

    mov rcx, [inputSize]                                                                    ; counter for loop
    mov r8, [input]                                                                         ; start of input
countFrequenciesLoop:                                   
    movzx r9, byte[r8]                                                                      ; load character from input
    inc qword[r9*QWORD_SIZE + frequencies]                                                  ; increase its frequency
    inc r8                                                                                  ; go to the next character
    loop countFrequenciesLoop                                   

    mov rcx, SYMBOLS                                                                        ; counter for lopp
    mov r8, frequencies                                                                     ; start of frequencies
    mov r10, treenodes                                                                      ; start of treenodes
pushLeavesLoop:                                 
    mov r9, qword[r8]                                                                       ; load frequency
    cmp r9, 0                                   
    je continuePushLeavesLoop                                   

    mov r11, r8                                                                             ; current freq position
    sub r11, frequencies                                                                    ; difference/current freq = index*qword
    shr r11, FREQUENCY_SIZE_POWER                                                           ; index = symbol
    mov byte[r10], r11b                                                                     ; save symbol
    mov qword[r10 + SYMBOL_SIZE], r9                                                        ; save frequency
                
    add r10, TREENODE_SIZE                                                                  ; next tree node
    inc byte[treenodesLeaves]                                                               ; count how many nodes there are
                
continuePushLeavesLoop:             
    add r8, QWORD_SIZE                                                                      ; go to the next frequency
    loop pushLeavesLoop

    movzx r8w, byte[treenodesLeaves]
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

    mov r8, r9                                                                              ; shift previous node
    mov r9, rcx                                                                             ; save new node

    cmp r8, 0
    je afterSetSmallest                                                                     ; don't set smallest until two nodes found
setSmallest:
    mov r11, qword[rcx + SYMBOL_SIZE]
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

    movzx rcx, byte[treenodesLeaves]
    mov r13, treenodes
createCodeLoop:
    mov r9b, byte[r13]                                                                      ; symbol
    mov r15, CODE_ENTRY_SIZE
    movzx rax, r9b
    mul r15
    add rax, codes                                                                          ; codes table offset for current symbol

    mov r15, rax
    add r15, BYTE_SIZE
    mov r14b, 0
    mov r8, r13
nextBitLoop:
    mov r10, qword[r8 + SYMBOL_SIZE + FREQUENCY_SIZE + QWORD_SIZE*2]                        ; parent
    cmp r10, 0
    je endNextBitLoop                                                                       ; if parent == 0, we hit root node, end
    mov r11, qword[r10 + SYMBOL_SIZE + FREQUENCY_SIZE]                                      ; parent's left child
    cmp r8, r11
    setne r12b                                                                              ; 0 = left child, 1 = right child

    mov byte[r15], r12b                                                                     ; set bit

    mov r8, r10                                                                             ; current = parent
    inc r14b
    add r15, BYTE_SIZE
    jmp nextBitLoop
endNextBitLoop:
    mov byte[rax], r14b                                                                     ; save code length
    add r13, TREENODE_SIZE
    loop createCodeLoop

    mov r8, qword[output]
    mov r9b, byte[treenodesLeaves]                                                          ; save how many symbols were in input
    mov byte[r8], r9b
    inc r8
    mov qword[currentOutPos], r8

    movzx rcx, byte[treenodesLeaves]
    mov r11, qword[currentOutPos]
    mov r8, treenodes
writeHeaderLoop:
    mov r9b, byte[r8]                                                                       ; load symbol
    movzx rax, r9b
    shl rax, FREQUENCY_SIZE_POWER
    mov r10, qword[frequencies + rax]                                                       ; load frequency   
    mov byte[r11], r9b                                                                      ; save symbol
    mov qword[r11 + BYTE_SIZE], r10                                                         ; save frequency
    add r11, BYTE_SIZE + QWORD_SIZE
    add r8, TREENODE_SIZE                                       
    loop writeHeaderLoop

    mov qword[currentOutPos], r11

    mov rcx, qword[inputSize]                                                               ; input size as loop counter
    mov r8, qword[input]
    mov r15, qword[currentOutPos]
    mov r14b, 0                                                                             ; bit counter
encodeLoop:
    mov r9b, byte[r8]                                                                       ; load symbol
    movzx rax, r9b
    mov r9, CODE_ENTRY_SIZE
    mul r9
    movzx r9, byte[codes + rax]                                                             ; load code length

    push rcx                                                                                ; save rcx, because it will be used in another loop
    mov rcx, r9
    add rax, r9                                                                             ; read codes from end
writeCodeLoop:
    push rcx
    mov cl, r14b
    mov r10b, byte[codes + rax]                                                             ; bit
    shl r10b, cl                                                                            ; shift to correct position
    pop rcx

    or byte[r15], r10b                                                                      ; add to existing content
    inc r14b

    mov r10b, BITS_IN_BYTE
    dec r10b
    and r14b, r10b                                                                          ; current bit % 8

    cmp r14b, 0
    sete r10b
    movzx r10, r10b                                                                         
    add r15, r10                                                                            ; if bit counter overflew, increase byte counter

    dec rax
    loop writeCodeLoop

    pop rcx                                                                                 ; restore rcx

    inc r8

    loop encodeLoop

    mov qword[currentOutPos], r15

    mov rax, qword[currentOutPos]
    mov r9, qword[output]
    sub rax, r9

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