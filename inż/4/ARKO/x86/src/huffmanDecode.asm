global huffmanDecode

section .bss
	var:			resb 1

section .text

huffmanDecode:
    mov BYTE[var], 2
    movzx EAX, BYTE[var]
    ret