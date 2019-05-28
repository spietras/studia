global huffmanEncode

section .bss
	var:			resb 1

section .text

huffmanEncode:
    mov BYTE[var], 1
    movzx EAX, BYTE[var]
    ret