global func

section .bss
	var:			resb 1

section .text

func:
    mov BYTE[var], 64
    movzx EAX, BYTE[var]
    ret