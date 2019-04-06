.data

	.eqv SYSCALL_PRINTINT		1
	.eqv SYSCALL_PRINTINTBINARY	35
	.eqv SYSCALL_PRINTCHARACTER	11
	.eqv SYSCALL_PRINTSTRING	4
	.eqv SYSCALL_READSTRING		8
	.eqv SYSCALL_EXIT		10
	.eqv SYSCALL_OPENFILE		13
	.eqv SYSCALL_READFILE		14
	.eqv SYSCALL_WRITEFILE		15
	.eqv SYSCALL_CLOSEFILE		16

	.eqv CHUNK_LENGTH		1024
	.eqv MAX_SYMBOLS		256
	.eqv MAX_HUFFMAN_TREE_NODES	511				# (2*MAX_SYMBOLS - 1) - recalculate if something changes
	.eqv CODE_BUFFER_LENGTH		32				# MAX_SYMBOLS / 8 - recalculate if something changes
	.eqv FILENAME_LENGTH		256
	.eqv FILES			2
	
	masks:			.byte	128, 64, 32, 16, 8, 4, 2, 1	# 10000000, 01000000, 00100000, 00010000, 00001000, 00000100, 00000010, 00000001
			
	fileNameBuffer:		.byte	0:FILENAME_LENGTH
	.align 2
	fileDescriptors:	.word	0:FILES				# 0 - input, 1 - output
	fileEnded:		.byte	0:FILES
	
	.align 2
	tempWord:		.word	0				# helper for writing to files
	
	.align 2
	fileSize:		.word	0				# how many symbols are in original file
	symbolsCount:		.byte	0
	
	codeLength:		.byte	0				# currently processed code length
	codeBuffer:		.byte	0:CODE_BUFFER_LENGTH		# currently processed code
	codeExtended:		.byte	0:MAX_SYMBOLS			# currently processed extended code (byte = bit)
	
	.align 2
	huffmanTree:		.word	0:MAX_HUFFMAN_TREE_NODES
	.align 2
	currentNode:		.word	0
	currentSymbol:		.byte	0

	inputBuffer:		.byte	0:CHUNK_LENGTH			# input buffer
	.align 2
	inputBufferCount:	.word	0				# byte counter
	inputBitAfterByteCount:	.byte	0				# bit after byte counter
	
	.align 2
	symbolsDecoded:		.word	0
	
#	ENCODED FILE STRUCTURE:
#	file size [4]
#	symbols count [1]
#	codes list (for each symbol: symbol [1], code length [1], code [code length])
#	encoded symbols [...]
	
#	LOOP TEMPLATE
#	add $iterator, $zero, start
#	loopLabel:
#		bgt $iterator, end, endLoopLabel

#		...
#		add $iterator, $iterator, 1

#		b loopLabel
#	endLoopLabel:
	
.text

main:

	j end
	
# $a0 - address of buffer
changeNewlineToZero:
	findNewlineLoop:
		lb $s7, ($a0)
		
		add $a0, $a0, 1
		bne $s7, '\n', findNewlineLoop
		
	sb $zero, -1($a0)	
	jr $ra

# $a0 - word to print
printBinary:
	li $v0, SYSCALL_PRINTINTBINARY
	syscall
	jr $ra
	
# $a0 - int to print
printInt:
	li $v0, SYSCALL_PRINTINT
	syscall
	jr $ra

# $a0 - character to print
printCharacter:
	li $v0, SYSCALL_PRINTCHARACTER
	syscall
	jr $ra

# $v0 = $a0 % $a1
modulo:
	divu $a0, $a1
	mfhi $v0
	jr $ra

# fills buffer with value (byte)
# $a0 - buffer address, $a1 - buffer length in bytes, $a2 - value (byte)
fill:
	add $t9, $zero, 1
	fillLoop: # from 1 to buffer length
		bgt $t9, $a1, endFillLoop
		sb $a2, ($a0)
		add $a0, $a0, 1
	
		add $t9, $t9, 1
		b fillLoop
	endFillLoop:
	
	jr $ra
	
# $a0 - byte (value), $a2 - bit index (from left)
# $v0 - value 
readBit:
	lb $s7, masks($s2)
	and $a0, $a0, $s7
	li $s7, 7
	sub $s7, $s7, $a2
	srlv $v0, $a0, $s7
	jr $ra

# $a0 - address to filename (absolute), $a1 - mode (0 - read, 1 - write, 9 - append), $a2 - file index
openFile:
	li $v0, SYSCALL_OPENFILE
	syscall
	mul $a2, $a2, 4
	sw $v0, fileDescriptors($a2)
	jr $ra

# $a0 - file index, $a1 - buffer start, $a2 - buffer length
writeToFile:
	mul $a0, $a0, 4
	lw $a0, fileDescriptors($a0)
	li $v0, SYSCALL_WRITEFILE
	syscall
	jr $ra

# $a0 - file index, $a1 - buffer start, $a2 - buffer length
# $v0 - bytes read
readToBuffer:
	move $s7, $a0
	mul $a0, $a0, 4
	lw $a0, fileDescriptors($a0)
	li $v0, SYSCALL_READFILE
	syscall
	sne $s6, $v0, $a2
	sb $s6, fileEnded($s7)
	jr $ra

# $a0 - file index
closeFile:
	mul $a0, $a0, 4
	lw $a0, fileDescriptors($a0)
	li $v0, SYSCALL_CLOSEFILE
	syscall
	jr $ra
	
readFileSize:

	jr $ra
	
readSymbolsCount:

	jr $ra
	
extendCode:

	jr $ra
	
addLeaf:

	jr $ra
	
buildHuffmanTree:

	jr $ra
	
stepDownTheTree:

	jr $ra

end:
	li $v0, 10
	syscall