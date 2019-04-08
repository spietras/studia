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

	.eqv CHUNK_LENGTH		65536
	.eqv MAX_SYMBOLS		256
	.eqv BYTES_PER_NODE		12
	.eqv MAX_HUFFMAN_TREE_NODES	6132				# BYTES_PER_NODE * (2*MAX_SYMBOLS - 1) - recalculate if something changes
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
	.align 2
	symbolsCount:		.word	0
	
	codeLength:		.byte	0				# currently processed code length
	codeBuffer:		.byte	0:CODE_BUFFER_LENGTH		# currently processed code
	codeExtended:		.byte	0:MAX_SYMBOLS			# currently processed extended code (byte = bit)
	
	.align 2
	huffmanTree:		.byte	-1:MAX_HUFFMAN_TREE_NODES
	.align 2
	nodeCount:		.word	1
	.align 2
	currentNode:		.word	0				# current node when stepping down the tree while decoding
	currentSymbol:		.byte	0

	inputBuffer:		.byte	0:CHUNK_LENGTH			# input buffer
	.align 2
	inputBufferCount:	.word	0				# byte counter
	inputBitAfterByteCount:	.byte	0				# bit after byte counter
	
	.align 2
	symbolsDecoded:		.word	0
	
	inputPrompt:		.asciiz	"Input file:\n"
	outputPrompt:		.asciiz	"Output file: \n"
	
#	ENCODED FILE STRUCTURE:
#	file size [4]
#	symbols count [2]
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
	la $a0, inputPrompt
	li $v0, SYSCALL_PRINTSTRING
	syscall
	
	la $a0, fileNameBuffer
	li $a1, FILENAME_LENGTH
	li $v0, SYSCALL_READSTRING
	syscall
	
	la $a0, fileNameBuffer
	jal changeNewlineToZero
	
	la $a0, fileNameBuffer
	li $a1, 0
	li $a2, 0
	jal openFile
	
	jal readFileSize
	jal readSymbolsCount
	jal buildHuffmanTree
	
	li $s7, CHUNK_LENGTH
	sw $s7, inputBufferCount
	
	la $a0, outputPrompt
	li $v0, SYSCALL_PRINTSTRING
	syscall
	
	la $a0, fileNameBuffer
	li $a1, FILENAME_LENGTH
	li $v0, SYSCALL_READSTRING
	syscall
	
	la $a0, fileNameBuffer
	jal changeNewlineToZero
	
	la $a0, fileNameBuffer
	li $a1, 1
	li $a2, 1
	jal openFile
	
	decodeLoop:
		lw $s7, symbolsDecoded
		lw $s6, fileSize
		
		beq $s7, $s6, endDecodeLoop
		
		jal stepDownTheTree
		
		b decodeLoop
	endDecodeLoop:
	
	li $a0, 1
	jal closeFile
	
	li $a0, 0
	jal closeFile

	j end
	
# $a0 - address of buffer
changeNewlineToZero:
	findNewlineLoop:
		lbu $s7, ($a0)
		
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
	
# $a0 - byte (value), $a1 - bit index (from left)
# $v0 - value 
readBit:
	lbu $s7, masks($a1)
	and $a0, $a0, $s7
	li $s7, 7
	sub $s7, $s7, $a1
	srlv $v0, $a0, $s7
	and $v0, $v0, 1
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
	add $sp, $sp, -4
	sw $ra, ($sp)
	li $a0, 0
	la $a1, fileSize
	li $a2, 4
	jal readToBuffer
	lw $ra, ($sp)
	add $sp, $sp, 4
	
	jr $ra
	
readSymbolsCount:
	add $sp, $sp, -4
	sw $ra, ($sp)
	li $a0, 0
	la $a1, symbolsCount
	li $a2, 2
	jal readToBuffer
	lw $ra, ($sp)
	add $sp, $sp, 4
	
	jr $ra
	
extendCode:
	li $s7, 0		# current bit
	la $s6, codeBuffer	# current byte address
	
	lbu $t8, codeLength
	sub $t8, $t8, 1
	add $t9, $zero, 0
	extendCodeLoop: # from 0 to (code length - 1)
		bgt $t9, $t8, endExtendCodeLoop

		bne $s7, 8, endNextByte
			add $s6, $s6, 1
			li $s7, 0
		endNextByte:	
		
		add $sp, $sp, -20
		sw $s6, 16($sp)
		sw $s7, 12($sp)
		sw $t8, 8($sp)
		sw $t9, 4($sp)
		sw $ra, ($sp)
		lbu $a0, ($s6)
		move $a1, $s7
		jal readBit
		lw $ra, ($sp)
		lw $t9, 4($sp)
		lw $t8, 8($sp)
		lw $s7, 12($sp)
		lw $s6, 16($sp)
		add $sp, $sp, 20
		
		sb $v0, codeExtended($t9)
		
		add $s7, $s7, 1
		add $t9, $t9, 1

		b extendCodeLoop
	endExtendCodeLoop:
	
	jr $ra
	
addLeaf:
	li $s7, 0				# current node
	
	lbu $t8, codeLength
	sub $t8, $t8, 1
	add $t9, $zero, 0
	addLeafLoop: # from 0 to (code length - 1)
		bgt $t9, $t8, endAddLeafLoop

		lbu $s6, codeExtended($t9)	# bit

		mul $s5, $s7, BYTES_PER_NODE
		add $s5, $s5, 4
		mul $s6, $s6, 4
		add $s5, $s5, $s6
		
		lw $s4, huffmanTree($s5)
		
		bne $s4, -1, endAddNewNode
			lw $s4, nodeCount
			sw $s4, huffmanTree($s5)
			add $s3, $s4, 1
			sw $s3, nodeCount
		endAddNewNode:
		
		move $s7, $s4

		add $t9, $t9, 1

		b addLeafLoop
	endAddLeafLoop:
	
	mul $s7, $s7, BYTES_PER_NODE
	lbu $s6, currentSymbol
	sw $s6, huffmanTree($s7)
	
	jr $ra
	
buildHuffmanTree:
	lw $t8, symbolsCount
	sub $t8, $t8, 1
	add $t9, $zero, 0
	huffmanTreeLoop: # from 0 to (symbols count - 1)
		bgt $t9, $t8, endHuffmanTreeLoop

		add $sp, $sp, -12
		sw $t8, 8($sp)
		sw $t9, 4($sp)
		sw $ra, ($sp)
		li $a0, 0
		la $a1, currentSymbol
		li $a2, 1
		jal readToBuffer
		lw $ra, ($sp)
		lw $t9, 4($sp)
		lw $t8, 8($sp)
		add $sp, $sp, 12
		
		add $sp, $sp, -12
		sw $t8, 8($sp)
		sw $t9, 4($sp)
		sw $ra, ($sp)
		li $a0, 0
		la $a1, codeLength
		li $a2, 1
		jal readToBuffer
		lw $ra, ($sp)
		lw $t9, 4($sp)
		lw $t8, 8($sp)
		add $sp, $sp, 12
		
		lbu $s7, codeLength
		li $s6, 8
		div $s7, $s6
		
		mflo $s7
		mfhi $s6
		
		sne $s6, $s6, 0
		add $s7, $s7, $s6		# needed bytes
		
		add $sp, $sp, -12
		sw $t8, 8($sp)
		sw $t9, 4($sp)
		sw $ra, ($sp)
		li $a0, 0
		la $a1, codeBuffer
		move $a2, $s7
		jal readToBuffer		# read code
		lw $ra, ($sp)
		lw $t9, 4($sp)
		lw $t8, 8($sp)
		add $sp, $sp, 12
		
		add $sp, $sp, -12
		sw $t8, 8($sp)
		sw $t9, 4($sp)
		sw $ra, ($sp)
		jal extendCode
		lw $ra, ($sp)
		lw $t9, 4($sp)
		lw $t8, 8($sp)
		add $sp, $sp, 12
		
		add $sp, $sp, -12
		sw $t8, 8($sp)
		sw $t9, 4($sp)
		sw $ra, ($sp)
		jal addLeaf
		lw $ra, ($sp)
		lw $t9, 4($sp)
		lw $t8, 8($sp)
		add $sp, $sp, 12

		add $t9, $t9, 1

		b huffmanTreeLoop
	endHuffmanTreeLoop:

	jr $ra
	
stepDownTheTree:
	lw $s7, inputBufferCount
	bne $s7, CHUNK_LENGTH, endLoadAnotherChunk
		add $sp, $sp, -4
		sw $ra, ($sp)
		li $a0, 0
		la $a1, inputBuffer
		li $a2, CHUNK_LENGTH
		jal readToBuffer
		lw $ra, ($sp)
		add $sp, $sp, 4
		
		li $s7, 0
		sw $s7, inputBufferCount
		li $s6, 0
		sb $s6, inputBitAfterByteCount
	endLoadAnotherChunk:
	
	add $sp, $sp, -4
	sw $ra, ($sp)
	lbu $a0, inputBuffer($s7)
	lbu $a1, inputBitAfterByteCount
	jal readBit
	lw $ra, ($sp)
	add $sp, $sp, 4
	
	move $s5, $v0				# bit
	lbu $s6, inputBitAfterByteCount		# bit counter
	lw $s7, inputBufferCount		# byte counter
	
	add $s6, $s6, 1
	
	bne $s6, 8, endGoToTheNextByte
		add $s7, $s7, 1
		li $s6, 0
	endGoToTheNextByte:
	
	sw $s7, inputBufferCount
	sb $s6, inputBitAfterByteCount
	
	lw $s7, currentNode
	mul $s7, $s7, BYTES_PER_NODE
	add $s7, $s7, 4
	mul $s5, $s5, 4
	add $s7, $s7, $s5
	
	lw $s7, huffmanTree($s7)
	sw $s7, currentNode
	mul $s7, $s7, BYTES_PER_NODE
	lw $s7, huffmanTree($s7)		# current symbol
	sb $s7, currentSymbol
	
	beq $s7, -1, endLeafFound
		add $sp, $sp, -4
		sw $ra, ($sp)
		li $a0, 1
		la $a1, currentSymbol
		li $a2, 1
		jal writeToFile
		lw $ra, ($sp)
		add $sp, $sp, 4
		
		li $s7, 0
		sw $s7, currentNode
		
		lw $s7, symbolsDecoded
		add $s7, $s7, 1
		sw $s7, symbolsDecoded		
	endLeafFound:
	
	jr $ra

end:
	li $v0, 10
	syscall
