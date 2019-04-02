.data

	.eqv CHUNK_LENGTH	1024
	.eqv MAX_SYMBOLS	256
	.eqv BYTES_PER_NODE	20
	.eqv NODES_LENGTH	10220	# BYTES_PER_NODE * (2*MAX_SYMBOLS - 1) - recalculate if something changes
	.eqv BYTES_PER_PQNODE	12
	.eqv PQNODES_LENGTH	6132	# BYTES_PER_PQNODE * (2*MAX_SYMBOLS - 1) - recalculate if something changes
	.eqv CODES_LENGTH	66048	# MAX_SYMBOLS*(2+MAX_SYMBOLS) - recalculate if something changes
	.eqv FILENAME_LENGTH	256
	.eqv FILES		3
	
	masks:			.byte	128, 64, 32, 16, 8, 4, 2, 1	# 10000000, 01000000, 00100000, 00010000, 00001000, 00000100, 00000010, 00000001
	
	chunk:			.byte	0:CHUNK_LENGTH			# input buffer
	.align 2
	chunkCount:		.word	0				# how many symbols in chunk
	
	.align 2
	frequencies:		.word	0:MAX_SYMBOLS			# symbol frequency list
	
	nodes:			.byte	0:NODES_LENGTH			# nodes list for Huffman Tree
	.align 2
	nodesCount:		.word	0				# number of nodes
	
	pqNodes:		.byte	0:PQNODES_LENGTH		# nodes for priority queue
	.align 2
	pqHead:			.word	0				# index of head of priority queue
	.align 2
	pqUsed:			.word	0				# first free index in queue array
	.align 2
	pqCount:		.word	0				# number of nodes currently in queue
	
	.align 2
	codes:			.word	0:CODES_LENGTH			# symbol to code list
	symbolsCount:		.byte	0				# how many symbols are used
	
	codedData:		.byte	0:CHUNK_LENGTH			# output buffer
	.align 2
	codedDataFirstFreeBit:	.word	0				# first free bit of output buffer
	trailingBitsCount:	.byte	0				# how many trailing bits were added
	
	fileNameBuffer:		.byte	0:FILENAME_LENGTH
	.align 2
	fileDescriptors:	.word	0:FILES
	fileEnded:		.byte	0:FILES
	
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
	la $a0, fileNameBuffer
	li $a1, FILENAME_LENGTH
	li $v0, 8
	syscall
	
	la $a0, fileNameBuffer
	jal changeNewlineToZero
	
	la $a0, fileNameBuffer
	li $a1, 0
	li $a2, 0
	jal openFile
	
	li $a0, 0
	la $a1, chunk
	li $a2, CHUNK_LENGTH
	jal readToBuffer
	sw $v0, chunkCount
	
	jal countSymbolsInChunk
	
	jal printFrequencies
	
	jal createNodeList
	
	li $a0, 0
	jal closeFile
	
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
	li $v0, 35
	syscall
	jr $ra
	
# $a0 - int to print
printInt:
	li $v0, 1
	syscall
	jr $ra
	
printFrequencies:
	add $t9, $zero, 1
	li $s7, MAX_SYMBOLS
	la $s6, frequencies
	printLoop: # from 1 to 256
		bgt $t9, $s7, endPrintLoop
		lw $s5, ($s6)
		
		move $a0, $t9
		sub $a0, $a0, 1
		li $v0, 11
		syscall		# print symbol
		
		li $a0, ':'
		li $v0, 11
		syscall		# print ':'
		
		move $a0, $s5
		add $sp, $sp, -4
		sw $ra, ($sp)
		jal printInt	#print frequency
		lw $ra, ($sp)
		add $sp, $sp, 4
		
		li $a0, '\n'
		li $v0, 11
		syscall		#print new line
		
		add $s6, $s6, 4
	
		add $t9, $t9, 1
		b printLoop
	endPrintLoop:
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
	
# $a0 - byte (value), $a1 - bit index (from left), $a2 - value
# $v0 - changed byte
setBit:
	lb $s7, masks($a1) 	# n-th mask
	not $s7, $s7 		# inverse mask
	and $a0, $a0, $s7 	# clear bit in byte
	li $s7, 7
	sub $s7, $s7, $a1
	sllv $a2, $a2, $s7 	# shift left (7-n) times
	or $a0, $a0, $a2
	move $v0, $a0
	jr $ra

# $a0 - address to filename (absolute), $a1 - mode (0 - read, 1 - write, 9 - append), $a2 - file index
openFile:
	li $v0, 13
	syscall
	mul $a2, $a2, 4
	sw $v0, fileDescriptors($a2)
	jr $ra

# $a0 - file index, $a1 - buffer start, $a2 - buffer length
writeToFile:
	mul $a0, $a0, 4
	lw $a0, fileDescriptors($a0)
	li $v0, 15
	syscall
	jr $ra

# $a0 - file index, $a1 - buffer start, $a2 - buffer length
# $v0 - bytes read
readToBuffer:
	move $s7, $a0
	mul $a0, $a0, 4
	lw $a0, fileDescriptors($a0)
	li $v0, 14
	syscall
	sne $s6, $v0, $a2
	sb $s6, fileEnded($s7)
	jr $ra

# $a0 - file index
closeFile:
	mul $a0, $a0, 4
	lw $a0, fileDescriptors($a0)
	li $v0, 16
	syscall
	jr $ra

countSymbolsInChunk:
	li $s7, MAX_SYMBOLS
	sub $s7, $s7, 1
	add $t9, $zero, 0
	countSymbolsSymbolLoop: #for each symbol
		bgt $t9, $s7, endCountSymbolsSymbolLoop
		add $t8, $zero, 1
		lw $s6, chunkCount
		la $s5, chunk
		countSymbolsByteLoop: #for each byte in chunk
			bgt $t8, $s6, endCountSymbolByteLoop
			lb $s4, ($s5)
			add $s5, $s5, 1
			add $t8, $t8, 1
			bne $s4, $t9, countSymbolsByteLoop	# if symbol from chunk is not the one currently processed continue
			
			mul $s4, $s4, 4
			lw $s3, frequencies($s4)
			add $s3, $s3, 1
			sw $s3, frequencies($s4)		# else increment the symbol's frequency
	
			b countSymbolsByteLoop
		endCountSymbolByteLoop:
		add $t9, $t9, 1
		b countSymbolsSymbolLoop
	endCountSymbolsSymbolLoop:
	jr $ra

createNodeList:
	la $s7, nodes
	add $t9, $zero, 0
	li $s6, MAX_SYMBOLS
	sub $s6, $s6, 1
	createNodesLoop: # for each symbol
		bgt $t9, $s6, endcreateNodesLoop
		move $s5, $t9
		add $t9, $t9, 1
		
		mul $s4, $s5, 4
		lw $s4, frequencies($s4)
		
		beqz $s4, createNodesLoop
		
		sw $s5, ($s7) 	# symbol
		sw $s4, 4($s7) 	# frequency
		li $s4, -1
		sw $s4, 8($s7) 	# left child: -1
		sw $s4, 12($s7)	# right child: -1
		sw $s4, 16($s7)	# parent: -1
		
		add $s7, $s7, 20
		lb $s4, symbolsCount
		add $s4, $s4, 1
		sb $s4, symbolsCount

		b createNodesLoop
	endcreateNodesLoop:
	jr $ra

# $a0 - node index
pushQueue:

	jr $ra

# $v0 - popped node index
popQueue:

	jr $ra

buildHuffmanTree:

	jr $ra

# $a0 - bit, $a1 - code index, $a2 - bit index
writeCodeBit:

	jr $ra

# $a0 - code index
inverseCode:

	jr $ra

# $a0 - code index
createCode:

	jr $ra

createCodeList:

	jr $ra

writeCodeListToFile:

	jr $ra

writeCodedDataToFile:

	jr $ra

# $a0 - value
addToCodedData:

	jr $ra

writeReplacedSymbols:

	jr $ra

writeRestOfCodedData:

	jr $ra

addTrailingBitsInfo:

	jr $ra

end:
	li $v0, 10
	syscall
