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
	.eqv BYTES_PER_NODE		20
	.eqv NODES_LENGTH		10220				# BYTES_PER_NODE * (2*MAX_SYMBOLS - 1) - recalculate if something changes
	.eqv BYTES_PER_PQNODE		12
	.eqv PQNODES_LENGTH		6132				# BYTES_PER_PQNODE * (2*MAX_SYMBOLS - 1) - recalculate if something changes
	.eqv CODES_LENGTH		66048				# MAX_SYMBOLS*(2+MAX_SYMBOLS) - recalculate if something changes
	.eqv FILENAME_LENGTH		256
	.eqv FILES			2
	
	masks:			.byte	128, 64, 32, 16, 8, 4, 2, 1	# 10000000, 01000000, 00100000, 00010000, 00001000, 00000100, 00000010, 00000001
	
	chunk:			.byte	0:CHUNK_LENGTH			# input buffer
	.align 2
	chunkCount:		.word	0				# how many symbols in chunk
	
	.align 2
	frequencies:		.word	0:MAX_SYMBOLS			# symbol frequency list
	
	nodes:			.byte	0:NODES_LENGTH			# nodes list for Huffman Tree (symbol[4], frequency[4], leftChildIndex[4], rightChildIndex[4], parentIndex[4])
	.align 2
	nodesCount:		.word	0				# number of nodes
	
	pqNodes:		.byte	0:PQNODES_LENGTH		# nodes for priority queue (priority[4], next[4], nodeIndex[4])
	.align 2
	pqHead:			.word	-1				# index of head of priority queue
	.align 2
	pqUsed:			.word	0				# first free index in queue array
	.align 2
	pqCount:		.word	0				# number of nodes currently in queue
	
	.align 2
	codes:			.word	0:CODES_LENGTH			# symbol to code list (symbol[1], length[1], code[256])
	symbolsCount:		.byte	0				# how many symbols are used
	
	codedData:		.byte	0:CHUNK_LENGTH			# output buffer
	.align 2
	codedDataFirstFreeBit:	.word	0				# first free bit of output buffer
	
	fileNameBuffer:		.byte	0:FILENAME_LENGTH
	.align 2
	fileDescriptors:	.word	0:FILES				# 0 - input, 1 - output
	fileEnded:		.byte	0:FILES
	
	fileSize:		.word	0				# how many symbols are in original file
	
	tempWord:		.word	0				# helper for writing to files
	
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
	
	readCountLoop:
		li $a0, 0
		la $a1, chunk
		li $a2, CHUNK_LENGTH
		jal readToBuffer
		sw $v0, chunkCount
		
		jal countSymbolsInChunk
		
		lb $s7, fileEnded
		bne $s7, 1, readCountLoop
	endReadCountLoop:
	
	li $a0, 0
	jal closeFile
	
	la $a0, fileNameBuffer
	li $a1, 0
	li $a2, 0
	jal openFile
	
	jal printFrequencies
	
	jal createNodeList
	jal buildHuffmanTree	
	jal createCodeList
	
	jal printCodes
	
	la $a0, fileNameBuffer
	li $a1, FILENAME_LENGTH
	li $v0, 8
	syscall
	
	la $a0, fileNameBuffer
	jal changeNewlineToZero
	
	la $a0, fileNameBuffer
	li $a1, 1
	li $a2, 1
	jal openFile
	
	jal writeFileSizeToFile	
	jal writeCodeListToFile
	
	encodeLoop:
		li $a0, 0
		la $a1, chunk
		li $a2, CHUNK_LENGTH
		jal readToBuffer
		sw $v0, chunkCount
		
		jal writeReplacedSymbols
		
		lb $s7, fileEnded
		bne $s7, 1, encodeLoop
	endEncodeLoop:
	
	jal writeRestOfCodedData
	
	li $a0, 1
	jal closeFile
	
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
	
printFrequencies:
	add $t9, $zero, 1
	li $s7, MAX_SYMBOLS
	la $s6, frequencies
	printFrequenciesLoop: # from 1 to 256
		bgt $t9, $s7, endPrintFrequenciesLoop
		lw $s5, ($s6)
		
		move $a0, $t9
		sub $a0, $a0, 1
		li $v0, SYSCALL_PRINTCHARACTER
		syscall						# print symbol
		
		li $a0, ':'
		li $v0, SYSCALL_PRINTCHARACTER
		syscall						# print ':'
		
		move $a0, $s5
		add $sp, $sp, -4
		sw $ra, ($sp)
		jal printInt					#print frequency
		lw $ra, ($sp)
		add $sp, $sp, 4
		
		li $a0, '\n'
		li $v0, SYSCALL_PRINTCHARACTER
		syscall						#print new line
		
		add $s6, $s6, 4
	
		add $t9, $t9, 1
		b printFrequenciesLoop
	endPrintFrequenciesLoop:
	jr $ra
	
printCodes:
	add $t9, $zero, 1
	lb $s7, symbolsCount
	la $s6, codes
	printCodesLoop: # for every symbol used
		bgt $t9, $s7, endPrintCodesLoop
		lb $s5, ($s6)					# symbol
		
		move $s3, $s6
		
		move $a0, $s5
		li $v0, SYSCALL_PRINTCHARACTER
		syscall						# print symbol
		
		li $a0, ':'
		li $v0, SYSCALL_PRINTCHARACTER
		syscall						# print ':'
		
		add $s6, $s6, 1
		lb $s5, ($s6)					#length
		add $s6, $s6, 1
		add $t8, $zero, 1
		printSingleCodeLoop:
			bgt $t8, $s5, endPrintSingleCodeLoop

			lb $s4, ($s6)
			
			add $sp, $sp, -28
			sw $t9, ($sp)
			sw $s7, 4($sp)
			sw $s3, 8($sp)
			sw $s5, 12($sp)
			sw $t8, 16($sp)
			sw $s6, 20($sp)
			sw $ra, 24($sp)
			move $a0, $s4
			jal printInt				#print bit
			lw $ra, 24($sp)
			lw $s6, 20($sp)
			lw $t8, 16($sp)
			lw $s5, 12($sp)
			lw $s3, 8($sp)
			lw $s7, 4($sp)
			lw $t9, ($sp)
			add $sp, $sp, 28
			
			add $s6, $s6, 1
			add $t8, $t8, 1

			b printSingleCodeLoop
		endPrintSingleCodeLoop:
			
		li $a0, '\n'
		li $v0, SYSCALL_PRINTCHARACTER
		syscall						#print new line
		
		add $s3, $s3, 2
		add $s3, $s3, MAX_SYMBOLS
		move $s6, $s3
	
		add $t9, $t9, 1
		b printCodesLoop
	endPrintCodesLoop:
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

countSymbolsInChunk:
	add $t9, $zero, 1
	lw $t8, chunkCount
	la $s7, chunk
	countSymbolsByteLoop:
		bgt $t9, $t8, endCountSymbolsByteLoop
		add $t9, $t9, 1

		lb $s6, ($s7)
		add $s7, $s7, 1
		
		mul $s6, $s6, 4
		lw $s5, frequencies($s6)
		add $s5, $s5, 1
		sw $s5, frequencies($s6)

		b countSymbolsByteLoop
	endCountSymbolsByteLoop:
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
	mul $s7, $a0, BYTES_PER_NODE
	lw $s7, nodes+4($s7)					# priority
	lw $s0, pqUsed			
	mul $s6, $s0, BYTES_PER_PQNODE			# BYTES_PER_PQNODE * pqUsed
	sw $s7, pqNodes($s6)				# priority
	li $s5, -1
	sw $s5, pqNodes+4($s6)				# next: -1
	sw $a0, pqNodes+8($s6)				# node index
	
	lw $s5, pqHead					# pqHead
	beq $s5, -1, emptyHead
	
		mul $s4, $s5, BYTES_PER_PQNODE		# BYTES_PER_PQNODE * pqHead
		lw $s3, pqNodes($s4)
		ble $s3, $s7, pushAfterHead
		pushBeforeHead:
			sw $s5, pqNodes+4($s6)
			sw $s0, pqHead
			b endPush
		pushAfterHead:
			move $t8, $s5
			lw $t9, pqNodes+4($s4)		# next node
			pushAfterHeadLoop:
				sne $t7, $t9, -1
				mul $s4, $t9, BYTES_PER_PQNODE
				lw $s4, pqNodes($s4)
				slt $t6, $s4, $s7
				and $t6, $t7, $t6
				beqz $t6, endPushAfterHeadLoop
				
				move $t8, $t9		# current = next
				mul $t9, $t9, BYTES_PER_PQNODE
				lw $t9, pqNodes+4($t9)	# next
				b pushAfterHeadLoop
			endPushAfterHeadLoop: 		# appropriate place found
			mul $s6, $s0, BYTES_PER_PQNODE
			sw $t9, pqNodes+4($s6)		# new->next = next
			mul $s6, $t8, BYTES_PER_PQNODE
			sw $s0, pqNodes+4($s6)		# current->next = new
			b endPush
		emptyHead:
			sw $s0, pqHead
			b endPush
	endPush:
	
	add $s7, $s0, 1
	sw $s7, pqUsed
	
	lw $s7, pqCount
	add $s7, $s7, 1
	sw $s7, pqCount 
	
	jr $ra

# $v0 - popped node index
popQueue:
	lw $s7, pqHead
	mul $s7, $s7, BYTES_PER_PQNODE
	lw $v0, pqNodes+8($s7)		# node index
	lw $s6, pqNodes+4($s7)		# next
	sw $s6, pqHead
	lw $s7, pqCount
	sub $s7, $s7, 1
	sw $s7, pqCount
	jr $ra

buildHuffmanTree:
	lw $s7, symbolsCount
	sw $s7, nodesCount
	
	add $t9, $zero, 0
	sub $s7, $s7, 1
	treePushLoop:
		bgt $t9, $s7, endTreePushLoop
		add $t9, $t9, 1
		
		move $a0, $t9
		sub $a0, $a0, 1
		add $sp, $sp, -12
		sw $t9, ($sp)
		sw $s7, 4($sp)
		sw $ra, 8($sp)
		jal pushQueue			# push every leaf to queue
		lw $ra, 8($sp)
		lw $s7, 4($sp)
		lw $t9, ($sp)
		add $sp, $sp, 12

		b treePushLoop
	endTreePushLoop:
	
	treePopLoop:
		lw $t9, pqCount
		ble $t9, 1, endTreePopLoop	# while there is more than one node in queue
		
		add $sp, $sp, -4
		sw $ra, ($sp)
		jal popQueue
		lw $ra, ($sp)
		add $sp, $sp, 4
		
		move $s6, $v0			# first popped node
		
		add $sp, $sp, -8
		sw $s6, 4($sp)
		sw $ra, ($sp)
		jal popQueue
		lw $ra, ($sp)
		lw $s6, 4($sp)
		add $sp, $sp, 8
		
		move $s5, $v0			# second popped node
		
		#create new internal node
		lw $s7, nodesCount
		mul $s4, $s7, BYTES_PER_NODE
		li $s3, -1
		sw $s3, nodes($s4)		# invalid symbol
		sw $s3, nodes+16($s4)		# no parent
		sw $s6, nodes+8($s4)		# left child
		sw $s5, nodes+12($s4)		# right child
		
		mul $s6, $s6, BYTES_PER_NODE
		mul $s5, $s5, BYTES_PER_NODE
		
		lw $s3, nodes+4($s6)
		lw $s2, nodes+4($s5)
		add $s3, $s3, $s2
		sw $s3, nodes+4($s4)		# priority = left child priority + right child priority
		
		sw $s7, nodes+16($s6)		# left child parent = new node
		sw $s7, nodes+16($s5)		# right child parent = new node
		
		move $a0, $s7
		add $sp, $sp, -8
		sw $s7, ($sp)
		sw $ra, 4($sp)
		jal pushQueue			# push internal node to queue
		lw $ra, 4($sp)
		lw $s7, ($sp)
		add $sp, $sp, 8
		
		add $s7, $s7, 1
		sw $s7, nodesCount
		
		b treePopLoop
	endTreePopLoop:
	
	add $sp, $sp, -4
	sw $ra, ($sp)
	jal popQueue
	lw $ra, ($sp)
	add $sp, $sp, 4
	
	mul $s7, $v0, BYTES_PER_NODE
	lw $s7, nodes+4($s7)
	sw $s7, fileSize			# root stores how many symbols are in original file
	
	jr $ra

# $a0 - bit, $a1 - code index, $a2 - bit index
writeCodeBit:
	li $s7, MAX_SYMBOLS
	add $s7, $s7, 2
	mul $s7, $s7, $a1
	add $s7, $s7, $a2
	sb $a0, codes+2($s7)
	jr $ra

# $a0 - code index
inverseCode:
	li $t9, MAX_SYMBOLS
	add $t9, $t9, 2
	mul $t9, $t9, $a0
	add $t9, $t9, 1
	lb $s7, codes($t9)		# code length
	add $t9, $t9, 1			# start
	add $t8, $t9, $s7	
	sub $t8, $t8, 1			# end
	inverseCodeLoop:
		bge $t9, $t8, endInverseCodeLoop
		
		lb $s7, codes($t9)
		lb $s6, codes($t8)
		sb $s6, codes($t9)
		sb $s7, codes($t8)
		
		add $t9, $t9, 1
		sub $t8, $t8, 1

		b inverseCodeLoop
	endInverseCodeLoop:
	jr $ra

# $a0 - code index
createCode:
	lb $s7, symbolsCount
	bne $s7, 1, moreThanOneSymbol
	singleSymbol:
		add $sp, $sp, -8
		sw $a0, ($sp)
		sw $ra, 4($sp)
		move $a1, $a0
		li $a0, 0
		li $a2, 0
		jal writeCodeBit	# if there is only one symbol, it's code is 0
		lw $ra, 4($sp)
		lw $a0, ($sp)
		add $sp, $sp, 8
		
		li $s7, MAX_SYMBOLS
		add $s7, $s7, 2
		mul $s7, $s7, $a0
		li $s6, 1
		sb $s6, codes+1($s7)
		
		jr $ra
	moreThanOneSymbol:
	
	li $s7, 0					# current bit
	move $t9, $a0					# current node
	
	treeTraversalLoop:
		mul $s6, $t9, BYTES_PER_NODE
		lw $s6, nodes+16($s6)
		beq $s6, -1, endTreeTraversalLoop	# stop when node has no parent

		move $t8, $t9				# previous node
		mul $s6, $t8, BYTES_PER_NODE
		lw $t9, nodes+16($s6)			# current node = parent of current node
		
		mul $s6, $t9, BYTES_PER_NODE
		lw $s6, nodes+8($s6)
		
		seq $s6, $t8, $s6			# left child = 0, right child = 1
		
		add $sp, $sp, -16
		sw $t9, ($sp)
		sw $s7, 4($sp)
		sw $a0, 8($sp)
		sw $ra, 12($sp)
		move $a1, $a0
		move $a0, $s6
		move $a2, $s7
		jal writeCodeBit
		lw $ra, 12($sp)
		lw $a0, 8($sp)
		lw $s7, 4($sp)
		lw $t9, ($sp)
		add $sp, $sp, 16
		
		add $s7, $s7, 1

		b treeTraversalLoop
	endTreeTraversalLoop:
	
	li $s6, MAX_SYMBOLS
	add $s6, $s6, 2
	mul $s6, $s6, $a0
	sb $s7, codes+1($s6)				# code length
	
	add $sp, $sp, -4
	sw $ra, ($sp)
	jal inverseCode
	lw $ra, ($sp)
	add $sp, $sp, 4
	
	jr $ra

createCodeList:
	lb $s7, symbolsCount
	sub $s7, $s7, 1
	add $t9, $zero, 0
	createCodesLoop: # from 0 to (symbolsCount - 1)
		bgt $t9, $s7, endCreateCodesLoop
		
		mul $s6, $t9, BYTES_PER_NODE
		lw $s6, nodes($s6)			#symbol
		
		li $s5, MAX_SYMBOLS
		add $s5, $s5, 2
		mul $s5, $s5, $t9
		sb $s6, codes($s5)
		
		add $sp, $sp, -12
		sw $t9, ($sp)
		sw $s7, 4($sp)
		sw $ra, 8($sp)
		move $a0, $t9
		jal createCode
		lw $ra, 8($sp)
		lw $s7, 4($sp)
		lw $t9, ($sp)
		add $sp, $sp, 12

		add $t9, $t9, 1

		b createCodesLoop
	endCreateCodesLoop:
	jr $ra
	
writeFileSizeToFile:
	add $sp, $sp, -4
	sw $ra, ($sp)
	li $a0, 1
	la $a1, fileSize
	li $s2, 4
	jal writeToFile
	lw $ra, ($sp)
	add $sp, $sp, 4
	
	jr $ra

writeCodeListToFile:
	add $sp, $sp, -4
	sw $ra, ($sp)
	li $a0, 1
	la $a1, symbolsCount
	li $a2, 1
	jal writeToFile
	lw $ra, ($sp)
	add $sp, $sp, 4
	
	add $t9, $zero, 0
	lb $t8, symbolsCount
	sub $t8, $t8, 1
	codeListToFileSymbolLoop: # from 0 to (symbolsCount - 1)
		bgt $t9, $t8, endCodeListToFileSymbolLoop
		
		li $s7, MAX_SYMBOLS
		add $s7, $s7, 2
		mul $s7, $s7, $t9			# beginning of current code list entry
		
		lb $s6, codes($s7)			# symbol
		sb $s6, tempWord
		
		add $sp, $sp, -16
		sw $s7, 12($sp)
		sw $t8, 8($sp)
		sw $t9, 4($sp)
		sw $ra, ($sp)
		li $a0, 1
		la $a1, tempWord
		li $a2, 1
		jal writeToFile				# write symbol
		lw $ra, ($sp)
		lw $t9, 4($sp)
		lw $t8, 8($sp)
		lw $s7, 12($sp)
		add $sp, $sp, 16
		
		lb $s6, codes+1($s7)
		sb $s6, tempWord
		
		add $sp, $sp, -16
		sw $s7, 12($sp)
		sw $t8, 8($sp)
		sw $t9, 4($sp)
		sw $ra, ($sp)
		li $a0, 1
		la $a1, tempWord
		li $a2, 1
		jal writeToFile				# write length
		lw $ra, ($sp)
		lw $t9, 4($sp)
		lw $t8, 8($sp)
		lw $s7, 12($sp)
		add $sp, $sp, 16
		
		lb $s6, tempWord			# length
		
		li $s5, 0				# byte to write
		li $s4, 0				# how many bits used in byte
		
		add $t7, $zero, 0
		sub $t6, $s6, 1
		codeListToFileCodeLoop: # from 0 to (length - 1)
			bgt $t7, $t6, endCodeListToFileCodeLoop
			
			lb $s6, codes+2($s7)		# bit of code
			
			add $sp, $sp, -32
			sw $s4, 28($sp)
			sw $s5, 24($sp)
			sw $t6, 20($sp)
			sw $t7, 16($sp)
			sw $s7, 12($sp)
			sw $t8, 8($sp)
			sw $t9, 4($sp)
			sw $ra, ($sp)
			move $a0, $s5
			move $a1, $s4
			move $a2, $s6
			jal setBit			# set proper bit in byte
			lw $ra, ($sp)
			lw $t9, 4($sp)
			lw $t8, 8($sp)
			lw $s7, 12($sp)
			lw $t7, 16($sp)
			lw $t6, 20($sp)
			lw $s5, 24($sp)
			lw $s4, 28($sp)
			add $sp, $sp, 32
			
			move $s5, $v0
			
			add $s4, $s4, 1	
			add $s7, $s7, 1
			add $t7, $t7, 1
			
			bne $s4, 8, codeListToFileCodeLoop
			
			sb $s5, tempWord
			
			add $sp, $sp, -24
			sw $t6, 20($sp)
			sw $t7, 16($sp)
			sw $s7, 12($sp)
			sw $t8, 8($sp)
			sw $t9, 4($sp)
			sw $ra, ($sp)
			li $a0, 1
			la $a1, tempWord		# if whole byte is used, write it to file
			li $a2, 1
			jal writeToFile
			lw $ra, ($sp)
			lw $t9, 4($sp)
			lw $t8, 8($sp)
			lw $s7, 12($sp)
			lw $t7, 16($sp)
			lw $t6, 20($sp)
			add $sp, $sp, 24
			
			li $s5, 0
			li $s4, 0

			b codeListToFileCodeLoop
		endCodeListToFileCodeLoop:
		
		add $t9, $t9, 1
		
		beq $s4, 0, codeListToFileSymbolLoop
		
		sb $s5, tempWord
		
		add $sp, $sp, -12
		sw $t8, 8($sp)
		sw $t9, 4($sp)
		sw $ra, ($sp)
		li $a0, 1
		la $a1, tempWord			# write rest of byte
		li $a2, 1
		jal writeToFile
		lw $ra, ($sp)
		lw $t9, 4($sp)
		lw $t8, 8($sp)
		add $sp, $sp, 12

		b codeListToFileSymbolLoop
	endCodeListToFileSymbolLoop:
	
	jr $ra
	
# $a0 - symbol
# $v0 - index
findCodeIndex:
	add $t9, $zero, 0
	lb $s7, symbolsCount
	sub $s7, $s7, 1
	findCodeIndexLoop: # from 0 to (symbolsCount - 1)
		bgt $t9, $s7, codeIndexNotFound

		li $s6, MAX_SYMBOLS
		add $s6, $s6, 2
		mul $s6, $s6, $t9
		
		lb $s6, codes($s6)
		
		add $t9, $t9, 1
		
		bne $s6, $a0, findCodeIndexLoop
		
		codeIndexFound:
		sub $v0, $t9, 1
		jr $ra
	codeIndexNotFound:
	li $v0, -1
	jr $ra

writeCodedDataToFile:
	add $sp, $sp, -4
	sw $ra, ($sp)
	li $a0, 1
	la $a1, codedData
	li $a2, CHUNK_LENGTH
	jal writeToFile
	lw $ra, ($sp)
	add $sp, $sp, 4
	
	jr $ra

# $a0 - value
addToCodedData:
	move $s7, $a0
	lw $s6, codedDataFirstFreeBit
	
	li $s5, 8
	divu $s6, $s5
	mfhi $s5			# remainder
	mflo $s4			# which byte
	
	lb $s3, codedData($s4)		# loaded byte
	
	add $sp, $sp, -12
	sw $s6, 8($sp)
	sw $s4, 4($sp)
	sw $ra, ($sp)
	move $a0, $s3
	move $a1, $s5
	move $a2, $s7
	jal setBit
	lw $ra, ($sp)
	lw $s4, 4($sp)
	lw $s6, 8($sp)
	add $sp, $sp, 12
	
	move $s3, $v0
	
	sb $s3, codedData($s4)
	
	add $s6, $s6, 1
	sw $s6, codedDataFirstFreeBit
	
	li $s7, CHUNK_LENGTH
	mul $s7, $s7, 8
	blt $s6, $s7, endCodedDataFilled
	codedDataFilled:
		add $sp, $sp, -4
		sw $ra, ($sp)
		jal writeCodedDataToFile
		lw $ra, ($sp)
		add $sp, $sp, 4
		
		add $sp, $sp, -4
		sw $ra, ($sp)
		la $a0, codedData
		li $a1, CHUNK_LENGTH
		li $a2, 0
		jal fill
		lw $ra, ($sp)
		add $sp, $sp, 4
		
		li $s7, 0
		sw $s7, codedDataFirstFreeBit
	endCodedDataFilled:
	
	jr $ra

writeReplacedSymbols:
	add $t9, $zero, 0
	lw $t8, chunkCount
	sub $t8, $t8, 1
	replaceSymbolsChunkLoop: # for each byte in chunk
		bgt $t9, $t8, endReplaceSymbolsChunkLoop

		lb $s7, chunk($t9)		# symbol
		
		add $sp, $sp, -12
		sw $t9, 8($sp)
		sw $t8, 4($sp)
		sw $ra, ($sp)
		move $a0, $s7
		jal findCodeIndex
		lw $ra, ($sp)
		lw $t8, 4($sp)
		lw $t9, 8($sp)
		add $sp, $sp, 12
		
		move $s7, $v0			# code index
		
		li $s6, MAX_SYMBOLS
		add $s6, $s6, 2
		mul $s6, $s6, $s7		# (2 + MAX_SYMBOLS) * code index
		
		lb $s5, codes+1($s6)		# code length
		
		add $t7, $zero, 0
		sub $t6, $s5, 1
		replaceSymbolsBitLoop: # from 0 to (code length - 1)
			bgt $t7, $t6, endReplaceSymbolsBitLoop

			add $s4, $s6, $t7
			lb $s4, codes+2($s4)
			
			add $sp, $sp, -24
			sw $t6, 20($sp)
			sw $t7, 16($sp)
			sw $s6, 12($sp)
			sw $t9, 8($sp)
			sw $t8, 4($sp)
			sw $ra, ($sp)
			move $a0, $s4
			jal addToCodedData
			lw $ra, ($sp)
			lw $t8, 4($sp)
			lw $t9, 8($sp)
			lw $s6, 12($sp)
			lw $t7, 16($sp)
			lw $t6, 20($sp)
			add $sp, $sp, 24
			
			add $t7, $t7, 1

			b replaceSymbolsBitLoop
		endReplaceSymbolsBitLoop:
		
		add $t9, $t9, 1

		b replaceSymbolsChunkLoop
	endReplaceSymbolsChunkLoop:
	jr $ra

writeRestOfCodedData:
	lw $s7, codedDataFirstFreeBit
	li $s6, 8
	
	divu $s7, $s6
	mfhi $s7			# remainder
	mflo $s6
	
	sne $s7, $s7, 0
	add $s6, $s6, $s7		# needed bytes
	
	add $sp, $sp, -4
	sw $ra, ($sp)
	li $a0, 1
	la $a1, codedData
	move $a2, $s6
	jal writeToFile
	lw $ra, ($sp)
	add $sp, $sp, 4
	
	jr $ra

end:
	li $v0, 10
	syscall
