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
	.eqv CHUNK_LENGTH_IN_BITS	524288				# 8 * CHUNK_LENGTH - recalculate if something changes
	.eqv MAX_SYMBOLS		256
	.eqv BYTES_PER_FREQUENCY	8
	.eqv FREQUENCY_LENGTH		2048				# BYTES_PER_FREQUENCY * MAX_SYBOLS - recalculate if something changes
	.eqv BYTES_PER_NODE		24
	.eqv NODES_LENGTH		12264				# BYTES_PER_NODE * (2*MAX_SYMBOLS - 1) - recalculate if something changes
	.eqv BYTES_PER_PQNODE		12
	.eqv PQNODES_LENGTH		6132				# BYTES_PER_PQNODE * (2*MAX_SYMBOLS - 1) - recalculate if something changes
	.eqv BYTES_PER_CODE		258
	.eqv CODES_LENGTH		66048				# BYTES_PER_CODE * MAX_SYMBOLS - recalculate if something changes
	.eqv FILENAME_LENGTH		256
	.eqv FILES			2
	
	masks:			.byte	-128, 64, 32, 16, 8, 4, 2, 1	# 10000000, 01000000, 00100000, 00010000, 00001000, 00000100, 00000010, 00000001
	
	chunk:			.byte	0:CHUNK_LENGTH			# input buffer
	.align 2
	chunkCount:		.word	0				# how many symbols in chunk
	
	.align 2
	frequencies:		.byte	0:FREQUENCY_LENGTH		# symbol frequency list (frequency[4], node address[4])
	
	.align 2
	nodes:			.byte	0:NODES_LENGTH			# nodes list for Huffman Tree (symbol[4], frequency[4], leftChildAddress[4], rightChildAddress[4], parentAddress[4], code address [4])
	.align 2
	firstFreeNodeAddress:	.word	0				# number of nodes
	
	.align 2
	pqNodes:		.byte	0:PQNODES_LENGTH		# nodes for priority queue (priority[4], next[4], nodeAddress[4])
	.align 2
	pqHead:			.word	0				# address of head of priority queue
	.align 2
	pqFirstFreeAddress:	.word	0				# first free address in queue array
	.align 2
	pqCount:		.word	0				# number of nodes currently in queue
	
	.align 2
	codes:			.word	0:CODES_LENGTH			# symbol to code list (symbol[1], length[1], code[256])
	.align 2
	symbolsCount:		.word	0				# how many symbols are used
	
	codedData:		.byte	0:CHUNK_LENGTH			# output buffer
	.align 2
	codedDataFirstFreeBit:	.word	0				# first free bit of output buffer
	
	fileNameBuffer:		.byte	0:FILENAME_LENGTH
	.align 2
	fileDescriptors:	.word	0:FILES				# 0 - input, 1 - output
	fileEnded:		.byte	0:FILES
	
	.align 2
	fileSize:		.word	0				# how many symbols are in original file

	tempByte:		.byte	0				# helper for writing to files
	
	inputPrompt:		.asciiz	"\nInput file:\n"
	outputPrompt:		.asciiz	"\nOutput file: \n"
	freqHeading:		.asciiz "\nFrequencies:\n"
	codesHeading:		.asciiz "\nCodes:\n"
	
#	ENCODED FILE STRUCTURE:
#	file size [4]
#	symbols count [2]
#	codes list (for each symbol: symbol [1], code length [1], code [code length])
#	encoded symbols [...]
	
.text

main:
	la $a0, inputPrompt
	li $v0, SYSCALL_PRINTSTRING
	syscall
	
	la $a0, fileNameBuffer
	li $a1, FILENAME_LENGTH
	li $v0, SYSCALL_READSTRING
	syscall							# read input file name
	
	la $a0, fileNameBuffer
	jal changeNewlineToZero					# null-terminate file name
	
	la $a0, fileNameBuffer
	li $a1, 0
	li $a2, 0
	jal openFile						# open input file
	
	readCountLoop:
		li $a0, 0
		la $a1, chunk
		li $a2, CHUNK_LENGTH
		jal readToBuffer				# read chunk of data
		sw $v0, chunkCount
		
		jal countSymbolsInChunk				# count how many times each symbol appear in chunk
		
		lbu $s7, fileEnded
		bne $s7, 1, readCountLoop			# repeat until file has ended
	endReadCountLoop:
	
	li $a0, 0
	jal closeFile						# close input file
	
	la $a0, fileNameBuffer
	li $a1, 0
	li $a2, 0
	jal openFile						# and reopen it, so we can read from the beginning
	
	jal printFrequencies					# print frequency of each symbol
	
	jal createNodeList
	jal buildHuffmanTree					# knowing frequencies, build huffman tree with symbols as leaves	
	jal createCodeList					# assign code to each symbol based on traversing from root to leaf in huffman tree
	
	jal printCodes						# print code of each symbol
	
	la $a0, outputPrompt
	li $v0, SYSCALL_PRINTSTRING
	syscall
	
	
	la $a0, fileNameBuffer
	li $a1, FILENAME_LENGTH
	li $v0, SYSCALL_READSTRING
	syscall							# read output file name
	
	la $a0, fileNameBuffer
	jal changeNewlineToZero					# null-terminate it
	
	la $a0, fileNameBuffer
	li $a1, 1
	li $a2, 1
	jal openFile						# open output file
	
	jal writeFileSizeToFile					# write how many symbols were present in input file
	jal writeCodeListToFile					# write used symbols and their codes
	
	encodeLoop:
		li $a0, 0
		la $a1, chunk
		li $a2, CHUNK_LENGTH
		jal readToBuffer				# read chunk of data
		sw $v0, chunkCount
		
		jal writeReplacedSymbols			# encode it and write to output file if needed
		
		lbu $s7, fileEnded
		bne $s7, 1, encodeLoop
	endEncodeLoop:
	
	jal writeRestOfCodedData				# write rest of data if not written previously
	
	li $a0, 1
	jal closeFile						# close output file
	
	li $a0, 0
	jal closeFile						# close input file
	
	j end

# $a0 - address of buffer
changeNewlineToZero:
	findNewlineLoop:
		lbu $s7, ($a0)	
		addu $a0, $a0, 1
		bne $s7, '\n', findNewlineLoop			# find newline	
			
	sb $zero, -1($a0)					# replace it with zero
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
	addu $sp, $sp, -4
	sw $ra, ($sp)
	
	la $a0, freqHeading
	li $v0, SYSCALL_PRINTSTRING
	syscall
	
	addu $t9, $zero, 1
	li $s7, MAX_SYMBOLS
	la $s6, frequencies
	printFrequenciesLoop: 					# from 1 to 256
		bgt $t9, $s7, endPrintFrequenciesLoop
		lw $s5, ($s6)
		
		addu $s6, $s6, BYTES_PER_FREQUENCY
		addu $t9, $t9, 1
		
		beq $s5, 0, printFrequenciesLoop		# skip if symbol not used
		
		move $a0, $t9
		sub $a0, $a0, 2
		li $v0, SYSCALL_PRINTCHARACTER
		syscall						# print symbol
		
		li $a0, ':'
		li $v0, SYSCALL_PRINTCHARACTER
		syscall						# print ':'
		
		move $a0, $s5
		jal printInt					#print frequency
		
		li $a0, '\n'
		li $v0, SYSCALL_PRINTCHARACTER
		syscall						#print new line
		
		b printFrequenciesLoop
	endPrintFrequenciesLoop:
	
	lw $ra, ($sp)
	addu $sp, $sp, 4
	jr $ra
	
printCodes:
	addu $sp, $sp, -4
	sw $ra, ($sp)
	
	la $a0, codesHeading
	li $v0, SYSCALL_PRINTSTRING
	syscall
	
	addu $t9, $zero, 1
	lw $s7, symbolsCount
	la $s6, codes
	printCodesLoop: 					# for every symbol used
		bgt $t9, $s7, endPrintCodesLoop
		lbu $s5, ($s6)					# symbol
		
		move $s3, $s6
		
		move $a0, $s5
		li $v0, SYSCALL_PRINTCHARACTER
		syscall						# print symbol
		
		li $a0, ':'
		li $v0, SYSCALL_PRINTCHARACTER
		syscall						# print ':'
		
		addu $s6, $s6, 1
		lbu $s5, ($s6)					# load code length
		addu $s6, $s6, 1
		addu $t8, $zero, 1
		
		addu $sp, $sp, -12
		sw $t9, ($sp)
		sw $s7, 4($sp)
		sw $s3, 8($sp)
		
		printSingleCodeLoop:				# for each bit in code
			bgt $t8, $s5, endPrintSingleCodeLoop

			lbu $s4, ($s6)				# load bit
			
			addu $sp, $sp, -12
			sw $s5, ($sp)
			sw $t8, 4($sp)
			sw $s6, 8($sp)
			move $a0, $s4
			jal printInt				# print bit
			lw $s6, 8($sp)
			lw $t8, 4($sp)
			lw $s5, ($sp)
			addu $sp, $sp, 12
			
			addu $s6, $s6, 1
			addu $t8, $t8, 1

			b printSingleCodeLoop
		endPrintSingleCodeLoop:
		
		lw $s3, 8($sp)
		lw $s7, 4($sp)
		lw $t9, ($sp)
		addu $sp, $sp, 12
			
		li $a0, '\n'
		li $v0, SYSCALL_PRINTCHARACTER
		syscall						# print new line
		
		addu $s3, $s3, 2
		addu $s3, $s3, MAX_SYMBOLS
		move $s6, $s3					# go to the next code
	
		addu $t9, $t9, 1
		b printCodesLoop
	endPrintCodesLoop:
	
	lw $ra, ($sp)
	addu $sp, $sp, 4
	jr $ra

# fills buffer with value (byte)
# $a0 - buffer address, $a1 - buffer length in bytes, $a2 - value (byte)
fill:
	addu $t9, $zero, 1
	fillLoop: 						# from 1 to buffer length
		bgt $t9, $a1, endFillLoop
		sb $a2, ($a0)
		addu $a0, $a0, 1
	
		addu $t9, $t9, 1
		b fillLoop
	endFillLoop:
	
	jr $ra
	
# $a0 - byte (value), $a1 - bit index (from left), $a2 - value
# $v0 - changed byte
setBit:
	lbu $s7, masks($a1) 					# n-th mask
	not $s7, $s7 						# inverse mask
	and $a0, $a0, $s7 					# clear bit in byte
	li $s7, 7
	sub $s7, $s7, $a1
	sllv $a2, $a2, $s7 					# shift left (7-n) times
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
	addu $t9, $zero, 1
	lw $t8, chunkCount
	la $s7, chunk
	countSymbolsByteLoop:					# for each symbol in chunk
		bgt $t9, $t8, endCountSymbolsByteLoop
		addu $t9, $t9, 1

		lbu $s6, ($s7)
		addu $s7, $s7, 1
		
		mul $s6, $s6, BYTES_PER_FREQUENCY		# find proper index
		lw $s5, frequencies($s6)
		addu $s5, $s5, 1
		sw $s5, frequencies($s6)			# increment frequency and store it back

		b countSymbolsByteLoop
	endCountSymbolsByteLoop:
	jr $ra

createNodeList:
	la $s7, nodes
	la $t8, frequencies
	addu $t9, $zero, 0
	li $s6, MAX_SYMBOLS
	sub $s6, $s6, 1
	createNodesLoop: 					# for each symbol
		bgt $t9, $s6, endcreateNodesLoop
		move $s5, $t9
		
		lw $s3, ($t8)					# load frequency of that symbol
		
		addu $t9, $t9, 1
		addu $t8, $t8, BYTES_PER_FREQUENCY
		
		beqz $s3, createNodesLoop			# skip if frequency is 0
		
		sw $s5, ($s7) 					# store symbol in node
		sw $s3, 4($s7) 					# store frequency in node
		
		sw $s7, -4($t8)					# store node address in frequencies list
		
		addu $s7, $s7, BYTES_PER_NODE
		lw $s4, symbolsCount
		addu $s4, $s4, 1
		sw $s4, symbolsCount				# increment used symbols

		b createNodesLoop
	endcreateNodesLoop:
	sw $s7, firstFreeNodeAddress
	jr $ra

# $a0 - node address
pushQueue:
	lw $s7, 4($a0)						# load frequency (used as priority)
	lw $s0, pqFirstFreeAddress	
	sw $s7, ($s0)						# store priority in new queue node
	sw $zero, 4($s0)					# next: 0
	sw $a0, 8($s0)						# store node address
	
	lw $s5, pqHead						# load head of the queue
	beq $s5, 0, emptyHead
	
		lw $s3, ($s5)					# load priority of head
		ble $s3, $s7, pushAfterHead
		pushBeforeHead:
			sw $s5, 4($s0)
			sw $s0, pqHead
			b endPush
		pushAfterHead:
			move $t8, $s5
			lw $t9, 4($t8)				# next node
			pushAfterHeadLoop:
				beq $t9, 0, endPushAfterHeadLoop
				lw $s4, ($t9)
				bge $s4, $s7, endPushAfterHeadLoop
				
				move $t8, $t9			# current = next
				lw $t9, 4($t9)			# next
				b pushAfterHeadLoop
			endPushAfterHeadLoop: 			# appropriate place found
			sw $t9, 4($s0)				# new->next = next
			sw $s0, 4($t8)				# current->next = new
			b endPush
	emptyHead:
		sw $s0, pqHead
		b endPush
	endPush:
	
	addu $s7, $s0, BYTES_PER_PQNODE
	sw $s7, pqFirstFreeAddress
	
	lw $s7, pqCount
	addu $s7, $s7, 1
	sw $s7, pqCount 					# increment number of nodes in queue
	
	jr $ra

# $v0 - popped node address
popQueue:
	lw $s7, pqHead
	lw $v0, 8($s7)						# load node address
	lw $s6, 4($s7)						# next node
	sw $s6, pqHead
	lw $s7, pqCount
	sub $s7, $s7, 1
	sw $s7, pqCount
	jr $ra

buildHuffmanTree:
	addu $sp, $sp, -4
	sw $ra, ($sp)
	
	la $s7, pqNodes
	sw $s7, pqFirstFreeAddress

	lw $s7, symbolsCount
	
	la $t8, nodes
	addu $t9, $zero, 0
	sub $s7, $s7, 1
	treePushLoop:
		bgt $t9, $s7, endTreePushLoop
		addu $t9, $t9, 1
		
		move $a0, $t8
		addu $sp, $sp, -12
		sw $t8, 8($sp)
		sw $t9, 4($sp)
		sw $s7, ($sp)
		jal pushQueue					# push every leaf to queue
		lw $s7, ($sp)
		lw $t9, 4($sp)
		lw $t8, 8($sp)
		addu $sp, $sp, 12
		
		addu $t8, $t8, BYTES_PER_NODE

		b treePushLoop
	endTreePushLoop:
	
	treePopLoop:
		lw $t9, pqCount
		ble $t9, 1, endTreePopLoop			# while there is more than one node in queue
		
		jal popQueue
		
		move $s6, $v0					# first popped node
		
		addu $sp, $sp, -4
		sw $s6, ($sp)
		jal popQueue
		lw $s6, ($sp)
		addu $sp, $sp, 4
		
		move $s5, $v0					# second popped node
		
		#create new internal node
		lw $s7, firstFreeNodeAddress

		li $s3, -1
		sw $s3, ($s7)					# invalid symbol
		sw $s6, 8($s7)					# left child
		sw $s5, 12($s7)					# right child
		
		lw $s3, 4($s6)
		lw $s2, 4($s5)
		addu $s3, $s3, $s2
		sw $s3, 4($s7)					# priority = left child priority + right child priority
		
		sw $s7, 16($s6)					# left child parent = new node
		sw $s7, 16($s5)					# right child parent = new node
		
		move $a0, $s7
		addu $sp, $sp, -4
		sw $s7, ($sp)
		jal pushQueue					# push internal node to queue
		lw $s7, ($sp)
		addu $sp, $sp, 4
		
		addu $s7, $s7, BYTES_PER_NODE
		sw $s7, firstFreeNodeAddress
		
		b treePopLoop
	endTreePopLoop:
	
	jal popQueue
	
	lw $s7, 4($v0)
	sw $s7, fileSize					# root stores how many symbols are in original file
	
	lw $ra, ($sp)
	addu $sp, $sp, 4
	jr $ra

# $a0 - bit, $a1 - code address, $a2 - bit index
writeCodeBit:
	addu $s7, $a1, 2
	addu $s7, $s7, $a2
	sb $a0, ($s7)
	jr $ra

# $a0 - node address
inverseCode:
	lw $t9, 20($a0)
	lbu $s7, 1($t9)						# code length
	addu $t9, $t9, 2					# start
	addu $t8, $t9, $s7	
	sub $t8, $t8, 1						# end
	inverseCodeLoop:
		bge $t9, $t8, endInverseCodeLoop
		
		lbu $s7, ($t9)
		lbu $s6, ($t8)
		sb $s6, ($t9)					# swap bytes
		sb $s7, ($t8)
		
		addu $t9, $t9, 1
		sub $t8, $t8, 1

		b inverseCodeLoop
	endInverseCodeLoop:
	jr $ra

# $a0 - node address
createCode:
	addu $sp, $sp, -4
	sw $ra, ($sp)
	
	lw $s7, symbolsCount
	bne $s7, 1, moreThanOneSymbol
	singleSymbol:
		addu $sp, $sp, -4
		sw $a0, ($sp)
		lw $a1, 20($a0)
		li $a0, 0
		li $a2, 0
		jal writeCodeBit				# if there is only one symbol, it's code is 0
		lw $a0, ($sp)
		addu $sp, $sp, 4
		
		lw $s7, 20($a0)
		li $s6, 1
		sb $s6, 1($s7)					# and code length is 1
		
		jr $ra
	moreThanOneSymbol:
	
	move $t9, $a0
	li $s7, 0						# current bit
	
	treeTraversalLoop:
		lw $s6, 16($t9)
		beq $s6, 0, endTreeTraversalLoop		# stop when node has no parent

		move $t8, $t9					# previous node address
		move $t9, $s6					# current node = parent of current node
		
		lw $s6, 8($t9)
		
		seq $s6, $t8, $s6				# left child = 0, right child = 1
		
		addu $sp, $sp, -12
		sw $t9, ($sp)
		sw $s7, 4($sp)
		sw $a0, 8($sp)
		lw $a1, 20($a0)
		move $a0, $s6
		move $a2, $s7
		jal writeCodeBit
		lw $a0, 8($sp)
		lw $s7, 4($sp)
		lw $t9, ($sp)
		addu $sp, $sp, 12
		
		addu $s7, $s7, 1

		b treeTraversalLoop
	endTreeTraversalLoop:
	
	lw $s6, 20($a0)
	sb $s7, 1($s6)						# code length
	
	jal inverseCode
	
	lw $ra, ($sp)
	addu $sp, $sp, 4
	
	jr $ra

createCodeList:
	addu $sp, $sp, -4
	sw $ra, ($sp)
	
	la $t8, nodes
	la $t7, codes
	lw $s7, symbolsCount
	sub $s7, $s7, 1
	addu $t9, $zero, 0
	createCodesLoop: 					# from 0 to (symbolsCount - 1)
		bgt $t9, $s7, endCreateCodesLoop
		
		lw $s6, ($t8)					# load symbol
		sb $s6, ($t7)					# store symbol in code list
		
		sw $t7, 20($t8)					# store code address in node
		
		addu $sp, $sp, -16
		sw $t7, 12($sp)
		sw $t8, 8($sp)
		sw $t9, 4($sp)
		sw $s7, ($sp)
		move $a0, $t8
		jal createCode
		lw $s7, ($sp)
		lw $t9, 4($sp)
		lw $t8, 8($sp)
		lw $t7, 12($sp)
		addu $sp, $sp, 16
		
		addu $t7, $t7, BYTES_PER_CODE
		addu $t8, $t8, BYTES_PER_NODE
		addu $t9, $t9, 1

		b createCodesLoop
	endCreateCodesLoop:
	
	lw $ra, ($sp)
	addu $sp, $sp, 4
	jr $ra
	
writeFileSizeToFile:
	addu $sp, $sp, -4
	sw $ra, ($sp)
	
	li $a0, 1
	la $a1, fileSize
	li $s2, 4
	jal writeToFile
	
	lw $ra, ($sp)
	addu $sp, $sp, 4
	jr $ra

writeCodeListToFile:
	addu $sp, $sp, -4
	sw $ra, ($sp)
	
	li $a0, 1
	la $a1, symbolsCount					# wrtie how many symbols are used
	li $a2, 2
	jal writeToFile
	
	la $t5, codes
	addu $t9, $zero, 0
	lw $t8, symbolsCount
	sub $t8, $t8, 1
	codeListToFileSymbolLoop: 				# from 0 to (symbolsCount - 1)
		bgt $t9, $t8, endCodeListToFileSymbolLoop
		
		lbu $s6, ($t5)					# load symbol
		sb $s6, tempByte
		
		addu $sp, $sp, -8
		sw $t8, 4($sp)
		sw $t9, ($sp)
			
		addu $sp, $sp, -4
		sw $t5, ($sp)
		li $a0, 1
		la $a1, tempByte
		li $a2, 1
		jal writeToFile					# write symbol
		lw $t5, ($sp)
		addu $sp, $sp, 4
		
		lbu $s6, 1($t5)
		sb $s6, tempByte
		
		addu $sp, $sp, -4
		sw $t5, ($sp)
		li $a0, 1
		la $a1, tempByte
		li $a2, 1
		jal writeToFile					# write length
		lw $t5, ($sp)
		addu $sp, $sp, 4
		
		lw $t9, ($sp)
		lw $t8, 4($sp)
		addu $sp, $sp, 8
		
		lbu $s6, tempByte				# length
		
		li $s5, 0					# byte to write
		li $s4, 0					# how many bits used in byte
		
		addu $t4, $t5, 2
		
		addu $t7, $zero, 0
		sub $t6, $s6, 1
		
		addu $sp, $sp, -8
		sw $t8, 4($sp)
		sw $t9, ($sp)
		
		codeListToFileCodeLoop: 			# from 0 to (length - 1)
			bgt $t7, $t6, endCodeListToFileCodeLoop
			
			lbu $s6, ($t4)				# bit of code
			
			addu $sp, $sp, -20
			sw $s4, 16($sp)
			sw $s5, 12($sp)
			sw $t6, 8($sp)
			sw $t7, 4($sp)
			sw $t4, ($sp)
			move $a0, $s5
			move $a1, $s4
			move $a2, $s6
			jal setBit				# set proper bit in byte
			lw $t4, ($sp)
			lw $t7, 4($sp)
			lw $t6, 8($sp)
			lw $s5, 12($sp)
			lw $s4, 16($sp)
			addu $sp, $sp, 20
			
			move $s5, $v0
			
			addu $s4, $s4, 1	
			addu $t4, $t4 1
			addu $t7, $t7, 1
			
			bne $s4, 8, codeListToFileCodeLoop
			
			sb $s5, tempByte
			
			addu $sp, $sp, -12
			sw $t6, 8($sp)
			sw $t7, 4($sp)
			sw $t4, ($sp)
			li $a0, 1
			la $a1, tempByte			# if whole byte is used, write it to file
			li $a2, 1
			jal writeToFile
			lw $t4, ($sp)
			lw $t7, 4($sp)
			lw $t6, 8($sp)
			addu $sp, $sp, 12
			
			li $s5, 0
			li $s4, 0

			b codeListToFileCodeLoop
		endCodeListToFileCodeLoop:
		
		lw $t9, ($sp)
		lw $t8, 4($sp)
		addu $sp, $sp, 8
		
		addu $t9, $t9, 1
		addu $t5, $t5, BYTES_PER_CODE
		
		beq $s4, 0, codeListToFileSymbolLoop
		
		sb $s5, tempByte
		
		addu $sp, $sp, -8
		sw $t8, 4($sp)
		sw $t9, ($sp)
		li $a0, 1
		la $a1, tempByte				# write rest of byte
		li $a2, 1
		jal writeToFile
		lw $t9, ($sp)
		lw $t8, 4($sp)
		addu $sp, $sp, 8

		b codeListToFileSymbolLoop
	endCodeListToFileSymbolLoop:
	
	lw $ra, ($sp)
	addu $sp, $sp, 4
	jr $ra

writeCodedDataToFile:
	addu $sp, $sp, -4
	sw $ra, ($sp)
	li $a0, 1
	la $a1, codedData
	li $a2, CHUNK_LENGTH
	jal writeToFile
	lw $ra, ($sp)
	addu $sp, $sp, 4
	jr $ra

# $a0 - value
addToCodedData:
	addu $sp, $sp, -4
	sw $ra, ($sp)
	
	move $s7, $a0
	lw $s6, codedDataFirstFreeBit
	
	li $s5, 8
	divu $s6, $s5
	mfhi $s5						# remainder
	mflo $s4						# which byte
	
	lbu $s3, codedData($s4)					# loaded byte
	
	addu $sp, $sp, -8
	sw $s6, 4($sp)
	sw $s4, ($sp)
	move $a0, $s3
	move $a1, $s5
	move $a2, $s7
	jal setBit
	lw $s4, ($sp)
	lw $s6, 4($sp)
	addu $sp, $sp, 8
	
	move $s3, $v0
	
	sb $s3, codedData($s4)
	
	addu $s6, $s6, 1
	sw $s6, codedDataFirstFreeBit
	
	li $s7, CHUNK_LENGTH_IN_BITS
	blt $s6, $s7, endCodedDataFilled
	codedDataFilled:
		jal writeCodedDataToFile			# if coded data is filled up, write it to file
		
		la $a0, codedData
		li $a1, CHUNK_LENGTH
		li $a2, 0
		jal fill					# clean buffer
		
		li $s7, 0
		sw $s7, codedDataFirstFreeBit
	endCodedDataFilled:
	
	lw $ra, ($sp)
	addu $sp, $sp, 4
	jr $ra

writeReplacedSymbols:
	addu $sp, $sp, -4
	sw $ra, ($sp)
	
	addu $t9, $zero, 0
	lw $t8, chunkCount
	sub $t8, $t8, 1
	replaceSymbolsChunkLoop: 				# for each byte in chunk
		bgt $t9, $t8, endReplaceSymbolsChunkLoop

		lbu $s7, chunk($t9)				# load symbol
		mul $s7, $s7, BYTES_PER_FREQUENCY
		lw $s7, frequencies+4($s7)			# node address
		lw $s7, 20($s7)					# code address
		
		lbu $s5, 1($s7)					# code length
		
		addu $s7, $s7, 2
		
		addu $t7, $zero, 0
		sub $t6, $s5, 1
		
		addu $sp, $sp, -12
		sw $s6, 8($sp)
		sw $t9, 4($sp)
		sw $t8, ($sp)
		
		replaceSymbolsBitLoop: 				# from 0 to (code length - 1)
			bgt $t7, $t6, endReplaceSymbolsBitLoop

			lbu $s4, ($s7)				# load bit
			
			addu $sp, $sp, -12
			sw $s7, 8($sp)
			sw $t6, 4($sp)
			sw $t7, ($sp)
			move $a0, $s4
			jal addToCodedData
			lw $t7, ($sp)
			lw $t6, 4($sp)
			lw $s7, 8($sp)
			addu $sp, $sp, 12
			
			addu $s7, $s7, 1
			addu $t7, $t7, 1

			b replaceSymbolsBitLoop
		endReplaceSymbolsBitLoop:
		
		lw $t8, ($sp)
		lw $t9, 4($sp)
		lw $s6, 8($sp)
		addu, $sp, $sp, 12
		
		addu $t9, $t9, 1

		b replaceSymbolsChunkLoop
	endReplaceSymbolsChunkLoop:
	
	lw $ra, ($sp)
	addu $sp, $sp, 4
	jr $ra

writeRestOfCodedData:
	addu $sp, $sp, -4
	sw $ra, ($sp)
	
	lw $s7, codedDataFirstFreeBit
	li $s6, 8
	
	divu $s7, $s6
	mfhi $s7						# remainder
	mflo $s6
	
	sne $s7, $s7, 0
	addu $s6, $s6, $s7					# needed bytes
	
	li $a0, 1
	la $a1, codedData
	move $a2, $s6
	jal writeToFile
	
	lw $ra, ($sp)
	addu $sp, $sp, 4
	jr $ra

end:
	li $v0, 10
	syscall
