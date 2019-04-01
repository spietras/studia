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

	# one level for loop macro
	.macro for (%regIterator, %from, %to, %bodyMacroName)
	add %regIterator, $zero, %from
	Loop:
	jal %bodyMacroName
	add %regIterator, %regIterator, 1
	ble %regIterator, %to, Loop
	.end_macro
	
	masks:			.byte	128, 64, 32, 16, 8, 4, 2, 1	# 10000000, 01000000, 00100000, 00010000, 00001000, 00000100, 00000010, 00000001
	
	chunk:			.byte	0:CHUNK_LENGTH			# input buffer
	
	frequencies:		.word	0:MAX_SYMBOLS			# symbol frequency list
	
	nodes:			.byte	0:NODES_LENGTH			# nodes list for Huffman Tree
	nodesCount:		.word	0				# number of nodes
	
	pqNodes:		.byte	0:PQNODES_LENGTH		# nodes for priority queue
	pqHead:			.word	0				# index of head of priority queue
	pqUsed:			.word	0				# first free index in queue array
	pqCount:		.word	0				# number of nodes currently in queue
	
	codes:			.word	0:CODES_LENGTH			# symbol to code list
	symbolsCount:		.byte	0				# how many symbols are used
	
	codedData:		.byte	0:CHUNK_LENGTH			# output buffer
	codedDataFirstFreeBit:	.word	0				# first free bit of output buffer
	trailingBitsCount:	.byte	0				# how many trailing bits were added
	
	fileDescriptors:	.word	FILES
	fileEnded:		.byte	FILES
	
.text

main:
	
	j end

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

# $v0 = $a0 % $a1
modulo:
	divu $a0, $a1
	mfhi $v0
	jr $ra

# fills buffer with value (byte)
# $a0 - buffer address, $a1 - buffer length in bytes, $a2 - value (byte)
fill:
	add $t9, $zero, 1
	fillLoop:
	
	sb $a2, ($a0)
	add $a0, $a0, 1
	
	add $t9, $t9, 1
	ble $t9, $a1, fillLoop
	
	jr $ra
	
# $a0 - byte (value), $a1 - bit index (from left), $a2 - value
# $v0 - changed byte
setBit:
	lb $s7, masks($a1) # n-th mask
	not $s7, $s7 # inverse mask
	and $a0, $a0, $s7 # clear bit in byte
	li $s7, 7
	sub $s7, $s7, $a1
	sllv $a2, $a2, $s7 # shift left (7-n) times
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

	jr $ra

createNodeList:

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