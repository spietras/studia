#ifndef PROJEKT6_DEFINITIONS_H
#define PROJEKT6_DEFINITIONS_H

#include <stdint.h>

typedef uint32_t sizeType;

#define BLOCK_DATA_SIZE 4
#define FILENAME_MAX_LENGTH 31

typedef struct Superblock
{
    sizeType totalSize;
    sizeType dataSize;
} Superblock;

typedef struct iNode
{
    sizeType fileSize;
    sizeType firstBlockIndex;
    char fileName[FILENAME_MAX_LENGTH + 1];
} iNode;

typedef struct Block
{
    char data[BLOCK_DATA_SIZE];
    sizeType nextBlockIndex;
} Block;

#endif //PROJEKT6_DEFINITIONS_H
