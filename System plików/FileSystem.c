#include <stdlib.h>
#include <string.h>
#include "FileSystem.h"

static sizeType roundUpToNearestMultiple(sizeType numToRound, sizeType multiple)
{
    if (multiple == 0)
        return numToRound;

    int remainder = numToRound % multiple;
    if (remainder == 0)
        return numToRound;

    return numToRound + multiple - remainder;
}

static sizeType getMaxBlocksCount(FileSystem *fs)
{
    return (fs->superBlock->totalSize - sizeof(Superblock)) /
           (sizeof(char) + sizeof(iNode) + sizeof(char) + sizeof(Block)); //blocks count == inodes count
}

static sizeType getMaxINodeCount(FileSystem *fs)
{
    return getMaxBlocksCount(fs);
}

static sizeType getSuperBlockOffset()
{
    return 0;
}

static sizeType getINodesBitmapOffset()
{
    return getSuperBlockOffset() + sizeof(Superblock);
}

static sizeType getINodesSpaceOffset(FileSystem *fs)
{
    return getINodesBitmapOffset() + getMaxINodeCount(fs) * sizeof(char);
}

static sizeType getBlocksBitmapOffset(FileSystem *fs)
{
    return getINodesSpaceOffset(fs) + getMaxINodeCount(fs) * sizeof(iNode);
}

static sizeType getBlocksSpaceOffset(FileSystem *fs)
{
    return getBlocksBitmapOffset(fs) + getMaxBlocksCount(fs) * sizeof(char);
}

static sizeType getINodeOffset(FileSystem *fs, sizeType index)
{
    return getINodesSpaceOffset(fs) + index * sizeof(iNode);
}

static sizeType getBlockOffset(FileSystem *fs, sizeType index)
{
    return getBlocksSpaceOffset(fs) + index * sizeof(Block);
}

static int readSuperBlock(FileSystem *fs)
{
    return readFileWithStream(fs->superBlock, getSuperBlockOffset(), sizeof(Superblock), 1, fs->fileStream);
}

static int readINodesBitmap(FileSystem *fs)
{
    return readFileWithStream(fs->iNodesBitmap, getINodesBitmapOffset(), sizeof(char), getMaxINodeCount(fs),
                              fs->fileStream);
}

static int readBlocksBitmap(FileSystem *fs)
{
    return readFileWithStream(fs->blocksBitmap, getBlocksBitmapOffset(fs), sizeof(char), getMaxBlocksCount(fs),
                              fs->fileStream);
}

static int readINode(FileSystem *fs, iNode *n, sizeType index)
{
    int r = readFileWithStream(n, getINodeOffset(fs, index), sizeof(iNode), 1, fs->fileStream);
    n->fileName[FILENAME_MAX_LENGTH] = '\0'; //null-terminate the string just in case
    return r;
}

static int readBlock(FileSystem *fs, Block *b, sizeType index)
{
    return readFileWithStream(b, getBlockOffset(fs, index), sizeof(Block), 1, fs->fileStream);
}

static sizeType findFirstFreeINode(FileSystem *fs)
{
    for (sizeType i = 0; i < getMaxINodeCount(fs); i++)
        if (fs->iNodesBitmap[i] == 0) return i;

    return getMaxINodeCount(fs);
}

static sizeType findFirstFreeBlock(FileSystem *fs)
{
    for (sizeType i = 0; i < getMaxBlocksCount(fs); i++)
        if (fs->blocksBitmap[i] == 0) return i;

    return getMaxBlocksCount(fs);
}

static sizeType getMaxDataSize(FileSystem *fs)
{
    return getMaxBlocksCount(fs) * BLOCK_DATA_SIZE;
}

static sizeType getAvailableDataSize(FileSystem *fs)
{
    return getMaxDataSize(fs) - fs->superBlock->dataSize;
}

static sizeType getMaxBlockCountFittingDataSize(sizeType dataSize)
{
    dataSize = roundUpToNearestMultiple(dataSize, BLOCK_DATA_SIZE);
    return dataSize / BLOCK_DATA_SIZE;
}

static int removeINode(FileSystem *fs, iNode *n, sizeType index)
{
    int r = 0;
    sizeType currentBlockIndex = n->firstBlockIndex;
    Block *b = (Block *) calloc(1, sizeof(Block));
    if (!b)
        return 3;

    while (currentBlockIndex != getMaxBlocksCount(fs)) //repeat untill all blocks were removed
    {
        r = readBlock(fs, b, currentBlockIndex); //we need to get the next block
        if (r)
            return r;
        sizeType blockOffset = getBlockOffset(fs, currentBlockIndex);

        r = zeroOutRangeInFile(blockOffset, blockOffset + sizeof(Block),
                               fs->fileStream); //zero out data contents in fs file
        if (r)
            return r;
        fs->blocksBitmap[currentBlockIndex] = 0;

        currentBlockIndex = b->nextBlockIndex;
    }

    sizeType iNodeOffset = getINodeOffset(fs, index);
    r = zeroOutRangeInFile(iNodeOffset, iNodeOffset + sizeof(iNode), fs->fileStream); //zero out inode in fs file
    if (r)
        return r;
    fs->iNodesBitmap[index] = 0;
    fs->superBlock->dataSize -= roundUpToNearestMultiple(n->fileSize, BLOCK_DATA_SIZE);
    free(b);

    return 0;
}

static int readSourceToINode(FileSystem *fs, iNode *n, FILE *sourceFileStream)
{
    sizeType currentBlockIndex = n->firstBlockIndex;

    while (currentBlockIndex != getMaxBlocksCount(fs))
    {
        Block *b = (Block *) calloc(1, sizeof(Block));
        if (!b)
            return 3;
        fs->blocksBitmap[currentBlockIndex] = 1;
        size_t readBytes = fread(b->data, sizeof(char), BLOCK_DATA_SIZE, sourceFileStream);
        if (ferror(sourceFileStream))
        {
            free(b);
            return 1;
        }
        if (!isNextCharacterEOF(sourceFileStream))
            b->nextBlockIndex = findFirstFreeBlock(fs); //if there is still data to read, find next free block
        else
            b->nextBlockIndex = getMaxBlocksCount(fs); //otherwise set next to end

        int r = writeFileWithStream(b, getBlockOffset(fs, currentBlockIndex), sizeof(Block), 1,
                                    fs->fileStream); //write block to fs file
        if (r)
        {
            free(b);
            return r;
        }
        currentBlockIndex = b->nextBlockIndex;
        free(b);

        n->fileSize += readBytes;
    }

    return 0;
}

static void clearPointers(FileSystem *fs)
{
    free(fs->iNodesBitmap);
    free(fs->blocksBitmap);
    free(fs->superBlock);
    if (fs->fileStream) fclose(fs->fileStream);
    free(fs);
}

static sizeType findINodeByName(FileSystem *fs, const char *name)
{
    int r = 0;
    iNode *n = (iNode *) calloc(1, sizeof(iNode));
    if (!n)
        return getMaxINodeCount(fs);

    for (sizeType i = 0; i < getMaxINodeCount(fs); i++)
    {
        if (fs->iNodesBitmap[i] == 1) //if iNode exists
        {
            r = readINode(fs, n, i);
            if (r)
            {
                free(n);
                return getMaxINodeCount(fs);
            }

            if (!strcmp(n->fileName, name)) //if file name is equal, iNode found
            {
                free(n);
                return i;
            }
        }
    }

    //not found
    free(n);
    return getMaxINodeCount(fs);
}


/* ------------------------------ INTERFACE ------------------------------ */






FileSystem *createFileSystem(const char *name, sizeType dataSize)
{
    FileSystem *fs = (FileSystem *) calloc(1, sizeof(FileSystem));
    if (!fs)
        return NULL;
    fs->fileStream = fopen(name, "w+b"); //create and open for read/write
    if (!fs->fileStream)
    {
        clearPointers(fs);
        return NULL;
    }
    fs->superBlock = (Superblock *) calloc(1, sizeof(Superblock));
    if (!fs->superBlock)
    {
        clearPointers(fs);
        return NULL;
    }
    fs->superBlock->dataSize = 0; // empty at first
    sizeType maxBlocksCount = getMaxBlockCountFittingDataSize(dataSize), maxINodesCount = maxBlocksCount;
    fs->superBlock->totalSize = sizeof(Superblock) // superblock space
                                + maxINodesCount * sizeof(char) // iNodes bitmap space
                                + maxINodesCount * sizeof(iNode) // iNodes space
                                + maxBlocksCount * sizeof(char) // blocks bitmap space
                                + maxBlocksCount * sizeof(Block); // blocks space
    fs->iNodesBitmap = (char *) calloc(maxINodesCount, sizeof(char));
    if (!fs->iNodesBitmap)
    {
        clearPointers(fs);
        return NULL;
    }
    fs->blocksBitmap = (char *) calloc(maxBlocksCount, sizeof(char));
    if (!fs->blocksBitmap)
    {
        clearPointers(fs);
        return NULL;
    }
    if (zeroOutFile(fs->superBlock->totalSize, fs->fileStream)) //fill file with zeros
    {
        clearPointers(fs);
        return NULL;
    }
    if (writeFileWithStream(fs->superBlock, 0, sizeof(Superblock), 1, fs->fileStream)) //write superblock
    {
        clearPointers(fs);
        return NULL;
    }

    return fs;
}

FileSystem *loadFileSystem(const char *name)
{
    FileSystem *fs = (FileSystem *) calloc(1, sizeof(FileSystem));
    if (!fs)
        return NULL;
    fs->fileStream = fopen(name, "r+b"); //open for read and write
    if (!fs->fileStream)
    {
        clearPointers(fs);
        return NULL;
    }
    fs->superBlock = (Superblock *) calloc(1, sizeof(Superblock));
    if (!fs->superBlock)
    {
        clearPointers(fs);
        return NULL;
    }
    if (readSuperBlock(fs))
    {
        clearPointers(fs);
        return NULL;
    }
    fs->iNodesBitmap = (char *) calloc(getMaxINodeCount(fs), sizeof(char));
    if (!fs->iNodesBitmap)
    {
        clearPointers(fs);
        return NULL;
    }
    if (readINodesBitmap(fs))
    {
        clearPointers(fs);
        return NULL;
    }
    fs->blocksBitmap = (char *) calloc(getMaxBlocksCount(fs), sizeof(char));
    if (!fs->blocksBitmap)
    {
        clearPointers(fs);
        return NULL;
    }
    if (readBlocksBitmap(fs))
    {
        clearPointers(fs);
        return NULL;
    }

    return fs;
}

int copyFromOutside(FileSystem *fs, const char *sourceFileName, const char *destFileName)
{
    int r = 0;
    FILE *sourceFileStream = fopen(sourceFileName, "rb"); //open source for reading
    if (!sourceFileStream)
        return 1;
    sizeType sourceFileSize = getFileSize(sourceFileStream);
    if (sourceFileSize > getAvailableDataSize(fs)) //if source size is greater then available size we can't copy
    {
        fclose(sourceFileStream);
        return 4;
    }

    r = removeFile(fs, destFileName); //remove file with the same name if it already exists
    if (r)
    {
        fclose(sourceFileStream);
        return r;
    }

    sizeType iNodeIndex = findFirstFreeINode(fs);
    if (iNodeIndex == getMaxINodeCount(fs)) //if there are no free nodes we can't copy
    {
        fclose(sourceFileStream);
        return 4;
    }

    iNode *n = (iNode *) calloc(1, sizeof(iNode));
    if (!n)
    {
        fclose(sourceFileStream);
        return 3;
    }
    strcpy(n->fileName, destFileName);
    n->fileName[FILENAME_MAX_LENGTH] = '\0';
    n->firstBlockIndex = findFirstFreeBlock(fs); //assign first free block to node

    r = readSourceToINode(fs, n, sourceFileStream);
    if (r)
    {
        free(n);
        fclose(sourceFileStream);
        return r;
    }

    r = writeFileWithStream(n, getINodeOffset(fs, iNodeIndex), sizeof(iNode), 1, fs->fileStream); //write node
    if (r)
    {
        free(n);
        fclose(sourceFileStream);
        return r;
    }
    fs->superBlock->dataSize += roundUpToNearestMultiple(n->fileSize, BLOCK_DATA_SIZE);
    fs->iNodesBitmap[iNodeIndex] = 1;
    free(n);
    fclose(sourceFileStream);

    return 0;
}

int copyToOutside(FileSystem *fs, const char *sourceFileName, const char *destFileName)
{
    int r = 0;
    FILE *sourceFileStream = fopen(destFileName, "wb");
    if (!sourceFileStream)
        return 1;

    sizeType foundIndex = findINodeByName(fs, sourceFileName);
    if (foundIndex == getMaxINodeCount(fs)) //file not found
        return 5;

    iNode *n = (iNode *) calloc(1, sizeof(iNode));
    if (!n)
    {
        fclose(sourceFileStream);
        return 3;
    }

    r = readINode(fs, n, foundIndex);
    sizeType currentBlockIndex = n->firstBlockIndex, bytesLeft = n->fileSize;

    while (currentBlockIndex != getMaxBlocksCount(fs)) //write until all blocks were used
    {
        Block *b = (Block *) calloc(1, sizeof(Block));
        if (!b)
        {
            free(n);
            fclose(sourceFileStream);
            return 3;
        }

        r = readBlock(fs, b, currentBlockIndex);
        if (r)
        {
            free(b);
            free(n);
            fclose(sourceFileStream);
            return 3;
        }

        for (sizeType i = 0; i < BLOCK_DATA_SIZE && bytesLeft > 0; i++, bytesLeft--)
        {
            fwrite(&b->data[i], sizeof(char), 1, sourceFileStream);
            if (ferror(sourceFileStream))
            {
                free(b);
                free(n);
                fclose(sourceFileStream);
                return 1;
            }
        }

        currentBlockIndex = b->nextBlockIndex;
        free(b);
    }

    free(n);
    fclose(sourceFileStream);
    return 0;
}

void displayFiles(FileSystem *fs)
{
    iNode *n = (iNode *) calloc(1, sizeof(iNode));

    printf("\nTotal data size (padded): %d\n", fs->superBlock->dataSize);

    for (sizeType i = 0; i < getMaxINodeCount(fs); i++)
    {
        if (fs->iNodesBitmap[i] == 1)
        {
            readINode(fs, n, i);
            printf("File: %s (%d bytes, %d bytes padded)\n", n->fileName, n->fileSize,
                   roundUpToNearestMultiple(n->fileSize, BLOCK_DATA_SIZE));
        }
    }

    free(n);
}

int removeFile(FileSystem *fs, const char *fileName)
{
    int r = 0;
    sizeType foundIndex = findINodeByName(fs, fileName);
    if (foundIndex == getMaxINodeCount(fs)) //file not found
        return 0;

    iNode *n = (iNode *) calloc(1, sizeof(iNode));
    if (!n)
        return 3;

    r = readINode(fs, n, foundIndex);
    if (r)
    {
        free(n);
        return r;
    }

    r = removeINode(fs, n, foundIndex);
    if (r)
    {
        free(n);
        return r;
    }

    free(n);
    return 0;
}


void displayDataBitmap(FileSystem *fs)
{
    printf("\niNodes bitmap:\n");
    for (sizeType i = 0; i < getMaxINodeCount(fs); i++)
        printf("%d", fs->iNodesBitmap[i]);

    printf("\nBlocks bitmap:\n");
    for (sizeType i = 0; i < getMaxBlocksCount(fs); i++)
        printf("%d", fs->blocksBitmap[i]);

    printf("\n");
}

int closeFileSystem(FileSystem *fs)
{
    int r = 0;
    r = writeFileWithStream(fs->superBlock, getSuperBlockOffset(), sizeof(Superblock), 1,
                        fs->fileStream); //update superblock
    if (r)
        return r;
    r = writeFileWithStream(fs->iNodesBitmap, getINodesBitmapOffset(), sizeof(char), getMaxINodeCount(fs),
                        fs->fileStream); //update inodes bitmap
    if (r)
        return r;
    r = writeFileWithStream(fs->blocksBitmap, getBlocksBitmapOffset(fs), sizeof(char), getMaxBlocksCount(fs),
                        fs->fileStream); //update blocks bitmap
    if (r)
        return r;

    clearPointers(fs);
    return 0;
}

int deleteFileSystem(const char *name)
{
    FileSystem *fs = loadFileSystem(name);
    if (fs == NULL)
        return 1; //check if it is fileSystem, not some other file
    int r = closeFileSystem(fs);
    if (r)
        return r;
    if (remove(name))
        return 1;

    return 0;
}