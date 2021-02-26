#ifndef PROJEKT6_FILESYSTEM_H
#define PROJEKT6_FILESYSTEM_H

#include <stdio.h>
#include "definitions.h"
#include "FileHandle.h"

typedef struct FileSystem
{
    FILE *fileStream;
    Superblock *superBlock;
    char *iNodesBitmap;
    char *blocksBitmap;
} FileSystem;

FileSystem *createFileSystem(const char *name, sizeType dataSize);

FileSystem *loadFileSystem(const char *name);

int copyFromOutside(FileSystem *fs, const char *sourceFileName, const char *destFileName);

int copyToOutside(FileSystem *fs, const char *sourceFileName, const char *destFileName);

void displayFiles(FileSystem *fs);

int removeFile(FileSystem *fs, const char *fileName);

void displayDataBitmap(FileSystem *fs);

int closeFileSystem(FileSystem *fs);

int deleteFileSystem(const char *name);

#endif //PROJEKT6_FILESYSTEM_H
