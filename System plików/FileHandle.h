#ifndef PROJEKT6_FILEHANDLE_H
#define PROJEKT6_FILEHANDLE_H

#include <stdio.h>
#include "definitions.h"

int readFileWithStream(void *destination, sizeType offset, sizeType blockSize, sizeType blockCount, FILE *fileStream);

int
writeFileWithStream(const void *source, sizeType offset, sizeType blockSize, sizeType blockCount,
                    FILE *fileStream);

int zeroOutRangeInFile(sizeType begin, sizeType end, FILE *fileStream);

int zeroOutFile(sizeType count, FILE *fileStream);

sizeType getFileSize(FILE *fileStream);

int isNextCharacterEOF(FILE *fileStream);

#endif //PROJEKT6_FILEHANDLE_H
