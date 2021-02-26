#include <stdlib.h>
#include "FileHandle.h"

int readFileWithStream(void *destination, const sizeType offset, const sizeType blockSize, const sizeType blockCount,
                       FILE *fileStream)
{
    const long int currentPos = ftell(fileStream); //remember current position
    if (currentPos == -1L)
        return 2;
    if (fseek(fileStream, offset, SEEK_SET))
        return 2;
    fread(destination, blockSize, blockCount, fileStream);
    if (ferror(fileStream))
        return 1;
    if (fseek(fileStream, currentPos, SEEK_SET)) //return to previous position
        return 2;

    return 0;
}

int writeFileWithStream(const void *source, const sizeType offset, const sizeType blockSize, const sizeType blockCount,
                        FILE *fileStream)
{
    const long int currentPos = ftell(fileStream); //remember current position
    if (currentPos == -1L)
        return 2;
    if (fseek(fileStream, offset, SEEK_SET))
        return 2;
    fwrite(source, blockSize, blockCount, fileStream);
    if (ferror(fileStream))
        return 1;
    if (fseek(fileStream, currentPos, SEEK_SET)) //return to previous position
        return 2;

    return 0;
}

int zeroOutRangeInFile(const sizeType begin, const sizeType end, FILE *fileStream)
{
    const long int currentPos = ftell(fileStream);
    if (currentPos == -1L)
        return 2;
    char *buffer = (char *) calloc(end - begin, sizeof(char));
    if (!buffer)
        return 3;
    if (fseek(fileStream, begin, SEEK_SET))
        return 2;
    fwrite(buffer, sizeof(char), end - begin, fileStream);
    if (ferror(fileStream))
        return 1;
    if (fseek(fileStream, currentPos, SEEK_SET))
        return 2;
    free(buffer);

    return 0;
}

int zeroOutFile(const sizeType count, FILE *fileStream)
{
    return zeroOutRangeInFile(0, count, fileStream);
}

sizeType getFileSize(FILE *fileStream)
{
    const long int currentPos = ftell(fileStream);
    fseek(fileStream, 0, SEEK_END);
    const sizeType fileSize = (sizeType) ftell(fileStream);
    fseek(fileStream, currentPos, SEEK_SET);
    return fileSize;
}

int isNextCharacterEOF(FILE *fileStream)
{
    int c = fgetc(fileStream);
    ungetc(c, fileStream);

    return c == EOF;
}
