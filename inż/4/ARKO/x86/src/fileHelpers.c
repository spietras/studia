#include "fileHelpers.h"

FILE* openFile(const char* const name, const char* const mode)
{
    return fopen(name, mode);
}

void closeFile(FILE* const file)
{
    fclose(file);
}

long getFileSize(FILE* const file)
{
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);
    return size;
}

char* getContentOfFile(FILE* const file, const long fileSize)
{
    char* buffer = malloc(fileSize);
    if(buffer == NULL)
        return NULL;

    fseek(file, 0, SEEK_SET);
    size_t read = fread(buffer, 1, fileSize, file);
    if(read != fileSize)
        return NULL;

    fseek(file, 0, SEEK_SET);

    return buffer;
}

int writeToFile(FILE* const file, const char* const buffer, const long bufferSize)
{
    size_t written = fwrite(buffer, 1, bufferSize, file);

    if(written != bufferSize)
        return -1;

    return 0;
}