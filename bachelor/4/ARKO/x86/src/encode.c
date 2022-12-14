#include "encode.h"
/*
*   ENCODED FILE STRUCTURE:
*   original file size (8 bytes)
*   symbols count (1 byte)
*   symbol-frequency table: <symbol (1 byte), frequency (8 bytes)> for each symbol
*   encoded data (x bits)
*/

long huffmanEncode(char* input, long intpuSize, char* output); //from asm, returns actual size of output buffer

void printBuffer(const char* const buffer, const long size)
{
    fwrite(buffer, 1, size, stdout);
    printf("\n");
}

long getEncodeOutputBufferMaxSize(long inputSize)
{
    return MAXHEADERSIZE + inputSize;
}

int encodeMode(char* inputFilePath, char* outputFilePath)
{
    FILE* inputFile = openFile(inputFilePath, "rb");
    if(inputFile == NULL)
    {
        printf("Can't open file\n");
        return 1;
    }

    long inputFileSize = getFileSize(inputFile);
    if(inputFileSize == -1)
    {
        printf("Can't read file size\n");
        return 1;
    }
    char* content = getContentOfFile(inputFile, inputFileSize);
    if(content == NULL)
    {
        printf("Can't get entire file content\n");
        return 1;
    }

    closeFile(inputFile);

    long maxOutputSize = getEncodeOutputBufferMaxSize(inputFileSize);
    char* output = calloc(maxOutputSize, sizeof(char));
    if(output == NULL)
    {
        printf("Can't allocate output buffer\n");
        return 1;
    }

    long actualOutputSize = huffmanEncode(content, inputFileSize, output);

    free(content);

    FILE* outputFile = openFile(outputFilePath, "wb");
    if(outputFile == NULL)
    {
        printf("Can't open output file\n");
        return 1;
    }

    if(writeToFile(outputFile, &inputFileSize, sizeof(long))) //write original file size
    {
        printf("Can't write to output file\n");
        return 1;
    }

    if(writeToFile(outputFile, output, actualOutputSize)) //write content
    {
        printf("Can't write to output file\n");
        return 1;
    }

    free(output);
    closeFile(outputFile);

    return 0;
}