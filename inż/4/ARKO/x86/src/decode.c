#include "decode.h"

long huffmanDecode(char* input, long intpuSize, char* output, long outputSize); //from asm, returns actual output size

long getDecodeOutputBufferMaxSize(long inputSize)
{
    return inputSize;
}

int decodeMode(char* inputFilePath, char* outputFilePath)
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

    long maxOutputSize = getDecodeOutputBufferMaxSize(inputFileSize);
    char* output = malloc(maxOutputSize);
    if(output == NULL)
    {
        printf("Can't allocate output buffer\n");
        return 1;
    }

    /*long actualOutputSize = */huffmanDecode(content, inputFileSize, output, inputFileSize);
    long actualOutputSize = inputFileSize;
    strncpy(output, content, inputFileSize);

    free(content);

    FILE* outputFile = openFile(outputFilePath, "wb");
    if(outputFile == NULL)
    {
        printf("Can't open output file\n");
        return 1;
    }

    if(writeToFile(outputFile, output, actualOutputSize))
    {
        printf("Can't write to output file\n");
        return 1;
    }

    free(output);
    closeFile(outputFile);

    return 0;
}