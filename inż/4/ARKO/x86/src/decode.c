#include "decode.h"
/*
*   ENCODED FILE STRUCTURE:
*   original file size (8 bytes)
*   symbols count (1 byte)
*   symbol-frequency table: <symbol (1 byte), frequency (8 bytes)> for each symbol
*   encoded data (x bits)
*/

void huffmanDecode(char* input, char* output); //from asm

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

    long originalSize = 0;
    if(readFromFile(inputFile, &originalSize, sizeof(long)))
    {
        printf("Can't get original file size\n");
        return 1;
    }
    rewindFile(inputFile);

    char* content = getContentOfFile(inputFile, inputFileSize);
    if(content == NULL)
    {
        printf("Can't get entire file content\n");
        return 1;
    }

    closeFile(inputFile);

    char* output = malloc(originalSize);
    if(output == NULL)
    {
        printf("Can't allocate output buffer\n");
        return 1;
    }

    huffmanDecode(content, output);

    free(content);

    FILE* outputFile = openFile(outputFilePath, "wb");
    if(outputFile == NULL)
    {
        printf("Can't open output file\n");
        return 1;
    }

    if(writeToFile(outputFile, output, originalSize))
    {
        printf("Can't write to output file\n");
        return 1;
    }

    free(output);
    closeFile(outputFile);

    return 0;
}