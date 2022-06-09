#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fileHelpers.h"
#include "encode.h"
#include "decode.h"
 
 // usage: ./executable <encode|decode> <input path> <output path>
int main(int argc, char **argv)
{
    if(argc != 4)
    {
        printf("Wrong arguments\n");
        return 1;
    }

    char* mode = argv[1];
    char* inputFilePath = argv[2];
    char* outputFilePath = argv[3];

    if(strncmp(mode,"encode",6) == 0)
        return encodeMode(inputFilePath, outputFilePath);
    else if(strncmp(mode,"decode",6) == 0)
        return decodeMode(inputFilePath, outputFilePath);
    
    printf("Wrong mode\n");
    return 1;
}