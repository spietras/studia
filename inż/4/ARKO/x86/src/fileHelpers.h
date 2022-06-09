#ifndef FILEHELPERS_H
#define FILEHELPERS_H

#include <stdio.h>
#include <stdlib.h>

FILE* openFile(const char* const name, const char* const mode);
void closeFile(FILE* const file);
long getFileSize(FILE* const file);
void* getContentOfFile(FILE* const file, const long fileSize);
int writeToFile(FILE* const file, const void* const buffer, const long bufferSize);
int readFromFile(FILE* const file, void* const buffer, const long readSize);
void rewindFile(FILE* const file);

#endif