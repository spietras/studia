#ifndef FILEHELPERS_H
#define FILEHELPERS_H

#include <stdio.h>
#include <stdlib.h>

FILE* openFile(const char* const name, const char* const mode);
void closeFile(FILE* const file);
long getFileSize(FILE* const file);
char* getContentOfFile(FILE* const file, const long fileSize);
int writeToFile(FILE* const file, const char* const buffer, const long bufferSize);

#endif