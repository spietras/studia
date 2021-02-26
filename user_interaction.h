#ifndef USERINTERACTION_H_INCLUDED
#define USERINTERACTION_H_INCLUDED

#include "definitions.h"

void ViewStructureTree(const Directory*);
void ViewFileData(const TextFile*);
void ViewFileDataByPath(Directory*, const char*);
void ViewStructureTreeByPath(Directory*, const char*);
int ShowMenu(Volume*);
char* GetVolumeName();
char* GetDirectoryName();
char* GetFileName();
char* GetData();
char* GetFilePath(const char*);
char* GetDirectoryPath(const char*);

#endif