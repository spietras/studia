#ifndef ENTRIES_OPERATIONS_H_INCLUDED
#define ENTRIES_OPERATIONS_H_INCLUDED

#include "definitions.h"

TextFile* CreateEmptyFile(Volume*, const char*, const char*);
TextFile* AddFile(Volume*, Directory*, const char*, const char*);
TextFile* AddFileByPath(Volume*, const char*);
int AddDataToFile(Volume*, TextFile*, const char*);
int AddDataToFileByPath(Volume*, const char*, const char*);
Directory* CreateEmptyDirectory(Volume*, const char*);
Directory* AddDirectory(Volume*, Directory*, const char*);
Directory* AddDirectoryByPath(Volume*, const char*);


int ClearData(Volume*, Cluster*);
int DeleteFile(Volume*, TextFile*);
int DeleteFileByPath(Volume*, const char*);
int DeleteDirectoryTree(Volume*, Directory*);
int DeleteDirectoryByPath(Volume*, const char*);


int MoveFileToDirectory(Volume*, TextFile*, Directory*);
int MoveFileToDirectoryByPaths(Volume*, const char*, const char*);
int MoveDirectoryToDirectory(Volume*, Directory*, Directory*);
int MoveDirectoryToDirectoryByPaths(Volume*, const char*, const char*);


int CopyFileToDirectory(Volume*, TextFile*, Directory*);
int CopyFileToDirectoryByPaths(Volume*, const char*, const char*);
int CopyDirectoryToDirectory(Volume*, Directory*, Directory*);
int CopyDirectoryToDirectoryByPaths(Volume*, const char*, const char*);


int RenameFile(TextFile*, const char*);
int RenameFileByPath(Directory*, const char*, const char*);
int RenameDirectory(Directory*, const char*);
int RenameDirectoryByPath(Directory*, const char*, const char*);

#endif