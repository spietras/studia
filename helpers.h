#ifndef HELPERS_H_INCLUDED
#define HELPERS_H_INCLUDED

#include "definitions.h"

Directory* FindLastInDirectoryList(Directory*);
TextFile* FindLastInFileList(TextFile*);
Cluster* FindLastInClusterList(Cluster*);
Cluster* FindEmptyCluster(Volume*);
int IsValidFilePath(const char*);
int IsValidDirectoryPath(const char*);
int IsDestinationDirectoryPathBelowInHierarchy(const char*, const char*);
int IsFile(const char*);
int IsDirectory(const char*);
TextFile* FindFileByPath(Directory*, const char*);
Directory* FindDirectoryByPath(Directory*, const char*);
Directory* FindDirectoryByNameAndParent(const Directory*, const char*);
TextFile* FindFileByNameAndParent(const Directory*, const char*);
Cluster* CopyClusterList(const Cluster*);

#endif