#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum EntryType { FileType, FolderType } EntryType;

typedef struct Entry Entry;
typedef struct FileStruct FileStruct;
typedef struct FolderStruct FolderStruct;
typedef struct Cluster Cluster;
typedef struct FAT FAT;

struct Entry
{
	EntryType type;
	void* pointer;
};

struct FileStruct
{
	char name[10];
	char extension[10];
	Cluster* firstCluster;
};

struct FolderStruct
{
	char name[10];
	Entry** entries;
	int entriesAmount;
};

struct Cluster
{
	int occupied;
	unsigned char data[512];
	Cluster* next;
};

struct FAT
{
	Cluster** table;
	int size;
};

int Initialize(FolderStruct*, FAT*);
int AddFile(FolderStruct*, char*, FAT*);
int CheckFileName(char*);
int AddFolder(FolderStruct*, char*, FAT*);
int CheckFolderName(char*);
Cluster * FindEmptyCluster(FAT*);
void ViewDirectories(FolderStruct*, int);

int main()
{
    FolderStruct root;
    root.entriesAmount;
    FAT fat;

    Initialize(&root, &fat);

    ViewDirectories(&root, 0);

    return 0;
}

int Initialize(FolderStruct* root, FAT* fat)
{
	root->entriesAmount = 0;

	int i, n = 10;
	fat->table = (Cluster**)malloc(n*sizeof(Cluster*));

	for(i = 0; i < n; i++)
	{
		fat->table[i] = (Cluster*)malloc(sizeof(Cluster));
		fat->table[i]->occupied = 0;
		fat->table[i]->next = NULL;
	}

	strcpy(root->name, "root");
	AddFile(root, "gitara.txt", fat);
	AddFile(root, "abcd.jpg", fat);
	AddFolder(root, "folder1", fat);
	FolderStruct* f = (FolderStruct*)root->entries[root->entriesAmount-1]->pointer;
	AddFile(f, "makar.gif", fat);
	AddFile(f, "gowno.rar", fat);
	AddFile(root, "siema.elo", fat);

	return 1;
}

int AddFile(FolderStruct* subdirectory, char* name, FAT* fat)
{
	if(!CheckFileName(name))
	{
		return 0;
	}

	Cluster* c = FindEmptyCluster(fat);
	if(c == NULL)
	{
		return 0;
	}

	int n = subdirectory->entriesAmount;
	Entry** tEntries;

	if(n == 0)
	{
		tEntries = (Entry**)malloc(sizeof(Entry*));
	}
	else
	{
		tEntries = (Entry**)realloc(subdirectory->entries, (n+1)*sizeof(Entry*));
	}

	if(tEntries == NULL)
	{
		return 0;
	}

	subdirectory->entries = tEntries;

	Entry* e = (Entry*)malloc(sizeof(Entry));
	e->type = FileType;

	FileStruct* f = (FileStruct*)malloc(sizeof(FileStruct));

	char tName[20];
	strcpy(tName, name);
	strcpy(f->name, strtok(tName, "."));
	strcpy(f->extension, strtok(NULL, "."));
	c->occupied = 1;
	f->firstCluster = c;

	e->pointer = f;

	subdirectory->entries[n] = e;
	subdirectory->entriesAmount++;

	return 1;
}


int CheckFileName(char* name)
{
	return 1;
}

int AddFolder(FolderStruct* subdirectory, char* name, FAT* fat)
{
	if(!CheckFolderName(name))
	{
		return 0;
	}

	int n = subdirectory->entriesAmount;
	Entry** tEntries;

	if(n == 0)
	{
		tEntries = (Entry**)malloc(sizeof(Entry*));
	}
	else
	{
		tEntries = (Entry**)realloc(subdirectory->entries, (n+1)*sizeof(Entry*));
	}

	if(tEntries == NULL)
	{
		return 0;
	}

	subdirectory->entries = tEntries;

	Entry* e = (Entry*)malloc(sizeof(Entry));
	e->type = FolderType;

	FolderStruct* f = (FolderStruct*)malloc(sizeof(FolderStruct));

	char tName[20];
	strcpy(tName, name);
	strcpy(f->name, strtok(tName, "."));
	f->entriesAmount = 0;

	e->pointer = f;

	subdirectory->entries[n] = e;
	subdirectory->entriesAmount++;

	return 1;
}


int CheckFolderName(char* name)
{
	return 1;
}


Cluster * FindEmptyCluster(FAT* f)
{
	int n = 0;

	while(n < f->size)
	{
		if(f->table[n]->occupied != 1)
		{
			return f->table[n];
		}

		n++;
	}

	return NULL;
}

void ViewDirectories(FolderStruct* root, int level)
{
	int i, j;

	for(i = 0; i < root->entriesAmount; i++)
	{
		for(j = 0; j < level; j++)
		{
			printf("\t");
		}

		if(root->entries[i]->type == FolderType)
		{
			FolderStruct* folder = (FolderStruct*)(root->entries[i]->pointer);
			printf("%s:\n", folder->name);
			ViewDirectories(folder, level+1);
		}
		else
		{
			FileStruct* file = (FileStruct*)(root->entries[i]->pointer);
			printf("%s.%s\n", file->name, file->extension);
		}
	}
}