#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_VOLUME_SIZE 1073741824
#define MAX_DIR_TABLE_SIZE 134217728
#define MAX_FAT_SIZE 939524096

typedef enum EntryType { FileType, FolderType } EntryType;

typedef struct Entry Entry;
typedef struct FileStruct FileStruct;
typedef struct FolderStruct FolderStruct;
typedef struct Cluster Cluster;
typedef struct FAT FAT;
typedef struct Volume Volume;

struct Entry
{
	EntryType type;
	void* pointer;
};

struct FileStruct
{
	char name[8];
	char extension[4];
	Cluster* firstCluster;
};

struct FolderStruct
{
	char name[8];
	Entry** entries;
	int entriesAmount;
};

struct Cluster
{
	int occupied;
	unsigned char data[504];
	Cluster* next;
};

struct FAT
{
	Cluster** table;
	int size;
};

struct Volume
{
	FolderStruct* root;
	FAT* fat;
	unsigned int size;
};

int Initialize(Volume*);
int AddFile(FolderStruct*, char*, unsigned char*, unsigned int, Volume*);
int CheckFileName(char*);
int AddFolder(FolderStruct*, char*, Volume*);
int CheckFolderSize(Volume *);
int CheckFileSize(unsigned int, Volume *);
int CheckFolderName(char*);
Cluster * FindEmptyCluster(FAT*);
int AddContent(unsigned char*, unsigned int, Cluster*, FAT*);
void ViewDirectories(FolderStruct*, int);

int main()
{
    Volume vol;

    Initialize(&vol);

    ViewDirectories(vol.root, 0);

    return 0;
}

int Initialize(Volume* vol)
{
	vol->root = (FolderStruct*)malloc(sizeof(FolderStruct));
	vol->root->entries = (Entry**)malloc(sizeof(Entry*));
	vol->root->entriesAmount = 0;

	int i, n = 10;
	vol->fat = (FAT*)malloc(sizeof(FAT));
	vol->fat->table = (Cluster**)malloc(n*sizeof(Cluster*));

	for(i = 0; i < n; i++)
	{
		vol->fat->table[i] = (Cluster*)malloc(sizeof(Cluster));
		vol->fat->table[i]->occupied = 0;
		vol->fat->table[i]->next = NULL;
	}

	vol->fat->size = 10;

	strcpy(vol->root->name, "root");
	AddFile(vol->root, "gitara.txt", NULL, 0, vol);
	AddFile(vol->root, "abcd.jpg", NULL, 0, vol);
	AddFolder(vol->root, "folder1", vol);
	FolderStruct* f = (FolderStruct*)vol->root->entries[vol->root->entriesAmount-1]->pointer;
	AddFile(f, "makar.gif", NULL, 0, vol);
	AddFile(f, "gowno.rar", NULL, 0, vol);
	AddFile(vol->root, "siema.elo", NULL, 0, vol);

	return 1;
}

int AddFile(FolderStruct* subdirectory, char* name, unsigned char * content, unsigned int contentSize, Volume* vol)
{
	if(!CheckFileName(name))
	{
		return 0;
	}

	if(!CheckFileSize(contentSize, vol))
	{
		return 0;
	}

	Cluster* c = FindEmptyCluster(vol->fat);
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

	if(!AddContent(NULL, 0, c, vol->fat))
	{
		return 0;
	}

	e->pointer = f;

	subdirectory->entries[n] = e;
	subdirectory->entriesAmount++;

	return 1;
}

int CheckFileName(char* name)
{
	return 1;
}

int AddFolder(FolderStruct* subdirectory, char* name, Volume* vol)
{
	if(!CheckFolderName(name))
	{
		return 0;
	}

	if(!CheckFolderSize(vol))
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

int CheckFolderSize(Volume *vol)
{
	return sizeof(FolderStruct) < (MAX_DIR_TABLE_SIZE - vol->root->entriesAmount*sizeof(FolderStruct));
}

int CheckFileSize(unsigned int dataSize, Volume *vol)
{
	return (sizeof(FileStruct) < (MAX_DIR_TABLE_SIZE - vol->root->entriesAmount*sizeof(FileStruct))) && dataSize < (MAX_FAT_SIZE - vol->fat->size*sizeof(Cluster));
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

	if(sizeof(Cluster) < (MAX_FAT_SIZE - f->size*sizeof(Cluster)))
	{
		return NULL;
	}

	Cluster* c = (Cluster*)malloc(sizeof(Cluster));
	c->occupied = 0;
	c->next = NULL;

	f->table = (Cluster**)realloc(f->table, (f->size + 1)*sizeof(Cluster*));
	f->table[f->size] = c;
	f->size++;

	return c;

}

int AddContent(unsigned char * content, unsigned int contentSize, Cluster *firstCluster, FAT * f)
{
	unsigned int clusterDataSize = sizeof(firstCluster->data);
	Cluster *currentCluster = firstCluster;
	Cluster *previousCluster = firstCluster;

	unsigned int i = 0, j = 0;

	for(i = 0, j = 0; i < contentSize; i++, j++)
	{
		if(j < clusterDataSize)
		{
			currentCluster->data[j] = content[i];
		}
		else
		{
			currentCluster = FindEmptyCluster(f);
			if(currentCluster == NULL)
			{
				return 0;
			}
			currentCluster->occupied = 1;
			previousCluster->next = currentCluster;
			j = 0;
		}
	}

	currentCluster->next = NULL;
	return 1;
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