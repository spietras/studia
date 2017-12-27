#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NAME_LENGTH 16
#define EXTENSION_LENGTH 4
#define DATA_LENGTH 128
#define DEFAULT_CLUSTER_NUM 100

typedef struct Volume Volume;
typedef struct Directory Directory;
typedef struct DirectoryNode DirectoryNode;
typedef struct TextFile TextFile;
typedef struct FileNode FileNode;
typedef struct Cluster Cluster;

struct Volume
{
	Directory* root;
	Cluster** clusterTable;
	int clustersNum;
};

struct Directory
{
	char name[NAME_LENGTH];
	DirectoryNode* subdirs;
	FileNode* files;
	int entriesNum;
	Cluster* data;
};

struct DirectoryNode
{
	Directory* dir;
	DirectoryNode* next;
};

struct TextFile
{
	char name[NAME_LENGTH];
	char extension[EXTENSION_LENGTH];
	Cluster* data;
};

struct FileNode
{
	TextFile* file;
	FileNode* next;
};

struct Cluster
{
	char data[DATA_LENGTH];
	Cluster* next;
};

int InitializeVolume(Volume*);
int AddDirectory(Volume *, Directory*, const char*);
DirectoryNode* FindLastInList(DirectoryNode*);
int IsAnotherClusterNeededForEntry(int);
int FindEmptyClusterIndex(Cluster**, int);
void ViewStructureTree(Directory *);
void Indent(int);
DirectoryNode* CreateDirectory(Volume *, const char*);
int AddEntrySpace(Volume *, Directory*);
int AssignClusterToDirectory(Volume *, Directory*);
void ViewLevel(Directory *, int);

int main()
{
	Volume v;
	if(!InitializeVolume(&v))
	{
		printf("Initialization error");
		return 1;
	}
	ViewStructureTree(v.root);
	return 0;
}

int InitializeVolume(Volume* v)
{
    v->root = (Directory*)malloc(sizeof(Directory));
    if(v->root == NULL)
	{
		return 0;
	}
    strcpy(v->root->name, "root");
    v->root->files = NULL;
    v->root->subdirs = NULL;
    v->root->entriesNum = 0;

    /* DEFAULT_CLUSTER_NUM clusters */

    v->clusterTable = (Cluster**)malloc(DEFAULT_CLUSTER_NUM*sizeof(Cluster*));
    if(v->clusterTable == NULL)
	{
		free(v->root);
		return 0;
	}

	v->clustersNum = DEFAULT_CLUSTER_NUM;

    int i;
    for(i = 0; i < DEFAULT_CLUSTER_NUM; i++)
	{
		v->clusterTable[i] = NULL;
	}

    v->clusterTable[0] = (Cluster*)malloc(sizeof(Cluster));
    if(v->clusterTable[0] == NULL)
	{
		free(v->root);
		free(v->clusterTable);
		v->clustersNum = 0;
		return 0;
	}
    v->clusterTable[0]->next = NULL;
    v->root->data = v->clusterTable[0];

    if(!AddDirectory(v, v->root, "gitara"))
	{
		free(v->root);
		free(v->clusterTable);
		v->clustersNum = 0;
		return 0;
	}
    if(!AddDirectory(v, v->root, "siema"))
	{
		free(v->root);
		free(v->clusterTable);
		v->clustersNum = 0;
		return 0;
	}
    if(!AddDirectory(v, v->root->subdirs->dir, "elo"))
	{
		free(v->root);
		free(v->clusterTable);
		v->clustersNum = 0;
		return 0;
	}

    return 1;
}

int AddDirectory(Volume *v, Directory* parent, const char* name)
{
	DirectoryNode* last = FindLastInList(parent->subdirs);
    DirectoryNode* create = CreateDirectory(v, name);
    if(create == NULL)
	{
		return 0;
	}

    if(last == NULL)
	{
		parent->subdirs = create;
	}
	else
	{
		last->next = create;
	}

    if(!AddEntrySpace(v, parent))
	{
		if(last == NULL)
		{
			parent->subdirs = NULL;
		}
		else
		{
			last->next = NULL;
		}
		free(create);
		return 0;
	}

    return 1;
}

DirectoryNode* CreateDirectory(Volume *v, const char* name)
{
	DirectoryNode* create = (DirectoryNode*)malloc(sizeof(DirectoryNode));
	if(create == NULL)
	{
		return NULL;
	}

	create->next = NULL;

	create->dir = (Directory*)malloc(sizeof(Directory));
	if(create->dir == NULL)
	{
		free(create);
		return NULL;
	}
    strcpy(create->dir->name, name);
    create->dir->entriesNum = 0;
    create->dir->files = NULL;
    create->dir->subdirs = NULL;

    if(!AssignClusterToDirectory(v, create->dir))
	{
		free(create->dir);
		free(create);
		return NULL;
	}

	return create;
}

int AssignClusterToDirectory(Volume *v, Directory* d)
{
	int i = FindEmptyClusterIndex(v->clusterTable, v->clustersNum);
    if(i == -1)
	{
		return 0;
	}

	v->clusterTable[i] = (Cluster*)malloc(sizeof(Cluster));
	if(v->clusterTable[i] == NULL)
	{
		return 0;
	}
	d->data = v->clusterTable[i];

	return 1;
}

int AddEntrySpace(Volume *v, Directory* parent)
{
	if(IsAnotherClusterNeededForEntry(parent->entriesNum))
	{
        int i = FindEmptyClusterIndex(v->clusterTable, v->clustersNum);
        if(i == -1)
		{
			return 0;
		}

		v->clusterTable[i] = (Cluster*)malloc(sizeof(Cluster));
		if(v->clusterTable[i] == NULL)
		{
			return 0;
		}
		parent->data->next = v->clusterTable[i];
	}

	parent->entriesNum++;

	return 1;
}

DirectoryNode* FindLastInList(DirectoryNode* first)
{
	if(first == NULL)
	{
		return NULL;
	}

    DirectoryNode* t = first;

    while(t->next != NULL)
	{
		t = t->next;
	}

	return t;
}

int IsAnotherClusterNeededForEntry(int entriesNum)
{
	if(entriesNum < 0)
	{
		entriesNum = 0;
	}

	return (entriesNum % 4) + 1 > 4;
}

int FindEmptyClusterIndex(Cluster** clusterTable, int clusterNum)
{
	int i;

	for(i = 0; i < clusterNum; i++)
	{
		if(clusterTable[i] == NULL)
		{
			return i;
		}
	}

	return -1;
}

void ViewStructureTree(Directory *d)
{
	if(d == NULL)
	{
		printf("Directory does not exist");
	}

	if(d->files == NULL && d->subdirs == NULL)
	{
		printf("Empty directory");
	}

	int startLevel = 0;

	ViewLevel(d, startLevel);
}

void ViewLevel(Directory *d, int level)
{
    if(level < 0)
	{
		level = 0;
	}

	DirectoryNode* tn = d->subdirs;
	if(tn != NULL)
	{
		do
		{
			Indent(level);
			printf("%s\n", tn->dir->name);
			ViewLevel(tn->dir, level+1);
			tn = tn->next;
		}while(tn != NULL);
	}

	FileNode* fn = d->files;
	if(fn != NULL)
	{
		do
		{
			Indent(level);
			printf("%s.%s\n", fn->file->name, fn->file->extension);
			fn = fn->next;
		}while(fn != NULL);
	}
}

void Indent(int level)
{
	int i;

	for(i = 0; i < level; i++)
	{
		printf("\t");
	}
}