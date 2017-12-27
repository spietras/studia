#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NAME_LENGTH 16
#define EXTENSION_LENGTH 4
#define CLUSTER_DATA_LENGTH 128
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
	char name[NAME_LENGTH+1];
	DirectoryNode* subdirs;
	FileNode* files;
	int entriesNum;
	Cluster* data;
};

struct DirectoryNode
{
	Directory* dir;
	DirectoryNode* next;
	DirectoryNode* previous;
};

struct TextFile
{
	char name[NAME_LENGTH+1];
	char extension[EXTENSION_LENGTH+1];
	Cluster* data;
};

struct FileNode
{
	TextFile* file;
	FileNode* next;
	FileNode* previous;
};

struct Cluster
{
	int id;
	char data[CLUSTER_DATA_LENGTH+1];
	Cluster* next;
};

int InitializeVolume(Volume*);
int AddDirectory(Volume *, Directory*, const char*);
DirectoryNode* FindLastInDirectoryList(DirectoryNode*);
int IsAnotherClusterNeededForEntry(int);
int FindEmptyClusterIndex(Cluster**, int);
void ViewStructureTree(Directory *);
void Indent(int);
DirectoryNode* CreateDirectory(Volume *, const char*);
int AddEntrySpace(Volume *, Directory*);
Cluster* FindEmptyCluster(Volume *);
void ViewLevel(Directory *, int);
FileNode* FindLastInFileList(FileNode*);
FileNode* CreateFile(Volume *, const char*, const char*);
int AddFile(Volume *, Directory*, const char*, const char*);
int AddExampleEntries(Volume*);
int NumberOfNeededClusters(const char*);
int IsEnoughFreeClusters(Cluster**, int, int);
int AddDataToClusterChain(Volume*, TextFile*, const char*, int);
void ViewFileData(TextFile*);
int AddDataToFile(Volume*, TextFile*, const char*);
int ClearFileData(Volume *, Cluster*);
int DeleteFile(Volume*, Directory*, FileNode*);
void OrganizeFileListAfterDeletion(Directory*, FileNode*);

int main()
{
	Volume v;
	if(!InitializeVolume(&v))
	{
		printf("Initialization error");
		return 1;
	}
	ViewStructureTree(v.root);
	ViewFileData(v.root->subdirs->dir->files->file);
	DeleteFile(&v, v.root->subdirs->dir, v.root->subdirs->dir->files);
	ViewStructureTree(v.root);
	ViewFileData(v.root->subdirs->dir->files->file);
	return 0;
}

int DeleteFile(Volume* v, Directory* parent, FileNode* n)
{
	if(v == NULL || parent == NULL || n == NULL || parent->files == NULL || n->file == NULL || n->file->data == NULL)
	{
		return 0;
	}

    TextFile* f = n->file;

    if(!ClearFileData(v, f->data))
	{
		return 0;
	}
    v->clusterTable[f->data->id] = NULL;
    free(f->data);
    free(f);

    OrganizeFileListAfterDeletion(parent, n);

	free(n);

	return 1;
}

void OrganizeFileListAfterDeletion(Directory* parent, FileNode* n)
{
	if(n->previous == NULL && n->next == NULL)
	{
        parent->files = NULL;
	}
	else if(n->previous == NULL && n->next != NULL)
	{
		n->next->previous = NULL;
		parent->files = n->next;
	}
	else if(n->previous != NULL && n->next == NULL)
	{
		n->previous->next = NULL;
	}
	else if(n->previous != NULL && n->next != NULL)
	{
		n->previous->next = n->next;
		n->next->previous = n->previous;
	}
}

int InitializeVolume(Volume* v)
{
	if(v == NULL)
	{
		return 0;
	}
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
    v->clusterTable[0]->data[0] = '\0';
    v->clusterTable[0]->id = 0;
    v->root->data = v->clusterTable[0];

    if(!AddExampleEntries(v))
	{
		free(v->root);
		free(v->clusterTable);
		v->clustersNum = 0;
		return 0;
	}

    return 1;
}

int AddExampleEntries(Volume* v)
{
	if(v == NULL)
	{
		return 0;
	}

	return
	AddDirectory(v, v->root, "Folder1") &&
	AddDirectory(v, v->root, "Folder2") &&
	AddDirectory(v, v->root->subdirs->dir, "Folder1") &&
	AddDirectory(v, v->root->subdirs->dir, "Folder2") &&
	AddDirectory(v, v->root->subdirs->next->dir, "Folder1") &&
	AddDirectory(v, v->root->subdirs->next->dir, "Folder2") &&
	AddDirectory(v, v->root->subdirs->next->dir, "Folder3") &&
	AddDirectory(v, v->root->subdirs->dir->subdirs->next->dir, "Folder1") &&
	AddFile(v, v->root, "File1", "txt") &&
	AddFile(v, v->root->subdirs->dir, "File1", "txt") &&
	AddDataToFile(v, v->root->subdirs->dir->files->file, "abababaabababbababaabbaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") &&
	AddFile(v, v->root->subdirs->dir, "File2", "txt") &&
	AddFile(v, v->root->subdirs->next->dir->subdirs->dir, "File1", "txt");

}

int AddDirectory(Volume *v, Directory* parent, const char* name)
{
	if(v == NULL || parent == NULL || strlen(name) > NAME_LENGTH)
	{
		return 0;
	}
	DirectoryNode* last = FindLastInDirectoryList(parent->subdirs);
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
		create->previous = last;
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

int AddFile(Volume *v, Directory* parent, const char* name, const char* extension)
{
	if(v == NULL || parent == NULL || strlen(name) > NAME_LENGTH || strlen(extension) > EXTENSION_LENGTH)
	{
		return 0;
	}

	FileNode* last = FindLastInFileList(parent->files);
    FileNode* create = CreateFile(v, name, extension);
    if(create == NULL)
	{
		return 0;
	}

    if(last == NULL)
	{
		parent->files = create;
	}
	else
	{
		last->next = create;
		create->previous = last;
	}

    if(!AddEntrySpace(v, parent))
	{
		if(last == NULL)
		{
			parent->files = NULL;
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

int AddDataToFile(Volume* v, TextFile* f, const char* data)
{
	if(v == NULL || f == NULL || data == NULL)
	{
		return 0;
	}

	int neededClusters = NumberOfNeededClusters(data);

	if(!IsEnoughFreeClusters(v->clusterTable, v->clustersNum, neededClusters))
	{
		return 0;
	}

	if(!ClearFileData(v, f->data))
	{
		return 0;
	}

    if(neededClusters == 1)
	{
		strcpy(f->data->data, data);
	}
	else
	{
        if(!AddDataToClusterChain(v, f, data, neededClusters))
		{
			return 0;
		}
	}

	return 1;
}

int ClearFileData(Volume *v, Cluster*  dataCluster)
{
	if(v == NULL || dataCluster == NULL)
	{
		return 0;
	}

	dataCluster->data[0] = '\0';

	Cluster* t;
    Cluster* current = dataCluster->next;

    if(current == NULL)
	{
		return 1;
	}

    do
	{
		v->clusterTable[current->id] = NULL;
		t = current;
		current = current->next;
		free(t);
	}while(current != NULL);

	dataCluster->next = NULL;

	return 1;
}

int AddDataToClusterChain(Volume* v, TextFile* f, const char* data, int neededClusters)
{
	if(v == NULL || f == NULL || data == NULL || neededClusters == 0)
	{
		return 0;
	}

	int i;

	Cluster* previous = f->data;

	for(i = 0; i < (neededClusters-1); i++)
	{
		previous->next = FindEmptyCluster(v);
		if(previous->next == NULL)
		{
			f->data->next = NULL;
			return 0;
		}
		previous = previous->next;
	}

	char c = data[0];
	int n, m = 0;
	Cluster* current = f->data;

	while(c != '\0')
	{
		for(n = 0; n < CLUSTER_DATA_LENGTH; n++, m++)
		{
			c = data[m];
			if(c == '\0')
			{
				break;
			}
			current->data[n] = c;
		}

		current->data[n] = '\0';

		current = current->next;
	}

	return 1;
}

int IsEnoughFreeClusters(Cluster** clusterTable, int clusterNum, int needed)
{
	if(clusterTable == NULL)
	{
		return 0;
	}

	int i, j = 0;

	for(i = 0; i < clusterNum; i++)
	{
		if(clusterTable[i] == NULL)
		{
			j++;
		}

		if(j >= needed)
		{
			return 1;
		}
	}

	return 0;
}

int NumberOfNeededClusters(const char* data)
{
	if(data == NULL)
	{
		return 0;
	}

	int dataSize = strlen(data);

	if(dataSize % CLUSTER_DATA_LENGTH == 0)
	{
		return dataSize / CLUSTER_DATA_LENGTH;
	}

	return dataSize / CLUSTER_DATA_LENGTH + 1;
}

DirectoryNode* CreateDirectory(Volume *v, const char* name)
{
	if(v == NULL || strlen(name) > NAME_LENGTH)
	{
		return NULL;
	}

	DirectoryNode* create = (DirectoryNode*)malloc(sizeof(DirectoryNode));
	if(create == NULL)
	{
		return NULL;
	}

	create->next = NULL;
	create->previous = NULL;

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

    create->dir->data = FindEmptyCluster(v);

    if(create->dir->data == NULL)
	{
		free(create->dir);
		free(create);
		return NULL;
	}

	return create;
}

FileNode* CreateFile(Volume *v, const char* name, const char* extension)
{
	if(v == NULL || strlen(name) > NAME_LENGTH || strlen(extension) > EXTENSION_LENGTH)
	{
		return NULL;
	}

	FileNode* create = (FileNode*)malloc(sizeof(FileNode));
	if(create == NULL)
	{
		return NULL;
	}

	create->next = NULL;
	create->previous = NULL;

	create->file = (TextFile*)malloc(sizeof(TextFile));
	if(create->file == NULL)
	{
		free(create);
		return NULL;
	}
    strcpy(create->file->name, name);
    strcpy(create->file->extension, extension);
    create->file->data = FindEmptyCluster(v);

    if(create->file->data == NULL)
	{
		free(create->file);
		free(create);
		return NULL;
	}

	return create;
}

Cluster* FindEmptyCluster(Volume *v)
{
	if(v == NULL)
	{
		return NULL;
	}

	int i = FindEmptyClusterIndex(v->clusterTable, v->clustersNum);
    if(i == -1)
	{
		return NULL;
	}

	v->clusterTable[i] = (Cluster*)malloc(sizeof(Cluster));
	if(v->clusterTable[i] == NULL)
	{
		return NULL;
	}

	v->clusterTable[i]->next = NULL;
	v->clusterTable[i]->id = i;
	v->clusterTable[i]->data[0] = '\0';

	return v->clusterTable[i];
}

int AddEntrySpace(Volume *v, Directory* parent)
{
	if(v == NULL || parent == NULL)
	{
		return 0;
	}

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
		v->clusterTable[i]->next = NULL;
		v->clusterTable[i]->data[0] = '\0';
		v->clusterTable[i]->id = i;
		parent->data->next = v->clusterTable[i];
	}

	parent->entriesNum++;

	return 1;
}

DirectoryNode* FindLastInDirectoryList(DirectoryNode* first)
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

FileNode* FindLastInFileList(FileNode* first)
{
	if(first == NULL)
	{
		return NULL;
	}

    FileNode* t = first;

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
	if(clusterTable == NULL)
	{
		return -1;
	}

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
		printf("Directory does not exist\n");
	}

	if(d->files == NULL && d->subdirs == NULL)
	{
		printf("Empty directory\n");
	}

	int startLevel = 0;

	ViewLevel(d, startLevel);

    printf("\n");
}

void ViewLevel(Directory *d, int level)
{
	if(d == NULL)
	{
		return;
	}
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

	printf("|");
	for(i = 0; i < level; i++)
	{
		printf(" |");
	}

	printf("____");
}

void ViewFileData(TextFile* f)
{
	if(f == NULL)
	{
		printf("File does not exist\n");
		return;
	}
	if(f->data == NULL || strlen(f->data->data) == 0)
	{
		printf("File is empty\n");
		return;
	}

	Cluster* current = f->data;

	printf("%s.%s file data:\n", f->name, f->extension);

	do
	{
		printf("%s", current->data);
		current = current->next;
	}while(current != NULL);

	printf("\n\n");
}