#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NAME_SIZE 20
#define EXTENSION_SIZE 8
#define ENTRIES_PER_CLUSTER 4
#define MAX_VOLUME_SIZE 1048576
#define DEFAULT_CLUSTER_FRACTION 0.125
#define ENTRY_DATA_SIZE (NAME_SIZE+EXTENSION_SIZE+4)
#define CLUSTER_DATA_SIZE (ENTRIES_PER_CLUSTER*ENTRY_DATA_SIZE)
#define MAX_CLUSTER_NUM ((MAX_VOLUME_SIZE)/(CLUSTER_DATA_SIZE))
#define DEFAULT_CLUSTER_NUM (DEFAULT_CLUSTER_FRACTION * MAX_CLUSTER_NUM)
#define FILE_INVALID_CHARACTERS "/ \\"
#define DIRECTORY_INVALID_CHARACTERS "/ \\."

typedef struct Volume Volume;
typedef struct Directory Directory;
typedef struct TextFile TextFile;
typedef struct Cluster Cluster;

struct Volume
{
	Directory* root;
	Cluster** clusterTable;
	int clustersNum;
};

struct Directory
{
	char name[NAME_SIZE+1];
	Directory* subdirs;
	TextFile* files;
	int entriesNum;
	Cluster* dataClusters;
	Directory* parent;
	Directory* next;
	Directory* previous;
};

struct TextFile
{
	char name[NAME_SIZE+1];
	char extension[EXTENSION_SIZE+1];
	Cluster* dataClusters;
	Directory* parent;
	TextFile* next;
	TextFile* previous;
};

struct Cluster
{
	int id;
	char data[CLUSTER_DATA_SIZE+1];
	Cluster* previous;
	Cluster* next;
};

int InitializeVolume(Volume*);
Directory* AddDirectory(Volume *, Directory*, const char*);
Directory* FindLastInDirectoryList(Directory*);
int IsAnotherClusterNeededForEntry(int);
int FindEmptyClusterIndex(Cluster**, int);
void ViewStructureTree(Directory *);
void Indent(int);
Directory* CreateEmptyDirectory(Volume *, const char*);
int AddEntrySpace(Volume *, Directory*);
Cluster* FindEmptyCluster(Volume *);
void ViewLevel(Directory *, int);
TextFile* FindLastInFileList(TextFile*);
TextFile* CreateEmptyFile(Volume *, const char*, const char*);
TextFile* AddFile(Volume *, Directory*, const char*, const char*);
int AddExampleEntries(Volume*);
int NumberOfNeededClusters(const char*);
int IsEnoughFreeClusters(Cluster**, int, int);
int AddDataToClusterChain(Volume*, TextFile*, const char*, int);
void ViewFileData(TextFile*);
int AddDataToFile(Volume*, TextFile*, const char*);
int ClearData(Volume *, Cluster*);
int DeleteFile(Volume*, TextFile*);
int DeleteFileByPath(Volume*, const char*);
int IsFile(const char*);
Directory* FindDirectoryByNameAndParent(Directory*, const char*);
TextFile* FindFileByNameAndParent(Directory*, const char*);
int IsValidFilePath(const char*);
int IsDirectory(const char*);
int IsValidDirectoryPath(const char*);
Directory* FindDirectoryByPath(Directory*, const char*);
TextFile* FindFileByPath(Directory*, const char*);
void OrganizeFileListAfterDeletion(TextFile*);
void OrganizeSubdirectoryListAfterDeletion(Directory*);
int DeleteSingleDirectory(Volume*, Directory*);
int DeleteDirectoryTree(Volume*, Directory*);
int DeleteDirectoryByPath(Volume* v, const char* path);
TextFile* AddFileByPath(Volume*, const char*);
Directory* AddDirectoryByPath(Volume*, const char*);
int AddDataToFileByPath(Volume*, const char*, const char*);
void ViewFileDataByPath(Directory*, const char*);
void ViewStructureTreeByPath(Directory*, const char*);
Cluster* FindLastInClusterList(Cluster*);
int IsLastClusterNeededAfterDeletingEntry(int);
int MoveFileToDirectoryByPaths(Volume*v, const char*, const char*);
int MoveFileToDirectory(Volume*, TextFile*, Directory*);
int RemoveEntrySpace(Volume*, Directory*);
int MoveDirectoryToDirectoryByPaths(Volume*, const char*, const char*);
int MoveDirectoryToDirectory(Volume*, Directory*, Directory*);

int main()
{
	Volume v;
	if(!InitializeVolume(&v))
	{
		printf("\nInitialization error\n");
		return 1;
	}
	ViewStructureTree(v.root);
	ViewFileDataByPath(v.root, "root/Folder1/File1.txt");
	DeleteFileByPath(&v, "root/Folder1/File1.txt");
	ViewStructureTree(v.root);
	ViewFileDataByPath(v.root, "root/Folder1/File1.txt");
	DeleteFileByPath(&v, "root/Folder1/File2.txt");
	ViewStructureTree(v.root);
	DeleteDirectoryByPath(&v, "root/Folder2");
	ViewStructureTree(v.root);
	AddFileByPath(&v, "root/Folder1/Folder2/Folder1/F/G/H/XD.txt");
	AddDirectoryByPath(&v, "root/Folder1/A/B");
	ViewStructureTree(v.root);
	MoveFileToDirectoryByPaths(&v, "root/File1.txt", "root/Folder1/Folder2/Folder1/F/G/H");
	ViewStructureTree(v.root);
	MoveDirectoryToDirectoryByPaths(&v, "root/Folder1/Folder2/Folder1/F", "root/Folder1");
	ViewStructureTree(v.root);

	return 0;
}

/* Moves directory with given path to directory with given path */
int MoveDirectoryToDirectoryByPaths(Volume* v, const char* dirPath, const char* destPath)
{
	if(v == NULL || !IsValidDirectoryPath(dirPath) || !IsValidDirectoryPath(destPath) || strstr(destPath, dirPath) != NULL)
	{
		return 0;
	}

	Directory* d = FindDirectoryByPath(v->root, dirPath);
	if(d == NULL)
	{
		return 0;
	}

	Directory* dest = FindDirectoryByPath(v->root, destPath);
	if(dest == NULL)
	{
		return 0;
	}

	if(dest == d->parent)
	{
		return 0;
	}

	if(!MoveDirectoryToDirectory(v, d, dest))
	{
		return 0;
	}

	return 1;
}

/* Moves directory d to directory destination */
int MoveDirectoryToDirectory(Volume* v, Directory* d, Directory* destination)
{
	if(!AddEntrySpace(v, destination))
	{
		return 0;
	}

	if(!RemoveEntrySpace(v, d->parent))
	{
		return 0;
	}
	OrganizeSubdirectoryListAfterDeletion(d);

	d->parent = destination;

	Directory *last = FindLastInDirectoryList(destination->subdirs);
	if(last == NULL)
	{
		destination->subdirs = d;
	}
	else
	{
		last->next = d;
		d->previous = last;
	}

	return 1;
}

/* Moves file with given path to directory with given path */
int MoveFileToDirectoryByPaths(Volume*v, const char* fPath, const char* dirPath)
{
	if(v == NULL || !IsValidFilePath(fPath) || !IsValidDirectoryPath(dirPath))
	{
		return 0;
	}

	TextFile* f = FindFileByPath(v->root, fPath);
	if(f == NULL)
	{
		return 0;
	}

	Directory* d = FindDirectoryByPath(v->root, dirPath);
	if(d == NULL)
	{
		return 0;
	}

	if(!MoveFileToDirectory(v, f, d))
	{
		return 0;
	}

	return 1;
}

/* Moves file f to directory d */
int MoveFileToDirectory(Volume* v, TextFile* f, Directory* d)
{
    if(!AddEntrySpace(v, d))
	{
		return 0;
	}

	if(!RemoveEntrySpace(v, f->parent))
	{
		return 0;
	}
	OrganizeFileListAfterDeletion(f);

	f->parent = d;

	TextFile *last = FindLastInFileList(d->files);
	if(last == NULL)
	{
		d->files = f;
	}
	else
	{
		last->next = f;
		f->previous = last;
	}

	return 1;
}

/* Copies list of clusters */
Cluster* CopyClusterList(Cluster* first)
{
	if(first == NULL)
	{
		return NULL;
	}

	Cluster* t = (Cluster*)malloc(sizeof(Cluster));
	strcpy(t->data, first->data);
	t->id = first->id;
	t->next = CopyClusterList(first->next);
	return t;
}

/* Views tree structure of directory with given path */
void ViewStructureTreeByPath(Directory* root, const char* path)
{
	if(root == NULL)
	{
		printf("\nNo root folder\n");
		return;
	}

	if(!IsValidDirectoryPath(path))
	{
		printf("\nInvalid path\n");
		return;
	}

	Directory* d = FindDirectoryByPath(root, path);

	if(d == NULL)
	{
		printf("\nDirectory does not exist\n");
		return;
	}

	ViewStructureTree(d);
}

/* Views data of file with given path */
void ViewFileDataByPath(Directory* root, const char* path)
{
	if(root == NULL)
	{
		printf("\nNo root folder\n");
		return;
	}

	if(!IsValidFilePath(path))
	{
		printf("\nInvalid path\n");
		return;
	}

	TextFile* f = FindFileByPath(root, path);

	if(f == NULL)
	{
		printf("\nFile does not exist\n");
		return;
	}

	ViewFileData(f);
}

/* Adds given data to file with given path */
int AddDataToFileByPath(Volume* v, const char* path, const char* data)
{
	if(v == NULL || !IsValidFilePath(path) || data == NULL)
	{
		return 0;
	}

	TextFile *f = FindFileByPath(v->root, path);
	if(f == NULL)
	{
		return 0;
	}

	return AddDataToFile(v, f, data);
}

/* Adds directory with given path */
Directory* AddDirectoryByPath(Volume*v, const char* path)
{
	if(v == NULL || !IsValidDirectoryPath(path))
	{
		return NULL;
	}

	char *pathClone = malloc(strlen(path)+1);
	strcpy(pathClone, path);

	char* cName = strtok(pathClone, "/");
	cName = strtok(NULL, "/");

	Directory* current = v->root;
	Directory* t = current;

	while(cName != NULL)
	{
		current = FindDirectoryByNameAndParent(current, cName);

		if(current == NULL)
		{
			current = AddDirectory(v, t, cName);
			if(current == NULL)
			{
				return NULL;
			}
		}

		t = current;
		cName = strtok(NULL, "/");
	}

	return current;
}

/* Adds file with given path */
TextFile* AddFileByPath(Volume* v, const char* path)
{
	if(v == NULL || !IsValidFilePath(path))
	{
		return NULL;
	}

	char *pathClone = malloc(strlen(path)+1);
	strcpy(pathClone, path);

	char* cName = strtok(pathClone, "/");
	cName = strtok(NULL, "/");

	Directory* current = v->root;
	Directory* t = current;

	while(!IsFile(cName))
	{
		current = FindDirectoryByNameAndParent(current, cName);

		if(current == NULL)
		{
			current = AddDirectory(v, t, cName);
			if(current == NULL)
			{
				return NULL;
			}
		}

		t = current;
		cName = strtok(NULL, "/");
	}

	char* name = strtok(cName, ".");
	char* ext = strtok(NULL, ".");

	return AddFile(v, current, name, ext);
}

/* Delete directory (and all subdirectories and files within it) with given path */
int DeleteDirectoryByPath(Volume* v, const char* path)
{
	if(v == NULL || path == NULL)
	{
        return 0;
	}

    Directory* d = FindDirectoryByPath(v->root, path);

    if(!DeleteDirectoryTree(v, d))
	{
		return 0;
	}

	return 1;
}

/* Deletes recursively all subdirectories and files within given directory (and that directory) */
int DeleteDirectoryTree(Volume*v, Directory* d)
{
	if(v == NULL || d == NULL)
	{
		return 0;
	}

	while(d->subdirs != NULL)
	{
		DeleteDirectoryTree(v, d->subdirs);
	}

	while(d->files != NULL)
	{
		DeleteFile(v, d->files);
	}

	DeleteSingleDirectory(v, d);

	return 1;
}

/* Deletes d directory (with no subdirectories) */
int DeleteSingleDirectory(Volume* v, Directory* d)
{
	if(v == NULL  || d == NULL || d->dataClusters == NULL || d->parent == NULL || d->parent->subdirs == NULL || d->subdirs != NULL)
	{
		return 0;
	}

    if(!ClearData(v, d->dataClusters))
	{
		return 0;
	}
    v->clusterTable[d->dataClusters->id] = NULL;
    free(d->dataClusters);

    if(!RemoveEntrySpace(v, d->parent))
	{
		return 0;
	}

    OrganizeSubdirectoryListAfterDeletion(d);


	free(d);

	return 1;
}

/* Deletes file by given path */
int DeleteFileByPath(Volume* v, const char* path)
{
	if(v == NULL || path == NULL)
	{
        return 0;
	}

    TextFile* f = FindFileByPath(v->root, path);

    if(!DeleteFile(v, f))
	{
		return 0;
	}

	return 1;
}

/* Checks if path is valid for file */
int IsValidFilePath(const char* path)
{
	if(path == NULL || strstr(path, "//") != NULL)
	{
		return 0;
	}

    char *pathClone = malloc(strlen(path) + 1);
    strcpy(pathClone, path);

    char* t = malloc(strlen(pathClone) + 1);

    pathClone = strtok(pathClone, "/");

    if(strcmp(pathClone, "root") != 0)
	{
		free(pathClone);
		free(t);
		return 0;
	}

	do
	{
		t = pathClone;
		pathClone = strtok(NULL, "/");

		if(!IsDirectory(t) && pathClone != NULL)
		{
			free(pathClone);
			free(t);
			return 0;
		}
	}while(pathClone != NULL);

	if(!IsFile(t))
	{
		free(pathClone);
		free(t);
		return 0;
	}

	free(pathClone);
	free(t);
	return 1;
}

/* Checks if path is valid for directory */
int IsValidDirectoryPath(const char* path)
{
	if(path == NULL || strstr(path, "//") != NULL)
	{
		return 0;
	}

    char *pathClone = malloc(strlen(path) + 1);
    strcpy(pathClone, path);

    pathClone = strtok(pathClone, "/");

    if(strcmp(pathClone, "root") != 0)
	{
		free(pathClone);
		return 0;
	}

	do
	{
		if(!IsDirectory(pathClone))
		{
			free(pathClone);
			return 0;
		}

		pathClone = strtok(NULL, "/");
	}while(pathClone != NULL);

	free(pathClone);
	return 1;
}

/* Checks if name is valid for file */
int IsFile(const char* name)
{
	if(name == NULL || name[0] == '.' || name[strlen(name) - 1] == '.')
	{
		return 0;
	}

	return strstr(name, ".") != NULL && strpbrk(name, FILE_INVALID_CHARACTERS) == NULL;
}

/* Checks if name is valid for directory */
int IsDirectory(const char* name)
{
	if(name == NULL)
	{
		return 0;
	}
	return strpbrk(name, DIRECTORY_INVALID_CHARACTERS) == NULL;
}

/* Finds file by path */
TextFile* FindFileByPath(Directory* root, const char* path)
{
	if(root == NULL || !IsValidFilePath(path))
	{
		return NULL;
	}

    char *last = strrchr(path, '/');
    char *dirPath = malloc(last - path + 1);
    strncpy(dirPath, path, last-path);
    dirPath[last-path] = '\0';
    last = last+1;

    Directory* parent = FindDirectoryByPath(root, dirPath);

    free(dirPath);
    return FindFileByNameAndParent(parent, last);
}

/* Finds directory by path */
Directory* FindDirectoryByPath(Directory* root, const char* path)
{
	if(root == NULL || !IsValidDirectoryPath(path))
	{
		return NULL;
	}

	char* pathClone = malloc(strlen(path) + 1);
    strcpy(pathClone, path);

    pathClone = strtok(pathClone, "/");
    pathClone = strtok(NULL, "/");

	Directory* current = root;

	while(pathClone != NULL)
	{
		current = FindDirectoryByNameAndParent(current, pathClone);
		pathClone = strtok(NULL, "/");
	}

	free(pathClone);
	return current;
}

/* Finds directory by parent directory and name */
Directory* FindDirectoryByNameAndParent(Directory* parent, const char* name)
{
	if(parent == NULL)
	{
		return NULL;
	}

	Directory* t = parent->subdirs;

    while(t != NULL)
	{
		if(strcmp(t->name, name) == 0)
		{
			return t;
		}

		t = t->next;
	}

	return NULL;
}

/* Finds file by parent directory and name */
TextFile* FindFileByNameAndParent(Directory* parent, const char* name)
{
	if(parent == NULL)
	{
		return NULL;
	}

	char* nameClone = malloc(strlen(name) + 1);
	strcpy(nameClone, name);
	const char* nameTok = strtok(nameClone, ".");
	if(nameTok == NULL)
	{
		free(nameClone);
		return NULL;
	}
	TextFile* t = parent->files;

    while(t != NULL)
	{
		if(strcmp(t->name, nameTok) == 0)
		{
			free(nameClone);
			return t;
		}

		t = t->next;
	}

	free(nameClone);
	return NULL;
}

/* Deletes given file */
int DeleteFile(Volume* v, TextFile* f)
{
	if(v == NULL  || f == NULL || f->dataClusters == NULL || f->parent == NULL || f->parent->files == NULL)
	{
		return 0;
	}

    if(!ClearData(v, f->dataClusters))
	{
		return 0;
	}
    v->clusterTable[f->dataClusters->id] = NULL;
    free(f->dataClusters);

    if(!RemoveEntrySpace(v, f->parent))
	{
		return 0;
	}

    OrganizeFileListAfterDeletion(f);

	free(f);

	return 1;
}

/* Cleans up list of subdirectories when deleting one */
void OrganizeSubdirectoryListAfterDeletion(Directory* d)
{
	if(d == NULL || d->parent == NULL)
	{
		return;
	}

	if(d->previous == NULL && d->next == NULL)
	{
        d->parent->subdirs = NULL;
	}
	else if(d->previous == NULL && d->next != NULL)
	{
		d->next->previous = NULL;
		d->parent->subdirs = d->next;
	}
	else if(d->previous != NULL && d->next == NULL)
	{
		d->previous->next = NULL;
	}
	else if(d->previous != NULL && d->next != NULL)
	{
		d->previous->next = d->next;
		d->next->previous = d->previous;
	}
}

/* Cleans up list of files when deleting one */
void OrganizeFileListAfterDeletion(TextFile* f)
{
	if(f == NULL || f->parent == NULL)
	{
		return;
	}

	if(f->previous == NULL && f->next == NULL)
	{
        f->parent->files = NULL;
	}
	else if(f->previous == NULL && f->next != NULL)
	{
		f->next->previous = NULL;
		f->parent->files = f->next;
	}
	else if(f->previous != NULL && f->next == NULL)
	{
		f->previous->next = NULL;
	}
	else if(f->previous != NULL && f->next != NULL)
	{
		f->previous->next = f->next;
		f->next->previous = f->previous;
	}

	f->previous = NULL;
	f->next = NULL;
	f->parent = NULL;
}

/* Volume initialization */
int InitializeVolume(Volume* v)
{
	if(v == NULL)
	{
		return 0;
	}
    v->root = (Directory*)calloc(1, sizeof(Directory));
    if(v->root == NULL)
	{
		return 0;
	}
    strcpy(v->root->name, "root");

    /* DEFAULT_CLUSTER_NUM clusters */

    v->clusterTable = (Cluster**)calloc(DEFAULT_CLUSTER_NUM, sizeof(Cluster*));
    if(v->clusterTable == NULL)
	{
		free(v->root);
		return 0;
	}

	v->clustersNum = DEFAULT_CLUSTER_NUM;

    v->clusterTable[0] = (Cluster*)calloc(1, sizeof(Cluster));
    if(v->clusterTable[0] == NULL)
	{
		free(v->root);
		free(v->clusterTable);
		v->clustersNum = 0;
		return 0;
	}
    v->clusterTable[0]->data[0] = '\0';
    v->root->dataClusters = v->clusterTable[0];

    if(!AddExampleEntries(v))
	{
		free(v->root);
		free(v->clusterTable);
		v->clustersNum = 0;
		return 0;
	}

    return 1;
}

/* Adding some entries to volume */
int AddExampleEntries(Volume* v)
{
	if(v == NULL)
	{
		return 0;
	}

	return
	AddDirectoryByPath(v, "root/Folder1") != NULL &&
	AddDirectoryByPath(v, "root/Folder2") != NULL &&
	AddDirectoryByPath(v, "root/Folder1/Folder1") != NULL &&
	AddDirectoryByPath(v, "root/Folder1/Folder2") != NULL &&
	AddDirectoryByPath(v, "root/Folder2/Folder1") != NULL &&
	AddDirectoryByPath(v, "root/Folder2/Folder2") != NULL &&
	AddDirectoryByPath(v, "root/Folder2/Folder3") != NULL &&
	AddDirectoryByPath(v, "root/Folder1/Folder2/Folder1") != NULL &&
	AddFileByPath(v, "root/File1.txt") != NULL &&
	AddFileByPath(v, "root/Folder1/File1.txt") != NULL &&
	AddDataToFileByPath(v, "root/Folder1/File1.txt", "abababaabababbababaabbaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") != 0 &&
	AddFileByPath(v, "root/Folder1/File2.txt") != NULL &&
	AddFileByPath(v, "root/Folder2/Folder1/File1.txt") != NULL;

}

/* Adds directory with given name to parent directory */
Directory* AddDirectory(Volume *v, Directory* parent, const char* name)
{
	if(v == NULL || parent == NULL || strlen(name) > NAME_SIZE)
	{
		return NULL;
	}
	Directory* last = FindLastInDirectoryList(parent->subdirs);
    Directory* create = CreateEmptyDirectory(v, name);
    if(create == NULL)
	{
		return NULL;
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

	create->parent = parent;

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
		return NULL;
	}

    return create;
}

/* Adds file with given name and extension to parent directory */
TextFile* AddFile(Volume *v, Directory* parent, const char* name, const char* extension)
{
	if(v == NULL || parent == NULL || strlen(name) > NAME_SIZE || strlen(extension) > EXTENSION_SIZE)
	{
		return NULL;
	}

	TextFile* last = FindLastInFileList(parent->files);
    TextFile* create = CreateEmptyFile(v, name, extension);
    if(create == NULL)
	{
		return NULL;
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

	create->parent = parent;

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
		return NULL;
	}

    return create;
}

/* Adds data to file */
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

	if(!ClearData(v, f->dataClusters))
	{
		return 0;
	}

    if(neededClusters == 1)
	{
		strcpy(f->dataClusters->data, data);
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

/* Clears data of file (given file's data cluster) */
int ClearData(Volume *v, Cluster*  dataCluster)
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

/* Adds data to file's cluster chain */
int AddDataToClusterChain(Volume* v, TextFile* f, const char* data, int neededClusters)
{
	if(v == NULL || f == NULL || data == NULL || neededClusters == 0)
	{
		return 0;
	}

	int i;

	Cluster* previous = f->dataClusters;

	for(i = 0; i < (neededClusters-1); i++)
	{
		previous->next = FindEmptyCluster(v);
		if(previous->next == NULL)
		{
			f->dataClusters->next = NULL;
			return 0;
		}
		previous->next->previous = previous;
		previous = previous->next;
	}

	char c = data[0];
	int n, m = 0;
	Cluster* current = f->dataClusters;

	while(c != '\0')
	{
		for(n = 0; n < CLUSTER_DATA_SIZE; n++, m++)
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

/* Checks if there are enough free cluster for given needed amount */
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

/* Returns number of needed clusters for given data */
int NumberOfNeededClusters(const char* data)
{
	if(data == NULL)
	{
		return 0;
	}

	int dataSize = strlen(data);

	if(dataSize % CLUSTER_DATA_SIZE == 0)
	{
		return dataSize / CLUSTER_DATA_SIZE;
	}

	return dataSize / CLUSTER_DATA_SIZE + 1;
}

/* Creates empty directory and returns pointer to it */
Directory* CreateEmptyDirectory(Volume *v, const char* name)
{
	if(v == NULL || strlen(name) > NAME_SIZE)
	{
		return NULL;
	}

	Directory* create = (Directory*)calloc(1,sizeof(Directory));
	if(create == NULL)
	{
		return NULL;
	}

    strcpy(create->name, name);

    create->dataClusters = FindEmptyCluster(v);

    if(create->dataClusters == NULL)
	{
		free(create);
		return NULL;
	}

	return create;
}

/* Creates empty file and returns pointer to it */
TextFile* CreateEmptyFile(Volume *v, const char* name, const char* extension)
{
	if(v == NULL || strlen(name) > NAME_SIZE || strlen(extension) > EXTENSION_SIZE)
	{
		return NULL;
	}

	TextFile* create = (TextFile*)calloc(1,sizeof(TextFile));
	if(create == NULL)
	{
		return NULL;
	}

    strcpy(create->name, name);
    strcpy(create->extension, extension);
    create->dataClusters = FindEmptyCluster(v);

    if(create->dataClusters == NULL)
	{
		free(create);
		return NULL;
	}

	return create;
}

/* Returns pointer to empty cluster */
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

	v->clusterTable[i] = (Cluster*)calloc(1,sizeof(Cluster));
	if(v->clusterTable[i] == NULL)
	{
		return NULL;
	}

	v->clusterTable[i]->id = i;
	v->clusterTable[i]->data[0] = '\0';

	return v->clusterTable[i];
}

/* Makes space for one more entry to parent directory entries */
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

		v->clusterTable[i] = (Cluster*)calloc(1,sizeof(Cluster));
		if(v->clusterTable[i] == NULL)
		{
			return 0;
		}
		v->clusterTable[i]->data[0] = '\0';
		v->clusterTable[i]->id = i;

		Cluster* last = FindLastInClusterList(parent->dataClusters);
		last->next = v->clusterTable[i];
		last->next->previous = last;
	}

	parent->entriesNum++;

	return 1;
}

/* Removes space for one entry from parent directory entries */
int RemoveEntrySpace(Volume* v, Directory* parent)
{
	if(v == NULL || parent == NULL || parent->entriesNum <= 0)
	{
		return 0;
	}

	if(IsLastClusterNeededAfterDeletingEntry(parent->entriesNum))
	{
		Cluster* last = FindLastInClusterList(parent->dataClusters);

		last->previous->next = NULL;
		v->clusterTable[last->id] = NULL;
		free(last);
	}

	parent->entriesNum--;

	return 1;
}

/* Returns last directory in list */
Directory* FindLastInDirectoryList(Directory* first)
{
	if(first == NULL)
	{
		return NULL;
	}

    Directory* t = first;

    while(t->next != NULL)
	{
		t = t->next;
	}

	return t;
}

/* Returns last file in list */
TextFile* FindLastInFileList(TextFile* first)
{
	if(first == NULL)
	{
		return NULL;
	}

    TextFile* t = first;

    while(t->next != NULL)
	{
		t = t->next;
	}

	return t;
}

/* Returns last cluster in list */
Cluster* FindLastInClusterList(Cluster* first)
{
	if(first == NULL)
	{
		return NULL;
	}

    Cluster* t = first;

    while(t->next != NULL)
	{
		t = t->next;
	}

	return t;
}

/* Checks if there is another cluster needed for one more entry with given number of previousEntries */
int IsAnotherClusterNeededForEntry(int entriesNum)
{
	if(entriesNum < 0)
	{
		entriesNum = 0;
	}

	return entriesNum % ENTRIES_PER_CLUSTER == 0;
}

/* Checks if there will be empty cluster after removing entry */
int IsLastClusterNeededAfterDeletingEntry(int entriesNum)
{
	if(entriesNum <= 1)
	{
		return 0;
	}

	return entriesNum % ENTRIES_PER_CLUSTER == 1;
}

/* Returns index of first empty cluster in clusterTable */
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

/* View structure tree of given directory */
void ViewStructureTree(Directory *d)
{
	if(d == NULL)
	{
		printf("\nDirectory does not exist\n");
	}

	if(d->files == NULL && d->subdirs == NULL)
	{
		printf("\nEmpty directory\n");
	}

	int startLevel = 0;
	printf("\n%s\n", d->name);

	ViewLevel(d, startLevel);

    printf("\n");
}

/* Recursively prints all files and subdirectories of given parent directory */
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

	Directory* t = d->subdirs;
	while(t != NULL)
	{
		Indent(level);
		printf("%s\n", t->name);
		ViewLevel(t, level+1);
		t = t->next;
	}

	TextFile* f = d->files;
	while(f != NULL)
	{
		Indent(level);
		printf("%s.%s\n", f->name, f->extension);
		f = f->next;
	}

}

/* Prints cool characters B| */
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

/* Prints contents of given file to console */
void ViewFileData(TextFile* f)
{
	if(f == NULL)
	{
		printf("\nFile does not exist\n");
		return;
	}
	if(f->dataClusters == NULL || strlen(f->dataClusters->data) == 0)
	{
		printf("\nFile is empty\n");
		return;
	}

	Cluster* current = f->dataClusters;

	printf("%s.%s file data:\n", f->name, f->extension);

	do
	{
		printf("%s", current->data);
		current = current->next;
	}while(current != NULL);

	printf("\n\n");
}