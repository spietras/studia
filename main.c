#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define VOLUME_NAME_SIZE 10
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
	char name[VOLUME_NAME_SIZE+1];
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

Volume* InitializeVolume();
Directory* AddDirectory(Volume*, Directory*, const char*);
Directory* FindLastInDirectoryList(Directory*);
int IsAnotherClusterNeededForEntry(const int);
int FindEmptyClusterIndex(Cluster**, const int);
void ViewStructureTree(const Directory*);
void Indent(const int);
Directory* CreateEmptyDirectory(Volume*, const char*);
int AddEntrySpace(Volume*, Directory*);
Cluster* FindEmptyCluster(Volume*);
void ViewLevel(const Directory*, int);
TextFile* FindLastInFileList(TextFile*);
TextFile* CreateEmptyFile(Volume*, const char*, const char*);
TextFile* AddFile(Volume*, Directory*, const char*, const char*);
int AddExampleEntries(Volume*);
int NumberOfNeededClusters(const char*);
int IsEnoughFreeClusters(Cluster**, const int, const int);
int AddDataToClusterChain(Volume*, TextFile*, const char*, int);
void ViewFileData(const TextFile*);
int AddDataToFile(Volume*, TextFile*, const char*);
int ClearData(Volume*, Cluster*);
int DeleteFile(Volume*, TextFile*);
int DeleteFileByPath(Volume*, const char*);
int IsFile(const char*);
Directory* FindDirectoryByNameAndParent(const Directory*, const char*);
TextFile* FindFileByNameAndParent(const Directory*, const char*);
int IsValidFilePath(const char*);
int IsDirectory(const char*);
int IsValidDirectoryPath(const char*);
Directory* FindDirectoryByPath(Directory*, const char*);
TextFile* FindFileByPath(Directory*, const char*);
void OrganizeFileListAfterDeletion(TextFile*);
void OrganizeSubdirectoryListAfterDeletion(Directory*);
int DeleteSingleEmptyDirectory(Volume*, Directory*);
int DeleteDirectoryTree(Volume*, Directory*);
int DeleteDirectoryByPath(Volume*, const char*);
TextFile* AddFileByPath(Volume*, const char*);
Directory* AddDirectoryByPath(Volume*, const char*);
int AddDataToFileByPath(Volume*, const char*, const char*);
void ViewFileDataByPath(Directory*, const char*);
void ViewStructureTreeByPath(Directory*, const char*);
Cluster* FindLastInClusterList(Cluster*);
int IsLastClusterNeededAfterDeletingEntry(const int);
int MoveFileToDirectoryByPaths(Volume*, const char*, const char*);
int MoveFileToDirectory(Volume*, TextFile*, Directory*);
int RemoveEntrySpace(Volume*, Directory*);
int MoveDirectoryToDirectoryByPaths(Volume*, const char*, const char*);
int MoveDirectoryToDirectory(Volume*, Directory*, Directory*);
int CopyFileToDirectory(Volume*, TextFile*, Directory*);
int CopyFileToDirectoryByPaths(Volume*, const char*, const char*);
Directory* CopySingleDirectoryToDirectory(Volume*, Directory*, Directory*);
int CopyDirectoryToDirectory(Volume*, Directory*, Directory*);
int CopyDirectoryToDirectoryByPaths(Volume*, const char*, const char*);
int IsDestinationDirectoryPathBelowInHierarchy(const char*, const char*);
int RenameFile(TextFile*, const char*);
int RenameFileByPath(Directory*, const char*, const char*);
int RenameDirectory(Directory*, const char*);
int RenameDirectoryByPath(Directory*, const char*, const char*);
int SaveClustersData(const Volume*, FILE*);
int SaveFAT(const Volume*, FILE*);
int GetOffset(const int, const int);
int SaveFileInfo(const TextFile*, FILE*, const int);
int SaveDirectoryInfo(const Directory*, FILE*, const int);
int SaveDirectoryEntries(const Directory*, FILE*, const int);
int SaveAllDirectoriesEntries(const Volume*, FILE*);
int Save(const Volume*, const char*);
int GetClustersNumber(const int);
int GetNextID(FILE*, const int);
int ReadDirectoryEntries(FILE*, Directory*, Cluster**, const int);
Volume* Load(const char*);
int ReadFile(FILE*, Directory*, Cluster**, const int);
int ReadDirectory(FILE*, Directory*, Cluster**, const int);
Volume* CreateVolume(const char*, int);
void ClearBuffer();
char* GetDirectoryPath(const char*);
int GetMenuChoice(const int, const int);
char* GetFilePath(const char*);
char* GetData();
char* GetFileName();
char* GetDirectoryName();
char* GetVolumeName();
int ShowMenu(Volume*);

int main()
{
    Volume* v = InitializeVolume();

    ShowMenu(v);

	return 0;
}

/* Shows menu and calls chosen functions */
int ShowMenu(Volume* v)
{
	while(1)
	{
		printf("\nChoose what to do:\n");
		printf("1. View all directories and files\n");
		printf("2. View specific directory and all its subdirectories and files\n");
		printf("3. View data of file\n");
		printf("4. Create empty file\n");
		printf("5. Add data to file\n");
		printf("6. Create empty directory\n");
		printf("7. Delete file\n");
		printf("8. Delete directory\n");
		printf("9. Move file\n");
		printf("10. Move directory\n");
		printf("11. Copy file\n");
		printf("12. Copy directory\n");
		printf("13. Rename file\n");
		printf("14. Rename directory\n");
		printf("15. Save volume to disk with custom name\n");
		printf("16. Exit (automatically saves volume to disk with current name)\n");

		int choice = GetMenuChoice(1, 16);

		switch(choice)
		{
		case 1:
			{
				ViewStructureTree(v->root);
				break;
			}
		case 2:
			{
				char* path = GetDirectoryPath("Write directory path to show");
				ViewStructureTreeByPath(v->root, path);
				free(path);
				break;
			}
		case 3:
			{
				char* path = GetFilePath("Write file path to show");
				ViewFileDataByPath(v->root, path);
				free(path);
				break;
			}
		case 4:
			{
				char* path = GetFilePath("Write file path to add");
				if(AddFileByPath(v, path) == NULL) printf("\nCan't add file\n");
				else printf("\nFile added successfully\n");
				free(path);
				break;
			}
		case 5:
			{
				char* path = GetFilePath("Write file path to add data to");
				char* data = GetData();
				if(!AddDataToFileByPath(v, path, data)) printf("\nCan't add data to that file\n");
				else printf("\nData added successfully\n");
				free(path);
				free(data);
				break;
			}
		case 6:
			{
				char* path = GetDirectoryPath("Write directory path to add");
				if(!AddDirectoryByPath(v, path)) printf("\nCan't add that directory\n");
				else printf("\nDirectory added successfully\n");
				free(path);
				break;
			}
		case 7:
			{
				char* path = GetFilePath("Write file path to delete");
				if(!DeleteFileByPath(v, path)) printf("\nCan't delete that file\n");
				else printf("\nFile deleted successfully\n");
				free(path);
				break;
			}
		case 8:
			{
				char* path = GetDirectoryPath("Write directory path to delete");
				if(!DeleteDirectoryByPath(v, path)) printf("\nCan't delete that directory\n");
				else printf("\nDirectory deleted successfully\n");
				free(path);
				break;
			}
		case 9:
			{
				char* fPath = GetFilePath("Write file path to move");
				char* dPath = GetDirectoryPath("Write directory path to move to");
				if(!MoveFileToDirectoryByPaths(v, fPath, dPath)) printf("\nCan't move that file to that directory\n");
				else printf("\nFile moved successfully");
				free(fPath);
				free(dPath);
				break;
			}
		case 10:
			{
				char* dirPath = GetDirectoryPath("Write directory path to move");
				char* destPath = GetDirectoryPath("Write directory path to move to");
				if(!MoveDirectoryToDirectoryByPaths(v, dirPath, destPath)) printf("\nCan't move that directory to that directory\n");
				else printf("\nDirectory moved successfully");
				free(dirPath);
				free(destPath);
				break;
			}
		case 11:
			{
				char* fPath = GetFilePath("Write file path to copy");
				char* dPath = GetDirectoryPath("Write directory path to copy to");
				if(!CopyFileToDirectoryByPaths(v, fPath, dPath)) printf("\nCan't copy that file to that directory\n");
				else printf("\nFile copied successfully");
				free(fPath);
				free(dPath);
				break;
			}
		case 12:
			{
				char* dirPath = GetDirectoryPath("Write directory path to copy");
				char* destPath = GetDirectoryPath("Write directory path to copy to");
				if(!CopyDirectoryToDirectoryByPaths(v, dirPath, destPath)) printf("\nCan't copy that directory to that directory\n");
				else printf("\nDirectory copied successfully");
				free(dirPath);
				free(destPath);
				break;
			}
		case 13:
			{
				char* path = GetFilePath("Write file path to rename");
				char* name = GetFileName();
				if(!RenameFileByPath(v->root, path, name)) printf("\n Can't rename that file\n");
				else printf("\nFile renamed successfully\n");
				free(path);
				free(name);
				break;
			}
		case 14:
			{
				char* path = GetDirectoryPath("Write directory path to rename");
				char* name = GetDirectoryName();
				if(!RenameDirectoryByPath(v->root, path, name)) printf("\nCan't rename that directory\n");
				else printf("\nDirectory renamed successfully");
				free(path);
				free(name);
				break;
			}
		case 15:
			{
				char* name = GetVolumeName();
				if(!Save(v, name)) printf("\nCan't save volume to disk\n");
				else printf("\nSuccessfully saved volume to disk\n");
				free(name);
				break;
			}
		case 16:
			{
				if(!Save(v, v->name)) printf("\nCan't save volume to disk\n");
				else return 1;
				break;
			}
		default:
			break;
		}
	}
}

/* Gets volume name from user */
char* GetVolumeName()
{
	int n;
	char* name = malloc(257);

	while(1)
	{
		printf("\nWrite volume name (max %d characters):\n", VOLUME_NAME_SIZE);

		n = scanf("%256s", name);

		if(n == 1 && strlen(name) <= VOLUME_NAME_SIZE)
		{
			ClearBuffer();
			return name;
		}

		printf("Invalid volume name. Write again");

		ClearBuffer();
	}
}

/* Gets directory name from user */
char* GetDirectoryName()
{
	int n;
	char* name = malloc(257);

	while(1)
	{
		printf("\nWrite directory name (max %d characters):\n", NAME_SIZE);

		n = scanf("%256s", name);

		if(n == 1 && IsDirectory(name))
		{
			ClearBuffer();
			return name;
		}

		printf("Invalid directory name. Write again");

		ClearBuffer();
	}
}

/* Gets file name from user */
char* GetFileName()
{

	int n;
	char* name = malloc(257);

	while(1)
	{
		printf("\nWrite file name (max %d characters):\n", NAME_SIZE);

		n = scanf("%256s", name);

		if(n == 1 && IsFile(name))
		{
			ClearBuffer();
			return name;
		}

		printf("Invalid file name. Write again");

		ClearBuffer();
	}
}

/* Gets data from user */
char* GetData()
{

	int n = 0;
	char* data = malloc(1);
	char c;

	printf("\nWrite data:\n");

	while((c = getchar()) != '\n')
	{
		n++;
		data = realloc(data, n);
		data[n-1] = c;
	}

	return data;
}

/* Gets file path from user */
char* GetFilePath(const char* message)
{

	int n;
	char* path = malloc(257);

	while(1)
	{
		printf("\n%s (for example: root/Folder1/File1.txt):\n", message);

		n = scanf("%256s", path);

		if(n == 1 && IsValidFilePath(path))
		{
			ClearBuffer();
			return path;
		}

		printf("Invalid file path. Write again");

		ClearBuffer();
	}
}

/* Gets menu choice from user */
int GetMenuChoice(const int min, const int max)
{

	int choice, n;

	while(1)
	{
		printf("\nYour choice: ");
		n = scanf("%d", &choice);

		if(n == 1 && (choice >= min && choice <= max))
		{
			ClearBuffer();
			return choice;
		}

		printf("\nInvalid choice. Your choice must be from rang %d - %d. Choose again\n", min, max);
		ClearBuffer();
	}
}

/* Gets directory path from user */
char* GetDirectoryPath(const char* message)
{

	int n;
	char* path = malloc(257);

	while(1)
	{
		printf("\n%s (for example: root/Folder1/Folder2):\n", message);

		n = scanf("%256s", path);

		if(n == 1 && IsValidDirectoryPath(path))
		{
			ClearBuffer();
			return path;
		}

		printf("Invalid directory path. Write again");

		ClearBuffer();
	}
}

/* Clears buffer */
void ClearBuffer()
{
	while(getchar() != '\n');
}

/* Creates empty volume */
Volume* CreateVolume(const char* name, const int size)
{
	if(name == NULL || strlen(name) > VOLUME_NAME_SIZE || size > MAX_VOLUME_SIZE) return NULL;

	Volume* v = (Volume*)calloc(1, sizeof(Volume));

	strcpy(v->name, name);

    v->root = (Directory*)calloc(1, sizeof(Directory));
    if(v->root == NULL)
	{
		free(v);
		return NULL;
	}
    strcpy(v->root->name, "root");

    v->clustersNum = size / CLUSTER_DATA_SIZE;

    v->clusterTable = (Cluster**)calloc(v->clustersNum, sizeof(Cluster*));
    if(v->clusterTable == NULL)
	{
		free(v->root);
		free(v);
		return NULL;
	}

    v->clusterTable[0] = (Cluster*)calloc(1, sizeof(Cluster));
    if(v->clusterTable[0] == NULL)
	{
		free(v->root);
		free(v->clusterTable);
		free(v);
		return NULL;
	}
    v->clusterTable[0]->data[0] = '\0';
    v->root->dataClusters = v->clusterTable[0];

    return v;
}

/* Loads volume from disk by name */
Volume* Load(const char* name)
{
    if(name == NULL || strlen(name) > VOLUME_NAME_SIZE)
	{
		return NULL;
	}

	char* fullName = malloc(strlen(name) + 1 + 3 + 1);
	strcpy(fullName, name);
	strcat(fullName, ".bin");

    FILE* volumeFile = fopen(fullName, "rb");

    if(volumeFile == NULL)
	{
		free(fullName);
		return NULL;
	}

	fseek(volumeFile, 0, SEEK_END);
	int size = ftell(volumeFile);
	fseek(volumeFile, 0, SEEK_SET);

	if(size > MAX_VOLUME_SIZE)
	{
		fclose(volumeFile);
		free(fullName);
		return NULL;
	}

	Volume* v = (Volume*)calloc(1, sizeof(Volume));
	if(v == NULL)
	{
		fclose(volumeFile);
		free(fullName);
		return NULL;
	}
	strcpy(v->name, name);

	v->clustersNum = GetClustersNumber(size);

	v->clusterTable = (Cluster**)calloc(v->clustersNum, sizeof(Cluster*));
	if(v->clusterTable == NULL)
	{
		fclose(volumeFile);
		free(fullName);
		free(v);
		return NULL;
	}

	v->root = (Directory*)calloc(1, sizeof(Directory));
	if(v->root == NULL)
	{
		fclose(volumeFile);
		free(fullName);
		free(v->clusterTable);
		free(v);
		return NULL;
	}
	strcpy(v->root->name, "root");

	v->root->dataClusters = v->clusterTable[0] = (Cluster*)calloc(1, sizeof(Cluster));

	if(!ReadDirectoryEntries(volumeFile, v->root, v->clusterTable, v->clustersNum))
	{
		fclose(volumeFile);
		free(fullName);
		free(v->clusterTable);
		free(v->root);
		free(v);
		return NULL;
	}

	fclose(volumeFile);
	return v;

}

/* Reads info about all files and directories from volumeFile and creates every file and directory */
int ReadDirectoryEntries(FILE* volumeFile, Directory* d, Cluster** clusterTable, const int clusterNum)
{
	if(volumeFile == NULL || d == NULL || clusterTable == NULL || clusterNum <= 0) return 0;

	int i = 0;
	int currentId = d->dataClusters->id;
	int beginOffset;
	char* extension = malloc(EXTENSION_SIZE+1);

	while(1)
	{

		if(i == ENTRIES_PER_CLUSTER)
		{
			if((currentId = GetNextID(volumeFile, currentId)) == -1)
			{
				break;
			}

			i = 0;
		}

		beginOffset = GetOffset(clusterNum, currentId);
		if(fseek(volumeFile, beginOffset + i*ENTRY_DATA_SIZE, SEEK_SET))
		{
			free(extension);
			return 0;
		}
		int a = fgetc(volumeFile);
		if(fseek(volumeFile, -1, SEEK_CUR))
		{
			free(extension);
			return 0;
		}
		if(a == 0)
		{
			break;
		}

		if(fseek(volumeFile, NAME_SIZE, SEEK_CUR))
		{
			free(extension);
			return 0;
		}
		if(fgets(extension, EXTENSION_SIZE+1, volumeFile) == NULL)
		{
			free(extension);
			return 0;
		}

		if(strcmp(extension, "dir") == 0)
		{
			if(!ReadDirectory(volumeFile, d, clusterTable, clusterNum))
			{
				free(extension);
				return 0;
			}
			i++;
		}
		else
		{
			if(!ReadFile(volumeFile, d, clusterTable, clusterNum))
			{
				free(extension);
				return 0;
			}
			i++;
		}
	}

	Directory* currentDir = d->subdirs;

	while(currentDir != NULL)
	{
		if(!ReadDirectoryEntries(volumeFile, currentDir, clusterTable, clusterNum))
		{
			free(extension);
			return 0;
		}
		currentDir = currentDir->next;
	}

	free(extension);
	return 1;
}

/* Reads info about file from volumeFile and creates that file */
int ReadFile(FILE* volumeFile, Directory* parent, Cluster** clusterTable, const int clusterNum)
{
	if(volumeFile == NULL || parent == NULL || clusterTable == NULL || clusterNum <= 0) return 0;

	TextFile* rf = (TextFile*)calloc(1, sizeof(TextFile));
	if(rf == NULL) return 0;

	if(fseek(volumeFile, -(NAME_SIZE+EXTENSION_SIZE), SEEK_CUR))
	{
		free(rf);
		return 0;
	}
	if(fgets(rf->name, NAME_SIZE+1, volumeFile) == NULL)
	{
		free(rf);
		return 0;
	}

	if(fgets(rf->extension, EXTENSION_SIZE+1, volumeFile) == NULL)
	{
		free(rf);
		return 0;
	}

	int ID;
	if(fread(&ID, sizeof(ID), 1, volumeFile) != 1)
	{
		free(rf);
		return 0;
	}

	clusterTable[ID] = (Cluster*)calloc(1, sizeof(Cluster));
	if(clusterTable[ID] == NULL)
	{
		free(rf);
		return 0;
	}
	clusterTable[ID]->id = ID;

	rf->dataClusters = clusterTable[ID];
	Cluster* previousCluster = rf->dataClusters;
	Cluster* currentCluster = previousCluster;

	if(fseek(volumeFile, GetOffset(clusterNum, currentCluster->id), SEEK_SET))
	{
		free(rf);
		return 0;
	}
	if(fgets(currentCluster->data, CLUSTER_DATA_SIZE+1, volumeFile) == NULL)
	{
		free(rf);
		return 0;
	}

	while((ID = GetNextID(volumeFile, ID)) != -1)
	{
		currentCluster = clusterTable[ID] = (Cluster*)calloc(1, sizeof(Cluster));
		if(currentCluster == NULL)
		{
			free(rf);
			return 0;
		}
		currentCluster->id = ID;

		if(fseek(volumeFile, GetOffset(clusterNum, currentCluster->id), SEEK_SET))
		{
			free(rf);
			return 0;
		}
		if(fgets(currentCluster->data, CLUSTER_DATA_SIZE+1, volumeFile) == NULL)
		{
			free(rf);
			return 0;
		}

		previousCluster->next = currentCluster;
		previousCluster = currentCluster;
	}

	if(parent->files == NULL)
	{
		parent->files = rf;
	}
	else
	{
		TextFile* last = FindLastInFileList(parent->files);
		last->next = rf;
		rf->previous = last;
	}

	rf->parent = parent;

	parent->entriesNum++;

	return 1;
}

/* Reads info about directory from volumeFile and creates that directory */
int ReadDirectory(FILE* volumeFile, Directory* parent, Cluster** clusterTable, const int clusterNum)
{
	if(volumeFile == NULL || parent == NULL || clusterTable == NULL || clusterNum <= 0) return 0;

	Directory* rd = (Directory*)calloc(1, sizeof(Directory));
	if(rd == NULL) return 0;

	if(fseek(volumeFile, -(NAME_SIZE+EXTENSION_SIZE), SEEK_CUR))
	{
		free(rd);
		return 0;
	}
	if(fgets(rd->name, NAME_SIZE+1, volumeFile) == NULL)
	{
		free(rd);
		return 0;
	}

	if(fseek(volumeFile, EXTENSION_SIZE, SEEK_CUR))
	{
		free(rd);
		return 0;
	}

	int ID;
	if(fread(&ID, sizeof(ID), 1, volumeFile) != 1)
	{
		free(rd);
		return 0;
	}

	clusterTable[ID] = (Cluster*)calloc(1, sizeof(Cluster));
	if(clusterTable[ID] == NULL)
	{
		free(rd);
		return 0;
	}
	clusterTable[ID]->id = ID;

	rd->dataClusters = clusterTable[ID];
	Cluster* previousCluster = rd->dataClusters;
	Cluster* currentCluster;

	while((ID = GetNextID(volumeFile, ID)) != -1)
	{
		currentCluster = clusterTable[ID] = (Cluster*)calloc(1, sizeof(Cluster));
		if(currentCluster == NULL)
		{
			free(rd);
			return 0;
		}
		currentCluster->id = ID;

		previousCluster->next = currentCluster;
		previousCluster = currentCluster;
	}

	if(parent->subdirs == NULL)
	{
		parent->subdirs = rd;
	}
	else
	{
		Directory* last = FindLastInDirectoryList(parent->subdirs);
		last->next = rd;
		rd->previous = last;
	}

	rd->parent = parent;

	parent->entriesNum++;

	return 1;
}

/* Gets ID of next cluster from FAT */
int GetNextID(FILE* volumeFile, const int clusterID)
{
	if(volumeFile == NULL || clusterID > MAX_CLUSTER_NUM || clusterID < 0) return -1;

	int pos = clusterID * sizeof(int);

	if(fseek(volumeFile, pos, SEEK_SET)) return -1;

	int value;
	if(fread(&value, sizeof(value), 1, volumeFile) != 1) return -1;

	return value;
}

/* Gets number of clusters by volumeSize */
int GetClustersNumber(const int volumeSize)
{
	if(volumeSize > MAX_VOLUME_SIZE || volumeSize < 0) return -1;

	return volumeSize / (sizeof(int) + CLUSTER_DATA_SIZE);
}

/* Saves volume to disk with name */
int Save(const Volume* v, const char* name)
{
	if(v == NULL || name == NULL || strlen(name) > VOLUME_NAME_SIZE) return 0;

	char* fullName = malloc(strlen(name) + 1 + 3 + 1);
	strcpy(fullName, name);
	strcat(fullName, ".bin");

    FILE* volumeFile = fopen(fullName, "wb");

    if(volumeFile == NULL)
	{
		free(fullName);
		return 0;
	}

	/*
		First save FAT:
			for each item in clusterTable
				if it's NULL, save 0
				else if it's next cluster is NULL, save  -1
				else save next cluster's id
		Then save clusters data:
			for each item in clusterTable
				if it's NULL save CLUSTER_DATA_SIZE byte zeros
				else if it has data (strlen(data) > 0) save all its data (without null terminator) and fill with zeros to CLUSTER_DATA_SIZE
			then starting with root recursively save all subdirectories entries
				for each entry save its name, extension ("dir" for directory) and first cluster id (fill name and extension with zeros to NAME_SIZE and EXTENSION_SIZE)
				save ENTRIES_PER_CLUSTER entries in one cluster and move to next one if there is next one
	*/

	if(!SaveFAT(v, volumeFile))
	{
		fclose(volumeFile);
		remove(fullName);
		free(fullName);
		return 0;
	}

	if(!SaveClustersData(v, volumeFile))
	{
		fclose(volumeFile);
		remove(fullName);
		free(fullName);
		return 0;
	}

	if(!SaveAllDirectoriesEntries(v, volumeFile))
	{
		fclose(volumeFile);
		remove(fullName);
		free(fullName);
		return 0;
	}

	fclose(volumeFile);
	free(fullName);
	return 1;
}

/* Saves info about all directories and files to volumeFile */
int SaveAllDirectoriesEntries(const Volume* v, FILE* volumeFile)
{
	if(v == NULL || v->clustersNum <= 0 || v->clusterTable == NULL || v->root == NULL || volumeFile == NULL) return 0;

	if(!SaveDirectoryEntries(v->root, volumeFile, v->clustersNum)) return 0;

	return 1;
}

/* Saves info about all entries of directory d to volumeFile */
int SaveDirectoryEntries(const Directory* d, FILE* volumeFile, const int clustersNum)
{
	if(d == NULL || volumeFile == NULL || clustersNum <= 0) return 0;

	int i = 0;
	Cluster* currentCluster = d->dataClusters;

	Directory* currentDir = d->subdirs;

	while(currentDir != NULL)
	{
		if(!SaveDirectoryInfo(currentDir, volumeFile, GetOffset(clustersNum, currentCluster->id) + i*ENTRY_DATA_SIZE)) return 0;

		i++;
		if(i == ENTRIES_PER_CLUSTER)
		{
			i = i % ENTRIES_PER_CLUSTER;
			currentCluster = currentCluster->next;
		}
		currentDir = currentDir->next;
	}

	TextFile* currentFile = d->files;

	while(currentFile != NULL)
	{
		if(!SaveFileInfo(currentFile, volumeFile, GetOffset(clustersNum, currentCluster->id) + i*ENTRY_DATA_SIZE)) return 0;

		i++;
		if(i == ENTRIES_PER_CLUSTER)
		{
			i = i % ENTRIES_PER_CLUSTER;
			currentCluster = currentCluster->next;
		}
		currentFile = currentFile->next;
	}

	currentDir = d->subdirs;

	while(currentDir != NULL)
	{
		if(!SaveDirectoryEntries(currentDir, volumeFile, clustersNum)) return 0;

		currentDir = currentDir->next;
	}

	return 1;
}

/* Saves info about directory d to volumeFile at position */
int SaveDirectoryInfo(const Directory* d, FILE* volumeFile, const int position)
{
	if(d == NULL || d->name == NULL || d->dataClusters == NULL || volumeFile == NULL || position <= 0) return 0;

	int i;
	if(fseek(volumeFile, position, SEEK_SET)) return 0;

	if(fputs(d->name, volumeFile) == EOF) return 0;

	for(i = strlen(d->name); i < NAME_SIZE; i++)
	{
		if(fputc(0, volumeFile) == EOF) return 0;
	}

	if(fputs("dir", volumeFile) == EOF) return 0;

	for(i = strlen("dir"); i < EXTENSION_SIZE; i++)
	{
		if(fputc(0, volumeFile) == EOF) return 0;
	}

	if(fwrite(&(d->dataClusters->id), sizeof(d->dataClusters->id), 1, volumeFile) != 1) return 0;

	return 1;
}

/* Saves info about file f to volumeFile at position */
int SaveFileInfo(const TextFile* f, FILE* volumeFile, const int position)
{
	if(f == NULL || f->name == NULL || f->extension == NULL || f->dataClusters == NULL || volumeFile == NULL || position <= 0) return 0;

	int i;
	if(fseek(volumeFile, position, SEEK_SET)) return 0;

	if(fputs(f->name, volumeFile) == EOF) return 0;

	for(i = strlen(f->name); i < NAME_SIZE; i++)
	{
		if(fputc(0, volumeFile) == EOF) return 0;
	}

	if(fputs(f->extension, volumeFile) == EOF) return 0;

	for(i = strlen(f->extension); i < EXTENSION_SIZE; i++)
	{
		if(fputc(0, volumeFile) == EOF) return 0;
	}

	if(fwrite(&(f->dataClusters->id), sizeof(f->dataClusters->id), 1, volumeFile) != 1) return 0;

	return 1;
}

/* Gets offset of cluster by its ID */
int GetOffset(const int clustersNum, const int clusterID)
{
	if(clustersNum > MAX_CLUSTER_NUM || clusterID > MAX_CLUSTER_NUM || clustersNum <= 0 || clusterID < 0) return -1;

    return clustersNum * sizeof(int) + clusterID * CLUSTER_DATA_SIZE;
}

/* Saves File Allocation Table to volumeFile */
int SaveFAT(const Volume* v, FILE* volumeFile)
{
	if(v == NULL || volumeFile == NULL || v->clusterTable == NULL || v->clustersNum <= 0) return 0;

	int i;
	int z = 0;
	int end = -1;
	int id;

	for(i = 0; i < v->clustersNum; i++)
	{
		if(v->clusterTable[i] == NULL)
		{
			if(fwrite(&z, sizeof(z), 1, volumeFile) != 1) return 0;
		}
		else if(v->clusterTable[i]->next == NULL)
		{
			if(fwrite(&end, sizeof(end), 1, volumeFile) != 1) return 0;
		}
		else
		{
			id = v->clusterTable[i]->next->id;
			if(fwrite(&id, sizeof(id), 1, volumeFile) != 1) return 0;
		}
	}

	return 1;
}

/* Saves all clusters to volumeFile */
int SaveClustersData(const Volume* v, FILE* volumeFile)
{
	if(v == NULL || volumeFile == NULL || v->clusterTable == NULL || v->clustersNum <= 0) return 0;

	int i, j;

	for(i = 0; i < v->clustersNum; i++)
	{
		if(v->clusterTable[i] == NULL || strlen(v->clusterTable[i]->data) == 0)
		{
			for(j = 0; j < CLUSTER_DATA_SIZE; j++)
			{
				if(fputc(0, volumeFile) == EOF) return 0;
			}
		}
		else
		{
			if(fputs(v->clusterTable[i]->data, volumeFile) == EOF) return 0;

			j = strlen(v->clusterTable[i]->data);

			for(; j < CLUSTER_DATA_SIZE; j++)
			{
				if(fputc(0, volumeFile) == EOF) return 0;
			}
		}
	}

	return 1;
}

/* Sets directory with given path name to newName */
int RenameDirectoryByPath(Directory* root, const char* path, const char* newName)
{
	if(path == NULL || newName == NULL || !IsValidDirectoryPath(path)) return 0;

	Directory* d = FindDirectoryByPath(root, path);

	if(d == NULL || d == root) return 0;

	return RenameDirectory(d, newName);
}

/* Sets directory d name to newName */
int RenameDirectory(Directory* d, const char* newName)
{
	if(d == NULL || newName == NULL || strlen(newName) > NAME_SIZE) return 0;

	if(FindDirectoryByNameAndParent(d->parent, newName) != NULL) return 0;

	strcpy(d->name, newName);

	return 1;
}

/* Sets file with given path name to newName */
int RenameFileByPath(Directory* root, const char* path, const char* newName)
{
	if(path == NULL || newName == NULL || !IsValidFilePath(path)) return 0;

	TextFile* f = FindFileByPath(root, path);

	if(f == NULL) return 0;

	return RenameFile(f, newName);
}

/* Sets file f name to newName */
int RenameFile(TextFile* f, const char* newName)
{
    if(f == NULL || newName == NULL || strlen(newName) > NAME_SIZE) return 0;

	char* fullName = malloc(strlen(newName) + 1 + strlen(f->extension) + 1);
	strcpy(fullName, newName);
	strcat(fullName, ".");
	strcat(fullName, f->extension);

	if(FindFileByNameAndParent(f->parent, fullName) != NULL)
	{
		free(fullName);
		return 0;
	}

	strcpy(f->name, newName);

	free(fullName);
	return 1;
}

/* Checks if directory with destPath is below in hierarchy than directory with dirPath */
int IsDestinationDirectoryPathBelowInHierarchy(const char* dirPath, const char* destPath)
{
	if(!IsValidDirectoryPath(dirPath) || !IsValidDirectoryPath(destPath)) return 0;

	if(strstr(destPath, dirPath) == NULL) return 0;

	if(strlen(destPath) >= strlen(dirPath)) return 0;

	return 1;
}

/* Copies directory (and all within it)  with given path to directory with given path */
int CopyDirectoryToDirectoryByPaths(Volume* v, const char* dirPath, const char* destPath)
{
	if(v == NULL || !IsValidDirectoryPath(dirPath) || !IsValidDirectoryPath(destPath) || IsDestinationDirectoryPathBelowInHierarchy(dirPath, destPath)) return 0;

	Directory* d = FindDirectoryByPath(v->root, dirPath);
	if(d == NULL) return 0;

	Directory* dest = FindDirectoryByPath(v->root, destPath);
	if(dest == NULL) return 0;

	if(dest == d->parent) return 0;

	if(!CopyDirectoryToDirectory(v, d, dest)) return 0;

	return 1;
}

/* Copies recursively directory d and all its files and subdirectories to directory destination */
int CopyDirectoryToDirectory(Volume* v, Directory* d, Directory* destination)
{
	if(v == NULL || d == NULL || destination == NULL) return 0;

	if(FindDirectoryByNameAndParent(destination, d->name) != NULL) return 0;

	Directory* copy = CopySingleDirectoryToDirectory(v, d, destination);

	if(copy == NULL) return 0;

    TextFile* currentFile = d->files;

    while(currentFile != NULL)
	{
		if(!CopyFileToDirectory(v, currentFile, copy))
		{
			DeleteSingleEmptyDirectory(v, copy);
			return 0;
		}
		currentFile = currentFile->next;
	}

	Directory* currentDir = d->subdirs;

	while(currentDir != NULL)
	{
		if(!CopyDirectoryToDirectory(v, currentDir, copy))
		{
			DeleteDirectoryTree(v, copy);
			return 0;
		}
		currentDir = currentDir->next;
	}

	return 1;
}

/* Copies single directory d to directory destination */
Directory* CopySingleDirectoryToDirectory(Volume* v, Directory* d, Directory* destination)
{
	if(v == NULL || d == NULL || destination == NULL) return NULL;

	if(FindDirectoryByNameAndParent(destination, d->name) != NULL) return NULL;

	if(!AddEntrySpace(v, destination)) return NULL;

	Directory* copy = AddDirectory(v, destination, d->name);

	if(copy == NULL)
	{
		DeleteSingleEmptyDirectory(v, copy);
		RemoveEntrySpace(v, destination);
		return NULL;
	}

	return copy;
}

/* Copies file with given path to directory with given path */
int CopyFileToDirectoryByPaths(Volume* v, const char* fPath, const char* dirPath)
{
	if(v == NULL || !IsValidFilePath(fPath) || !IsValidDirectoryPath(dirPath)) return 0;

	TextFile* f = FindFileByPath(v->root, fPath);
	if(f == NULL) return 0;

	Directory* d = FindDirectoryByPath(v->root, dirPath);
	if(d == NULL) return 0;

	if(!CopyFileToDirectory(v, f, d)) return 0;

	return 1;
}

/* Copies file f to directory d */
int CopyFileToDirectory(Volume* v, TextFile* f, Directory* d)
{
	if(v == NULL || f == NULL || d == NULL) return 0;

	char* fullName = malloc(strlen(f->name) + 1 + strlen(f->extension) + 1);
	strcpy(fullName, f->name);
	strcat(fullName, ".");
	strcat(fullName, f->extension);

	if(FindFileByNameAndParent(d, fullName) != NULL)
	{
		free(fullName);
		return 0;
	}

	if(!AddEntrySpace(v, d))
	{
		free(fullName);
		return 0;
	}

	TextFile* copy = AddFile(v, d, f->name, f->extension);

	if(copy == NULL)
	{
		RemoveEntrySpace(v, d);
		free(fullName);
		return 0;
	}

	int i = 1;
	Cluster* current = f->dataClusters;
	char* copyData = malloc(1);
	copyData[0] = '\0';

	do
	{
		copyData = realloc(copyData, i*CLUSTER_DATA_SIZE + 1);
		strcat(copyData, current->data);
		current = current->next;
		i++;
	}while(current != NULL);

	if(!AddDataToFile(v, copy, copyData))
	{
		RemoveEntrySpace(v, d);
		free(fullName);
		free(copyData);
		return 0;
	}

	free(fullName);
	free(copyData);
	return 1;
}

/* Moves directory with given path to directory with given path */
int MoveDirectoryToDirectoryByPaths(Volume* v, const char* dirPath, const char* destPath)
{
	if(v == NULL || !IsValidDirectoryPath(dirPath) || !IsValidDirectoryPath(destPath) || IsDestinationDirectoryPathBelowInHierarchy(dirPath, destPath)) return 0;

	Directory* d = FindDirectoryByPath(v->root, dirPath);
	if(d == NULL) return 0;

	Directory* dest = FindDirectoryByPath(v->root, destPath);
	if(dest == NULL) return 0;

	if(dest == d->parent) return 0;

	if(!MoveDirectoryToDirectory(v, d, dest)) return 0;

	return 1;
}

/* Moves directory d to directory destination */
int MoveDirectoryToDirectory(Volume* v, Directory* d, Directory* destination)
{
	if(v == NULL || d == NULL || destination == NULL) return 0;

	if(FindDirectoryByNameAndParent(destination, d->name) != NULL) return 0;

	if(!AddEntrySpace(v, destination)) return 0;

	if(!RemoveEntrySpace(v, d->parent))
	{
		RemoveEntrySpace(v, destination);
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
int MoveFileToDirectoryByPaths(Volume* v, const char* fPath, const char* dirPath)
{
	if(v == NULL || !IsValidFilePath(fPath) || !IsValidDirectoryPath(dirPath)) return 0;

	TextFile* f = FindFileByPath(v->root, fPath);
	if(f == NULL) return 0;

	Directory* d = FindDirectoryByPath(v->root, dirPath);
	if(d == NULL) return 0;

	if(!MoveFileToDirectory(v, f, d)) return 0;

	return 1;
}

/* Moves file f to directory d */
int MoveFileToDirectory(Volume* v, TextFile* f, Directory* d)
{
	if(v == NULL || f == NULL || d == NULL) return 0;

    char* fullName = malloc(strlen(f->name) + 1 + strlen(f->extension) + 1);
	strcpy(fullName, f->name);
	strcat(fullName, ".");
	strcat(fullName, f->extension);

	if(FindFileByNameAndParent(d, fullName) != NULL)
	{
		free(fullName);
		return 0;
	}

    if(!AddEntrySpace(v, d))
	{
		free(fullName);
		return 0;
	}

	if(!RemoveEntrySpace(v, f->parent))
	{
		RemoveEntrySpace(v, d);
		free(fullName);
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

	free(fullName);
	return 1;
}

/* Copies list of clusters */
Cluster* CopyClusterList(const Cluster* first)
{
	if(first == NULL) return NULL;

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
	if(v == NULL || !IsValidFilePath(path) || data == NULL) return 0;

	TextFile *f = FindFileByPath(v->root, path);
	if(f == NULL) return 0;

	return AddDataToFile(v, f, data);
}

/* Adds directory with given path */
Directory* AddDirectoryByPath(Volume* v, const char* path)
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
				free(pathClone);
				return NULL;
			}
		}

		t = current;
		cName = strtok(NULL, "/");
	}

	free(pathClone);
	return current;
}

/* Adds file with given path */
TextFile* AddFileByPath(Volume* v, const char* path)
{
	if(v == NULL || !IsValidFilePath(path)) return 0;

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
				free(pathClone);
				return NULL;
			}
		}

		t = current;
		cName = strtok(NULL, "/");
	}

	char* name = strtok(cName, ".");
	char* ext = strtok(NULL, ".");

	TextFile* f = AddFile(v, current, name, ext);

	free(pathClone);

	return f;
}

/* Delete directory (and all subdirectories and files within it) with given path */
int DeleteDirectoryByPath(Volume* v, const char* path)
{
	if(v == NULL || path == NULL) return 0;

    Directory* d = FindDirectoryByPath(v->root, path);

    if(!DeleteDirectoryTree(v, d)) return 0;

	return 1;
}

/* Deletes recursively all subdirectories and files within given directory (and that directory) */
int DeleteDirectoryTree(Volume*v, Directory* d)
{
	if(v == NULL || d == NULL) return 0;

	while(d->subdirs != NULL)
	{
		if(!DeleteDirectoryTree(v, d->subdirs)) return 0;
	}

	while(d->files != NULL)
	{
		if(!DeleteFile(v, d->files)) return 0;
	}

	if(!DeleteSingleEmptyDirectory(v, d)) return 0;

	return 1;
}

/* Deletes d directory (with no subdirectories) */
int DeleteSingleEmptyDirectory(Volume* v, Directory* d)
{
	if(v == NULL  || d == NULL || d->dataClusters == NULL || d->parent == NULL || d->parent->subdirs == NULL || d->subdirs != NULL) return 0;

    if(!ClearData(v, d->dataClusters)) return 0;

    v->clusterTable[d->dataClusters->id] = NULL;
    free(d->dataClusters);

    if(!RemoveEntrySpace(v, d->parent)) return 0;

    OrganizeSubdirectoryListAfterDeletion(d);

	free(d);

	return 1;
}

/* Deletes file by given path */
int DeleteFileByPath(Volume* v, const char* path)
{
	if(v == NULL || path == NULL) return 0;

    TextFile* f = FindFileByPath(v->root, path);

    if(!DeleteFile(v, f)) return 0;

	return 1;
}

/* Checks if path is valid for file */
int IsValidFilePath(const char* path)
{
	if(path == NULL || strstr(path, "//") != NULL) return 0;

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
	if(path == NULL || strstr(path, "//") != NULL) return 0;

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
	if(name == NULL || name[0] == '.' || name[strlen(name) - 1] == '.') return 0;

	return strstr(name, ".") != NULL && strpbrk(name, FILE_INVALID_CHARACTERS) == NULL;
}

/* Checks if name is valid for directory */
int IsDirectory(const char* name)
{
	if(name == NULL) return 0;

	return strpbrk(name, DIRECTORY_INVALID_CHARACTERS) == NULL;
}

/* Finds file by path */
TextFile* FindFileByPath(Directory* root, const char* path)
{
	if(root == NULL || !IsValidFilePath(path)) return NULL;

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
	if(root == NULL || !IsValidDirectoryPath(path)) return NULL;

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
Directory* FindDirectoryByNameAndParent(const Directory* parent, const char* name)
{
	if(parent == NULL) return NULL;

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
TextFile* FindFileByNameAndParent(const Directory* parent, const char* name)
{
	if(parent == NULL) return NULL;

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
	if(v == NULL  || f == NULL || f->dataClusters == NULL || f->parent == NULL || f->parent->files == NULL) return 0;

    if(!ClearData(v, f->dataClusters)) return 0;

    v->clusterTable[f->dataClusters->id] = NULL;
    free(f->dataClusters);

    if(!RemoveEntrySpace(v, f->parent)) return 0;

    OrganizeFileListAfterDeletion(f);

	free(f);

	return 1;
}

/* Cleans up list of subdirectories when deleting one */
void OrganizeSubdirectoryListAfterDeletion(Directory* d)
{
	if(d == NULL || d->parent == NULL) return;

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
	if(f == NULL || f->parent == NULL) return;

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
Volume* InitializeVolume()
{

	Volume* v = (Volume*)calloc(1, sizeof(Volume));

	strcpy(v->name, "vol");

    v->root = (Directory*)calloc(1, sizeof(Directory));
    if(v->root == NULL)
	{
		free(v);
		return NULL;
	}
    strcpy(v->root->name, "root");

    /* DEFAULT_CLUSTER_NUM clusters */

    v->clusterTable = (Cluster**)calloc(DEFAULT_CLUSTER_NUM, sizeof(Cluster*));
    if(v->clusterTable == NULL)
	{
		free(v->root);
		free(v);
		return NULL;
	}

	v->clustersNum = DEFAULT_CLUSTER_NUM;

    v->clusterTable[0] = (Cluster*)calloc(1, sizeof(Cluster));
    if(v->clusterTable[0] == NULL)
	{
		free(v->root);
		free(v->clusterTable);
		free(v);
		return NULL;
	}
    v->clusterTable[0]->data[0] = '\0';
    v->root->dataClusters = v->clusterTable[0];

    if(!AddExampleEntries(v))
	{
		free(v->root);
		free(v->clusterTable);
		free(v);
		return NULL;
	}

    return v;
}

/* Adding some entries to volume */
int AddExampleEntries(Volume* v)
{
	if(v == NULL) return 0;

	return
	AddDirectoryByPath(v, "root/a") != NULL &&
	AddDirectoryByPath(v, "root/b") != NULL &&
	AddDirectoryByPath(v, "root/a/c") != NULL &&
	AddDirectoryByPath(v, "root/a/d") != NULL &&
	AddDirectoryByPath(v, "root/b/e") != NULL &&
	AddDirectoryByPath(v, "root/b/f") != NULL &&
	AddDirectoryByPath(v, "root/b/g") != NULL &&
	AddDirectoryByPath(v, "root/a/d/i") != NULL &&
	AddFileByPath(v, "root/f1.txt") != NULL &&
	AddFileByPath(v, "root/a/f2.txt") != NULL &&
	AddDataToFileByPath(v, "root/a/f2.txt", "abababaabababbababaabbaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") != 0 &&
	AddFileByPath(v, "root/a/f3.txt") != NULL &&
	AddFileByPath(v, "root/b/h/f4.txt") != NULL;

}

/* Adds directory with given name to parent directory */
Directory* AddDirectory(Volume* v, Directory* parent, const char* name)
{
	if(v == NULL || parent == NULL || strlen(name) > NAME_SIZE) return NULL;

	if(FindDirectoryByNameAndParent(parent, name) != NULL) return NULL;

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
		DeleteSingleEmptyDirectory(v, create);
		free(create);
		return NULL;
	}

    return create;
}

/* Adds file with given name and extension to parent directory */
TextFile* AddFile(Volume* v, Directory* parent, const char* name, const char* extension)
{
	if(v == NULL || parent == NULL || strlen(name) > NAME_SIZE || strlen(extension) > EXTENSION_SIZE) return NULL;
	char* fullName = malloc(strlen(name) + 1 + strlen(extension) + 1);
	strcpy(fullName, name);
	strcat(fullName, ".");
	strcat(fullName, extension);

	if(FindFileByNameAndParent(parent, fullName) != NULL)
	{
		free(fullName);
		return NULL;
	}

	TextFile* last = FindLastInFileList(parent->files);
    TextFile* create = CreateEmptyFile(v, name, extension);
    if(create == NULL)
	{
		free(fullName);
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
		DeleteFile(v, create);
		free(fullName);
		free(create);
		return NULL;
	}

	free(fullName);
    return create;
}

/* Adds data to file */
int AddDataToFile(Volume* v, TextFile* f, const char* data)
{
	if(v == NULL || f == NULL || data == NULL) return 0;

	int neededClusters = NumberOfNeededClusters(data);

	if(!IsEnoughFreeClusters(v->clusterTable, v->clustersNum, neededClusters)) return 0;

	if(!ClearData(v, f->dataClusters)) return 0;

    if(neededClusters == 1)
	{
		strcpy(f->dataClusters->data, data);
	}

	if(!AddDataToClusterChain(v, f, data, neededClusters)) return 0;

	return 1;
}

/* Clears data of file (given file's data cluster) */
int ClearData(Volume* v, Cluster*  dataCluster)
{
	if(v == NULL || dataCluster == NULL) return 0;

	dataCluster->data[0] = '\0';

	Cluster* t;
    Cluster* current = dataCluster->next;

    if(current == NULL) return 1;

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
int AddDataToClusterChain(Volume* v, TextFile* f, const char* data, const int neededClusters)
{
	if(v == NULL || f == NULL || data == NULL || neededClusters == 0) return 0;

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
int IsEnoughFreeClusters(Cluster** clusterTable, const int clusterNum, const int needed)
{
	if(clusterTable == NULL) return 0;

	int i, j = 0;

	for(i = 0; i < clusterNum; i++)
	{
		if(clusterTable[i] == NULL)
		{
			j++;
		}

		if(j >= needed) return 1;
	}

	return 0;
}

/* Returns number of needed clusters for given data */
int NumberOfNeededClusters(const char* data)
{
	if(data == NULL) return 0;

	int dataSize = strlen(data);

	if(dataSize % CLUSTER_DATA_SIZE == 0)
	{
		return dataSize / CLUSTER_DATA_SIZE;
	}

	return dataSize / CLUSTER_DATA_SIZE + 1;
}

/* Creates empty directory and returns pointer to it */
Directory* CreateEmptyDirectory(Volume* v, const char* name)
{
	if(v == NULL || strlen(name) > NAME_SIZE) return NULL;

	Directory* create = (Directory*)calloc(1,sizeof(Directory));
	if(create == NULL)  return NULL;

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
TextFile* CreateEmptyFile(Volume* v, const char* name, const char* extension)
{
	if(v == NULL || strlen(name) > NAME_SIZE || strlen(extension) > EXTENSION_SIZE) return NULL;

	TextFile* create = (TextFile*)calloc(1,sizeof(TextFile));
	if(create == NULL) return NULL;

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
Cluster* FindEmptyCluster(Volume* v)
{
	if(v == NULL) return NULL;

	int i = FindEmptyClusterIndex(v->clusterTable, v->clustersNum);
    if(i == -1) return NULL;

	v->clusterTable[i] = (Cluster*)calloc(1,sizeof(Cluster));
	if(v->clusterTable[i] == NULL) return NULL;

	v->clusterTable[i]->id = i;
	v->clusterTable[i]->data[0] = '\0';

	return v->clusterTable[i];
}

/* Makes space for one more entry to parent directory entries */
int AddEntrySpace(Volume* v, Directory* parent)
{
	if(v == NULL || parent == NULL) return 0;

	if(IsAnotherClusterNeededForEntry(parent->entriesNum))
	{
		Cluster* newCluster = FindEmptyCluster(v);

		Cluster* last = FindLastInClusterList(parent->dataClusters);
		last->next = newCluster;
		newCluster->previous = last;
	}

	parent->entriesNum++;

	return 1;
}

/* Removes space for one entry from parent directory entries */
int RemoveEntrySpace(Volume* v, Directory* parent)
{
	if(v == NULL || parent == NULL || parent->entriesNum <= 0) return 0;

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
	if(first == NULL)  return NULL;

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
	if(first == NULL) return NULL;

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
	if(first == NULL) return NULL;

    Cluster* t = first;

    while(t->next != NULL)
	{
		t = t->next;
	}

	return t;
}

/* Checks if there is another cluster needed for one more entry with given number of previousEntries */
int IsAnotherClusterNeededForEntry(const int entriesNum)
{
	if(entriesNum < 0) return 0;

	if(entriesNum == 0) return 0;

	return entriesNum % ENTRIES_PER_CLUSTER == 0;
}

/* Checks if there will be empty cluster after removing entry */
int IsLastClusterNeededAfterDeletingEntry(const int entriesNum)
{
	if(entriesNum <= 1) return 0;

	return entriesNum % ENTRIES_PER_CLUSTER == 1;
}

/* Returns index of first empty cluster in clusterTable */
int FindEmptyClusterIndex(Cluster** clusterTable, const int clusterNum)
{
	if(clusterTable == NULL) return -1;

	int i;

	for(i = 0; i < clusterNum; i++)
	{
		if(clusterTable[i] == NULL) return i;
	}

	return -1;
}

/* View structure tree of given directory */
void ViewStructureTree(const Directory* d)
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
void ViewLevel(const Directory* d, int level)
{
	if(d == NULL) return;

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
void Indent(const int level)
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
void ViewFileData(const TextFile* f)
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

	printf("\n%s.%s file data:\n", f->name, f->extension);

	do
	{
		printf("%s", current->data);
		current = current->next;
	}while(current != NULL);

	printf("\n\n");
}