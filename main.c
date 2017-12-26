#include <stdio.h>
#include <stdlib.h>

#define MAX_VOLUME_SIZE 1048576
#define CLUSTER_DATA_SIZE 60
#define FILE_NAME_SIZE 12
#define FOLDER_NAME_SIZE 12
#define MAX_ENTRIES_NUMBER 256

typedef enum EntryType { FileEntry, FolderEntry } EntryType;

typedef struct Cluster Cluster;
typedef struct Entry Entry;
typedef struct EntryListNode EntryListNode;
typedef struct TextFile TextFile;
typedef struct Folder Folder;
typedef struct ClusterListNode ClusterListNode;
typedef struct Volume Volume;

struct Cluster
{
	char data[CLUSTER_DATA_SIZE];
	Cluster* nextCluster;
}; /* 64 bytes */

struct Entry
{
	EntryType type;
	void* entry;
}; /* 8 bytes */

struct EntryListNode
{
	Entry* entry;
	EntryListNode* nextNode;
}; /* 8 bytes */

struct TextFile
{
	char name[FILE_NAME_SIZE];
	Cluster* firstCluster;
}; /* 16 bytes */

struct Folder
{
	char name[FOLDER_NAME_SIZE];
	EntryListNode* entriesList;
}; /* 16 bytes */

struct ClusterListNode
{
	Cluster* cluster;
	ClusterListNode* nextNode;
}; /* 8 bytes */

struct Volume
{
	Folder* root;
	ClusterListNode* clustersList;
	unsigned int clustersNumber;
	unsigned int entriesNumber;
}; /* 16 bytes */

int AvailableClusters(Volume*);
int AvailableEntries(Volume*);
int CanAddEmptyEntry(Volume*);
int CanAddDataToFile(Volume *, unsigned int);

int main()
{
	Volume v;
	v.clustersNumber = 1;
	v.entriesNumber = 1;
	printf("%d", CanAddDataToFile(&v, 1200));
}






/* -------------------- HELPERS -------------------- */





int AvailableClusters(Volume* v)
{
	return ((MAX_VOLUME_SIZE-MAX_ENTRIES_NUMBER*sizeof(TextFile)) / sizeof(Cluster)) - v->clustersNumber;
}

int AvailableEntries(Volume* v)
{
	return MAX_ENTRIES_NUMBER - v->entriesNumber;
}

int CanAddEmptyEntry(Volume* v)
{
	return AvailableEntries(v) > 0;
}

int CanAddDataToFile(Volume *v, unsigned int dataSize)
{
	int isMultiple = (dataSize % CLUSTER_DATA_SIZE == 0);
	int neededClusters = isMultiple ? dataSize / CLUSTER_DATA_SIZE : dataSize / CLUSTER_DATA_SIZE + 1;
	return neededClusters < AvailableClusters(v);
}

