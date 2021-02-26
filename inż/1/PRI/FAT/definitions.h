#ifndef DEFINITIONS_H_INCLUDED
#define DEFINITIONS_H_INCLUDED

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
#define FILENAME_INVALID_CHARACTERS "/ \\."
#define DIRECTORY_INVALID_CHARACTERS "/ \\."
#define EXTENSION_INVALID_CHARACTERS "/ \\.0123456789"

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


#endif