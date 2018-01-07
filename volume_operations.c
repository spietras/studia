#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "definitions.h"
#include "helpers.h"
#include "entries_operations.h"
#include "volume_operations.h"

static int BusyClusterAmount(const Volume*);
static int ReadDirectoryEntries(FILE*, Directory*, Cluster**, const int);
static int ReadFile(FILE*, Directory*, Cluster**, const int);
static int ReadDirectory(FILE*, Directory*, Cluster**, const int);
static int GetNextID(FILE*, const int);
static int GetClustersNumber(const int);
static int SaveAllDirectoriesEntries(const Volume*, FILE*);
static int SaveDirectoryEntries(const Directory*, FILE*, const int);
static int SaveDirectoryInfo(const Directory*, FILE*, const int);
static int SaveFileInfo(const TextFile*, FILE*, const int);
static int GetOffset(const int, const int);
static int SaveFAT(const Volume*, FILE*);
static int SaveClustersData(const Volume*, FILE*);
static int AddExampleEntries(Volume*);

/**
	Zmienia rozmiar woluminu
	@param[in, out] v Wolumin
	@param[in] newSize Nowy rozmiar
*/
int ResizeVolume(Volume* v, const int newSize)
{
	if(newSize > MAX_VOLUME_SIZE || newSize < CLUSTER_DATA_SIZE) return 0;

	if(newSize < BusyClusterAmount(v)*CLUSTER_DATA_SIZE) return 0;

	v->clustersNum = newSize / CLUSTER_DATA_SIZE;
	v->clusterTable = realloc(v->clusterTable, v->clustersNum*sizeof(Cluster*));

	return 1;
}

/**
	Zwraca ilość zajętych klastrów woluminu v
	@param[in] v Wolumin
*/
static int BusyClusterAmount(const Volume* v)
{
	if(v == NULL || v->clustersNum < 0 || v->clusterTable == NULL) return 0;

	int i = 0;
	int amount = 0;

	for(i = 0; i < v->clustersNum; i++)
	{
		if(v->clusterTable[i] != NULL)
		{
			amount++;
		}
	}

	return amount;
}

/**
	Tworzy pusty wolumin
	@param[in] name Nazwa woluminu
	@param[in] size Rozmiar woluminu (maksymalny rozmiar danych do przechowania na woluminie)
*/
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

/**
	Odczytuje wolumin z pliku na dysku o podanej nazwie
	@param[in] name Nazwa pliku (bez rozszerzenia)
*/
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

/**
	Rekursywnie odczytuje informacje o wszystkich plikach i podkatalogach z pliku na dysku i tworzy je w programie
	@param[in] volumeFile Plik do odczytu
	@param[in, out] d Aktualny katalog, do którego zostaną dodane odczytane pliki i podkatalogi
	@param[in, out] clusterTable Tablica klastrów
	@param[in] clusterNum Maksymalna liczba klastrów
*/
static int ReadDirectoryEntries(FILE* volumeFile, Directory* d, Cluster** clusterTable, const int clusterNum)
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

/**
	Odczytuje informacje o pliku z pliku na dysku i tworzy ten plik w programie
	@param[in] volumeFile Plik do odczytu
	@param[in, out] parent Katalog, do którego zostanie dodany odczytany plik
	@param[in, out] clusterTable Tablica klastrów
	@param[in] clusterNum Maksymalna liczba klastrów
*/
static int ReadFile(FILE* volumeFile, Directory* parent, Cluster** clusterTable, const int clusterNum)
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

/**
	Odczytuje informacje o podkatalogu z pliku na dysku i tworzy ten podkatalog w programie
	@param[in] volumeFile Plik do odczytu
	@param[in, out] parent Katalog, do którego zostanie dodany odczytany podkatalog
	@param[in, out] clusterTable Tablica klastrów
	@param[in] clusterNum Maksymalna liczba klastrów
*/
static int ReadDirectory(FILE* volumeFile, Directory* parent, Cluster** clusterTable, const int clusterNum)
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

/**
	Zwraca ID następnego klastra z Tablicy Alokacji Plików
	@param[in] volumeFile Plik z zapisem woluminu
	@param[in] clusterID ID aktualnego klastra
*/
static int GetNextID(FILE* volumeFile, const int clusterID)
{
	if(volumeFile == NULL || clusterID > MAX_CLUSTER_NUM || clusterID < 0) return -1;

	int pos = clusterID * sizeof(int);

	if(fseek(volumeFile, pos, SEEK_SET)) return -1;

	int value;
	if(fread(&value, sizeof(value), 1, volumeFile) != 1) return -1;

	return value;
}

/**
	Zwraca liczbe klastrow dla podanego rozmiaru woluminu
	@param[in] volumeSize Rozmiar woluminu
*/
static int GetClustersNumber(const int volumeSize)
{
	if(volumeSize > MAX_VOLUME_SIZE || volumeSize < 0) return -1;

	return volumeSize / (sizeof(int) + CLUSTER_DATA_SIZE);
}

/**
	Zapisuje wolumin do pliku na dysku z podaną nazwą
	@param[in] v Wolumin
	@param[in] name Nazwa pliku (bez rozszerzenia)
 */
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

/**
	Zapisuje informacje o wszystkich plikach i katalogach danego woluminu do pliku na dysku
	@param[in] v Wolumin
	@param[in, out] volumeFile Plik do zapisu
*/
static int SaveAllDirectoriesEntries(const Volume* v, FILE* volumeFile)
{
	if(v == NULL || v->clustersNum <= 0 || v->clusterTable == NULL || v->root == NULL || volumeFile == NULL) return 0;

	if(!SaveDirectoryEntries(v->root, volumeFile, v->clustersNum)) return 0;

	return 1;
}

/**
	Zapisuje informacje o plikach i podkatalogach podanego katalogu do pliku na dysku
	@param[in] d Katalog, którego informacje o pozycjach zostaną zapisane
	@param[in, out] volumeFile Plik do zapisu
	@param[in] clustersNum Ilość wszystkich klastrów
*/
static int SaveDirectoryEntries(const Directory* d, FILE* volumeFile, const int clustersNum)
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

/**
	Zapisuje informację o katalogu do pliku na dysku
	@param[in] d Katalog, o którym informacje zostaną zapisane
	@param[in, out] volumeFile Plik do zapisu
	@param[in] position Pozycja do zapisu
*/
static int SaveDirectoryInfo(const Directory* d, FILE* volumeFile, const int position)
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

/**
	Zapisuje informację o pliku do pliku na dysku
	@param[in] f Plik, o którym informacje zostaną zapisane
	@param[in, out] volumeFile Plik do zapisu
	@param[in] position Pozycja do zapisu
*/
static int SaveFileInfo(const TextFile* f, FILE* volumeFile, const int position)
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

/**
	Zwraca pozycję poczatkową klastra z podanym ID klastra
	@param[in] clustersNum Maksymalna liczba klastrów
	@param[in] clusterID ID klastra
*/
static int GetOffset(const int clustersNum, const int clusterID)
{
	if(clustersNum > MAX_CLUSTER_NUM || clusterID > MAX_CLUSTER_NUM || clustersNum <= 0 || clusterID < 0) return -1;

    return clustersNum * sizeof(int) + clusterID * CLUSTER_DATA_SIZE;
}

/**
	Zapisuje Tablice Alokacji Plików do pliku na dysku
	@param[in] v Wolumin
	@param[in, out] volumeFile Plik do zapisu
*/
static int SaveFAT(const Volume* v, FILE* volumeFile)
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

/**
	Zapisuje dane z wszystkich klastrów do pliku na dysku
	@param[in] v Wolumin
	@param[in, out] volumeFile Plik do zapisu
*/
static int SaveClustersData(const Volume* v, FILE* volumeFile)
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

/**
	Inicjalizacja przykładowego woluminu
*/
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

/**
	Dodaje przykładowe pozycje do woluminu
	@param[in, out] v Wolumin
*/
static int AddExampleEntries(Volume* v)
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

