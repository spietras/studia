#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "definitions.h"
#include "helpers.h"
#include "entries_operations.h"

static int IsAnotherClusterNeededForEntry(const int);
static int AddEntrySpace(Volume*, Directory*);
static int AddDataToClusterChain(Volume*, TextFile*, const char*, const int);
static int IsEnoughFreeClusters(Cluster**, const int, const int);
static int NumberOfNeededClusters(const char*);
static int IsLastClusterNeededAfterDeletingEntry(const int);
static int RemoveEntrySpace(Volume*, Directory*);
static void OrganizeFileListAfterDeletion(TextFile*);
static int DeleteSingleEmptyDirectory(Volume* v, Directory*);
static void OrganizeSubdirectoryListAfterDeletion(Directory*);
static Directory* CopySingleDirectoryToDirectory(Volume*, Directory*, Directory*);

/*

-------------------------------------------------------------------------------------------------------------------------------------

ADDING

*/

/**
	Sprawdza, czy po dodaniu nowej pozycji bedzie potrzebny nowy klaster na zapisywanie danych o pozycjach
	@param[in] entriesNum Aktualna liczba pozycji
*/
static int IsAnotherClusterNeededForEntry(const int entriesNum)
{
	if(entriesNum < 0) return 0;

	if(entriesNum == 0) return 0;

	return entriesNum % ENTRIES_PER_CLUSTER == 0;
}

/**
	Dodaje miejsce na jednę pozycję w danego katalogu
	@param[in, out] v Wolumin
	@param[in, out] parent Katalog
*/
static int AddEntrySpace(Volume* v, Directory* parent)
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

/**
	Tworzy nowy pusty plik
	@param[in, out] v Wolumin
	@param[in] name Nazwa pliku
	@param[in] extension Rozszerzenie pliku
*/
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
		if(create != NULL) free(create);
		return NULL;
	}

	return create;
}

/**
	Tworzy nowy pusty plik z podaną nazwą i rozszerzeniem i dodaje go do podanego katalogu
	@param[in, out] v Wolumin
	@param[in, out] parent Katalog, do którego plik zostanie dodany
	@param[in] name Nazwa pliku
	@param[in] extension Rozszerzenie pliku
*/
TextFile* AddFile(Volume* v, Directory* parent, const char* name, const char* extension)
{
	if(v == NULL || parent == NULL || strlen(name) > NAME_SIZE || strlen(extension) > EXTENSION_SIZE) return NULL;
	char* fullName = malloc(strlen(name) + 1 + strlen(extension) + 1);
	strcpy(fullName, name);
	strcat(fullName, ".");
	strcat(fullName, extension);

	if(FindFileByNameAndParent(parent, fullName) != NULL)
	{
		if(fullName != NULL) free(fullName);
		return NULL;
	}

	TextFile* last = FindLastInFileList(parent->files);
    TextFile* create = CreateEmptyFile(v, name, extension);
    if(create == NULL)
	{
		if(fullName != NULL) free(fullName);
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
		if(fullName != NULL) free(fullName);
		if(fullName != NULL) free(create);
		return NULL;
	}

	if(fullName != NULL) free(fullName);
    return create;
}

/**
	Dodaje plik przez podanie ścieżki
	@param[in, out] v Wolumin
	@param[in] path Ścieżka do pliku
*/
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
				if(pathClone != NULL) free(pathClone);
				return NULL;
			}
		}

		t = current;
		cName = strtok(NULL, "/");
	}

	char* name = strtok(cName, ".");
	char* ext = strtok(NULL, ".");

	TextFile* f = AddFile(v, current, name, ext);

	if(pathClone != NULL) free(pathClone);

	return f;
}

/**
	Dodaje dane do łańcucha klastrów danego pliku
	@param[in, out] v Wolumin
	@param[in] f Plik
	@param[in] data Dane
	@param[in] neededClusters Ilość potrzebnych klastrów
*/
static int AddDataToClusterChain(Volume* v, TextFile* f, const char* data, const int neededClusters)
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

/**
	Sprawdza, czy jest wystarczająca liczba wolnych klastrów
	@param[in] clusterTable Tablica klastrów
	@param[in] clusterNum Maksymalna ilość klastrów
	@param[in] needed Potrzebna ilość klastrów
*/
static int IsEnoughFreeClusters(Cluster** clusterTable, const int clusterNum, const int needed)
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

/**
	Zwraca ilość potrzebnych klastrów na zapisanie podanych danych
	@param[in] data Dane
*/
static int NumberOfNeededClusters(const char* data)
{
	if(data == NULL) return 0;

	int dataSize = strlen(data);

	if(dataSize % CLUSTER_DATA_SIZE == 0)
	{
		return dataSize / CLUSTER_DATA_SIZE;
	}

	return dataSize / CLUSTER_DATA_SIZE + 1;
}

/**
	Dodaje dane do pliku
	Uwaga: funkcja nadpisuje aktualne dane w pliku
	@param[in, out] v Wolumin
	@param[in] f Plik
	@param[in] data Dane
*/
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

/**
	Dodaje dane do pliku przez podanie ścieżki
	@param[in, out] v Wolumin
	@param[in] path Ścieżka do pliku
	@param[in] data Dane
*/
int AddDataToFileByPath(Volume* v, const char* path, const char* data)
{
	if(v == NULL || !IsValidFilePath(path) || data == NULL) return 0;

	TextFile *f = FindFileByPath(v->root, path);
	if(f == NULL) return 0;

	return AddDataToFile(v, f, data);
}

/**
	Tworzy nowy pusty katalog
	@param[in, out] v Wolumin
	@param[in] name Nazwa katalogu
*/
Directory* CreateEmptyDirectory(Volume* v, const char* name)
{
	if(v == NULL || strlen(name) > NAME_SIZE) return NULL;

	Directory* create = (Directory*)calloc(1,sizeof(Directory));
	if(create == NULL)  return NULL;

    strcpy(create->name, name);

    create->dataClusters = FindEmptyCluster(v);

    if(create->dataClusters == NULL)
	{
		if(create != NULL) free(create);
		return NULL;
	}

	return create;
}

/**
	Tworzy nowy pusty katalog z podaną nazwą i dodaje go do podanego katalogu
	@param[in, out] v Wolumin
	@param[in, out] parent Katalog, do którego zostanie dodany katalog
	@param[in] name Nazwa katalogu
*/
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
		return NULL;
	}

    return create;
}

/**
	Dodaje katalog przez podanie ścieżki
	@param[in, out] v Wolumin
	@param[in] path Ścieżka do katalogu
*/
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
				if(pathClone != NULL) free(pathClone);
				return NULL;
			}
		}

		t = current;
		cName = strtok(NULL, "/");
	}

	if(pathClone != NULL) free(pathClone);
	return current;
}

/*

-------------------------------------------------------------------------------------------------------------------------------------

REMOVING

*/

/**
	Sprawdza, czy po usunięciu pozycji w katalogu zostanie niepotrzebny klaster do zapisywania danych o pozycjach
	@param[in] entriesNum Aktualna liczba pozycji
 */
static int IsLastClusterNeededAfterDeletingEntry(const int entriesNum)
{
	if(entriesNum <= 1) return 0;

	return entriesNum % ENTRIES_PER_CLUSTER == 1;
}

/**
	Usuwa miejsce na jedną pozycję z danego katalogu
	@param[in, out] v Wolumin
	@param[in, out] parent Katalog
*/
static int RemoveEntrySpace(Volume* v, Directory* parent)
{
	if(v == NULL || parent == NULL || parent->entriesNum <= 0) return 0;

	if(IsLastClusterNeededAfterDeletingEntry(parent->entriesNum))
	{
		Cluster* last = FindLastInClusterList(parent->dataClusters);

		last->previous->next = NULL;
		v->clusterTable[last->id] = NULL;
		if(last != NULL) free(last);
	}

	parent->entriesNum--;

	return 1;
}

/**
	Usuwa dane z danego łańcucha klastrów
	Przy okazji usuwa wszystkie niepotrzebne już puste klastry z łańcucha
	@param[in, out] v Wolumin
	@param[in, out] dataCluster Pierwszy klaster pliku
*/
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
		if(t != NULL) free(t);
	}while(current != NULL);

	dataCluster->next = NULL;

	return 1;
}

/**
	Usuwa podany plik i wszystkie klastry z nim związane
	@param[in, out] v Wolumin
	@param[in, out] f Plik
*/
int DeleteFile(Volume* v, TextFile* f)
{
	if(v == NULL  || f == NULL || f->dataClusters == NULL || f->parent == NULL || f->parent->files == NULL) return 0;

    if(!ClearData(v, f->dataClusters)) return 0;

    v->clusterTable[f->dataClusters->id] = NULL;
    if(f->dataClusters != NULL) free(f->dataClusters);

    if(!RemoveEntrySpace(v, f->parent)) return 0;

    OrganizeFileListAfterDeletion(f);

	if(f != NULL) free(f);

	return 1;
}

/**
	Usuwa plik przez podanie ścieżki
	@param[in, out] v Wolumin
	@param[in] path Scieżka do pliku
*/
int DeleteFileByPath(Volume* v, const char* path)
{
	if(v == NULL || path == NULL) return 0;

    TextFile* f = FindFileByPath(v->root, path);

    if(!DeleteFile(v, f)) return 0;

	return 1;
}

/**
	Organizuje listę plików po usunięciu danego pliku
	@param[in, out] f Plik
*/
static void OrganizeFileListAfterDeletion(TextFile* f)
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

/**
	Usuwa podany pusty katalog
	@param[in, out] v Wolumin
	@param[in, out] d Pusty katalog
*/
static int DeleteSingleEmptyDirectory(Volume* v, Directory* d)
{
	if(v == NULL  || d == NULL || d->dataClusters == NULL || d->parent == NULL || d->parent->subdirs == NULL || d->subdirs != NULL) return 0;

    if(!ClearData(v, d->dataClusters)) return 0;

    v->clusterTable[d->dataClusters->id] = NULL;
    if(d->dataClusters != NULL) free(d->dataClusters);

    if(!RemoveEntrySpace(v, d->parent)) return 0;

    OrganizeSubdirectoryListAfterDeletion(d);

	if(d != NULL) free(d);

	return 1;
}

/**
	Rekursywnie usuwa wszystkie pliki i podkatalogi z podanego katalogu (wraz z tym katalogiem)
	@param[in, out] v Wolumin
	@param[in, out] d Katalog
*/
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

/**
	Usuwa katalog (i wszystkie jego podkatalogi i pliki) przez podanie ścieżki
	@param[in, out] v Wolumin
	@param[in] path Ścieżka do katalogu
*/
int DeleteDirectoryByPath(Volume* v, const char* path)
{
	if(v == NULL || path == NULL) return 0;

    Directory* d = FindDirectoryByPath(v->root, path);

    if(!DeleteDirectoryTree(v, d)) return 0;

	return 1;
}

/**
	Organizuje listę katalogów po usunięciu danego katalogu
	@param[in, out] d katalog
 */
static void OrganizeSubdirectoryListAfterDeletion(Directory* d)
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

/*

-------------------------------------------------------------------------------------------------------------------------------------

MOVING

*/


/**
	Przenosi podany plik do podanego katalogu
	@param[in, out] v Wolumin
	@param[in, out] f Plik do przeniesienia
	@param[in, out] d Katalog, do którego plik zostanie przeniesiony
*/
int MoveFileToDirectory(Volume* v, TextFile* f, Directory* d)
{
	if(v == NULL || f == NULL || d == NULL) return 0;

    char* fullName = malloc(strlen(f->name) + 1 + strlen(f->extension) + 1);
	strcpy(fullName, f->name);
	strcat(fullName, ".");
	strcat(fullName, f->extension);

	if(FindFileByNameAndParent(d, fullName) != NULL)
	{
		if(fullName != NULL) free(fullName);
		return 0;
	}

    if(!AddEntrySpace(v, d))
	{
		if(fullName != NULL) free(fullName);
		return 0;
	}

	if(!RemoveEntrySpace(v, f->parent))
	{
		RemoveEntrySpace(v, d);
		if(fullName != NULL) free(fullName);
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

	if(fullName != NULL) free(fullName);
	return 1;
}

/**
	Przenosi plik z podaną ścieżką do katalogu z podaną ścieżką
	@param[in, out] v Wolumin
	@param[in] fPath Ścieżka do pliku do przeniesienia
	@param[in] dirPath Ścieżka do katalogu, do którego plik zostanie przeniesiony
*/
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


/**
	Przenosi podany katalog do podanego katalogu
	@param[in, out] v Wolumin
	@param[in, out] d Katalog do przeniesienia
	@param[in, out] destination Katalog, do którego katalog zostanie przeniesiony
*/
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

/**
	Przenosi katalog z podaną ściezka do katalogu z podaną ścieżka
	@param[in, out] v Wolumin
	@param[in] dirPath Ścieżka do katalogu do przeniesienia
	@param[in] destPath Ścieżka do katalogu, do którego katalog zostanie przeniesiony
*/
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

/*

-------------------------------------------------------------------------------------------------------------------------------------

COPYING

*/

/**
	Kopiuje podany plik do podanego katalogu
	@param[in, out] v Wolumin
	@param[in] f Plik do skopiowania
	@param[in, out] d Katalog, do którego plik zostanie skopiowany
*/
int CopyFileToDirectory(Volume* v, TextFile* f, Directory* d)
{
	if(v == NULL || f == NULL || d == NULL) return 0;

	char* fullName = malloc(strlen(f->name) + 1 + strlen(f->extension) + 1);
	strcpy(fullName, f->name);
	strcat(fullName, ".");
	strcat(fullName, f->extension);

	if(FindFileByNameAndParent(d, fullName) != NULL)
	{
		if(fullName != NULL) free(fullName);
		return 0;
	}

	if(!AddEntrySpace(v, d))
	{
		if(fullName != NULL) free(fullName);
		return 0;
	}

	TextFile* copy = AddFile(v, d, f->name, f->extension);

	if(copy == NULL)
	{
		RemoveEntrySpace(v, d);
		if(fullName != NULL) free(fullName);
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
		if(fullName != NULL) free(fullName);
		if(copyData != NULL) free(copyData);
		return 0;
	}

	if(fullName != NULL) free(fullName);
	if(copyData != NULL) free(copyData);
	return 1;
}

/**
	Kopiuje plik z podaną ścieżka do katalogu z podaną ścieżka
	@param[in, out] v Wolumin
	@param[in] fPath Ścieżka do pliku do skopiowania
	@param[in] dirPath Ścieżka do katalogu, do którego plik zostanie skopiowany
*/
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

/**
	Kopiuje podany pusty katalog do podanego katalogu
	@param[in, out] v Wolumin
	@param[in] d Katalog do skopiowania
	@param[in, out] destination Katalog, do którego katalog zostanie skopiowany
*/
static Directory* CopySingleDirectoryToDirectory(Volume* v, Directory* d, Directory* destination)
{
	if(v == NULL || d == NULL || destination == NULL || d->files != NULL || d->subdirs != NULL) return NULL;

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

/**
	Rekursywnie kopiuje podany katalog i wszystkie jego pliki i podkatalogi do podanego katalogu
	@param[in, out] v Wolumin
	@param[in] d Katalog do skopiowania
	@param[in, out] destination Katalog, do którego katalog zostanie skopiowany
*/
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

/**
	Kopiuje katalog (i jego zawartość) z podaną ścieżką do katalogu z podaną ścieżką
	@param[in, out] v Wolumin
	@param[in] dirPath Ścieżka do katalogu do skopiowania
	@param[in] destPath Ścieżka do katalogu, do którego katalog zostanie skopiowany
*/
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

/*

-------------------------------------------------------------------------------------------------------------------------------------

RENAMING

*/

/**
	Zmienia nazwę podanego pliku na podaną nazwę
	@param[in, out] f Plik, którego nazwa zostanie zmieniona
	@param[in] newName Nowa nazwa pliku
*/
int RenameFile(TextFile* f, const char* newName)
{
    if(f == NULL || newName == NULL || strlen(newName) > NAME_SIZE) return 0;

	char* fullName = malloc(strlen(newName) + 1 + strlen(f->extension) + 1);
	strcpy(fullName, newName);
	strcat(fullName, ".");
	strcat(fullName, f->extension);

	if(FindFileByNameAndParent(f->parent, fullName) != NULL)
	{
		if(fullName != NULL) free(fullName);
		return 0;
	}

	strcpy(f->name, newName);

	if(fullName != NULL) free(fullName);
	return 1;
}

/**
	Zmienia nazwę pliku z podaną ścieżką na podaną nazwę
	@param[in] root Katalog główny
	@param[in] path Ścieżka do pliku
	@param[in] newName Nowa nazwa pliku
*/
int RenameFileByPath(Directory* root, const char* path, const char* newName)
{
	if(path == NULL || newName == NULL || !IsValidFilePath(path)) return 0;

	TextFile* f = FindFileByPath(root, path);

	if(f == NULL) return 0;

	return RenameFile(f, newName);
}

/**
	Zmienia nazwę podanego katalogu na podaną nazwę
	@param[in, out] d Katalog, którego nazwa zostanie zmieniona
	@param[in] newName Nowa nazwa katalogu
*/
int RenameDirectory(Directory* d, const char* newName)
{
	if(d == NULL || newName == NULL || strlen(newName) > NAME_SIZE) return 0;

	if(FindDirectoryByNameAndParent(d->parent, newName) != NULL) return 0;

	strcpy(d->name, newName);

	return 1;
}

/**
	Zmienia nazwę katalogu z podaną ścieżką na podaną nazwę
	@param[in] root Katalog główny
	@param[in] path Ścieżka do katalogu
	@param[in] newName Nowa nazwa katalogu
*/
int RenameDirectoryByPath(Directory* root, const char* path, const char* newName)
{
	if(path == NULL || newName == NULL || !IsValidDirectoryPath(path)) return 0;

	Directory* d = FindDirectoryByPath(root, path);

	if(d == NULL || d == root) return 0;

	return RenameDirectory(d, newName);
}

