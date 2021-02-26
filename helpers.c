#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "definitions.h"
#include "helpers.h"

static int FindEmptyClusterIndex(Cluster**, const int);

/**
	Zwraca ostatni katalog z listy katalogów
	@param[in] first Pierwszy katalog
*/
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

/**
	Zwraca ostatni plik z listy plików
	@param[in] first Pierwszy plik
*/
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

/**
	Zwraca ostatni klaster z listy klastrów
	@param[in] first Pierwszy klaster
*/
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

/**
	Zwraca indeks pierwszego pustego klastra w tablicy klastrów
	@param[in] clusterTable Tablica klastrów
	@param[in] clusterNum Maksymalna ilość klastrów
*/
static int FindEmptyClusterIndex(Cluster** clusterTable, const int clusterNum)
{
	if(clusterTable == NULL) return -1;

	int i;

	for(i = 0; i < clusterNum; i++)
	{
		if(clusterTable[i] == NULL) return i;
	}

	return -1;
}

/**
	Tworzy i zwraca pusty klaster
	@param[in, out] v Wolumin
*/
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

/**
	Sprawdza, czy podana ścieżka jest prawidłowa dla piku
	@param[in] path Ścieżka
*/
int IsValidFilePath(const char* path)
{
	if(path == NULL || strstr(path, "//") != NULL) return 0;

    char *pathClone = malloc(strlen(path) + 1);
    if(pathClone == NULL) return 0;
    strcpy(pathClone, path);

    char* t;

    char *pathTok = strtok(pathClone, "/");

    if(strcmp(pathTok, "root") != 0)
	{
		free(pathClone);
		return 0;
	}

	do
	{
		t = pathTok;
		pathTok = strtok(NULL, "/");

		if(!IsValidDirectoryName(t) && pathTok != NULL)
		{
			free(pathClone);
			return 0;
		}
	}while(pathTok != NULL);

	if(!IsValidFileFullName(t))
	{
		free(pathClone);
		return 0;
	}

	free(pathClone);
	return 1;
}

/**
	Sprawdza, czy ścieżka jest poprawna dla katalogu
	@param[in] path Ścieżka
 */
int IsValidDirectoryPath(const char* path)
{
	if(path == NULL || strstr(path, "//") != NULL) return 0;

    char *pathClone = malloc(strlen(path) + 1);
    if(pathClone == NULL) return 0;
    strcpy(pathClone, path);

    char* pathTok = strtok(pathClone, "/");

    if(strcmp(pathTok, "root") != 0)
	{
		free(pathClone);
		return 0;
	}

	do
	{
		if(!IsValidDirectoryName(pathTok))
		{
			free(pathClone);
			return 0;
		}

		pathTok = strtok(NULL, "/");
	}while(pathTok != NULL);

	free(pathClone);
	return 1;
}

/**
	Sprawdza, czy katalog z podaną ścieżką zawiera się w katalogu z podaną ścieżką
	@param[in] dirPath Ścieżka do katalogu, który jest sprawdzany, czy zawiera się w drugim katalogu
	@param[in] destPath Ścieżka do katalogu, który jest sprawdzany, czy zawiera pierwszy katalog
*/
int IsDestinationDirectoryPathBelowInHierarchy(const char* dirPath, const char* destPath)
{
	if(!IsValidDirectoryPath(dirPath) || !IsValidDirectoryPath(destPath)) return 0;

	if(strstr(destPath, dirPath) == NULL) return 0;

	if(destPath[strlen(dirPath)] != '/') return 0;

	return 1;
}

/**
	Sprawdza, czy podana nazwa jest prawidłowa dla pliku
	@param[in] fullName Pełna nazwa
 */
int IsValidFileFullName(const char* fullName)
{
	if(fullName == NULL || fullName[0] == '.' || fullName[strlen(fullName) - 1] == '.') return 0;

	int i = 0;
	int d = 0;
	for(i = 0; i < strlen(fullName); i++)
	{
		if(fullName[i] == '.') d++;
	}

	if(d != 1) return 0;

	char* nameClone = malloc(strlen(fullName) + 1);
	if(nameClone == NULL) return 0;
	strcpy(nameClone, fullName);
    char* name = strtok(nameClone, ".");
    char* extension = strtok(NULL, ".");

    if(name == NULL || strlen(name) > NAME_SIZE || strpbrk(name, FILENAME_INVALID_CHARACTERS) != NULL || extension == NULL || strlen(extension) > EXTENSION_SIZE || strpbrk(extension, EXTENSION_INVALID_CHARACTERS) != NULL)
	{
		free(nameClone);
		return 0;
	}

	free(nameClone);
	return 1;
}

/**
	Sprawdza, czy podana nazwa jest prawidłowa dla katalogu
	@param[in] name Nazwa
*/
int IsValidDirectoryName(const char* name)
{
	if(name == NULL || strlen(name) > NAME_SIZE) return 0;

	return strpbrk(name, DIRECTORY_INVALID_CHARACTERS) == NULL;
}

/**
	Sprawdza, czy podana nazwa jest prawidłowa dla pliku
	@param[in] name Nazwa (bez rozszerzenia)
*/
int IsValidFileName(const char* name)
{
	if(name == NULL || strlen(name) > NAME_SIZE) return 0;

	return strpbrk(name, FILENAME_INVALID_CHARACTERS) == NULL;
}

/**
	Znajduje plik przez podanie ścieżki
	@param[in] root Katalog główny
	@param[in] path Ścieżka do pliku
*/
TextFile* FindFileByPath(Directory* root, const char* path)
{
	if(root == NULL || !IsValidFilePath(path)) return NULL;

    char *last = strrchr(path, '/');
    char *dirPath = malloc(last - path + 1);
    if(dirPath == NULL) return NULL;
    strncpy(dirPath, path, last-path);
    dirPath[last-path] = '\0';
    last = last+1;

    Directory* parent = FindDirectoryByPath(root, dirPath);

    free(dirPath);
    return FindFileByNameAndParent(parent, last);
}

/**
	Znajduje katalog przez podanie ścieżki
	@param[in] root Katalog główny
	@param[in] path Ścieżka do katalogu
*/
Directory* FindDirectoryByPath(Directory* root, const char* path)
{
	if(root == NULL || !IsValidDirectoryPath(path)) return NULL;

	char* pathClone = malloc(strlen(path) + 1);
	if(pathClone == NULL) return NULL;
    strcpy(pathClone, path);

    char* pathTok = strtok(pathClone, "/");
    pathTok = strtok(NULL, "/");

	Directory* current = root;

	while(pathTok != NULL)
	{
		current = FindDirectoryByNameAndParent(current, pathTok);
		pathTok = strtok(NULL, "/");
	}

	free(pathClone);

	return current;
}

/**
	Znajduje podkatalog przez podanie katalogu, w którym katalog jest szukany i nazwę podkatalogu
	@param[in] parent Katalog, w którym plik jest szukany
	@param[in] name Nazwa podkatalogu
*/
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

/**
	Znajduje plik przez podanie katalogu, w którym plik jest szukany i nazwę pliku
	@param[in] parent Katalog, w którym plik jest szukany
	@param[in] name Nazwa pliku
*/
TextFile* FindFileByNameAndParent(const Directory* parent, const char* name)
{
	if(parent == NULL) return NULL;

	char* nameClone = malloc(strlen(name) + 1);
	if(nameClone == NULL) return NULL;
	strcpy(nameClone, name);
	const char* nameTok = strtok(nameClone, ".");
	const char* extensionTok = strtok(NULL, ".");
	if(nameTok == NULL || extensionTok == NULL)
	{
		free(nameClone);
		return NULL;
	}
	TextFile* t = parent->files;

    while(t != NULL)
	{
		if(strcmp(t->name, nameTok) == 0 && strcmp(t->extension, extensionTok) == 0)
		{
			free(nameClone);
			return t;
		}

		t = t->next;
	}

	free(nameClone);
	return NULL;
}

/**
	Kopiuje listę klastrów
	@param[in] first Pierwszy klaster listy
*/
Cluster* CopyClusterList(const Cluster* first)
{
	if(first == NULL) return NULL;

	Cluster* t = (Cluster*)malloc(sizeof(Cluster));
	if(t == NULL) return NULL;
	strcpy(t->data, first->data);
	t->id = first->id;
	t->next = CopyClusterList(first->next);
	return t;
}

