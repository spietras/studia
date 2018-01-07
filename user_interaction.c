#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "definitions.h"
#include "helpers.h"
#include "user_interaction.h"
#include "entries_operations.h"
#include "volume_operations.h"

static void ViewLevel(const Directory*, int);
static void Indent(const int);
static int GetMenuChoice(const int, const int);
static void ClearBuffer();

/**
	Wyswietla drzewo wszystkich plików i podkatalogów danego katalogu
	@param[in] d Katalog, dla którego odbywa sie wyświetlanie
*/
void ViewStructureTree(const Directory* d)
{
	if(d == NULL)
	{
		printf("\nDirectory does not exist\n");
		return;
	}

	if(d->files == NULL && d->subdirs == NULL)
	{
		printf("\nEmpty directory\n");
		return;
	}

	int startLevel = 0;
	printf("\n%s\n", d->name);

	ViewLevel(d, startLevel);

    printf("\n");
}

/**
	Rekursywnie wyświetla wszystkie pliki i podkatalogi danego katalogu
	@param[in] d Katalog, dla którego aktualnie jest wyświetlanie
	@param[in] level Głębokość aktualnego katalogu
*/
static void ViewLevel(const Directory* d, int level)
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

/**
	Wyświetla kreski imitujące strukturę katalogu
	@param[in] level Głębokość
*/
static void Indent(const int level)
{
	int i;

	printf("|");
	for(i = 0; i < level; i++)
	{
		printf(" |");
	}

	printf("____");
}

/**
	Wyświetla dane z pliku
	@param[in] f Plik, którego dane zostaną wyświetlone
*/
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

/**
	Wyświetla dane pliku przez podanie ścieżki
	@param[in] root Katalog główny
	@param[in] path Ścieżka do pliku
*/
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

/**
	Wyświetla drzewo plików i podkatalogów katalogu przez podanie ścieżki
	@param[in] root Katalog główny
	@param[in] path Ścieżka do katalogu
*/
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

/**
	Pokazuje menu i wywołuje odpowiednie funkcje
	@param[in, out] v Wolumin
*/
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

		printf("\nPress ENTER key to continue...\n");
		while(getchar() != '\n');
	}
}

/**
	Pobiera do użytkownika wybór z menu
	@param[in] min Minimalna liczba do wyboru
	@param[in] max Maksymalna liczba do wyboru
*/
static int GetMenuChoice(const int min, const int max)
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

/**
	Pobiera do użytkownika nazwę woluminu
*/
char* GetVolumeName()
{
	int n;
	char* name = malloc(257);
	if(name == NULL) return NULL;

	while(1)
	{
		printf("\nWrite volume name (max %d characters):\n", VOLUME_NAME_SIZE);

		n = scanf("%256s", name);

		if(n == 1 && strlen(name) <= VOLUME_NAME_SIZE)
		{
			ClearBuffer();
			return name;
		}

		printf("\nInvalid volume name. Write again\n");

		ClearBuffer();
	}
}

/**
	Pobiera do użytkownika nazwę katalogu
*/
char* GetDirectoryName()
{
	int n;
	char* name = malloc(257);
	if(name == NULL) return NULL;

	while(1)
	{
		printf("\nWrite directory name (max %d characters):\n", NAME_SIZE);

		n = scanf("%256s", name);

		if(n == 1 && IsDirectory(name))
		{
			ClearBuffer();
			return name;
		}

		printf("\nInvalid directory name. Write again\n");

		ClearBuffer();
	}
}

/**
	Pobiera do użytkownika nazwę pliku
*/
char* GetFileName()
{

	int n;
	char* name = malloc(257);
	if(name == NULL) return NULL;

	while(1)
	{
		printf("\nWrite file name (max %d characters):\n", NAME_SIZE);

		n = scanf("%256s", name);

		if(n == 1 && IsFile(name))
		{
			ClearBuffer();
			return name;
		}

		printf("\nInvalid file name. Write again\n");

		ClearBuffer();
	}
}

/**
	Pobiera do użytkownika dane
*/
char* GetData()
{

	int n = 0;
	char* data = malloc(1);
	if(data == NULL) return NULL;
	char c;

	printf("\nWrite data:\n");

	while((c = getchar()) != '\n')
	{
		n++;
		data = realloc(data, n);
		if(data == NULL) return NULL;
		data[n-1] = c;
	}

	data = realloc(data, n+1);
	if(data == NULL) return NULL;
	data[n] = '\0';

	return data;
}

/**
	Pobiera do użytkownika ścieżkę do pliku
	@param[in] message Wiadomość do wyświetlenia
*/
char* GetFilePath(const char* message)
{

	int n;
	char* path = malloc(257);
	if(path == NULL) return NULL;

	while(1)
	{
		printf("\n%s (for example: root/Folder1/File1.txt):\n", message);

		n = scanf("%256s", path);

		if(n == 1 && IsValidFilePath(path))
		{
			ClearBuffer();
			return path;
		}

		printf("\nInvalid file path. Write again\n");

		ClearBuffer();
	}
}

/**
	Pobiera do użytkownika ścieżkę do katalogu
	@param[in] message Wiadomość do wyświetlenia
*/
char* GetDirectoryPath(const char* message)
{

	int n;
	char* path = malloc(257);
	if(path == NULL) return NULL;

	while(1)
	{
		printf("\n%s (for example: root/Folder1/Folder2):\n", message);

		n = scanf("%256s", path);

		if(n == 1 && IsValidDirectoryPath(path))
		{
			ClearBuffer();
			return path;
		}

		printf("\nInvalid directory path. Write again\n");

		ClearBuffer();
	}
}

/**
	Czysci bufor konsoli
*/
static void ClearBuffer()
{
	while(getchar() != '\n');
}

