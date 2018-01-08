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
		printf("\nKatalog nie istnieje\n");
		return;
	}

	if(d->files == NULL && d->subdirs == NULL)
	{
		printf("\nKatalog jest pusty\n");
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
		printf("\nPlik nie istnieje\n");
		return;
	}
	if(f->dataClusters == NULL || strlen(f->dataClusters->data) == 0)
	{
		printf("\nPlik jest pusty\n");
		return;
	}

	Cluster* current = f->dataClusters;

	printf("\nDane pliku %s.%s:\n", f->name, f->extension);

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
		printf("\nBrak katalogu glownego\n");
		return;
	}

	if(!IsValidFilePath(path))
	{
		printf("\nNiewlasciwa sciezka\n");
		return;
	}

	TextFile* f = FindFileByPath(root, path);

	if(f == NULL)
	{
		printf("\nPlik nie istnieje\n");
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
		printf("\nBrak katalogu glownego\n");
		return;
	}

	if(!IsValidDirectoryPath(path))
	{
		printf("\nNiewlasciwa sciezka\n");
		return;
	}

	Directory* d = FindDirectoryByPath(root, path);

	if(d == NULL)
	{
		printf("\nKatalog nie istnieje\n");
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
		printf("\nWybierz co zrobic:\n");
		printf("1. Wyswietlic wszystkie katalogi i pliki\n");
		printf("2. Wyswietlic konkretny katalog i wszystkie jego podkatalogi i pliki\n");
		printf("3. Wyswietlic dane pliku\n");
		printf("4. Stworzyc nowy pusty plik\n");
		printf("5. Dodac dane do pliku\n");
		printf("6. Stworzyc nowy pusty katalog\n");
		printf("7. Usunac plik\n");
		printf("8. Usunac katalog\n");
		printf("9. Przeniesc plik\n");
		printf("10. Przeniesc katalog\n");
		printf("11. Skopiowac plik\n");
		printf("12. Skopiowac katalog\n");
		printf("13. Zmienic nazwe pliku\n");
		printf("14. Zmienic nazwe katalogu\n");
		printf("15. Zapisac wolumin na dysku z nowa nazwa\n");
		printf("16. Wyjsc (automatycznie zapisuje wolumin na dysk z aktualna nazwa)\n");

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
				char* path = GetDirectoryPath("Podaj sciezke do katalogu do wyswietlenia");
				ViewStructureTreeByPath(v->root, path);
				free(path);
				break;
			}
		case 3:
			{
				char* path = GetFilePath("Podaj sciezke do pliku do wyswietlenia");
				ViewFileDataByPath(v->root, path);
				free(path);
				break;
			}
		case 4:
			{
				char* path = GetFilePath("Podaj sciezke do pliku, ktory chcesz dodac");
				if(AddFileByPath(v, path) == NULL) printf("\nNie mozna dodac tego pliku\n");
				else printf("\nPlik dodany pomyslnie\n");
				free(path);
				break;
			}
		case 5:
			{
				char* path = GetFilePath("Podaj sciezke do pliku, do ktorego chcesz dodac dane");
				char* data = GetData();
				if(!AddDataToFileByPath(v, path, data)) printf("\nNie mozna dodac danych do tego pliku\n");
				else printf("\nDane dodane pomyslnie\n");
				free(path);
				free(data);
				break;
			}
		case 6:
			{
				char* path = GetDirectoryPath("Podaj sciezke do katalogu, ktory chcesz dodac");
				if(!AddDirectoryByPath(v, path)) printf("\nNie mozna dodac tego katalogu\n");
				else printf("\nKatalog dodany pomyslnie\n");
				free(path);
				break;
			}
		case 7:
			{
				char* path = GetFilePath("Podaj sciezke do pliku do usuniecia");
				if(!DeleteFileByPath(v, path)) printf("\nNie mozna usunac tego pliku\n");
				else printf("\nPlik usuniety pomyslnie\n");
				free(path);
				break;
			}
		case 8:
			{
				char* path = GetDirectoryPath("Podaj sciezke do katalogu do usuniecia");
				if(!DeleteDirectoryByPath(v, path)) printf("\nNie mozna usunac tego katalogu\n");
				else printf("\nKatalog usuniety pomyslnie\n");
				free(path);
				break;
			}
		case 9:
			{
				char* fPath = GetFilePath("Podaj sciezke do pliku, ktory chcesz przeniesc");
				char* dPath = GetDirectoryPath("Podaj sciezke do katalogu, do ktorego chcesz przeniesc");
				if(!MoveFileToDirectoryByPaths(v, fPath, dPath)) printf("\nNie mozna przeniesc tego pliku do tego katalogu\n");
				else printf("\nPlik przeniesiony pomyslnie");
				free(fPath);
				free(dPath);
				break;
			}
		case 10:
			{
				char* dirPath = GetDirectoryPath("Podaj sciezke do katalogu, ktory chcesz przeniesc");
				char* destPath = GetDirectoryPath("Podaj sciezke do katalogu, do ktorego chcesz przeniesc");
				if(!MoveDirectoryToDirectoryByPaths(v, dirPath, destPath)) printf("\nNie mozna przeniesc tego katalogu do tego katalogu\n");
				else printf("\nKatalog przeniesiony pomyslnie");
				free(dirPath);
				free(destPath);
				break;
			}
		case 11:
			{
				char* fPath = GetFilePath("Podaj sciezke do pliku do skopiowania");
				char* dPath = GetDirectoryPath("Podaj sciezke do katalogu, do ktorego chcesz skopiowac plik");
				if(!CopyFileToDirectoryByPaths(v, fPath, dPath)) printf("\nNie mozna skopiowac tego pliku do tego katalogu\n");
				else printf("\nKatalog skopiowany pomyslnie");
				free(fPath);
				free(dPath);
				break;
			}
		case 12:
			{
				char* dirPath = GetDirectoryPath("Podaj sciezke do katalogu do skopiowania");
				char* destPath = GetDirectoryPath("Podaj sciezke do katalogu, do ktorego chcesz skopiowac katalog");
				if(!CopyDirectoryToDirectoryByPaths(v, dirPath, destPath)) printf("\nNie mozna skopiowac tego katalogu do tego katalogu\n");
				else printf("\nKatalog dodany pomyslnie");
				free(dirPath);
				free(destPath);
				break;
			}
		case 13:
			{
				char* path = GetFilePath("Podaj sciezke do pliku, ktorego nazwe chcesz zmienic");
				char* name = GetFileName();
				if(!RenameFileByPath(v->root, path, name)) printf("\nNie mozna zmienic nazwy tego pliku\n");
				else printf("\nPomyslnie zmieniono nazwe pliku\n");
				free(path);
				free(name);
				break;
			}
		case 14:
			{
				char* path = GetDirectoryPath("Podaj sciezke do katalogu, ktorego nazwe chcesz zmienic");
				char* name = GetDirectoryName();
				if(!RenameDirectoryByPath(v->root, path, name)) printf("\nNie mozna zmienic nazwy tego katalogu\n");
				else printf("\nPomyslnie zmieniono nazwe katalogu\n");
				free(path);
				free(name);
				break;
			}
		case 15:
			{
				char* name = GetVolumeName();
				if(!Save(v, name)) printf("\nNie mozna zapisac tego woluminu na dysk\n");
				else printf("\nPomyslnie zapisano wolumin na dysk\n");
				free(name);
				break;
			}
		case 16:
			{
				if(!Save(v, v->name)) printf("\nNie mozna zapisac woluminu na dysk\n");
				else return 1;
				break;
			}
		default:
			break;
		}

		printf("\nNacisnij ENTER, zeby kontynuowac...\n");
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
		printf("\nTwoj wybor: ");
		n = scanf("%d", &choice);

		if(n == 1 && (choice >= min && choice <= max))
		{
			ClearBuffer();
			return choice;
		}

		printf("\nNieprawidlowy wybor. Musisz wybrac miedzy %d - %d. Wybierz jeszcze raz\n", min, max);
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
		printf("\nPodaj nazwe woluminu (maksymalnie %d znakow):\n", VOLUME_NAME_SIZE);

		n = scanf("%256s", name);

		if(n == 1 && strlen(name) <= VOLUME_NAME_SIZE)
		{
			ClearBuffer();
			return name;
		}

		printf("\nNieprawidlowa nazwa woluminu. Podaj jeszcze raz\n");

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
		printf("\nPodaj nazwe katalogu (maksymalnie %d znakow):\n", NAME_SIZE);

		n = scanf("%256s", name);

		if(n == 1 && IsValidDirectoryName(name))
		{
			ClearBuffer();
			return name;
		}

		printf("\nNieprawidlowa nazwa katalogu. Podaj jeszcze raz\n");

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
		printf("\nPodaj nazwe pliku (maksymalnie %d znakow):\n", NAME_SIZE);

		n = scanf("%256s", name);

		if(n == 1 && IsValidFileName(name))
		{
			ClearBuffer();
			return name;
		}

		printf("\nNieprawidlowa nazwa pliku. Podaj jeszcze raz\n");

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

	printf("\nPodaj dane:\n");

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
		printf("\n%s (na przyklad: root/Katalog1/Folder1.txt):\n", message);

		n = scanf("%256s", path);

		if(n == 1 && IsValidFilePath(path))
		{
			ClearBuffer();
			return path;
		}

		printf("\nNieprawidlowa sciezka do pliku. Podaj jeszcze raz\n");

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
		printf("\n%s (na przyklad: root/Katalog1/Katalog2):\n", message);

		n = scanf("%256s", path);

		if(n == 1 && IsValidDirectoryPath(path))
		{
			ClearBuffer();
			return path;
		}

		printf("\nNieprawidlowa sciezka do katalogu. Podaj jeszcze raz\n");

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

