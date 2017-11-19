#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Club
{
	int index;
	char name[256];
	int league;
	unsigned long fansNumber;
	unsigned long stadiumCapacity;
	unsigned long budget;
	unsigned int leaguePosition;
};

int Initialize(struct Club **, int *);
struct Club * Allocate(struct Club *, int n);
struct Club * Reallocate(struct Club *, int n);
void ShowClub(struct Club *);
void ShowAll(struct Club *, int);
int AddClub(struct Club **, int *);
int RemoveClub(struct Club **, int *, int);

int main()
{
	struct Club *clubs;
	int length;

    if(!Initialize(&clubs, &length))
	{
		printf("\nBlad inicjalizacji");
		return 0;
	}

    if(!AddClub(&clubs, &length))
	{
		printf("\nBlad przy dodawaniu klubu");
		return 0;
	}

	ShowAll(clubs, length);

	int p;
	printf("\nZ jakiej pozycji chcesz usunac klub?\n");
	scanf("%d", &p);

	if(!RemoveClub(&clubs, &length, p - 1))
	{
		printf("\nBlad przy usuwaniu klubu");
	}

    ShowAll(clubs, length);

    return 0;
}

/* Dodanie przykladowych klubow na poczatku programu */
int Initialize(struct Club **clubArrayPointer, int *length)
{
	struct Club c1 = { 1, "Legia Warszawa", 1, 100000, 60000, 200000000, 1};
	struct Club c2 = { 2, "Wislaw Krakow" , 1, 80000, 50000, 100000000, 2};
	struct Club c3 = { 3, "Arka Gdynia", 1, 50000, 45000, 50000000, 3};

	*length = 3;

	*clubArrayPointer = Allocate(*clubArrayPointer, *length);

	if(*clubArrayPointer == NULL)
	{
		return 0;
	}

	(*clubArrayPointer)[0] = c1;
	(*clubArrayPointer)[1] = c2;
	(*clubArrayPointer)[2] = c3;

	return 1;
}

/* Alokacja pamieci na tablice klubow */
struct Club * Allocate(struct Club *clubArray, int length)
{
	return malloc(length * sizeof(*clubArray));
}

/* Realokacja pamieci na tablice klubow o nowej dlugosci */
struct Club * Reallocate(struct Club *clubArray, int length)
{
	return realloc(clubArray, length * sizeof(*clubArray));
}

/* Wypisanie informacji o klubie */
void ShowClub(struct Club *club)
{
	printf("\n%d.\nNazwa: %s\nLiga: %u\nLiczba kibicow: %lu\nPojemnosc stadionu: %lu\nBudzet: %lu\nPozycja w lidze: %u\n", (*club).index, (*club).name, (*club).league, (*club).fansNumber, (*club).stadiumCapacity, (*club).budget, (*club).leaguePosition);
}

/* Wypisanie wszystkich klubow */
void ShowAll(struct Club *clubArray, int length)
{
	int i;

	for(i = 0; i < length; i++)
	{
		ShowClub(&(clubArray[i]));
	}
}

/* Dodawanie nowego klubu */
int AddClub(struct Club **clubArrayPointer, int *length)
{
	int n, r;
	char c;
	char s[256];
	struct Club *p;

	(*length)++;

	p = Reallocate(*clubArrayPointer, *length);

	if(p == NULL)
	{
		(*length)--;
		return 0;
	}

	*clubArrayPointer = p;

	(*clubArrayPointer)[*length - 1].index = *length;

	/* Ponizej odbieranie od uzytkownika danych
		Jezeli uzytkownik podal bledne dane, to powtarzamy pobieranie az poda poprawne
	*/

	do
	{
		r = 0;
		printf("\nPodaj nazwe klubu: ");
		/* W nazwie klubu moga wystepowac wszystkie znaki oprocz nowej linii */
		n = scanf(" %255[^\n]s", &((*clubArrayPointer)[*length - 1].name));
		while(getchar() != '\n');


		if(n == 0)
		{
			printf("\nNieprawidlowa nazwa");
			r = 1;
		}
	} while(r == 1);

	do
	{
		r = 0;
		printf("\nPodaj lige (1, 2, 3): ");
		n = scanf(" %255[^\n]s", &s);
		while(getchar() != '\n');

		if((strcmp(s, "1") && strcmp(s, "2") && strcmp(s, "3")) || n == 0)
		{
			printf("\nNieprawidlowa liga");
			r = 1;
		}
	}while(r == 1);

	(*clubArrayPointer)[*length - 1].league = s[0] - '0';

	do
	{
		r = 0;
		printf("\nPodaj liczbe fanow: ");
		n = scanf(" %lu", &((*clubArrayPointer)[*length - 1].fansNumber));
		while(getchar() != '\n');

		if(n == 0)
		{
			printf("Nieprawidlowa liczba fanow");
			r = 1;
		}
	}while(r == 1);

	do
	{
		r = 0;
		printf("\nPodaj pojemnosc stadionu: ");
		n = scanf(" %lu", &((*clubArrayPointer)[*length - 1].stadiumCapacity));
		while(getchar() != '\n');

		if(n == 0)
		{
			printf("Nieprawidlowa pojemnosc stadionu");
			r = 1;
		}
	}while(r == 1);

	do
	{
		r = 0;
		printf("\nPodaj budzet (w zl): ");
		n = scanf(" %lu", &((*clubArrayPointer)[*length - 1].budget));
		while(getchar() != '\n');

		if(n == 0)
		{
			printf("Nieprawidlowy budzet");
			r = 1;
		}
	}
	while(r == 1);

	do
	{
		r = 0;
		printf("\nPodaj pozycje w lidze: ");
		n = scanf(" %u", &((*clubArrayPointer)[*length - 1].leaguePosition));
		while(getchar() != '\n');

		if(n == 0)
		{
			printf("Nieprawidlowa pozycja");
			r = 1;
		}
	}while(r == 1);

	return 1;
}

/* Usuwanie istniejacego klubu o podanym indeksie
   Indeks klubu w strukturze pokrywa sie z indeksem klubu w tablicy + 1 */
int RemoveClub(struct Club **clubArrayPointer, int *length, int index)
{
	int i;
	/* Kopia zapasowa usuwanego klubu, w razie gdyby trzeba bedzie sie cofnac */
	struct Club t = (*clubArrayPointer)[index];
	struct Club *p;

	(*length)--;


	/* Przesuwamy wszystko wieksze od indeksu w dol */
	for(i = index; i < *length; i++)
	{
		(*clubArrayPointer)[i] = (*clubArrayPointer)[i+1];
		(*clubArrayPointer)[i].index--;
	}

	p = Reallocate(*clubArrayPointer, *length);

	if(p == NULL)
	{
		(*length)++;

		for(i = *length - 1; i < index; i--)
		{
			(*clubArrayPointer)[i] = (*clubArrayPointer)[i-1];
			(*clubArrayPointer)[i].index++;
		}

		(*clubArrayPointer)[index] = t;

		return 0;
	}

	*clubArrayPointer = p;

	return 1;
}
