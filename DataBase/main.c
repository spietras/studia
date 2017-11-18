#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Club
{
	char name[256];
	int league;
	unsigned long long fansNumber;
	unsigned long long stadiumCapacity;
	unsigned long long budget;
	unsigned int leaguePosition;
};

int Initialize(struct Club **, int *);
struct Club * Allocate(struct Club *, int n);
struct Club * Reallocate(struct Club *, int n);
void ShowClub(struct Club *);
void ShowAll(struct Club *, int);
int AddClub(struct Club **, int *);

int main()
{
	struct Club *clubs;
	int length;

    if(Initialize(&clubs, &length) == 0)
	{
		printf("Blad inicjalizacji");
		return 0;
	}

    if(AddClub(&clubs, &length) == 0)
	{
		printf("Blad przy dodawaniu klubu");
		return 0;
	}

    ShowAll(clubs, length);

    return 0;
}

int Initialize(struct Club **clubArrayPointer, int *length)
{
	struct Club c1 = { "Legia Warszawa", 1, 100000, 60000, 200000000, 1};
	struct Club c2 = { "Wislaw Krakow" , 1, 80000, 50000, 100000000, 2};
	struct Club c3 = { "Arka Gdynia", 1, 50000, 45000, 50000000, 3};

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

struct Club * Allocate(struct Club *clubArray, int length)
{
	return malloc(length * sizeof(*clubArray));
}

struct Club * Reallocate(struct Club *clubArray, int length)
{
	return realloc(clubArray, length * sizeof(*clubArray));
}

void ShowClub(struct Club *club)
{
	printf("\nNazwa: %s\nLiga: %u\nLiczba kibicow: %llu\nPojemnosc stadionu: %llu\nBudzet: %llu\nPozycja w lidze: %u\n", (*club).name, (*club).league, (*club).fansNumber, (*club).stadiumCapacity, (*club).budget, (*club).leaguePosition);
}

void ShowAll(struct Club *clubArray, int length)
{
	int i;

	for(i = 0; i < length; i++)
	{
		ShowClub(&(clubArray[i]));
	}
}

int AddClub(struct Club **clubArrayPointer, int *length)
{
	int n, r;
	char c;

	(*length)++;

	*clubArrayPointer = Reallocate(*clubArrayPointer, *length);

	if(*clubArrayPointer == NULL)
	{
		return 0;
	}

	do
	{
		r = 0;
		printf("\nPodaj nazwe klubu: ");
		n = scanf(" %255[^\n]s %*s", &((*clubArrayPointer)[*length - 1].name));

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
		n = scanf(" %c", &c);
		while(getchar() != '\n');

		if((c != '1' && c != '2' && c != '3') || n == 0)
		{
			printf("\nNieprawidlowa liga");
			r = 1;
		}
	}while(r == 1);

	(*clubArrayPointer)[*length - 1].league = c - '0';

	do
	{
		r = 0;
		printf("\nPodaj liczbe fanow: ");
		n = scanf(" %llu", &((*clubArrayPointer)[*length - 1].fansNumber));
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
		n = scanf(" %llu", &((*clubArrayPointer)[*length - 1].stadiumCapacity));
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
		printf("\nPodaj budzet: ");
		n = scanf(" %llu", &((*clubArrayPointer)[*length - 1].budget));
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
