#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Club
{
	char name[256];
	unsigned int league;
	unsigned long long fansNumber;
	unsigned long long stadiumCapacity;
	unsigned long long budget;
	unsigned int leaguePosition;
};

void Initialize(struct Club *, int *);
void ShowClub(struct Club *);
void ShowAll(struct Club *, int);
void AddClub(struct Club *, int *);

int main()
{
	struct Club clubs[100];
	int length;

    Initialize(clubs, &length);

    AddClub(clubs, &length);

    ShowAll(clubs, length);

    return 0;
}

void Initialize(struct Club *c, int *n)
{
	struct Club c1 = { "Legia Warszawa", 1, 100000, 60000, 200000000, 1};
	struct Club c2 = { "Wislaw Krakow" , 1, 80000, 50000, 100000000, 2};
	struct Club c3 = { "Arka Gdynia", 1, 50000, 45000, 50000000, 3};


	c[0] = c1;
	c[1] = c2;
	c[2] = c3;

	*n = 3;
}

void ShowClub(struct Club *c)
{
	printf("\nNazwa: %s\nLiga: %u\nLiczba kibicow: %llu\nPojemnosc stadionu: %llu\nBudzet: %llu\nPozycja w lidze: %u\n", (*c).name, (*c).league, (*c).fansNumber, (*c).stadiumCapacity, (*c).budget, (*c).leaguePosition);
}

void ShowAll(struct Club *c, int n)
{
	int i;

	for(i = 0; i < n; i++)
	{
		ShowClub(&c[i]);
	}
}

void AddClub(struct Club *c, int *n)
{
	*n = *n + 1;

	printf("\nPodaj nazwe klubu: ");
	scanf("%s", &(c[*n - 1].name));

	printf("\nPodaj lige (1, 2, 3): ");
	scanf("%u", &(c[*n - 1].league));

	printf("\nPodaj liczbe fanow: ");
	scanf("%llu", &(c[*n - 1].fansNumber));

	printf("\nPodaj pojemnosc stadionu: ");
	scanf("%llu", &(c[*n - 1].stadiumCapacity));

	printf("\nPodaj budzet: ");
	scanf("%llu", &(c[*n - 1].budget));

	printf("\nPodaj pozycje w lidze: ");
	scanf("%u", &(c[*n - 1].leaguePosition));
}
