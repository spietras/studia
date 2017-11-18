#include <stdio.h>
#include <stdlib.h>

struct Club
{
	char name[256];
	unsigned int league;
	unsigned long long fansNumber;
	unsigned long long stadiumCapacity;
	unsigned long long budget;
	unsigned int leaguePosition;
};

struct Club * Allocate(struct Club **, int);
void Initialize(struct Club **, int *);
void ShowClub(struct Club);

int main()
{
	struct Club *clubs;
	int length;

    Initialize(&clubs, &length);

    ShowClub(clubs[2]);

    return 0;
}

struct Club * Allocate(struct Club **c, int n)
{
	return malloc(n * sizeof(**c));
}

void Initialize(struct Club **c, int *n)
{
	struct Club c1 = { "Legia Warszawa", 1, 100000, 60000, 200000000, 1};
	struct Club c2 = { "Wislaw Krakow" , 1, 80000, 50000, 100000000, 2};
	struct Club c3 = { "Arka Gdynia", 1, 50000, 45000, 50000000, 3};

	*c = Allocate(c, 3);

	(*c)[0] = c1;
	(*c)[1] = c2;
	(*c)[2] = c3;

	*n = 3;
}

void ShowClub(struct Club c)
{
	printf("\nNazwa: %s\nLiga: %u\nLiczba kibicow: %llu\nPojemnosc stadionu: %llu\nBudzet: %llu\nPozycja w lidze: %u\n", c.name, c.league, c.fansNumber, c.stadiumCapacity, c.budget, c.leaguePosition);
}
