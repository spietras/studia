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

int main()
{
    struct Club c1 = { "abc", 2, 435435, 123123, 3453454, 1};

    ShowClub(c1);
}

void ShowClub(struct Club c)
{
	printf("\nNazwa: %s\nLiga: %u\nLiczba kibicow: %llu\nPojemnosc stadionu: %llu\nBudzet: %llu\nPozycja w lidze: %u\n", c.name, c.league, c.fansNumber, c.stadiumCapacity, c.budget, c.leaguePosition);
}
