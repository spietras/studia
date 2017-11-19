#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

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

int Initialize(struct Club **, int *, struct Club **, int *);
struct Club * Allocate(struct Club *, int n);
struct Club * Reallocate(struct Club *, int n);
void ShowClub(struct Club *);
void ShowAll(struct Club *, int);
int AddClub(struct Club **, int *);
int RemoveClub(struct Club **, int *);
struct Club * Search(struct Club *, int, struct Club **, int *);
struct Club * SearchByExact(struct Club *, int, struct Club **, int *, int);
struct Club * SearchByRange(struct Club *, int, struct Club **, int *, int);
struct Club * SearchByMore(struct Club *, int, struct Club **, int *, int);
struct Club * SearchByLess(struct Club *, int, struct Club **, int *, int);
unsigned long long GetValue(struct Club *, int, int);
int ShowMenu(struct Club **, int *, struct Club **, int *);
int ToNumber(char *, int);

int main()
{
	struct Club *clubs, *out;
	int length, outLength;

    if(!Initialize(&clubs, &length, &out, &outLength))
	{
		printf("\nBlad inicjalizacji\n");
		return 0;
	}

    while(ShowMenu(&clubs, &length, &out, &outLength));

    return 0;
}

/* Dodanie przykladowych klubow na poczatku programu */
int Initialize(struct Club **clubArrayPointer, int *length, struct Club **out, int *outLength)
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

	*outLength = 1;
	*out = Allocate(*out, *outLength);

	if(*out == NULL)
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
	int n, r, d, i;
	unsigned int pos;
	char c;
	char s[256];
	struct Club *p;
	struct Club cl;

	(*length)++;

	p = Reallocate(*clubArrayPointer, *length);

	if(p == NULL)
	{
		(*length)--;
		*clubArrayPointer = Allocate(*clubArrayPointer, *length);
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
			printf("\nNieprawidlowa nazwa\n");
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
			printf("\nNieprawidlowa liga\n");
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
			printf("\nNieprawidlowa liczba fanow\n");
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
			printf("\nNieprawidlowa pojemnosc stadionu\n");
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
			printf("\nNieprawidlowy budzet\n");
			r = 1;
		}
	}
	while(r == 1);

	do
	{
		r = 0;
		d = 0;
		printf("\nPodaj pozycje w lidze: ");
		n = scanf(" %u", &pos);
		while(getchar() != '\n');

		for(i = 0; i < *length - 1; i++)
		{
			cl = (*clubArrayPointer)[i];

			if((cl.league == (*clubArrayPointer)[*length - 1].league) && cl.leaguePosition == pos)
			{
				d = 1;
				break;
			}
		}

		if(n == 0 || d == 1)
		{
			printf("\nNieprawidlowa pozycja\n");
			r = 1;
		}
	}while(r == 1);

	(*clubArrayPointer)[*length - 1].leaguePosition = pos;

	return 1;
}

/* Usuwanie istniejacego klubu o podanym indeksie
   Indeks klubu w strukturze pokrywa sie z indeksem klubu w glownej tablicy + 1 */
int RemoveClub(struct Club **clubArrayPointer, int *length)
{
	int i, r, n, index;
	char s[256];
	struct Club t;
	struct Club *p;

	do
	{
		r = 0;
		printf("\nPodaj indeks klubu do usuniecia: ");
		n = scanf(" %255[^\n]s", &s);
		while(getchar() != '\n');

		index = ToNumber(s, 256);

		if(index == 0 || n == 0)
		{
			printf("\nNieprawidlowy indeks\n");
			r = 1;
		}
	}while(r == 1);

	index--;
	/* Kopia zapasowa usuwanego klubu, w razie gdyby trzeba bedzie sie cofnac */
	t = (*clubArrayPointer)[index];

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

		*clubArrayPointer = Allocate(*clubArrayPointer, *length);

		for(i = *length - 1; i > index; i--)
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

/* Wyszukiwanie klubu */
struct Club * Search(struct Club *clubArray, int length, struct Club **out, int *outLength)
{
	int field, mode, r, n;
	char s[256];

	/* Uzytkownik podaje co i jak chce wyszukac */

	do
	{
		r = 0;
		printf("\nWybierz wedlug czego chcesz wyszukac:\n1. Wedlug liczby kibicow\n2. Wedlug pojemnosci stadionu\n\n");
		n = scanf(" %255[^\n]s", &s);
		while(getchar() != '\n');

		if((strcmp(s, "1") && strcmp(s, "2")) || n == 0)
		{
			printf("\nNieprawidlowy wybor\n");
			r = 1;
		}
	}while(r == 1);

	field = s[0] - '0';

	do
	{
		r = 0;
		printf("\nWybierz jak chcesz wyszukac:\n1. Dokladna wartosc\n2. Pomiedzy\n3. Wiecej niz\n4. Mniej niz\n\n");
		n = scanf(" %255[^\n]s", &s);
		while(getchar() != '\n');

		if((strcmp(s, "1") && strcmp(s, "2") && strcmp(s, "3") && strcmp(s, "4")) || n == 0)
		{
			printf("\nNieprawidlowy wybor\n");
			r = 1;
		}
	}while(r == 1);

	mode = s[0] - '0';

	switch(mode)
	{
	case 1:
		return SearchByExact(clubArray, length, out, outLength, field);
	case 2:
		return SearchByRange(clubArray, length, out, outLength, field);
	case 3:
		return SearchByMore(clubArray, length, out, outLength, field);
	case 4:
		return SearchByLess(clubArray, length, out, outLength, field);
	}

	return NULL;
}

/* Szukanie dokladnej wartosci */
struct Club * SearchByExact(struct Club *clubArray, int length, struct Club **out, int *outLength, int field)
{
	int r, n, i;
	unsigned long value;

	do
	{
		r = 0;
		printf("\nPodaj dokladna wartosc: ");
		n = scanf(" %lu", &value);
		while(getchar() != '\n');

		if(n == 0)
		{
			printf("\nNieprawidlowa wartosc\n");
			r = 1;
		}
	}while(r == 1);

	for(i = 0, *outLength = 0; i < length; i++)
	{
		if(GetValue(clubArray, i, field) == value)
		{
			*out = Reallocate(*out, *outLength + 1);
			(*out)[*outLength] = clubArray[i];
			(*outLength)++;
		}
	}

	if(*outLength == 0)
	{
		return NULL;
	}

	return *out;
}

/* Szukanie wartosci w przedziale */
struct Club * SearchByRange(struct Club *clubArray, int length, struct Club **out, int *outLength, int field)
{
	int r, n, i;
	unsigned long valueMin, valueMax, v;

	do
	{
		r = 0;
		printf("\nPodaj wartosc minimalna: ");
		n = scanf(" %lu", &valueMin);
		while(getchar() != '\n');

		if(n == 0)
		{
			printf("\nNieprawidlowa wartosc\n");
			r = 1;
		}
	}while(r == 1);

	do
	{
		r = 0;
		printf("\nPodaj wartosc maksymalna: ");
		n = scanf(" %lu", &valueMax);
		while(getchar() != '\n');

		if(n == 0)
		{
			printf("\nNieprawidlowa wartosc\n");
			r = 1;
		}
	}while(r == 1);

	for(i = 0, *outLength = 0; i < length; i++)
	{
		v = GetValue(clubArray, i, field);
		if(v >= valueMin && v <= valueMax)
		{
			*out = Reallocate(*out, *outLength + 1);
			(*out)[*outLength] = clubArray[i];
			(*outLength)++;
		}
	}

	if(*outLength == 0)
	{
		return NULL;
	}

	return *out;
}

/* Szukanie wartosci wiekszej niz podana */
struct Club * SearchByMore(struct Club *clubArray, int length, struct Club **out, int *outLength, int field)
{
int r, n, i;
	unsigned long value;

	do
	{
		r = 0;
		printf("\nPodaj wartosc: ");
		n = scanf(" %lu", &value);
		while(getchar() != '\n');

		if(n == 0)
		{
			printf("\nNieprawidlowa wartosc\n");
			r = 1;
		}
	}while(r == 1);

	for(i = 0, *outLength = 0; i < length; i++)
	{
		if(GetValue(clubArray, i, field) > value)
		{
			*out = Reallocate(*out, *outLength + 1);
			(*out)[*outLength] = clubArray[i];
			(*outLength)++;
		}
	}

	if(*outLength == 0)
	{
		return NULL;
	}

	return *out;
}

/* Szukanie wartosci mniejszej niz podana */
struct Club * SearchByLess(struct Club *clubArray, int length, struct Club **out, int *outLength, int field)
{
	int r, n, i;
	unsigned long value;

	do
	{
		r = 0;
		printf("\nPodaj wartosc: ");
		n = scanf(" %lu", &value);
		while(getchar() != '\n');

		if(n == 0)
		{
			printf("\nNieprawidlowa wartosc\n");
			r = 1;
		}
	}while(r == 1);

	for(i = 0, *outLength = 0; i < length; i++)
	{
		if(GetValue(clubArray, i, field) < value)
		{
			*out = Reallocate(*out, *outLength + 1);
			(*out)[*outLength] = clubArray[i];
			(*outLength)++;
		}
	}

	if(*outLength == 0)
	{
		return NULL;
	}

	return *out;
}

/* Funkcja zwracajaca wartosc w danym polu */
unsigned long long GetValue(struct Club *clubArray, int index, int field)
{
	switch(field)
	{
	case 1:
		return clubArray[index].fansNumber;
	case 2:
		return clubArray[index].stadiumCapacity;
	}

	return 0;
}

/* Wyswietlanie glownego menu */
int ShowMenu(struct Club **clubArrayPointer, int *length, struct Club **out, int *outLength)
{
	int r, n, m;
	char s[256];
	printf("\nBaza danych klubow pilkarskich\n");

	do
	{
		r = 0;
		printf("\nCo chcesz zrobic?\n");
		printf("\n1. Pokazac wszystkie kluby\n2. Wyszukac kluby\n3. Dodac klub\n4. Usunac klub\n5. Wyjsc\n\n");
		n = scanf(" %255[^\n]s", &s);
		while(getchar() != '\n');

		if((strcmp(s, "1") && strcmp(s, "2") && strcmp(s, "3") && strcmp(s, "4") && strcmp(s, "5")) || n == 0)
		{
			printf("\nNieprawidlowy wybor\n");
			r = 1;
		}
	}while(r == 1);

	m = s[0] - '0';

	switch(m)
	{
	case 1:
		ShowAll(*clubArrayPointer, *length);
		printf("\nNacisnij enter, zeby kontynuowac...\n");
		while(getchar() != '\n');
		return 1;
	case 2:
		*out = Search(*clubArrayPointer, *length, out, outLength);
		if(*out == NULL)
		{
			printf("\nNie znaleziono\n");
			printf("\nNacisnij enter, zeby kontynuowac...\n");
			while(getchar() != '\n');
			return 1;
		}
		else
		{
			ShowAll(*out, *outLength);
			printf("\nNacisnij enter, zeby kontynuowac...\n");
			while(getchar() != '\n');
			return 1;
		}
		return 0;
	case 3:
		if(!AddClub(clubArrayPointer, length))
		{
			printf("\nBlad przy dodawaniu klubu\n");
			return 1;
		}
		else
		{
			printf("\nPomyslnie dodano klub\n");
			printf("\nNacisnij enter, zeby kontynuowac...\n");
			while(getchar() != '\n');
			return 1;
		}
		return 0;
	case 4:
		if(!RemoveClub(clubArrayPointer, length))
		{
			printf("\nBlad przy usuwaniu klubu\n");
			printf("\nNacisnij enter, zeby kontynuowac...\n");
			while(getchar() != '\n');
			return 1;
		}
		else
		{
			printf("\nPomyslnie usunieto klub\n");
			printf("\nNacisnij enter, zeby kontynuowac...\n");
			while(getchar() != '\n');
			return 1;
		}
		return 0;
	case 5:
		return 0;
	}

	return 0;
}

/* Konwersja (i sprawdzanie) liczby w stringu na inta */
int ToNumber(char *string, int length)
{
	int i, j, n = 0;
	char c;

	for(i = 0; i < length; i++)
	{
		if(string[i] == '\0')
		{
			length = i;
			break;
		}
	}

	for(i = length - 1, j = 0; i > 0; i--, j++)
	{
		c = string[i];

		if(c >= '0' && c <= '9')
		{
			n += (c - '0')*pow(10, j);
		}
		else
		{
			return 0;
		}
	}

	c = string[0];

	if(c >= '1' && c <= '9')
	{
		n += (c - '0')*pow(10, j);
	}
	else
	{
		return 0;
	}

	return n;
}
