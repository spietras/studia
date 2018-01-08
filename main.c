#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "helpers.h"
#include "definitions.h"
#include "entries_operations.h"
#include "volume_operations.h"
#include "user_interaction.h"


/**
	Lista możliwych parametrów:
	"-load <name>" - Wczytuje wolumin o podanej nazwie <name>
	"-format <name> <size>" - Tworzy nowy wolumin o podanej nazwie <name> i rozmiarze <size> (maksymalna ilość możliwych danych do zapisania w bajtach)
	"-resize <name> <size>" - Zmienia rozmiar woluminu o podanej nazwie <name> i wczytuje ten wolumin do programu
*/
int main(int argc, char** argv)
{
	Volume* v;

	if(argc < 1 || argc > 4)
	{
		printf("\nNiewlasciwa ilosc argumentow\n");
		return 0;
	}
	else if(argc > 1)
	{
		if(argc == 3 && strcmp(argv[1], "-load") == 0)
		{
			v = Load(argv[2]);
			if(v == NULL)
			{
				printf("\nNie mozna wczytac tego woluminu\n");
				return 0;
			}
		}
		else if(argc == 4 && strcmp(argv[1], "-format") == 0)
		{
			v = CreateVolume(argv[2], atoi(argv[3]));
			if(v == NULL)
			{
				printf("\nNie mozna sformatowac tego woluminu\n");
				return 0;
			}
		}
		else if(argc == 4 && strcmp(argv[1], "-resize") == 0)
		{
			v = Load(argv[2]);
			if(v == NULL)
			{
				printf("\nNie mozna wczytac tego woluminu\n");
				return 0;
			}
			if(!ResizeVolume(v, atoi(argv[3])))
			{
				printf("\nNie mozna zmienic rozmiaru\n");
				return 0;
			}
		}
		else
		{
			printf("\nNiewlasciwy parametr\n");
			return 0;
		}
	}
	else
	{
		v = InitializeVolume();
		if(v == NULL)
		{
			printf("\nNie można zainicjalizowac woluminu\n");
			return 0;
		}
	}

    ShowMenu(v);

	return 0;
}

