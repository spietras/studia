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
		printf("\nInvalid argument amount\n");
		return 0;
	}
	else if(argc > 1)
	{
		if(argc == 3 && strcmp(argv[1], "-load") == 0)
		{
			v = Load(argv[2]);
			if(v == NULL)
			{
				printf("\nCan't load that volume\n");
				return 0;
			}
		}
		else if(argc == 4 && strcmp(argv[1], "-format") == 0)
		{
			v = CreateVolume(argv[2], atoi(argv[3]));
			if(v == NULL)
			{
				printf("\nCan't format that volume\n");
				return 0;
			}
		}
		else if(argc == 4 && strcmp(argv[1], "-resize") == 0)
		{
			v = Load(argv[2]);
			if(v == NULL)
			{
				printf("\nCan't load that volume\n");
				return 0;
			}
			if(!ResizeVolume(v, atoi(argv[3])))
			{
				printf("\nCan't resize that volume\n");
				return 0;
			}
		}
		else
		{
			printf("\nInvalid parameter\n");
			return 0;
		}
	}
	else
	{
		v = InitializeVolume();
	}

    ShowMenu(v);

	return 0;
}

