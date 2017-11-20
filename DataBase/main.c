#include <stdio.h>
#include <stdlib.h>
#include "functions.h"

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

