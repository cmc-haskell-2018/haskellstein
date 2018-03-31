#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
	int i=0, j=0;
	srand(time(NULL));
	for(i = 0; i < 15; i++)
		printf("w01 ");
	printf("w01\n");
	for(i = 0; i < 14; i++){
		printf("w01 ");
		for(j = 1; j < 15; j++)
			if(i == 0 && j == 1)
				printf("p00 ");
			else{
				int random = rand()%50;
				if(random > 2)
					printf("v00 ");
				else if (random == 1)
					printf("e01 ");
				else if (random == 2)
					printf("e02 ");
				else
					printf("e03 ");
			}
		printf("w01\n");
	}
	for(i = 0; i < 15; i++)
		printf("w01 ");
	printf("w01");
	return 0;
}
