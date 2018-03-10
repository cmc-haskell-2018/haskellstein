#include <stdio.h>

int main()
{
	int i=0, j=0;
	for(i = 0; i < 31; i++)
		printf("w01 ");
	printf("w01\n");
	for(i = 0; i < 30; i++){
		printf("w01 ");
		for(j = 1; j < 31; j++)
			if(i == 0 && j == 1)
				printf("p00 ");
			else
				printf("v00 ");
		printf("w01\n");
	}
	for(i = 0; i < 31; i++)
		printf("w01 ");
	printf("w01");
	return 0;
}
