#include <stdio.h>

struct frec {
	int indx;
	char c[74];
} rec;

void main()
{
	FILE *fp;
	
	char *cp, buf[75];
	int i,x,mask,c;
	
	fp = fopen("dtext.dat","rb");
	x = 0;
	buf[74] = 0;
	while (fread(&rec,sizeof(rec),1,fp) == 1) {
		x++;
		for (i=1; i<75; i++) {
			mask = (x & 31)+i;
			buf[i-1] = rec.c[i-1] ^ mask;
		}
		cp = &buf[73];
		while (*cp == ' ') *cp-- = 0;
		printf("%d %s\n",rec.indx,buf);
	}
}
