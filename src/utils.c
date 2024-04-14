
#include <stdio.h>

#ifdef _WIN32
	#define DEL_FILE DEL_FILE
#else
	#define DEL_FILE del_file_
#endif
int DEL_FILE(char* filename)
{
	//printf("filename = \"%s\"\n", filename);
	return remove(filename);
}

