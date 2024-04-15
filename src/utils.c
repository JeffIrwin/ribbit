
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

#ifndef _WIN32
	#define MAKE_DIR make_dir_
#endif
int MAKE_DIR(char* dir)
{
	return mkdir(dir, 0755);
}

