
#include <stdio.h>

#ifdef _WIN32
	#define del_file_ DEL_FILE
#endif
int del_file_(char* filename)
{
	//printf("filename = \"%s\"\n", filename);
	return remove(filename);
}

#ifdef _WIN32
	#define make_dir_ MAKE_DIR
#endif
int make_dir_(char* dir)
{
	return mkdir(dir, 0755);
}

