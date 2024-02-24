
#include <iostream>

#include <assimp/Exporter.hpp>
#include <assimp/Importer.hpp>      // C++ importer interface
#include <assimp/scene.h>           // Output data structure
#include <assimp/postprocess.h>     // Post processing flags

int convert(const std::string& infile, const std::string& outfile)
{
	std::cout << "infile  = " << infile  << std::endl;
	std::cout << "outfile = " << outfile << std::endl;

	// Create an instance of the Importer class
	Assimp::Importer importer;

	// And have it read the given file with some example postprocessing
	// Usually - if speed is not the most important aspect for you - you'll
	// probably to request more postprocessing than we do in this example.
	const aiScene* scene = importer.ReadFile(infile,
		aiProcess_CalcTangentSpace       |
		aiProcess_Triangulate            |
		aiProcess_JoinIdenticalVertices  |
		aiProcess_SortByPType);

	// If the import failed, report it
	if (nullptr == scene) {
		//DoTheErrorLogging( importer.GetErrorString());
		std::cout << importer.GetErrorString() << std::endl;
		return EXIT_FAILURE;
	}

	//// Now we can access the file's contents.
	//DoTheSceneProcessing( scene);

	Assimp::Exporter exporter;

	// TODO: get export file type from outfile extension
	exporter.Export(scene, "obj", outfile);

	// We're done. Everything will be cleaned up by the importer destructor
	return EXIT_SUCCESS;
}

int main(int argc, char* argv[])
{
	std::cout << "hello conv" << std::endl;

	if (argc != 3)
	{
		std::cout << "Error: bad command-line arguments" << std::endl;
		std::cout << "Usage:" << std::endl;
		std::cout << "    conv file.in file.out" << std::endl;
		return EXIT_FAILURE;
	}

	//printf("%s\n", argv[i]);
	std::string infile  = argv[1];
	std::string outfile = argv[2];

	return convert(infile, outfile);
}

