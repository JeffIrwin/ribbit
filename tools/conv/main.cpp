
#include <iostream>

#include <assimp/Exporter.hpp>
#include <assimp/Importer.hpp>   // C++ importer interface
#include <assimp/scene.h>        // Output data structure
#include <assimp/postprocess.h>  // Post processing flags

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
		aiProcess_CalcTangentSpace      |
		aiProcess_Triangulate           |
		aiProcess_JoinIdenticalVertices |
		aiProcess_SortByPType);

	// If the import failed, report it
	if (scene == nullptr)
	{
		//DoTheErrorLogging( importer.GetErrorString());
		std::cout << importer.GetErrorString() << std::endl;
		return EXIT_FAILURE;
	}

	Assimp::Exporter exporter;

	// TODO: file extension should probably be converted to lowercase because
	// assimp accepts "stl" but not "STL"
	int dot_pos = outfile.find_last_of('.');
	std::string outfile_ext = outfile.substr(dot_pos + 1, outfile.length() - dot_pos - 1);
	//std::string outfile_ext = "obj";

	std::cout << "outfile extension = \"" << outfile_ext << "\"" << std::endl;

	int status = exporter.Export(scene, outfile_ext, outfile);
	std::cout << "status = " << status << std::endl;
	if (status != 0)
	{
		std::cout << "Error:  cannot export outfile \"" << outfile << "\"" << std::endl;
	}

	// We're done. Everything will be cleaned up by the destructors
	return status;
}

//==============================================================================

int main(int argc, char* argv[])
{
	//std::cout << "hello conv" << std::endl;

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

