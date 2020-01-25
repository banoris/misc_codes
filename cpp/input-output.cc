#include <cstdio>
#include <iostream>
#include <fstream>
#include <string>

int main(int argc, char const *argv[]) {
	std::ifstream myfile("hostlist.txt");

	std::string host;
	while (myfile >> host) {
		std::cout << host << "\n";
	}

	return 0;
}
