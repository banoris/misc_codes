#include <cstdio>
#include <cstdint>
#include <iostream>
#include <string>
#include <unordered_set>
#include <vector>
#include <cassert>

#include <unordered_set>
#include <algorithm>

using namespace std;

int main(int argc, char const *argv[])
{
	hash<long> hasher_long;
	size_t hashcode_42 = hasher_long(42);
	assert(hashcode_42 == hasher_long(42));

	// Don't confuse with the double parentheses.
	// First one is for instantiating the hash object
	//   - unnamed/anonymous object instantiation?
	// Second is calling the operator() --> recall functor
	assert(hash<long>()(42) == hasher_long(42));


	// ------------------------------------------------- //
	// Unorder set practice
	// ------------------------------------------------- //

	// https://thispointer.com/how-to-initialize-an-unordered_set-in-c11/
	// You did some mods on () vs {}. Opinionated guide: stick to braced init

	// 1. Create unordered_set from plain array
	int arr[] = { 2, 4, 6, 1, 3, 6, 8, 3, 2 };

	// 2. Create an unordered set and initialize it with the array
	//   Use parentheses ctor with (arr_var, sizeof(arr_var) / sizeof(arr_type))
	// Set will contain only unique elements -- IMPORTANT!
	std::unordered_set<int> setOfNum(arr, arr + sizeof(arr) / sizeof(int));
	std::unordered_set<int> my_setOfNum{arr, arr + sizeof(arr) / sizeof(int)};

	// Iterate over the set and display contents
	for (int val : my_setOfNum)
		std::cout << val << "  ";
	std::cout << std::endl;

	// Create an unordered set and initialize it initializer_list
	//   TODO: study initializer_list (╯°□°）╯︵ ┻━┻)
	//     - Do we need to init with parentheses + braces? It works if remove '()'
	//       - Yup, removed '()' and everything seems to work fine
	std::unordered_set<int> setOfNum2( { 1, 2, 3, 1, 3, 4, 2 });
	std::unordered_set<int> my_setOfNum2{ 1, 2, 3, 1, 3, 4, 2 };

	// Set will contain only unique elements
	// Iterate over the set and display contents
	for (int val : my_setOfNum2)
		std::cout << val << "  ";
	std::cout << std::endl;

	std::vector<int> vec( { 14, 5, 6, 7 });
	std::vector<int> my_vec{ 14, 5, 6, 7 };

	// Create an unordered set and initialize it with vector
	std::unordered_set<int> setOfNum3(vec.begin(), vec.end());
	std::unordered_set<int> my_setOfNum3{my_vec.begin(), my_vec.end()};

	// Set will contain only unique elements
	// Iterate over the set and display contents
	for (int val : my_setOfNum3)
		std::cout << val << "  ";
	std::cout << std::endl;

	return 0;
}
