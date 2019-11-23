#include <cstdio>
#include <cstdint>
#include <iostream>
#include <string>
#include <cassert>
#include <unordered_set>
#include <algorithm>

using namespace std;

// https://thispointer.com/using-unordered_set-with-custom-hasher-and-comparision-function/

// Custom hasher
struct StrHashByLength {
	size_t operator()(const string &str) const {
		int length = str.length();
		// TODO: why double parentheses
		//   https://stackoverflow.com/questions/40024008/how-to-understand-two-pairs-of-parentheses-in-this-code-fragment
		return hash<int>()(length);
	}
};

// Custom Comparator function
struct StrCompareByLength {
	// Recall const args vs const member function
	// TODO: This is weird. Isn't it comparator operator== ?
	bool operator()(const string &lhs, const string &rhs) const {
		if (lhs.length() == rhs.length())
			return true;
		else
			return false;
	}
};

int main(int argc, char const *argv[])
{
	// NOTE: notice the ordering. Spent quite some time debugging just because
	//   you put hasher arg as the 3rd args to the template (╯°□°）╯︵ ┻━┻)
	//   C++ template compile error is super cryptic and magical, defeating Perl
	//   Recipe: unordered_set<_type_, _hash-func_, _comparator_>
	unordered_set<string, StrHashByLength, StrCompareByLength> setOfStrs;

	setOfStrs.insert("First");
	setOfStrs.insert("Second");

	// Try to insert element with same length as "First"
	// It will not be added
	setOfStrs.insert("third");

	// Try to insert element with same length as "Second"
	// It will not be added

	setOfStrs.insert("second");
	setOfStrs.insert("five");

	// Only three elements are added in set
	// Display them
	for (std::string s : setOfStrs)
		std::cout << s << std::endl;

	return 0;
}
