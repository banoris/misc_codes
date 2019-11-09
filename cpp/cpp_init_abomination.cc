#include <cstdio>
#include <cstdint>
#include <iostream>
#include <string>

struct MyPODStruct {
	int my_int;
	std::string my_string;
	bool my_bool;
};

struct MyClass {
	MyClass() {
		printf("Default ctor for MyClass\n");
	}

	MyClass(int x) {
		printf("Ctor for MyClass with int x=%d\n", x);
	}

};

int main(int argc, char const *argv[])
{
	// Init to 0
	int a = 0;
	int b{}; // braced initialization
	int b1{0};
	int c = {}; // equal+braced initialization
	int c1 = {0};
	int d; // maybe 0, undefined behavior

	// Init to 42. Don't confuse f' and 'h'
	int e = 42;
	int f{42};
	int g = {42};
	int h(42); // parentheses init


	// Init a generic POD to zero
	MyPODStruct ms1{};
	MyPODStruct ms2{0};
	MyPODStruct ms3 = {0};
	// MyPODStruct ms4 = 0; // compile error


	// Init generic POD to other value
	MyPODStruct ms4{11, "Ikan"}; // my_bool will implicitly set to 0
	MyPODStruct ms5{11, "Ikan", true};
	// MyPODStruct ms5(11, "Ikan", true); // parentheses - compile error
	// TODO:magic - why not compile error ms6()? But when you try to access
	//   the field, compiler throws error
	MyPODStruct ms6();
	// std::cout << ms6.my_bool; // Compile error


	// Init class.
	MyClass mc1;
	MyClass mc2{5}; // ctor with int called
	MyClass mc3(6);
	MyClass mc4 = {7};
	MyClass mc5{};
	// NOTE: Don't use this. Nothing gets printed. Can you guess why?
	//   Looks like function call right? Google most vexing parse
	MyClass mc6();

	/* Moral of the story: braces init almost always the right choice
	 * Plot twist: there are some exceptions -__-
	 */

	return 0;
}
