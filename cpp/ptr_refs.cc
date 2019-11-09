#include <memory>
#include <cstdio>
#include <iostream>
#include <bitset>
#include <array>

using namespace std;

struct MyClass {
public:
	MyClass() {
		printf("Default ctor for MyClass\n");
	}
	MyClass(int x) {
		printf("Ctor for MyClass with int x=%d\n", x);
		data = x;
	}

	int data;
};


int main(int argc, char const *argv[])
{

	/*
		Many different ways to use instantiate and assign object
		to unique_ptr. Protips: just stick to make_unique
	 */
	unique_ptr<MyClass> mc1 = make_unique<MyClass>();
	unique_ptr<MyClass> mc4 = make_unique<MyClass>(7);
	unique_ptr<MyClass> mc2{new MyClass()}; // default ctor
	unique_ptr<MyClass> mc3{new MyClass(5)}; // int ctor
	cout << "mc3->data = " << mc3->data << endl;

	/*
		Using uniq_ptr.get(), you can get a raw pointer.
		You can also change the underlying object.
		Where is the uniqueness then you might ask? ¯\_(ツ)_/¯
		To be safe, use 'const' keyword to avoid a pointer
		to modify the object that it is pointing
	 */
	MyClass *p_mc = mc3.get();
	p_mc = mc1.get();
	cout << "p_mc->data = " << p_mc->data << endl;
	p_mc->data = 55;
	cout << "p_mc->data = " << p_mc->data << endl;


	return 0;
}
