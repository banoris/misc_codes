#include <cstdio>
#include <iostream>
#include <string>

int myglobal = 0;

// https://www.youtube.com/watch?v=UTUdhjzws5g
/**
 * Summary
 *
 * 1. Every C++ expression yields either an rvalue or a lvalue
 * 2. If the expression has an identifiable memory address, it's lvalue;
 *    otherwise, it's and rvalue
 */


// TODO: foo returns an int ref. Shouldn't it be `return &myglobal`?
//   But compile error...
//   Well, & is syntactic sugar for * pointer. Relate with add1 below
int& foo() {
	return myglobal;
}

void add1(int& input) {
	input += 1;
}

void add1(int&& rvalue) {
	rvalue += 1;
	std::cout << "rvalue=" << rvalue << "\n";
}

int main(int argc, char const *argv[]) {

	/***** Misconception1: function or operator value always yield rvalue *****/

	// Below is a counter example where foo() returns an lvalue
	// and we modify it. C++ is complicated -__-
	std::cout << "BEFORE myglobal=" << myglobal << "\n"; // myglobal=0
	foo() = 123;
	std::cout << "AFTER myglobal=" << myglobal << "\n"; // myglobal=123

	int input = 2;
	std::cout << "input=" << input << "\n"; // 2

	add1(input);
	std::cout << "input=" << input << "\n"; // 3

	add1(6); // print rvalue=7

	return 0;
}