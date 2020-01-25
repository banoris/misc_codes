#include <vector>
#include <cstdio>
#include <iostream>
#include <string>

// Taken and modified from:
// https://lgong30.github.io/skill/2017/01/30/C++11-Move-Semantics-Investigation.html

class exampleA {
    int *x;
    int size;
public:
    // default constructor
    exampleA(): x(NULL), size(0) {
        std::cout << "Default constructor is called" << std::endl;
    }

    // constructor from a std::vector
    exampleA(const std::vector<int>& vec){
        std::cout << "exampleA Ctor with vector called" << std::endl;
        size = vec.size();
        x = new int[size];
        for (int i = 0;i < size;++ i) x[i] = vec[i];
    }

    // copy constructor
    exampleA(const exampleA& other)
    {
        std::cout << "exampleA copy ctor called" << std::endl;
        size = other.size;
        if (size > 0) {
            x = new int[size];
            for (int i = 0;i < size;++ i) x[i] = other.x[i];
        }else
        {
            x = NULL;
        }
    }

    // move constructor
    exampleA(exampleA&& other)
    {
        std::cout << "exampleA move ctor called" << std::endl;
        size = other.size;
        x = other.x;
        other.size = 0;
        other.x = NULL;
    }

    // deconstructor
    ~exampleA() {
        if (x != NULL) delete [] x;
        x = NULL;
        size = 0;
        std::cout << "exampleA Dtor called" << std::endl;
    }
    // friend function: overloading operator <<
    friend std::ostream& operator<<(std::ostream& os, const exampleA& a);
};


// definition of (or implementation of) overloading operator <<
std::ostream& operator<<(std::ostream& os, const exampleA& a){
    for (int i = 0;i < a.size;++ i) os << a.x[i] << " ";
    return os;
}

// function to create an exampleA object
exampleA createObject(){
    exampleA a(std::vector<int>(10, 1));
    return a;
}
// function uses an exampleA object as a parameter
void passByValue(exampleA arg) {
    std::cout << "Dump exampleA: " << arg << "\n";
}

template<typename T>
void forwarder(T&& arg) {
    std::cout << "Dump exampleA: " << std::forward<T>( arg ) << "\n";
}

int main()
{
    std::cout << "Create an exampleA object using default ctor" << "\n";
    exampleA test1;

    std::cout << "======================================\n\n";
    std::cout << "Create an exampleA object using vector" << "\n";
    exampleA test2(std::vector<int>(10, 1));
    std::cout << "======================================\n\n";

    std::cout << "passByValue(test2)" << "\n";
    passByValue(test2);
    std::cout << "======================================\n\n";

    std::cout << "passByValue(createObject())" << "\n";
    passByValue(createObject());

    std::cout << "======================================\n\n";

    std::cout << "***** Using forwarder std::forward *****" << "\n\n";
    std::cout << "forwarder(test2)" << "\n";
    forwarder(test2);
    std::cout << "======================================\n\n";

    std::cout << "forwarder(createObject())" << "\n";
    forwarder(createObject());
    std::cout << "======================================\n\n";
    return 0;
}

/* Output when executed, with your own notes

Create an exampleA object using default ctor  --> test1 created
Default constructor is called
======================================

Create an exampleA object using vector  --> test2 created
exampleA Ctor with vector called
======================================

// lvalue
// test1 is passed-by-value to passByValue function. As you can see,
// copy ctor is called and destroyed at the end of the func call
// This is bad since for large object, copy ctor is expensive
passByValue(test2)
exampleA copy ctor called
Dump exampleA: 1 1 1 1 1 1 1 1 1 1 
exampleA Dtor called
======================================

// rvalue -- createObject() expression yield rvalue
// No copy ctor called here
passByValue(createObject())
exampleA Ctor called
Dump exampleA: 1 1 1 1 1 1 1 1 1 1 
exampleA Dtor called
======================================

// end of main, test1 and test2 instances are out of scope thus destroyed
exampleA Dtor called
exampleA Dtor called

 */

/* Another run using forwarder().
With just one function, it can handle both rvalue and lvalue input
without copy ctor called

// See, MAGIC! No more copy ctor called
forwarder(test2)
Dump exampleA: 1 1 1 1 1 1 1 1 1 1 
======================================

// Works the same as passByValue(createObject())
forwarder(createObject())
exampleA Ctor with vector called
Dump exampleA: 1 1 1 1 1 1 1 1 1 1 
exampleA Dtor called

 */
