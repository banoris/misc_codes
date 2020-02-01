#include <cstdio>
#include <cstdint>
#include <iostream>
#include <string>
#include <unordered_set>
#include <vector>
#include <cassert>

#include <unordered_set>
#include <algorithm>
#include <queue>
#include <set>

using namespace std;

void queue_container();
void set_container();
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
    // Unordered set practice
    // ------------------------------------------------- //

    // https://thispointer.com/how-to-initialize-an-unordered_set-in-c11/
    // You did some mods on () vs {}. Opinionated guide: stick to braced init

    // 1. Create unordered_set from plain array
    int arr[] = { 2, 4, 6, 1, 3, 6, 8, 3, 2 };

    // 2. Create an unordered set and initialize it with the array
    //   Use parentheses ctor with (arr_var, sizeof(arr_var) / sizeof(arr_type))
    //   This looks somewhat complicated - any other syntax we can use?
    //     YES!! 2.b
    // Set will contain only unique elements -- IMPORTANT!
    unordered_set<int> setOfNum(arr, arr + sizeof(arr) / sizeof(int));
    unordered_set<int> my_setOfNum{arr, arr + sizeof(arr) / sizeof(int)};

    // Iterate over the set and display contents
    for (int val : my_setOfNum)
        cout << val << "  ";
    cout << endl;

    // 2.b -- alternative syntax to sizeof(...) thingy
    cout << "===== C++11 for transforming raw array to unordered_set =====" << endl;
    unordered_set<int> setOfNum4{begin(arr), end(arr)};
    for (int val : setOfNum4)
        cout << val << "  ";
    cout << endl;
    cout << "====================" << endl;

    // Create an unordered set and initialize it initializer_list
    //   TODO: study initializer_list (╯°□°）╯︵ ┻━┻)
    //     - Do we need to init with parentheses + braces? It works if remove '()'
    //       - Yup, removed '()' and everything seems to work fine
    unordered_set<int> setOfNum2( { 1, 2, 3, 1, 3, 4, 2 });
    unordered_set<int> my_setOfNum2{ 1, 2, 3, 1, 3, 4, 2 };

    // Set will contain only unique elements
    // Iterate over the set and display contents
    for (int val : my_setOfNum2)
        cout << val << "  ";
    cout << endl;

    vector<int> vec( { 14, 5, 6, 7 });
    vector<int> my_vec{ 14, 5, 6, 7 };

    // Create an unordered set and initialize it with vector
    unordered_set<int> setOfNum3(vec.begin(), vec.end());
    unordered_set<int> my_setOfNum3{my_vec.begin(), my_vec.end()};

    // Set will contain only unique elements
    // Iterate over the set and display contents
    for (int val : my_setOfNum3)
        cout << val << "  ";
    cout << endl;


    // ------------------------------------------------- //
    // Vector practice                                   //
    // ------------------------------------------------- //


    // An array (aka list in Python) of strings
    vector<string> strings_list{"hello", "world", "test", "123"};

    for (const auto str : strings_list) {
        cout << str << " ";
    }
    cout << endl;

    // int 2d, 3x3 matrix
    cout << "***** 3x3 matrix 2d int vector *****" << endl;
    vector<vector<int>> vec_2d_int{{1, 2, 3},
                                            {4, 5, 6},
                                            {7, 8, 9}};

    for (int i = 0; i < vec_2d_int.size(); i++) {
        for (int j = 0; j < vec_2d_int[i].size(); j++) {
            cout << vec_2d_int[i][j] << " ";
        }
        cout << endl;
    }

    vector<vector<string>> string_2d{ {"hello1", "world1"},
                                                {"hello2", "world2"},
                                                {"hello3", "world3"}};

    for (vector<string>& pstr : string_2d) {
        for (string& str : pstr) {
            cout << str << endl;
        }
    }

    // Using auto compared to above forloop
    // 'const' means you cannot modify the content of the pointer
    // Try change and you will face compile error
    // If you remove 'const', then you can modify
    for (const auto& pstr : string_2d) {
        for (const auto& str : pstr) {
            cout << str << endl;
        }
    }

    queue_container();
    set_container();

    return 0;
}

struct myObject {
    int intField;
    string strField = "Default";

    // Ctor
    myObject(int in_int, string in_str = "Default")
                : intField(in_int), strField(in_str) {
        cout << "myObject Ctor: intField=" << in_int <<
            " strField=" << in_str << "\n";
    }

    // Why only one arg? Because we define inside of class/struct
    // NOTE: you missed the 2nd 'const' causing compile error when
    //   inserting to std::set. Of course the error is super cryptic
    bool operator<(const myObject& rhs) const {
        return intField < rhs.intField;
    }

    // Dtor
    ~myObject() {
        cout << "myObject Dtor: intField=" << intField <<
        " strField=" << strField << "\n";;
    }
};

void queue_container() {
    cout << "\n===== BEGIN Qeueu Practice ====\n\n";

    // NOTE: From Ctor/Dtor print, lots of object copying around
    //   Not the case for heap allocated where only single Ctor/Dtor pair printed
    myObject obj1{1};
    myObject obj2{2};
    myObject obj3{3};

    queue<myObject> obj_q;
    obj_q.push(obj1);
    obj_q.push(obj3); // note the ordering, will impact q.front()
    obj_q.push(obj2);

    // while queue is not empty, cont processing
    while(!obj_q.empty()) {
        myObject obj = obj_q.front();
        cout << "intField=" << obj.intField << "\n";
        obj_q.pop();
    }

    myObject* obj_heap1 = new myObject(11);
    myObject* obj_heap2 = new myObject(22);
    queue<myObject*> obj_q_heap;

    obj_q_heap.push(obj_heap1);
    obj_q_heap.push(obj_heap2);

    while(!obj_q_heap.empty()) {
        myObject* obj = obj_q_heap.front();
        cout << "Heap: intField=" << obj->intField << "\n";
        obj_q_heap.pop();
    }

    delete obj_heap1;
    delete obj_heap2;
    cout << "\n===== END Qeueu Practice ====\n\n";
}

struct DescendCompare {
    bool operator() (const myObject& lhs, const myObject& rhs) {
        return lhs.intField > rhs.intField;
    }
};

// std::set ordering using functor operator() vs operator<
void set_container() {
    cout << "\n===== BEGIN Set Practice ====\n\n";

    set<myObject> myset;
    myObject obj1{11};
    myObject obj2{22};
    myObject obj3{33};

    // Notice the ordering, let's test whether our operator< works
    // We insert obj3=33 as the 1st object but it will get printed last
    // std::set is ordered, don't confuse with unordered_map
    myset.insert(obj3);
    myset.insert(obj1);
    myset.insert(obj2);

    cout << "Test set ordering with custom operator<. Ascending order" << "\n";
    for (auto const& obj : myset) {
        cout << "   intField=" << obj.intField << "\n"; // 11, 22, 33
    }

    cout << "Test set ordering with custom operator(). Descending order" << "\n";
    // notice the 2nd arg
    set<myObject, DescendCompare> myset_descend;
    myset_descend.insert(obj2);
    myset_descend.insert(obj1);
    myset_descend.insert(obj3);

    for (auto const& obj : myset_descend) {
        cout << "   intField=" << obj.intField << "\n"; // 33, 22, 11
    }

    cout << "\n===== END Set Practice ====\n\n";

}