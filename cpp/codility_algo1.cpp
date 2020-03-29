#include <unordered_set>

/*

Given an array A of N integers, returns the smallest positive integer
(greater than 0) that does not occur in A.
For example, given A = [1, 3, 6, 4, 1, 2], the function should return 5

Given A = [1, 2, 3], the function should return 4.
Given A = [−1, −3], the function should return 1.

*/

int solution(vector<int> &A) {
    // write your code in C++14 (g++ 6.2.0)

    unordered_set<int> num_set;
    for (int a : A) {
        if (a > 0) {
            num_set.insert(a);
        }
    }

    int smallest_possible = 1;

    // trial and error to find smallest number
    // keep on incrementing until we find a number not in set
    // Loop at-most A.size() times
    while (1) {
        auto search = num_set.find(smallest_possible);
        if (search == num_set.end()) {
            return smallest_possible;
        }
        smallest_possible++;
    }

}