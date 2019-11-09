#include <cstdio>
#include <iostream>
#include <bitset>


int main(int argc, char const *argv[])
{
    int x = 43; // 0010 1011
    int mask = 0xF;

    // Expected result: get only the last 4 bits. So 1011 is decimal 11
    std::cout << "x=" << std::bitset<8>(x) << ". After (x & 0xF), x=" <<
    std::bitset<4>(x & mask) << std::endl;

    return 0;
}
