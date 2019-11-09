#include <cstdio>
#include <iostream>
#include <bitset>
#include <array>

using std::array;

short Parity(unsigned long x) {
    short result = 0;
    while(x) {
        result ^= (x & 1);
        x >>= 1;
    }

    return result;
}

/**
 * Build Parity table for 6 bit. 2^6=64
 */
array<short, 1 << 6> BuildTable() {
    array<short, 1 << 6> result;
    for (int i = 0; i < (1 << 6); ++i) {
        result[i] = Parity(i);
        std::cout << "i=" << std::bitset<6>(i) << " Parity(i) = " << Parity(i) << std::endl;
    }
    return result;
}


int main(int argc, char const *argv[])
{
    array<short, 1 << 6> result;
    result = BuildTable();
    return 0;
}
