#include <cstdio>
#include <iostream>
#include <bitset>
#include <experimental/optional>

int main(int argc, char const *argv[])
{
	std::experimental::optional<unsigned short> port;
	printf("Pre-decrement, decrement first, check condition after that\n");
	int cycle_len = 4; // printed 3 times
	while (--cycle_len) {
		printf("\tcycle_len=%d\n", cycle_len);
	}

	printf("Post-decrement, check condition first, increment after that\n");
	cycle_len = 4; // printed 4 times
	while (cycle_len--) {
		printf("\tcycle_len=%d\n", cycle_len);
	}

    return 0;
}
