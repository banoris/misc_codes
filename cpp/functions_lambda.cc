#include <cstdio>
#include <cstdint>
#include <iostream>
#include <bitset>
#include <functional>

/*
In English: tranform() is a template function that receives
a function object, Fn, that will map in[i] -> Fn operation -> out[i]
Like math: f(x) = 2x
 */
template <typename Fn>
void transform(Fn fn, const int* in, int * out, size_t length)
{
    for (size_t i = 0; i < length; i++) {
        out[i] = fn(in[i]);
    }
}

void static_func()
{
    printf("A static function.\n");
}

int main(int argc, char const *argv[])
{
    /******************** Example 1 ********************/

    // If you call empty function,
    // will get bad_function_call exception
    std::function<void()> func;

    try {
        func();
    }
    catch (const std::bad_function_call& e) {
        printf("Exception: %s\n", e.what());
    }

    std::function<void()> mylambda { [] { printf("A lambda\n"); } };
    mylambda();
    mylambda = static_func;
    mylambda();

    /******************** Example 2 ********************/
    /*
     * Result of out depends on the function object arg, fn,
     * in transform()
     */

    // TODO: constexpr ???
    constexpr size_t len = 3;

    int inputs[] = {1, 2, 3};
    int out1[len], out2[len], out3[len];

    transform([](int x) {
        return 1;
    }, inputs, out1, len);

    transform([](int x) {
        return x;
    }, inputs, out2, len);

    // math: y = mx + c
    transform([](int x) {
        return 5*x + 10;
    }, inputs, out3, len);

    for (int i = 0; i < len; i++) {
        printf("i=%d: out1[i]=%d, out2[i]=%d, out3[i]=%d\n",
            i, out1[i], out2[i], out3[i]);
    }

    return 0;
}
