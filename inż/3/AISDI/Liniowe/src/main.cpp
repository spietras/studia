#include <cstddef>
#include <cstdlib>
#include <string>
#include <cmath>
#include <iostream>
#include <chrono>

#include "Vector.h"
#include "LinkedList.h"

namespace
{

template <typename T>
using LinkedList = aisdi::LinkedList<T>;

template <typename T>
using Vector = aisdi::Vector<T>;

} // namespace

int main()
{
    auto before = std::chrono::system_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now() - before);
    auto result = duration.count()/1000.0;
    for(int i = 2; i <=5; i++)
    {
        LinkedList<std::string> testList;
        Vector<std::string> testVector;

        std::size_t quantity = (std::size_t)pow(10, i);
        for(std::size_t i = 0; i < quantity; i++)
        {
            testList.append("XD");
            testVector.append("XD");
        }

        before = std::chrono::system_clock::now();

        testList.append("XD");

        duration = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now() - before);
        result = duration.count()/1000.0;

        std::cout << "List append() duration: " << result << " ms with 10^" << i << " elements" << std::endl;

        before = std::chrono::system_clock::now();

        testList.append("XD");

        duration = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now() - before);
        result = duration.count()/1000.0;

        std::cout << "Vector append() Duration: " << result << " ms with 10^" << i << " elements" << std::endl;
    }

    return 0;
}
