#include <cstddef>
#include <cstdlib>
#include <string>
#include <cmath>
#include <iostream>
#include <chrono>
#include <vector>
#include <iomanip>
#include <sstream>

#include "HashMap.h"
#include "TreeMap.h"

namespace
{

    template<typename K, typename V>
    using HashMap = aisdi::HashMap<K, V>;

    template<typename K, typename V>
    using TreeMap = aisdi::TreeMap<K, V>;

}

void print(const std::string& msg, const std::vector<std::pair<int, std::size_t>>& points)
{
  std::cout << std::endl << msg << std::endl;
  for(const auto& pair : points)
  {
    std::stringstream ss;
    ss << "10^" << pair.first;
    std::string s = ss.str();
    std::cout << std::left << std::setw(10) << s << "\t";
  }
  std::cout << std::endl;
  for(const auto& pair : points) std::cout << std::left << std::setw(10) << pair.second << "\t";
  std::cout << std::endl;

}

int main()
{
  std::chrono::high_resolution_clock::time_point before = std::chrono::high_resolution_clock::now();
  std::chrono::high_resolution_clock::time_point after = std::chrono::high_resolution_clock::now();
  auto duration = std::chrono::duration_cast<std::chrono::nanoseconds>( after - before ).count();
  std::vector<std::pair<int, std::size_t>> hashMapInsert, treeMapInsert, hashMapRemove, treeMapRemove, hashMapCopy, treeMapCopy, hashMapFind, treeMapFind;
  std::vector<std::size_t> temp;

  for(int i = 2; i <= 7; i++)
  {
    HashMap<std::size_t, bool> testHashMap;
    TreeMap <std::size_t , bool> testTreeMap;

    auto quantity = (std::size_t)pow(10, i);
    for(std::size_t k = 0; k < quantity; k++)
    {
        std::cout << "Adding " << k << std::endl;
      testHashMap[k] = true;
      testTreeMap[k] = true;
    }

    /*------------------------------------------------------------------------------------------------------------*/

    for(int j = 0; j < 3; j++) {
      before = std::chrono::high_resolution_clock::now();

      testHashMap[quantity + j] = true;

      after = std::chrono::high_resolution_clock::now();
      duration = std::chrono::duration_cast<std::chrono::nanoseconds>(after - before).count();

      std::cout << "HashMap insert duration: " << duration << " nanos with 10^" << i << " elements" << std::endl;

      temp.push_back(duration);
    }


    hashMapInsert.emplace_back(i, (temp.at(0) + temp.at(1) + temp.at(2))/3);

    temp.clear();

    /*------------------------------------------------------------------------------------------------------------*/

    for(int j = 0; j < 3; j++) {
      before = std::chrono::system_clock::now();

      testTreeMap[quantity + j] = true;

      after = std::chrono::high_resolution_clock::now();
      duration = std::chrono::duration_cast<std::chrono::nanoseconds>(after - before).count();

      std::cout << "TreeMap insert duration: " << duration << " nanos with 10^" << i << " elements"
                << std::endl;

      temp.push_back(duration);

    }

    treeMapInsert.emplace_back(i, (temp.at(0) + temp.at(1) + temp.at(2))/3);

    temp.clear();


    /*------------------------------------------------------------------------------------------------------------*/

    for(int j = 0; j < 3; j++) {
      before = std::chrono::high_resolution_clock::now();

      testHashMap.remove(quantity - 1 - j);

      after = std::chrono::high_resolution_clock::now();
      duration = std::chrono::duration_cast<std::chrono::nanoseconds>(after - before).count();

      std::cout << "HashMap remove duration: " << duration << " nanos with 10^" << i << " elements" << std::endl;

      temp.push_back(duration);

    }

    hashMapRemove.emplace_back(i, (temp.at(0) + temp.at(1) + temp.at(2))/3);

    temp.clear();


    /*------------------------------------------------------------------------------------------------------------*/

    for(int j = 0; j < 3; j++) {
      before = std::chrono::system_clock::now();

      testTreeMap.remove(quantity - 1 - j);

      after = std::chrono::high_resolution_clock::now();
      duration = std::chrono::duration_cast<std::chrono::nanoseconds>(after - before).count();

      std::cout << "TreeMap remove duration: " << duration << " nanos with 10^" << i << " elements" << std::endl;

      temp.push_back(duration);

    }

    treeMapRemove.emplace_back(i, (temp.at(0) + temp.at(1) + temp.at(2))/3);

    temp.clear();


    /*------------------------------------------------------------------------------------------------------------*/

    for(int j = 0; j < 3; j++) {
      before = std::chrono::high_resolution_clock::now();

      auto copied = testHashMap;

      after = std::chrono::high_resolution_clock::now();
      duration = std::chrono::duration_cast<std::chrono::microseconds>(after - before).count();

      std::cout << "HashMap copy duration: " << duration << " micros with 10^" << i << " elements" << std::endl;

      temp.push_back(duration);

    }

    hashMapCopy.emplace_back(i, (temp.at(0) + temp.at(1) + temp.at(2))/3);

    temp.clear();


    /*------------------------------------------------------------------------------------------------------------*/

    for(int j = 0; j < 3; j++) {
      before = std::chrono::system_clock::now();

      auto copied = testTreeMap;

      after = std::chrono::high_resolution_clock::now();
      duration = std::chrono::duration_cast<std::chrono::microseconds>(after - before).count();

      std::cout << "TreeMap copy Duration: " << duration << " micros with 10^" << i << " elements" << std::endl;

      temp.push_back(duration);

    }

    treeMapCopy.emplace_back(i, (temp.at(0) + temp.at(1) + temp.at(2))/3);

    temp.clear();


    /*------------------------------------------------------------------------------------------------------------*/

    for(int j = 0; j < 3; j++) {
      before = std::chrono::high_resolution_clock::now();

      testHashMap.find(quantity - 1 - j);

      after = std::chrono::high_resolution_clock::now();
      duration = std::chrono::duration_cast<std::chrono::nanoseconds>(after - before).count();

      std::cout << "HashMap find duration: " << duration << " nanos with 10^" << i << " elements" << std::endl;

      temp.push_back(duration);

    }

    hashMapFind.emplace_back(i, (temp.at(0) + temp.at(1) + temp.at(2))/3);

    temp.clear();


    /*------------------------------------------------------------------------------------------------------------*/

    for(int j = 0; j < 3; j++) {
      before = std::chrono::system_clock::now();

      testTreeMap.find(quantity - 1 - j);

      after = std::chrono::high_resolution_clock::now();
      duration = std::chrono::duration_cast<std::chrono::nanoseconds>(after - before).count();

      std::cout << "TreeMap find Duration: " << duration << " nanos with 10^" << i << " elements" << std::endl;

      temp.push_back(duration);

    }

    treeMapFind.emplace_back(i, (temp.at(0) + temp.at(1) + temp.at(2))/3);

    temp.clear();


    /*------------------------------------------------------------------------------------------------------------*/
  }

  print("HashMap insert times (ns):", hashMapInsert);
  print("TreeMap insert times (ns):", treeMapInsert);
  print("HashMap remove times (ns):", hashMapRemove);
  print("TreeMap remove times (ns):", treeMapRemove);
  print("HashMap find times (ns):", hashMapFind);
  print("TreeMap find times (ns):", treeMapFind);
  print("HashMap copy times (ms):", hashMapCopy);
  print("TreeMap copy times (ms):", treeMapCopy);

  return 0;
}
