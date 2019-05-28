#include <iostream>

extern "C" {
    int huffmanEncode();
}

extern "C" {
    int huffmanDecode();
}
 
int main()
{
  std::cout << huffmanEncode() << " " << huffmanDecode() << std::endl;
  return 0;
}
