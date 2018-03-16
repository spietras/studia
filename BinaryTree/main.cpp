#include <iostream>
#include "binarytree.h"

using namespace std;

int main()
{
	BinaryTree b(5);
	b.addNodes({-67, 3, 75, 0, 20, -420, 7, 1, 2, 3, 4, 5, 6, 7, 8});
	b.printPretty();
	b.removeNode(3);
	b.printPretty();
	b.addNodes({-1, -2, -3, -4});
	b.printPretty();

	cin.ignore();
}
