#include <iostream>
#include "node.h"
#include "binarytree.h"

using namespace std;

int main()
{
	BinaryTree* b = new BinaryTree(5, {2, 8, 4, 7, 3, 1, 6, 9});

	b->print();
	cin.ignore();
	delete b;
}