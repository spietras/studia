#include <iostream>
#include "node.h"
#include "binarytree.h"

using namespace std;

int main()
{
	BinaryTree b(5, {2, 8, 4, 7, 3, 1, 6, 9});
	BinaryTree a = b;
	BinaryTree c(3);
	c = b;

	b.addNode({100, 101, 102, -100, -101, -102});

	a.addNode({69, 68, 70, -69, -68, -70});
	c.addNode({420, 421, 422, -420, -421, -422});

	auto t = b.getRootCopy().getNodesCopies();

	b.printPretty();
	a.printPretty();
	c.printPretty();

	cin.ignore();
}
