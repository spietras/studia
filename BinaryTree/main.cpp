#include <iostream>
#include "node.h"
#include "binarytree.h"

using namespace std;

int main()
{
	BinaryTree b(5);
	b.addNode({-67, 3, 75, 0, 20, -420, 7, 1, 2, 3, 4, 5, 6, 7, 8});

	cout << "Nodes: " << b.getNodeCount() << ", Height: " << b.getHeight() << endl;
	b.printAscending();
	b.printDescending();
	b.printPretty();

	cin.ignore();
}
