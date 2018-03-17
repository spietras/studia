#include <iostream>
#include <string>
#include "binarytree.h"

using namespace std;

void printTree(BinaryTree& t, string message)
{
	cout << message << endl;
	cout << "Nodes: " << t.getNodeCount() << ", Height: " << t.getHeight() << ", Lowest: " << t.getLowest() <<
		", Highest: " << t.getHighest() << endl;
	t.printPreorder();
	t.printAscending();
	t.printDescending();
	t.printPretty();
}

void test()
{
	BinaryTree b(4, {2, 1, 3, 16, 9, 6, 5, 8, 13, 11, 15, 25, 20, 27});
	printTree(b, "Initialized b");
	BinaryTree a = b;
	printTree(a, "Copied b to a");
	a.addNode(420);
	printTree(a, "Added 420");
	a.addNodes({-10, -9, -8});
	printTree(a, "Added -10, -9, -8");
	a.removeNode(16);
	printTree(a, "Removed 16");
	a.removeNodes({2, 0, -420});
	printTree(a, "Removed 2, 0, -420");
	cout << "5: " << a.contains(5) << ", -420: " << a.contains(-420) << endl;

	cout << "Compare a to b: " << endl;
	printTree(a, "a: ");
	printTree(b, "b: ");

	BinaryTree c(1, {2, 3, 4, 5});
	printTree(c, "Initialized c");
	BinaryTree d(0, {2, 6, 7});
	d.addNodes(c);
	printTree(d, "Added c to d");
}


int main()
{
	test();

	cin.ignore();
}
