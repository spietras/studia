#include <iostream>
#include <string>
#include "binarytree.h"

using namespace std;

void test(BinaryTree& t, string message)
{
	cout << message << endl;
	cout << "Nodes: " << t.getNodeCount() << ", Height: " << t.getHeight() << ", Lowest: " << t.getLowest() << ", Highest: " << t.getHighest() << endl;
	t.printAscending();
	t.printDescending();
	t.printPretty();
}


int main()
{
	BinaryTree b(1, { 5,8,2,10,1,3,6,11,4,9,7,0,8,11,15,14,12,13 });
	test(b, "Initialized b");
	BinaryTree a = b;
	test(a, "Copied b to a");
	a.addNode(420);
	test(a, "Added 420");
	a.addNodes({ -10,-9,-8 });
	test(a, "Added -10, -9, -8");
	a.removeNode(8);
	test(a, "Removed 8");
	a.removeNodes({ 2,0,-420 });
	test(a, "Removed 2, 0, -420");
	cout << "5: " << a.contains(5) << ", -420: " << a.contains(-420) << endl;

	cout << "Compare a to b: " << endl;
	test(a, "a: ");
	test(b, "b: ");

	BinaryTree c(1,{2, 3, 4, 5});
	test(c, "Initialized c");;
	BinaryTree d(0, { 2,6,7 });
	d.addNodes(c);
	test(d, "Added c to d");

	cin.ignore();
}
