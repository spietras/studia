#include <iostream>
#include <limits>
#include "binarytree.h"

using namespace std;

int main()
{
	int n;
	cout << "Write root number: " << endl;
	cin >> n;
	cin.ignore();
	BinaryTree tree(n);

	cout << "Write numbers to add to binary tree: " << endl;
	cin >> tree;
	cin.ignore();

	cout << "BinaryTree in preorder: " << endl;
	cout << tree << endl;

	cin.ignore();
}
