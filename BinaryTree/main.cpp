#include <iostream>
#include "node.h"

using namespace std;

int main()
{
	Node* a = new Node(5);
	a->setRightChild(2);
	a->setLeftChild(3);

	Node* b = new Node(*a);

	b->setRightChild(4);

	cout << "a: " << a->getNumber() << ", a.right: " << a->getRightChild()->getNumber() << ", a.left: " << a->getLeftChild()->getNumber() << endl;
	cout << "b: " << b->getNumber() << ", b.right: " << b->getRightChild()->getNumber() << ", b.left: " << b->getLeftChild()->getNumber() << endl;
	cin.ignore();
}