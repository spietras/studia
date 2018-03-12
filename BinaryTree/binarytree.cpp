#include <iostream>
#include "binarytree.h"

using namespace std;

BinaryTree::BinaryTree(const int num)
{
	root_ = new Node(num);
}

BinaryTree::BinaryTree(const int root, const vector<int>& values)
{
	root_ = new Node(root);
	for (int value : values)
		addNode(value);
}

BinaryTree::BinaryTree(const BinaryTree& bt)
{
	root_ = new Node(*bt.root_);
}

BinaryTree::BinaryTree(BinaryTree&& bt) noexcept
{
	root_ = bt.root_;
	bt.root_ = nullptr;
}

BinaryTree::BinaryTree(const Node& node)
{
	root_ = new Node(node);
}

BinaryTree::~BinaryTree()
{
	delete root_;
}

BinaryTree& BinaryTree::operator=(const BinaryTree& bt)
{
	if (this == &bt)
		return *this;

	delete root_;
	root_ = new Node(*bt.root_);
	return *this;
}

BinaryTree& BinaryTree::operator=(BinaryTree&& bt) noexcept
{
	delete root_;
	root_ = bt.root_;
	bt.root_ = nullptr;
	return *this;
}

void BinaryTree::addNode(const int num) const
{
	if (root_->getNumber() == num)
		return;
	
	Node* p = findNode(root_, num);

	if (p->getNumber() == num)
		return;

	if (p->getNumber() > num)
	{
		p->setLeftChild(num);
		return;
	}

	p->setRightChild(num);
	return;
}

Node* BinaryTree::findNode(Node* currentNode, const int num)
{
	if (currentNode->getNumber() == num)
		return currentNode;

	if (currentNode->getNumber() > num)
	{
		if (currentNode->getLeftChild())
			return findNode(currentNode->getLeftChild(), num);

		return currentNode;
	}

	if (currentNode->getRightChild())
		return findNode(currentNode->getRightChild(), num);

	return currentNode;
}

void BinaryTree::print() const
{
	cout << endl << "Binary Tree Structure: " << endl;

	printNode(root_, 0);

	cout << endl;
}

void BinaryTree::printNode(const Node* n, const int level)
{
	if (n == nullptr) return;

	indent(level);
	cout << n->getNumber() << endl;
	printNode(n->getLeftChild(), level + 1);
	printNode(n->getRightChild(), level + 1);

}

void BinaryTree::indent(const int level)
{
	if (level == 0)
		return;

	for (int i = 0; i < level - 1; i++)
	{
		cout << "|";
		for (int j = 0; j < 4; j++)
			cout << " ";
	}


	cout << "|";
	for (int i = 0; i < 4; i++)
		cout << "-";
}


