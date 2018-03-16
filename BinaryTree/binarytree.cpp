#include <iostream>
#include <algorithm>
#include "binarytree.h"

using namespace std;

BinaryTree::BinaryTree(const int num) : nodeCount_(1)
{
	root_ = new Node(num);
}

BinaryTree::BinaryTree(const int root, const vector<int>& values) : nodeCount_(1)
{
	root_ = new Node(root);
	addNode(values);
}

BinaryTree::BinaryTree(const Node& root, const std::vector<Node>& nodes) : nodeCount_(1)
{
	root_ = new Node(root);
	addNode(nodes);
}

BinaryTree::BinaryTree(const BinaryTree& bt) : nodeCount_(1)
{
	//Copies root from given binary tree
	root_ = new Node(*bt.root_);
}

BinaryTree::BinaryTree(BinaryTree&& bt) noexcept : nodeCount_(1)
{
	//Moves pointer to the root to this object
	root_ = bt.root_;
	bt.root_ = nullptr;
}

BinaryTree::BinaryTree(const Node& node) : nodeCount_(1)
{
	root_ = new Node(node);
}

BinaryTree::~BinaryTree()
{
	delete root_;
}

BinaryTree& BinaryTree::operator=(const BinaryTree& bt)
{
	//Check if object is not assigning to itself
	if(this == &bt)
		return *this;

	//Delete current root and make new
	delete root_;
	root_ = new Node(*bt.root_);
	return *this;
}

BinaryTree& BinaryTree::operator=(BinaryTree&& bt) noexcept
{
	//Delete current root and just "steal" existing root from given object 
	delete root_;
	root_ = bt.root_;
	bt.root_ = nullptr;
	return *this;
}

void BinaryTree::addNode(const int num)
{
	if(root_->num_ == num)
		return;

	//Find parent node
	Node* p = findParentNodePointer(root_, num);

	if(p->num_ == num)
		return;

	if(p->num_ > num)
	{
		p->setLeftChild(num);
		nodeCount_++;
		return;
	}

	p->setRightChild(num);
	nodeCount_++;
}

void BinaryTree::addNode(const std::vector<int>& values)
{
	for(int value : values)
		addNode(value);
}

void BinaryTree::addNode(const std::vector<Node>& nodes)
{
	for(Node n : nodes)
		addNode(n);
}

void BinaryTree::addNode(Node& node)
{
	auto nodes = node.getNodesCopies();

	for(Node n : nodes)
		addNode(n.num_);
}

void BinaryTree::removeNode(int num)
{
	Node* n = findNodePointer(num);
	if (!n) return;
	Node* p = findParentNodePointer(root_, n->num_);
	if (p == n) return; //If parent is the same as node to remove, then the node is root, and we can't remove root

	//Get all subnodes of node to remove...
	auto subnodes = n->getNodesCopies();
	int pos = 0;
	for(unsigned int i = 0; i < subnodes.size(); i++)
	{
		if(subnodes.at(i).num_ == num)
		{
			pos = i;
			break;
		}
	}

	//.. except the node to remove
	subnodes.erase(subnodes.begin() + pos);

	if (p->leftChild_ == n)
		p->leftChild_ = nullptr;
	else
		p->rightChild_ = nullptr;

	delete n;

	nodeCount_--;

	addNode(subnodes);
}

void BinaryTree::removeNodes(const std::vector<int>& values)
{
	for (int n : values)
		removeNode(n);
}

Node* BinaryTree::findNodeCopyPointer(const int num) const
{
	Node* n = getNodePointer(root_, num);

	if(!n)
		return nullptr;

	return new Node(*n);
}

Node* BinaryTree::getNodePointer(Node* currentNode, const int num)
{
	if(currentNode->num_ == num)
		return currentNode;

	if(currentNode->num_ > num)
	{
		if(currentNode->leftChild_)
			return getNodePointer(currentNode->leftChild_, num);

		return nullptr;
	}

	if(currentNode->rightChild_)
		return getNodePointer(currentNode->rightChild_, num);

	return nullptr;
}

Node* BinaryTree::findParentNodePointer(Node* currentNode, int n)
{
	if (currentNode->num_ == n)
		return currentNode;

	if (!currentNode->rightChild_ && !currentNode->leftChild_)
		return currentNode;

	if(currentNode->leftChild_ && n < currentNode->num_)
	{
		if (n == currentNode->leftChild_->num_)
			return currentNode;

		return findParentNodePointer(currentNode->leftChild_, n);
	}

	if(currentNode->rightChild_ && n > currentNode->num_)
	{
		if (n == currentNode->rightChild_->num_)
			return currentNode;

		return findParentNodePointer(currentNode->rightChild_, n);
	}

	return currentNode;
}

int BinaryTree::getHeight(Node* n) const
{
	if(!n->rightChild_ && !n->leftChild_)
		return 1;

	if(n->leftChild_ && n->rightChild_)
		return 1 + max(getHeight(n->leftChild_), getHeight(n->rightChild_));

	if(n->leftChild_)
		return 1 + getHeight(n->leftChild_);

	return 1 + getHeight(n->rightChild_);
}

void BinaryTree::printPretty() const
{
	cout << endl << "Binary Tree Structure: " << endl;

	printNode(root_, 0);

	cout << endl;
}

void BinaryTree::printAscending() const
{
	cout << endl << "Nodes in ascending order: " << endl;
	auto nodes = root_->getNodesCopies();
	for(Node n : nodes)
		cout << " " << n.num_ << " ";

	cout << endl;
}

void BinaryTree::printDescending() const
{
	cout << endl << "Nodes in descending order: " << endl;
	auto nodes = root_->getNodesCopies();
	reverse(nodes.begin(), nodes.end());
	for(Node n : nodes)
		cout << " " << n.num_ << " ";

	cout << endl;
}

void BinaryTree::printNode(const Node* n, const int level)
{
	if(n == nullptr) return;

	indent(level);
	cout << "(" << n->num_ << ")" << endl;
	printNode(n->leftChild_, level + 1);
	printNode(n->rightChild_, level + 1);
}

void BinaryTree::indent(const int level)
{
	if(level == 0)
		return;

	for(int i = 0; i < level - 1; i++)
	{
		cout << " |";
		for(int j = 0; j < 4; j++)
			cout << " ";
	}


	cout << " |";
	for(int i = 0; i < 4; i++)
		cout << "-";
}
