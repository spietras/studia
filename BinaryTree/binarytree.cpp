#include <iostream>
#include <algorithm>
#include <functional>
#include <cassert>
#include "binarytree.h"

using namespace std;

BinaryTree::BinaryTree(const int num) : nodeCount_(0)
{
	root_ = new Node(num);
	nodeCount_++;
}

BinaryTree::BinaryTree(const int root, const vector<int>& values) : nodeCount_(0)
{
	root_ = new Node(root);
	nodeCount_++;
	addNodes(values);
}

BinaryTree::BinaryTree(const int root, const std::vector<BinaryTree>& trees) : nodeCount_(0)
{
	root_ = new Node(root);
	nodeCount_++;
	addNodes(trees);
}

BinaryTree::BinaryTree(const BinaryTree& bt) : nodeCount_(0)
{
	//Copies root from given binary tree
	root_ = new Node(*bt.root_);
	nodeCount_ = bt.getNodeCount();
}

BinaryTree::BinaryTree(BinaryTree&& bt) noexcept : nodeCount_(0)
{
	//Moves pointer to the root to this object
	root_ = bt.root_;
	nodeCount_ = bt.getNodeCount();
	bt.root_ = nullptr;
}

BinaryTree::BinaryTree(const Node& node) : nodeCount_(0)
{
	root_ = new Node(node);
	nodeCount_ = node.getValuesPreOrder().size();
}

BinaryTree& BinaryTree::operator=(const BinaryTree& bt)
{
	//Check if object is not assigning to itself
	if(this == &bt)
		return *this;

	//Delete current root and make new
	delete root_;
	root_ = new Node(*bt.root_);
	nodeCount_ = bt.getNodeCount();
	return *this;
}

BinaryTree& BinaryTree::operator=(BinaryTree&& bt) noexcept
{
	//Delete current root and just "steal" existing root from given object 
	delete root_;
	root_ = bt.root_;
	nodeCount_ = bt.getNodeCount();
	bt.root_ = nullptr;
	return *this;
}

void BinaryTree::addNode(const int num)
{
	if(root_->num_ == num)
		return;

	if(findNodePointer(num))
		return;

	//Find parent node
	Node* p = findParentNodePointer(root_, num);
	if(!p) return;

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

void BinaryTree::removeNode(Node* n)
{
	assert(n);

	if(n->num_ == root_->num_ || !findNodePointer(n->num_))
		return;

	//Now we are sure node n is in this binary tree and it isn't root. Therefore it must have a parent

	Node* p = findParentNodePointer(root_, n->num_);

	if(n->leftChild_ && n->rightChild_)
		return removeReplace(n);

	if(!n->leftChild_ && !n->rightChild_)
	{
		if(p->leftChild_ == n)
			p->leftChild_ = nullptr;
		else
			p->rightChild_ = nullptr;

		delete n;

		nodeCount_--;

		return;
	}

	removeDelete(n, p);
}

Node* BinaryTree::findNodeCopyPointer(const int num) const
{
	Node* n = getNodePointer(root_, num);

	if(!n) return nullptr;

	return new Node(*n);
}

int BinaryTree::getLowest() const
{
	auto v = getValuesPreOrder();
	sort(v.begin(), v.end());
	return v.front();
}

int BinaryTree::getHighest() const
{
	auto v = getValuesPreOrder();
	sort(v.begin(), v.end());
	return v.back();
}

Node* BinaryTree::getNodePointer(Node* currentNode, const int num)
{
	assert(currentNode);

	if(currentNode->num_ == num) //Node found
		return currentNode;

	if(currentNode->num_ > num) //Go left
	{
		if(currentNode->leftChild_) //Left child exists
			return getNodePointer(currentNode->leftChild_, num);

		return nullptr; //Left child doesn't exist
	}

	//Go right

	if(currentNode->rightChild_) //Right child exist
		return getNodePointer(currentNode->rightChild_, num);

	return nullptr; //Right child doesn't exist
}

Node* BinaryTree::findParentNodePointer(Node* currentNode, int n) const
{
	assert(currentNode);
	assert(n != root_->num_);

	if(currentNode->num_ == n) //Starting node, can't get parent
		return currentNode;

	if(!currentNode->rightChild_ && !currentNode->leftChild_) //No children, this is potential parent
		return currentNode;

	if(currentNode->leftChild_ && n < currentNode->num_) //There is a left child and we should go left
	{
		if(n == currentNode->leftChild_->num_) //Node found
			return currentNode;

		return findParentNodePointer(currentNode->leftChild_, n);
	}

	if(currentNode->rightChild_ && n > currentNode->num_) //There is a right child and we should go right
	{
		if(n == currentNode->rightChild_->num_) //Node found
			return currentNode;

		return findParentNodePointer(currentNode->rightChild_, n);
	}

	return currentNode; //The direction we should go is null, this is potential parent
}

int BinaryTree::getHeight(Node* n) const
{
	assert(n);

	if(!n->rightChild_ && !n->leftChild_) //No children, start from here
		return 1;

	if(n->leftChild_ && n->rightChild_) //Both children exist
		return 1 + max(getHeight(n->leftChild_), getHeight(n->rightChild_)); //Increment maximum count of both children

	if(n->leftChild_) //Only left child exist
		return 1 + getHeight(n->leftChild_); //Increment count of left child

	return 1 + getHeight(n->rightChild_); //Only right child exist, increment count of right child
}

void BinaryTree::removeDelete(Node* node, Node* parent)
{
	assert(node);
	assert(parent);
	assert(node->num_ != root_->num_);
	assert(findNodePointer(node->num_));
	assert(findNodePointer(parent->num_));
	assert(!node->leftChild_ != !node->rightChild_);

	/*
	 * This function is viable for deleting node with only one children
	 * At this point we asserted that
	 * So we can now just get all subnode values, delete the node and readd subnode values
	 */

	//Get all subnodes of node to remove...
	auto subnodes = node->getValuesPreOrder();

	nodeCount_ -= subnodes.size();

	//.. except the node to remove
	subnodes.erase(subnodes.begin());

	if(parent->leftChild_ == node)
		parent->leftChild_ = nullptr;
	else
		parent->rightChild_ = nullptr;

	delete node;

	addNodes(subnodes);
}

void BinaryTree::removeReplace(Node* node)
{
	assert(node);
	assert(node->num_ != root_->num_);
	assert(findNodePointer(node->num_));
	assert(node->leftChild_ && node->rightChild_);

	/*
	 * This function is viable for node with both children present
	 * At this point we asserted that
	 * Now we can find minimum from right subtree or maximum from left subtree and just change values with node
	 */

	Node* rightMininum = findNodePointer(BinaryTree(*node->rightChild_).getLowest());
	const int n = rightMininum->num_;
	removeNode(rightMininum);
	node->num_ = n;
}

void BinaryTree::removeNode(int num)
{
	Node* node = findNodePointer(num);
	if(!node) return;
	removeNode(node);
}

void BinaryTree::printPretty() const
{
	cout << endl << "Binary Tree Structure: " << endl;

	printNode(root_, 0);

	cout << endl;
}

void BinaryTree::printPreorder() const
{
	cout << endl << "Nodes in preorder: " << endl;
	auto values = getValuesPreOrder();
	for(int n : values)
		cout << " " << n << " ";

	cout << endl;
}

void BinaryTree::printAscending() const
{
	cout << endl << "Nodes in ascending order: " << endl;
	auto values = getValuesPreOrder();
	sort(values.begin(), values.end());
	for(int n : values)
		cout << " " << n << " ";

	cout << endl;
}

void BinaryTree::printDescending() const
{
	cout << endl << "Nodes in descending order: " << endl;
	auto values = getValuesPreOrder();
	sort(values.begin(), values.end(), greater<>());
	for(int n : values)
		cout << " " << n << " ";

	cout << endl;
}

void BinaryTree::printNode(const Node* n, const int level)
{
	assert(level >= 0);

	if(n == nullptr) return;

	indent(level);
	cout << "(" << n->num_ << ")" << endl;
	printNode(n->leftChild_, level + 1);
	printNode(n->rightChild_, level + 1);
}

void BinaryTree::indent(const int level)
{
	assert(level >= 0);

	if(level == 0) return;

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
