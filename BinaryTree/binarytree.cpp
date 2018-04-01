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
	nodeCount_ = getValuesPreOrderRecursive(&node).size();
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
	if(root_->getNumber() == num)
		return;

	if(findNodePointer(num))
		return;

	//Find parent node
	Node* p = findParentNodePointer(root_, num);
	if(!p) return;

	if(p->getNumber() == num)
		return;

	if(p->getNumber() > num)
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

	if(n->getNumber() == root_->getNumber() || !findNodePointer(n->getNumber()))
		return;

	//Now we are sure node n is in this binary tree and it isn't root. Therefore it must have a parent

	Node* p = findParentNodePointer(root_, n->getNumber());

	if(n->getLeftChild() && n->getRightChild())
		return removeReplace(n);

	if(!n->getLeftChild() && !n->getRightChild())
	{
		if(p->getLeftChild() == n)
			p->setLeftChild(nullptr);
		else
			p->setRightChild(nullptr);

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

	if(currentNode->getNumber() == num) //Node found
		return currentNode;

	if(currentNode->getNumber() > num) //Go left
	{
		if(currentNode->getLeftChild()) //Left child exists
			return getNodePointer(currentNode->getLeftChild(), num);

		return nullptr; //Left child doesn't exist
	}

	//Go right

	if(currentNode->getRightChild()) //Right child exist
		return getNodePointer(currentNode->getRightChild(), num);

	return nullptr; //Right child doesn't exist
}

Node* BinaryTree::findParentNodePointer(Node* currentNode, int n) const
{
	assert(currentNode);
	assert(n != root_->getNumber());

	if(currentNode->getNumber() == n) //Starting node, can't get parent
		return currentNode;

	if(!currentNode->getLeftChild() && !currentNode->getRightChild()) //No children, this is potential parent
		return currentNode;

	if(currentNode->getLeftChild() && n < currentNode->getNumber()) //There is a left child and we should go left
	{
		if(n == currentNode->getLeftChild()->getNumber()) //Node found
			return currentNode;

		return findParentNodePointer(currentNode->getLeftChild(), n);
	}

	if(currentNode->getRightChild() && n > currentNode->getNumber()) //There is a right child and we should go right
	{
		if(n == currentNode->getRightChild()->getNumber()) //Node found
			return currentNode;

		return findParentNodePointer(currentNode->getRightChild(), n);
	}

	return currentNode; //The direction we should go is null, this is potential parent
}

int BinaryTree::getHeight(Node* n) const
{
	assert(n);

	if(!n->getRightChild() && !n->getLeftChild()) //No children, start from here
		return 1;

	if(n->getLeftChild() && n->getRightChild()) //Both children exist
		return 1 + max(getHeight(n->getLeftChild()), getHeight(n->getRightChild())); //Increment maximum count of both children

	if(n->getLeftChild()) //Only left child exist
		return 1 + getHeight(n->getLeftChild()); //Increment count of left child

	return 1 + getHeight(n->getRightChild()); //Only right child exist, increment count of right child
}

void BinaryTree::removeDelete(Node* node, Node* parent)
{
	assert(node);
	assert(parent);
	assert(node->getNumber() != root_->getNumber());
	assert(findNodePointer(node->getNumber()));
	assert(findNodePointer(parent->getNumber()));
	assert(!node->getLeftChild() != !node->getRightChild());

	/*
	 * This function is viable for deleting node with only one children
	 * At this point we asserted that
	 * So we can now just get all subnode values, delete the node and readd subnode values
	 */

	//Get all subnodes of node to remove...
	auto subnodes = getValuesPreOrderRecursive(node);

	nodeCount_ -= subnodes.size();

	//.. except the node to remove
	subnodes.erase(subnodes.begin());

	if(parent->getLeftChild() == node)
		parent->setLeftChild(nullptr);
	else
		parent->setRightChild(nullptr);

	addNodes(subnodes);
}

void BinaryTree::removeReplace(Node* node)
{
	assert(node);
	assert(node->getNumber() != root_->getNumber());
	assert(findNodePointer(node->getNumber()));
	assert(node->getLeftChild() && node->getRightChild());

	/*
	 * This function is viable for node with both children present
	 * At this point we asserted that
	 * Now we can find minimum from right subtree or maximum from left subtree and just change values with node
	 */

	Node* rightMininum = findNodePointer(BinaryTree(*node->getRightChild()).getLowest());
	const int n = rightMininum->getNumber();
	removeNode(rightMininum);
	node->setNumber(n);
}

std::vector<int> BinaryTree::getValuesPreOrderRecursive(const Node* current)
{
	vector<int> values;

	//Get all subnodes in preorder

	values.push_back(current->getNumber());

	if(current->getLeftChild())
	{
		auto leftNodes = getValuesPreOrderRecursive(current->getLeftChild());
		values.insert(values.end(), leftNodes.begin(), leftNodes.end());
	}

	if(current->getRightChild())
	{
		auto rightNodes = getValuesPreOrderRecursive(current->getRightChild());
		values.insert(values.end(), rightNodes.begin(), rightNodes.end());
	}
	return values;
}

void BinaryTree::removeNode(int num)
{
	Node* node = findNodePointer(num);
	if(!node) return;
	removeNode(node);
}

void BinaryTree::changeNumber(int oldNum, int newNum)
{
	Node* n = findNodePointer(oldNum);
	if(!n) return;

	if(findNodePointer(newNum)) return;

	if(n->getLeftChild() && newNum <= BinaryTree(*n->getLeftChild()).getHighest()) return;

	if(n->getRightChild() && newNum >= BinaryTree(*n->getRightChild()).getLowest()) return;

	if(n == root_)
	{
		n->setNumber(newNum);
		return;
	}

	Node* p = findParentNodePointer(root_, oldNum);

	if(p->getLeftChild() == n && newNum < p->getNumber())
		n->setNumber(newNum);
	else if(p->getRightChild() == n && newNum > p->getNumber())
		n->setNumber(newNum);
}

void BinaryTree::printPretty() const
{
	cout << endl << "Binary Tree Structure: " << endl;

	printNode(root_, 0);

	cout << endl;
}

void BinaryTree::printPreorder(ostream& str) const
{
	auto values = getValuesPreOrder();
	for(int n : values)
		str << " " << n << " ";
}

void BinaryTree::printAscending(ostream& str) const
{
	auto values = getValuesPreOrder();
	sort(values.begin(), values.end());
	for(int n : values)
		str << " " << n << " ";
}

void BinaryTree::printDescending(ostream& str) const
{
	auto values = getValuesPreOrder();
	sort(values.begin(), values.end(), greater<int>());
	for(int n : values)
		str << " " << n << " ";
}

bool BinaryTree::contains(const std::vector<int>& values) const
{
	for(int n : values) 
		if(!contains(n)) return false;
	return true;
}

std::istream& operator>>(std::istream& is, BinaryTree& tree)
{
	int n;
	vector<int> values;

	while(is.peek() != '\n')
	{
		if(is.peek() == ' ' || is.peek() == '\t')
		{
			is.get();
			continue;
		}

		if(is >> n)
		{
			values.push_back(n);
			continue;
		}

		break;
	}

	tree.addNodes(values);

	return is;
}

void BinaryTree::printNode(const Node* n, const int level)
{
	assert(level >= 0);

	if(n == nullptr) return;

	indent(level);
	cout << "(" << n->getNumber() << ")" << endl;
	printNode(n->getLeftChild(), level + 1);
	printNode(n->getRightChild(), level + 1);
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