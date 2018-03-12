#include "node.h"

Node::Node(const int num)
{
	//Single node with given number and no children
	num_ = num;
	leftChild_ = nullptr;
	rightChild_ = nullptr;
}

Node::Node(const Node & node)
{
	//Create new node and copy given node and all its subnodes
	num_ = node.getNumber();
	leftChild_ = nullptr;
	rightChild_ = nullptr;
	setLeftChild(node.getLeftChild());
	setRightChild(node.getRightChild());
}

Node::Node(Node && node) noexcept
{
	num_ = node.getNumber();
	leftChild_ = node.getLeftChild();
	rightChild_ = node.getRightChild();
	node.leftChild_ = nullptr;
	node.rightChild_ = nullptr;
}

Node & Node::operator=(const Node & node)
{
	//Copies given node and all its subnodes
	num_ = node.getNumber();
	setLeftChild(node.getLeftChild());
	setRightChild(node.getRightChild());

	return *this;
}

Node& Node::operator=(Node&& node) noexcept
{
	num_ = node.getNumber();
	delete leftChild_;
	delete rightChild_;
	leftChild_ = node.getLeftChild();
	rightChild_ = node.getRightChild();
	node.leftChild_ = nullptr;
	node.rightChild_ = nullptr;
	return *this;
}

Node::~Node()
{
	//Recursively delete all nodes
	delete leftChild_;
	leftChild_ = nullptr;
	delete rightChild_;
	rightChild_ = nullptr;
}

void Node::setLeftChild(const Node* node)
{
	if (node == leftChild_)
		return;

	delete leftChild_;

	if (node == nullptr)
	{
		leftChild_ = nullptr;
		return;
	}

	leftChild_ = new Node(*node);
}

void Node::setLeftChild(const int num, const bool over)
{
	//Create new node if overriding is enabled or child is null
	if (over || !leftChild_)
	{
		Node* n = new Node(num);
		setLeftChild(n);
		return;
	}

	leftChild_->setNumber(num);
}

void Node::setRightChild(const Node* node)
{
	if (node == rightChild_)
		return;

	delete rightChild_;

	if (node == nullptr)
	{
		rightChild_ = nullptr;
		return;
	}

	rightChild_ = new Node(*node);
}

void Node::setRightChild(const int num, const bool over)
{
	//Create new node if overriding is enabled or child is null
	if(over || !rightChild_)
	{
		Node* n = new Node(num);
		setRightChild(n);
		return;
	}
	
	rightChild_->setNumber(num);
}
