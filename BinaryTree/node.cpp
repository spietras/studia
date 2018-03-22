#include "node.h"

using namespace std;

Node::Node(const int num)
{
	num_ = num;
	leftChild_ = nullptr;
	rightChild_ = nullptr;
}

Node::Node(const Node& node)
{
	num_ = node.num_;
	leftChild_ = nullptr;
	rightChild_ = nullptr;
	//Copies children
	setLeftChild(node.leftChild_);
	setRightChild(node.rightChild_);
}

Node::Node(Node&& node) noexcept
{
	num_ = node.num_;
	//Move children
	leftChild_ = node.leftChild_;
	rightChild_ = node.rightChild_;
	node.leftChild_ = nullptr;
	node.rightChild_ = nullptr;
}

Node& Node::operator=(const Node& node)
{
	num_ = node.num_;
	//If it is necessary, existing children are deleted inside these functions
	setLeftChild(node.leftChild_);
	setRightChild(node.rightChild_);

	return *this;
}

Node& Node::operator=(Node&& node) noexcept
{
	num_ = node.num_;
	delete leftChild_;
	delete rightChild_;
	leftChild_ = node.leftChild_;
	rightChild_ = node.rightChild_;
	node.leftChild_ = nullptr;
	node.rightChild_ = nullptr;
	return *this;
}

Node::~Node()
{
	//Recursively deletes all subnodes
	delete leftChild_;
	leftChild_ = nullptr;
	delete rightChild_;
	rightChild_ = nullptr;
}

void Node::setLeftChild(const Node* node)
{
	if(node == leftChild_)
		return;

	delete leftChild_;

	if(node == nullptr)
	{
		leftChild_ = nullptr;
		return;
	}

	leftChild_ = new Node(*node);
}

void Node::setLeftChild(const int num)
{
	if(!leftChild_)
	{
		leftChild_ = new Node(num);
		return;
	}

	leftChild_->num_ = num;
}

void Node::setRightChild(const Node* node)
{
	if(node == rightChild_)
		return;

	delete rightChild_;

	if(node == nullptr)
	{
		rightChild_ = nullptr;
		return;
	}

	rightChild_ = new Node(*node);
}

void Node::setRightChild(const int num)
{
	if(!rightChild_)
	{
		rightChild_ = new Node(num);
		return;
	}

	rightChild_->num_ = num;
}