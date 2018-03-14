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

void Node::setLeftChild(const int num, const bool over)
{
	//Create new node if overriding is enabled or child is null
	if(over || !leftChild_)
	{
		Node* n = new Node(num);
		setLeftChild(n);
		delete n;
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

void Node::setRightChild(const int num, const bool over)
{
	//Create new node if overriding is enabled or child is null
	if(over || !rightChild_)
	{
		Node* n = new Node(num);
		setRightChild(n);
		delete n;
		return;
	}

	rightChild_->num_ = num;
}

vector<Node> Node::getNodesCopies() const
{
	vector<Node> nodes;

	//Get all subnodes in ascending order

	if(leftChild_)
	{
		auto leftNodes = leftChild_->getNodesCopies();
		nodes.insert(nodes.end(), leftNodes.begin(), leftNodes.end());
	}

	nodes.push_back(Node(*this));

	if(rightChild_)
	{
		auto rightNodes = rightChild_->getNodesCopies();
		nodes.insert(nodes.end(), rightNodes.begin(), rightNodes.end());
	}
	return nodes;
}
