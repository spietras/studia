#pragma once
#include <vector>

class BinaryTree;

class Node
{
	friend class BinaryTree;
private:
	int num_;
	Node* rightChild_;
	Node* leftChild_;

	//Setters
	void setNumber(const int num) { num_ = num; }
	void setLeftChild(const Node*);
	void setLeftChild(int, bool = false);
	void setRightChild(const Node*);
	void setRightChild(int, bool = false);
public:
	//Constructors
	explicit Node(int);
	Node(const Node&);
	Node(Node&&) noexcept;

	//Destructor
	~Node();

	//Operators
	Node& operator=(const Node&);
	Node& operator=(Node&&) noexcept;

	//Getters
	Node getLeftChildCopy() const { return Node(*leftChild_); }
	Node getRightChildCopy() const { return Node(*rightChild_); }

	std::vector<Node> getNodesCopies() const;
};
