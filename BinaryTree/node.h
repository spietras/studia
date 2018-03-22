#pragma once

class Node
{
private:
	int num_;
	Node* rightChild_;
	Node* leftChild_;

public:
	//Constructors

	explicit Node(int);
	Node(const Node&); //Copy constructor
	Node(Node&&) noexcept; //Move constructor

	//Destructor
	~Node();

	//Operators

	Node& operator=(const Node&); //Copy assignment
	Node& operator=(Node&&) noexcept; //Move assignment

	//Getters

	int getNumber() const { return num_; }
	Node* getLeftChild() const { return leftChild_; }
	Node* getRightChild() const { return rightChild_; }

	//Setters

	void setNumber(int n) { num_ = n; }
	void setLeftChild(Node*);
	void setLeftChild(int num);
	void setRightChild(Node*);
	void setRightChild(int num);
};
