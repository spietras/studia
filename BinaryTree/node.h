#pragma once

class BinaryTree;

class Node
{
	friend class BinaryTree;
private:
	int num_;
	Node* rightChild_;
	Node* leftChild_;

	//Setters
	inline void setNumber(const int num) { num_ = num; }
	void setLeftChild(const Node*);
	void setLeftChild(const int, const bool = false);
	void setRightChild(const Node*);
	void setRightChild(const int, const bool = false);
public:
	//Constructors
	explicit Node(const int);
	Node(const Node&);
	Node(Node&&) noexcept;

	//Destructor
	~Node();
	
	//Operators
	Node& operator=(const Node&);
	Node& operator=(Node&&) noexcept;

	//Getters
	inline int getNumber() const { return num_; }
	inline Node* getRightChild() const { return rightChild_; }
	inline Node* getLeftChild() const { return leftChild_; }
};

