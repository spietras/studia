#pragma once

class Node
{
private:
	int num_;
	Node* rightChild_;
	Node* leftChild_;
public:
	//Constructors
	Node(const int);
	Node(const Node& node);

	//Destructor
	~Node();
	
	//Operators
	Node& operator=(const Node& node);

	//Getters
	inline int getNumber() const { return num_; }
	inline Node* getRightChild() const { return rightChild_; }
	inline Node* getLeftChild() const { return leftChild_; }

	//Setters
	inline void setNumber(const int num) { num_ = num; }
	void setLeftChild(const Node*);
	void setLeftChild(const int, const bool = false);
	void setRightChild(const Node*);
	void setRightChild(const int, const bool = false);
};

