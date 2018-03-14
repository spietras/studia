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
	Node(const Node&); //Copy constructor
	Node(Node&&) noexcept; //Move constructor

	//Destructor
	~Node();

	//Operators

	Node& operator=(const Node&); //Copy assignment
	Node& operator=(Node&&) noexcept; //Move assignment

	//Getters

	int getNumber() const { return num_; }

	/**
	 * \brief Gets pointer to copy of left child \n
	 * 
	 * You are responsible for deleting the pointer! \n
	 * 
	 * \return Nullptr if leftchild doesn't exist \n
	 *		   Pointer to the copy of it otherwise
	 */
	Node* getLeftChildCopy() const
	{
		if(!leftChild_) return nullptr;
		return new Node(*leftChild_);
	}

	/**
	* \brief Gets pointer to copy of right child \n
	*
	* You are responsible for deleting the pointer! \n
	*
	* \return Nullptr if rightchild doesn't exist \n
	*		   Pointer to the copy of it otherwise
	*/
	Node* getRightChildCopy() const
	{
		if(!rightChild_) return nullptr;
		return new Node(*rightChild_);
	}

	std::vector<Node> getNodesCopies() const;
};
