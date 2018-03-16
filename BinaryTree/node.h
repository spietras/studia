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

	void setLeftChild(const Node*);
	/**
	 * \param n Number
	 * \param over If false changes number of existing node. If true makes new node in place of existing
	 */
	void setLeftChild(int n, bool over = false);
	void setRightChild(const Node*);
	/**
	 *\param n Number
	* \param over If false changes number of existing node. If true makes new node in place of existing
	*/
	void setRightChild(int n, bool over = false);

	std::vector<int> getValues() const;
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
	Node* getLeftChildCopyPointer() const
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
	Node* getRightChildCopyPointer() const
	{
		if(!rightChild_) return nullptr;
		return new Node(*rightChild_);
	}
};
