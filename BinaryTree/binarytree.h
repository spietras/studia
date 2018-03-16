#pragma once
#include <vector>
#include "node.h"

class BinaryTree
{
private:
	Node* root_;
	int nodeCount_;

	static void printNode(const Node*, int);
	static void indent(int);
	/**
	* \brief Recursively searches for node and return pointer to it if found \n
	*
	* It's a pointer to actual object, not a copy! \n
	*
	* \return Nullptr if node not found \n
	*		  Pointer to it otherwise
	*/
	static Node* getNodePointer(Node*, int);

	/**
	* \brief Searches for node and return pointer to it if found \n
	*
	* It's a pointer to actual object, not a copy! \n
	*
	* \return Nullptr if node not found \n
	*		  Pointer to it otherwise
	*/
	Node* findNodePointer(int n) const { return getNodePointer(root_, n); }
	/**
	* \brief Recursively searches for parent of node with given value and returns pointer to it \n
	*
	* It's a pointer to actual object, not a copy! \n
	*
	* \return Pointer to root if searching for root parent \n
	*		  Pointer to parent of existing node if node found
	*		  Pointer to node where value should be added if not found
	*/
	static Node* findParentNodePointer(Node*, int);
	int getHeight(Node*) const;
public:
	//Constructors

	explicit BinaryTree(int);
	BinaryTree(int, const std::vector<int>&);
	BinaryTree(const Node&, const std::vector<Node>&);
	BinaryTree(const BinaryTree&); //Copy constructor
	BinaryTree(BinaryTree&&) noexcept; //Move constructor
	explicit BinaryTree(const Node&);

	//Destructor
	~BinaryTree() { delete root_; }

	//Operators

	BinaryTree& operator=(const BinaryTree&); //Copy assignment
	BinaryTree& operator=(BinaryTree&&) noexcept; //Move assignment

	//Getters

	Node getRootCopy() const { return Node(*root_); }
	int getHeight() const { return getHeight(root_); }
	int getNodeCount() const { return nodeCount_; }
	/**
	* \brief Finds node and returns pointer to its copy \n
	*
	* You are responsible for deleting the pointer! \n
	*
	* \return Nullptr if node not found \n
	*		  Pointer to the copy of it otherwise
	*/
	Node* findNodeCopyPointer(int) const;
	std::vector<Node> getNodesCopies() const { return root_->getNodesCopies(); }
	Node getLowestNodeCopy() const { return getNodesCopies().front(); }
	Node getHighestNodeCopy() const { return getNodesCopies().back(); }

	//Adding

	void addNode(int);
	void addNode(Node&);
	void addNodes(const std::vector<int>& values) { for(int n : values) addNode(n); }
	void addNodes(const std::vector<Node>& nodes) { for(Node n : nodes) addNode(n); }

	//Removing
	void removeNode(int);
	void removeNodes(const std::vector<int>& values) { for(int n : values) removeNode(n); }

	//Output

	void printPretty() const;
	void printAscending() const;
	void printDescending() const;
};
