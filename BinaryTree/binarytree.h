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
	* \return Nullptr if node not found
	* \return Pointer to it otherwise
	*/
	static Node* getNodePointer(Node*, int);
	/**
	* \brief Searches for node and return pointer to it if found \n
	*
	* It's a pointer to actual object, not a copy! \n
	*
	* \return Nullptr if node not found
	* \return Pointer to it otherwise
	*/
	Node* findNodePointer(int n) const { return getNodePointer(root_, n); }
	/**
	* \brief Recursively searches for parent of node with given value and returns pointer to it \n
	*
	* It's a pointer to actual object, not a copy! \n
	*
	* \return Pointer to root if searching for root parent
	* \return Pointer to parent of existing node if node found
	* \return Pointer to node where value should be added if not found
	*/
	Node* findParentNodePointer(Node*, int) const;
	int getHeight(Node*) const;
	void removeNode(Node* n);
	void removeDelete(Node* node, Node* parent);
	void removeReplace(Node*);
	static std::vector<int> getValuesPreOrderRecursive(const Node*);
public:
	//Constructors

	explicit BinaryTree(int);
	BinaryTree(int, const std::vector<int>&);
	BinaryTree(int, const std::vector<BinaryTree>&);
	BinaryTree(const BinaryTree&); //Copy constructor
	BinaryTree(BinaryTree&&) noexcept; //Move constructor
	explicit BinaryTree(const Node&);

	//Destructor
	~BinaryTree() { delete root_; }

	//Operators

	BinaryTree& operator=(const BinaryTree&); //Copy assignment
	BinaryTree& operator=(BinaryTree&&) noexcept; //Move assignment
	BinaryTree& operator+=(const BinaryTree& right)
	{
		addNodes(right);
		return *this;
	}
	BinaryTree& operator+=(int n)
	{
		addNode(n);
		return *this;
	}
	BinaryTree& operator+=(const std::vector<int>& values)
	{
		addNodes(values);
		return *this;
	}
	BinaryTree& operator-=(const BinaryTree& right)
	{
		removeNodes(right);
		return *this;
	}
	BinaryTree& operator-=(int n)
	{
		removeNode(n);
		return *this;
	}
	BinaryTree& operator-=(const std::vector<int>& values)
	{
		removeNodes(values);
		return *this;
	}
	bool operator==(const BinaryTree& right) const { return getValuesPreOrder() == right.getValuesPreOrder(); }
	bool operator!=(const BinaryTree& right) const { return !(*this==right); }
	bool operator<(const BinaryTree& right) const { return getNodeCount() < right.getNodeCount(); }
	bool operator>(const BinaryTree& right) const { return right < *this; }
	bool operator<=(const BinaryTree& right) const { return !(*this > right); }
	bool operator>=(const BinaryTree& right) const { return !(*this < right); }
	BinaryTree operator+(const BinaryTree& right) const
	{
		BinaryTree tree = *this;
		tree += right;
		return tree;
	}
	BinaryTree operator+(int right) const
	{
		BinaryTree tree = *this;
		tree += right;
		return tree;
	}
	BinaryTree operator+(const std::vector<int>& right) const
	{
		BinaryTree tree = *this;
		tree += right;
		return tree;
	}
	BinaryTree operator-(const BinaryTree& right) const
	{
		BinaryTree tree = *this;
		tree -= right;
		return tree;
	}
	BinaryTree operator-(int right) const
	{
		BinaryTree tree = *this;
		tree -= right;
		return tree;
	}
	BinaryTree operator-(const std::vector<int>& right) const
	{
		BinaryTree tree = *this;
		tree -= right;
		return tree;
	}
	Node* operator[](int n) const { return findNodeCopyPointer(n); }

	//Getters

	Node getRootCopy() const { return Node(*root_); }
	int getHeight() const { return getHeight(root_); }
	int getNodeCount() const { return nodeCount_; }
	/**
	* \brief Finds node and returns pointer to its copy \n
	*
	* You are responsible for deleting the pointer! \n
	*
	* \return Nullptr if node not found
	* \return Pointer to the copy of it otherwise
	*/
	Node* findNodeCopyPointer(int) const;
	std::vector<int> getValuesPreOrder() const { return getValuesPreOrderRecursive(root_); }
	int getLowest() const;
	int getHighest() const;

	//Adding

	void addNode(int);
	void addNodes(const BinaryTree& tree) { addNodes(tree.getValuesPreOrder()); }
	void addNodes(const std::vector<int>& values) { for(int n : values) addNode(n); }
	void addNodes(const std::vector<BinaryTree>& trees) { for(const BinaryTree& t : trees) addNodes(t); }

	//Removing
	void removeNode(int);
	void removeNodes(const BinaryTree& tree) { removeNodes(tree.getValuesPreOrder()); }
	void removeNodes(const std::vector<int>& values) { for(int n : values) removeNode(n); }

	void changeNumber(int oldNum, int newNum);

	//Output

	void printPretty() const;
	void printPreorder(std::ostream& = std::cout) const;
	void printAscending(std::ostream& = std::cout) const;
	void printDescending(std::ostream& = std::cout) const;

	bool contains(int n) const { return findNodePointer(n) != nullptr; }
	bool contains(const std::vector<int>& values) const;
	bool contains(const BinaryTree& tree) const { return contains(tree.getValuesPreOrder()); }
};

inline std::ostream& operator<<(std::ostream& os, const BinaryTree& tree) { tree.printPreorder(os); return os; }

std::istream& operator>>(std::istream& is, BinaryTree& tree);
