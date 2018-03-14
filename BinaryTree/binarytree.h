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
	static Node* getNode(Node*, int);
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
	~BinaryTree();

	//Operators

	BinaryTree& operator=(const BinaryTree&); //Copy assignment
	BinaryTree& operator=(BinaryTree&&) noexcept; //Move assignment

	Node getRootCopy() const { return Node(*root_); }
	int getHeight() const { return getHeight(root_); }
	int getNodeCount() const { return nodeCount_; }

	//Adding

	void addNode(int);
	void addNode(const std::vector<int>&);
	void addNode(const std::vector<Node>&);
	void addNode(Node&);

	Node findNodeCopy(int) const;

	//Output

	void printPretty() const;
	void printAscending() const;
	void printDescending() const;
};
