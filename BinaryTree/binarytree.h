#pragma once
#include <vector>
#include "node.h"

class BinaryTree
{
private:
	Node* root_;

	static void printNode(const Node*, int);
	static void indent(int);
	static Node* getNode(Node*, int);
public:
	//Constructors
	explicit BinaryTree(int);
	BinaryTree(int, const std::vector<int>&);
	BinaryTree(const BinaryTree&);
	BinaryTree(BinaryTree&&) noexcept;
	explicit BinaryTree(const Node&);

	//Destructor
	~BinaryTree();

	//Operators
	BinaryTree& operator=(const BinaryTree&);
	BinaryTree& operator=(BinaryTree&&) noexcept;

	Node getRootCopy() const { return Node(*root_); }

	void addNode(int) const;
	void addNode(const std::vector<int>&) const;
	void addNode(Node&) const;
	Node findNodeCopy(int) const;
	void printPretty() const;
};
