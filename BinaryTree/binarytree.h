#pragma once
#include <vector>
#include "node.h"

class BinaryTree
{
private:
	Node* root_;

	static void printNode(const Node*, const int);
	static void indent(const int);
public:
	explicit BinaryTree(const int);
	BinaryTree(const int, const std::vector<int>&);
	BinaryTree(const BinaryTree&);
	BinaryTree(BinaryTree&&) noexcept;
	explicit BinaryTree(const Node&);
	~BinaryTree();

	BinaryTree& operator=(const BinaryTree&);
	BinaryTree& operator=(BinaryTree&&) noexcept;

	void addNode(const int) const;
	static Node* findNode(Node*, const int);
	void print() const;
};

