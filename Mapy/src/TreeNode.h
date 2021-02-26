#ifndef AISDI_MAPS_TREENODE_H
#define AISDI_MAPS_TREENODE_H

#include <cstddef>
#include <stdexcept>
#include <utility>
#include <iostream>
#include <string>

template <typename KeyType, typename ValueType>
class TreeNode
{
public:
    using key_type = KeyType;
    using mapped_type = ValueType;
    using value_type = std::pair<const key_type, mapped_type>;
    using size_type = std::size_t;
    using reference = value_type&;
    using const_reference = const value_type&;

private:
    value_type* data;
    int balanceFactor;

    void swapWithNode(TreeNode&& other)
    {
        std::swap(data, other.data);
        std::swap(parent, other.parent);
        std::swap(leftChild, other.leftChild);
        std::swap(rightChild, other.rightChild);
        std::swap(balanceFactor, other.balanceFactor);
    }

public:
    TreeNode* parent;
    TreeNode* leftChild;
    TreeNode* rightChild;

    ~TreeNode() { delete data; }

    TreeNode() : data(nullptr), balanceFactor(0), parent(nullptr), leftChild(nullptr), rightChild(nullptr) {}

    explicit TreeNode(const value_type& otherData) : data(new value_type(otherData)), balanceFactor(0), parent(nullptr), leftChild(nullptr), rightChild(nullptr) {}

    explicit TreeNode(value_type&& otherData) : data(new value_type(otherData)), balanceFactor(0), parent(nullptr), leftChild(nullptr), rightChild(nullptr) {}

    TreeNode(const TreeNode& other) : data(new value_type(other.data)), balanceFactor(other.balanceFactor), parent(other.parent), leftChild(other.leftChild), rightChild(other.rightChild) {}

    TreeNode(TreeNode&& other) : TreeNode() { swapWithNode(other); }

    TreeNode& operator=(const TreeNode& other) { return *this = TreeNode(other); } //copy and swap

    TreeNode& operator=(TreeNode&& other)
    {
        clear();
        swapWithNode(other);
        return *this;
    }

    void clear()
    {
        delete data;
        data = parent = leftChild = rightChild = nullptr;
        balanceFactor = 0;
    }

    bool isEmpty() const { return data == nullptr; }

    void addData(const value_type& otherData)
    {
        if(!isEmpty()) throw std::logic_error("Can't replace data");
        data = new value_type(otherData);
    }

    void addData(value_type&& otherData)
    {
        if(!isEmpty()) throw std::logic_error("Can't replace data");
        data = new value_type(otherData);
    }

    reference getData() { return *data; }

    const_reference getData() const { return *data; }

    bool isUnbalanced() const { return balanceFactor < -1 || balanceFactor > 1; }

    void replaceData(const value_type& otherData)
    {
        delete data;
        data = new value_type(otherData);
    }

    bool hasBothChildren() const { return rightChild && leftChild; }

    bool hasOnlyRightChild() const { return rightChild && !leftChild; }

    bool hasOnlyLeftChild() const { return !rightChild && leftChild; }

    bool hasNoChildren() const { return !rightChild && !leftChild; }

    void increaseBalance()
    {
        if(balanceFactor > 1) throw std::logic_error("Can't further unbalance node");
        balanceFactor++;
    }

    void decreaseBalance()
    {
        if(balanceFactor < -1) throw std::logic_error("Can't further unbalance node");
        balanceFactor--;
    }

    bool isLeftHeavy() const { return balanceFactor < 0; }

    bool isRightHeavy() const { return balanceFactor > 0; }

    bool isUnheavy() const { return balanceFactor == 0; }

    void printNode(std::ostream& os) const
    {
        os << "Node: " << data->first << std::endl;

        os << "Parent: ";
        if(parent)
        {
            if(parent->data) os << parent->data->first;
            else os << "superroot";
        }
        else os << "null";
        os << ", left child: ";
        if(leftChild) os << leftChild->data->first;
        else os << "null";
        os << ", right child: ";
        if(rightChild) os << rightChild->data->first;
        else os << "null";
        os << std::endl;

        os << "Balance: " << balanceFactor << std::endl;
        os << std::endl;
    }

    bool operator==(const TreeNode& other) const
    {
        return data == other.data && parent == other.parent && leftChild == other.leftChild && rightChild == other.rightChild && balanceFactor == other.balanceFactor;
    }
    bool operator!=(const TreeNode& other) const
    {
        return !(*this == other);
    }
};

  #endif /* AISDI_MAPS_TREENODE_H */
