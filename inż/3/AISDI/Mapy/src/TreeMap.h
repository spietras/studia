#ifndef AISDI_MAPS_TREEMAP_H
#define AISDI_MAPS_TREEMAP_H

#include <cstddef>
#include <initializer_list>
#include <stdexcept>
#include <utility>
#include "TreeNode.h"

namespace aisdi
{

template <typename KeyType, typename ValueType>
class TreeMap
{
public:
  using key_type = KeyType;
  using mapped_type = ValueType;
  using value_type = std::pair<const key_type, mapped_type>;
  using size_type = std::size_t;
  using reference = value_type&;
  using const_reference = const value_type&;

  class ConstIterator;
  class Iterator;
  class PreOrderConstIterator;
  class PreOrderIterator;
  using iterator = Iterator;
  using const_iterator = ConstIterator;
  using preorder_iterator = PreOrderIterator;
  using preorder_const_iterator = PreOrderConstIterator;

  using Node = TreeNode<key_type, mapped_type>;

private:

  Node* superRoot;
  size_type elementsCount;

  static const Node* getLeftMost(const Node* n) { while(n->leftChild) n = n->leftChild; return n; }
  static Node* getLeftMost(Node* n) { while(n->leftChild) n = n->leftChild; return n; }
  static const Node* getRightMost(const Node* n) { while(n->rightChild) n = n->rightChild; return n; }
  static Node* getRightMost(Node* n) { while(n->rightChild) n = n->rightChild; return n; }
  static const Node* getLeftMostParent(const Node* n) { while (n == n->parent->leftChild) n = n->parent; return n->parent; }
  static Node* getLeftMostParent(Node* n) { while (n == n->parent->leftChild) n = n->parent; return n->parent; }
  static const Node* getRightMostParent(const Node* n) { while (n == n->parent->rightChild) n = n->parent; return n->parent; }
  static Node* getRightMostParent(Node* n) { while (n == n->parent->rightChild) n = n->parent; return n->parent; }
  static const Node* getLeftMostChildUpwards(const Node* n) { while (n->parent && !n->leftChild) n = n->parent; if(!n->parent) return n; else return n->leftChild; }
  static Node* getLeftMostChildUpwards(Node* n) { while (n->parent && !n->leftChild) n = n->parent; if(!n->parent) return n; else return n->leftChild; }
  static const Node* getRightMostChildUpwards(const Node* n) { while (n->parent && !n->rightChild) n = n->parent; if(!n->parent) return n; else return n->rightChild; }
  static Node* getRightMostChildUpwards(Node* n) { while (n->parent && !n->rightChild) n = n->parent; if(!n->parent) return n; else return n->rightChild; }

  static const Node* findRecursively(const Node* n, const key_type& key)
  {
    if(n == nullptr || n->getData().first == key) return n; //return nullptr if not found, pointer to node if found
    if(key < n->getData().first) return findRecursively(n->leftChild, key);
    return findRecursively(n->rightChild, key);
  }

  static Node* findRecursively(Node* n, const key_type& key) { return const_cast<Node*>(findRecursively((const Node*)n, key)); }

  static const Node* findLastRecursively(const Node* n, const key_type& key)
  {
    if(!n) throw std::out_of_range("Node out of range");
    if(key == n->getData().first) return n;
    if(key < n->getData().first)
    {
      if(n->leftChild) return findLastRecursively(n->leftChild, key);
      return n; //if left child doesn't exist, it's the last node
    }

    if(n->rightChild) return findLastRecursively(n->rightChild, key);
    return n; //if right child doesn't exist, it's the last node
  }

  static Node* findLastRecursively(Node* n, const key_type& key) { return const_cast<Node*>(findLastRecursively((const Node*)n, key)); }

  void clear(Node* n)
  {
    if(!n) return;
    clear(n->leftChild);
    clear(n->rightChild);
    delete n;
  }

  mapped_type& addFirstNode(const key_type& key)
  {
    if(superRoot->leftChild) throw std::invalid_argument("Not first node");
    superRoot->leftChild = new Node(value_type(key, {}));
    superRoot->leftChild->parent = superRoot;
    elementsCount++;
    retraceAfterInsertion(superRoot, true);
    return begin()->second;
  }

  mapped_type& addLeftNode(Node* n, const key_type& key)
  {
    if(n->leftChild) throw std::invalid_argument("Node already has left child");
    auto inserted = new Node(value_type(key, {}));
    n->leftChild = inserted;
    inserted->parent = n;
    elementsCount++;
    retraceAfterInsertion(n, true); //check if rebalancing if necessary
    return inserted->getData().second;
  }

  mapped_type& addRightNode(Node* n, const key_type& key)
  {
    if(n->rightChild) throw std::invalid_argument("Node already has right child");
    auto inserted = new Node(value_type(key, {}));
    n->rightChild = inserted;
    inserted->parent = n;
    elementsCount++;
    retraceAfterInsertion(n, false);  //check if rebalancing if necessary
    return inserted->getData().second;
  }

  void removeNodeWithNoChildren(Node* n)
  {
    if(!n->hasNoChildren()) throw std::invalid_argument("Node must have no children");
    const auto fromLeft = n->parent->leftChild == n;
    if(fromLeft) n->parent->leftChild = nullptr;
    else n->parent->rightChild = nullptr;
    elementsCount--;
    retraceAfterDeletion(n->parent, fromLeft, true);  //check if rebalancing if necessary
    delete n;
  }

  void removeNodeWithOnlyLeftChild(Node* n)
  {
    if(!n->hasOnlyLeftChild()) throw std::invalid_argument("Node must have only left child");
    const auto fromLeft = n->parent->leftChild == n;
    if(fromLeft) n->parent->leftChild = n->leftChild;
    else n->parent->rightChild = n->leftChild;
    n->leftChild->parent = n->parent;
    elementsCount--;
    retraceAfterDeletion(n->parent, fromLeft, true);  //check if rebalancing if necessary
    delete n;
  }

  void removeNodeWithOnlyRightChild(Node* n)
  {
    if(!n->hasOnlyRightChild()) throw std::invalid_argument("Node must have only right child");
    const auto fromLeft = n->parent->leftChild == n;
    if(fromLeft) n->parent->leftChild = n->rightChild;
    else n->parent->rightChild = n->rightChild;
    n->rightChild->parent = n->parent;
    elementsCount--;
    retraceAfterDeletion(n->parent, fromLeft, true);  //check if rebalancing if necessary
    delete n;
  }

  void removeNodeWithBothChildren(Node* n)
  {
    if(!n->hasBothChildren()) throw std::invalid_argument("Node must have both children");
    auto successor = (++iterator(*this, n)).getCurrentNode(); //find inOrder successor
    if(successor != n->rightChild)
    {
      successor->parent->leftChild = successor->rightChild;
      if(successor->rightChild) successor->rightChild->parent = successor->parent;
    }
    else n->rightChild = nullptr;
    n->replaceData(successor->getData()); //just replace data
    elementsCount--;
    retraceAfterDeletion(successor->parent, successor->parent->leftChild == successor->rightChild, true);  //check if rebalancing if necessary
    delete successor;
  }

  void retraceAfterInsertion(Node* current, bool fromLeft)
  {
    if(current == superRoot) return;

    //update balance
    if(fromLeft) current->decreaseBalance();
    else current->increaseBalance();

    if(current->isUnheavy()) return; //if after updating balance some node became balanced, whole tree is balanced
    if(!current->isUnbalanced()) //go further
      return retraceAfterInsertion(current->parent, current->parent->leftChild == current);

    //rotation necessary
    auto parent = current->parent;
    auto wasCurrentLeftChild = (parent->leftChild == current);
    if(current->isRightHeavy())
    {
      if (current->rightChild->isLeftHeavy()) current = rotateRightLeft(current, current->rightChild);
      else current = rotateLeft(current, current->rightChild);
    }
    else
    {
      if (current->leftChild->isRightHeavy()) current = rotateLeftRight(current, current->leftChild);
      else current = rotateRight(current, current->leftChild);
    }

    current->parent = parent;
    if(wasCurrentLeftChild) parent->leftChild = current;
    else parent->rightChild = current;
  }

  void retraceAfterDeletion(Node* current, bool fromLeft, bool updateBalance)
  {
    if(current == superRoot) return;

    if(updateBalance)
    {
      if(fromLeft) current->increaseBalance();
      else current->decreaseBalance();
    }

    if(!current->isUnheavy()) updateBalance = false; //when balance is not 0 anymore, don't update further balances

    if(!current->isUnbalanced()) return retraceAfterDeletion(current->parent, current->parent->leftChild == current, updateBalance);

    //rotation necessary
    auto parent = current->parent;
    auto wasCurrentLeftChild = (parent->leftChild == current);
    if(current->isRightHeavy())
    {
      if (current->rightChild->isLeftHeavy()) current = rotateRightLeft(current, current->rightChild);
      else current = rotateLeft(current, current->rightChild);
    }
    else
    {
      if (current->leftChild->isRightHeavy()) current = rotateLeftRight(current, current->leftChild);
      else current = rotateRight(current, current->leftChild);
    }

    current->parent = parent;
    if(wasCurrentLeftChild) parent->leftChild = current;
    else parent->rightChild = current;

    //after rotation, we still have to go up and update balances
    retraceAfterDeletion(current->parent, current->parent->leftChild == current, true);
  }

  Node* rotateLeft(Node* root, Node* pivot)
  {
    if(!root || !pivot || pivot != root->rightChild || !root->isRightHeavy() || pivot->isLeftHeavy()) throw std::invalid_argument("Can't perform that rotation");
    //update relations
    root->rightChild = pivot->leftChild;
    if (pivot->leftChild) pivot->leftChild->parent = root;
    pivot->leftChild = root;
    root->parent = pivot;
    //update balances
    root->decreaseBalance();
    if (!pivot->isUnheavy()) root->decreaseBalance();
    pivot->decreaseBalance();
    return pivot; //return new root
  }

  Node* rotateRight(Node* root, Node* pivot)
  {
    if(!root || !pivot || pivot != root->leftChild || !root->isLeftHeavy() ||pivot->isRightHeavy()) throw std::invalid_argument("Can't perform that rotation");
    //update relations
    root->leftChild = pivot->rightChild;
    if (pivot->rightChild) pivot->rightChild->parent = root;
    pivot->rightChild = root;
    root->parent = pivot;
    //update balances
    root->increaseBalance();
    if (!pivot->isUnheavy()) root->increaseBalance();
    pivot->increaseBalance();
    return pivot; //return new root
  }

  Node* rotateRightLeft(Node* root, Node* pivot)
  {
    if(!root || !pivot || pivot != root->rightChild || !root->isRightHeavy() || !pivot->isLeftHeavy() || !pivot->leftChild) throw std::invalid_argument("Can't perform that rotation");
    //update relations
    auto secondPivot = pivot->leftChild;
    pivot->leftChild = secondPivot->rightChild;
    if(secondPivot->rightChild) secondPivot->rightChild->parent = pivot;
    secondPivot->rightChild = pivot;
    pivot->parent = secondPivot;
    root->rightChild = secondPivot->leftChild;
    if(secondPivot->leftChild) secondPivot->leftChild->parent = root;
    secondPivot->leftChild = root;
    root->parent = secondPivot;
    //update balances
    root->decreaseBalance();
    root->decreaseBalance();
    pivot->increaseBalance();
    if(secondPivot->isLeftHeavy())
    {
      if(!root->leftChild) root->increaseBalance();
      if(pivot->rightChild) pivot->increaseBalance();
      secondPivot->increaseBalance();
    }
    else if(secondPivot->isRightHeavy())
    {
      if(!pivot->rightChild) pivot->decreaseBalance();
      secondPivot->decreaseBalance();
    }
    return secondPivot; //return new root
  }

  Node* rotateLeftRight(Node* root, Node* pivot)
  {
    if(!root || !pivot || pivot != root->leftChild || !root->isLeftHeavy() || !pivot->isRightHeavy() || !pivot->rightChild) throw std::invalid_argument("Can't perform that rotation");
    //update relations
    auto secondPivot = pivot->rightChild;
    pivot->rightChild = secondPivot->leftChild;
    if(secondPivot->leftChild) secondPivot->leftChild->parent = pivot;
    secondPivot->leftChild = pivot;
    pivot->parent = secondPivot;
    root->leftChild = secondPivot->rightChild;
    if(secondPivot->rightChild) secondPivot->rightChild->parent = root;
    secondPivot->rightChild = root;
    root->parent = secondPivot;
    //update balances
    root->increaseBalance();
    root->increaseBalance();
    pivot->decreaseBalance();
    if(secondPivot->isRightHeavy())
    {
      if(!root->rightChild) root->increaseBalance();
      if(pivot->leftChild) pivot->decreaseBalance();
      secondPivot->decreaseBalance();
    }
    else if(secondPivot->isLeftHeavy())
    {
      if(!pivot->leftChild) pivot->decreaseBalance();
      secondPivot->increaseBalance();
    }
    return secondPivot; //return new root
  }

public:

  ~TreeMap() { clear(superRoot); }

  TreeMap() : superRoot(new Node()), elementsCount(0) {}

  TreeMap(std::initializer_list<value_type> l) : TreeMap()
  {
    for(const auto& value : l)
      (*this)[value.first] = value.second;
  }

  TreeMap(const TreeMap& other) : TreeMap()
  {
    for(auto it = other.preorder_begin(); it != other.preorder_end(); it++)
      (*this)[it->first] = it->second;
  }

  TreeMap(TreeMap&& other) : TreeMap()
  {
    std::swap(superRoot, other.superRoot);
    std::swap(elementsCount, other.elementsCount);
  }

  TreeMap& operator=(const TreeMap& other)
  {
    return *this = TreeMap(other); //copy and swap
  }

  TreeMap& operator=(TreeMap&& other)
  {
    clear(superRoot);
    superRoot = nullptr;
    elementsCount = 0;
    std::swap(superRoot, other.superRoot);
    std::swap(elementsCount, other.elementsCount);
    return *this;
  }

  bool isEmpty() const
  {
    return elementsCount == 0;
  }

  mapped_type& operator[](const key_type& key)
  {
    if(isEmpty()) return addFirstNode(key);

    auto lastFound = findLastRecursively(superRoot->leftChild, key);
    if(key == lastFound->getData().first) return lastFound->getData().second;
    if(key < lastFound->getData().first) return addLeftNode(lastFound, key);
    return addRightNode(lastFound, key);
  }

  const mapped_type& valueOf(const key_type& key) const
  {
    const auto it = find(key);
    if(it == end())
      throw std::out_of_range("Key not found");
    return it->second;
  }

  mapped_type& valueOf(const key_type& key)
  {
    const auto it = find(key);
    if(it == end())
      throw std::out_of_range("Key not found");
    return it->second;
  }

  const_iterator find(const key_type& key) const
  {
    if(isEmpty()) return end();
    const auto resultNode = findRecursively(superRoot->leftChild, key);
    if(!resultNode) return end();
    return const_iterator(*this, resultNode);
  }

  iterator find(const key_type& key)
  {
    if(isEmpty()) return end();
    const auto resultNode = findRecursively(superRoot->leftChild, key);
    if(!resultNode) return end();
    return iterator(*this, resultNode);
  }

  void remove(const key_type& key)
  {
    remove(find(key));
  }

  void remove(const iterator& it)
  {
    if(it == end()) throw std::out_of_range("Iterator is out of range");

    auto currentNode = it.getCurrentNode();
    if(currentNode->hasNoChildren()) removeNodeWithNoChildren(currentNode);
    else if(currentNode->hasOnlyLeftChild()) removeNodeWithOnlyLeftChild(currentNode);
    else if(currentNode->hasOnlyRightChild()) removeNodeWithOnlyRightChild(currentNode);
    else removeNodeWithBothChildren(currentNode);
  }

  size_type getSize() const
  {
    return elementsCount;
  }

  bool operator==(const TreeMap& other) const
  {
    if(other.getSize() != getSize()) return false;
    for(auto it = other.begin(); it != other.end(); it++) //two treemaps are equal if they contain the same elements
    {
      const auto found = find(it->first);
      if(found == end() || found->second != it->second) return false;
    }
    return true;
  }

  bool operator!=(const TreeMap& other) const
  {
    return !(*this == other);
  }

  iterator begin()
  {
    return iterator(*this, getLeftMost(superRoot));
  }

  iterator end()
  {
    return iterator(*this, superRoot);
  }

  const_iterator cbegin() const
  {
    return const_iterator(*this, getLeftMost(superRoot));
  }

  const_iterator cend() const
  {
    return const_iterator(*this, superRoot);
  }

  const_iterator begin() const
  {
    return cbegin();
  }

  const_iterator end() const
  {
    return cend();
  }

  preorder_iterator preorder_begin()
  {
    if(isEmpty()) return preorder_iterator(*this, superRoot);
    return preorder_iterator(*this, superRoot->leftChild);
  }

  preorder_iterator preorder_end()
  {
    return preorder_iterator(*this, superRoot);
  }

  preorder_const_iterator preorder_cbegin() const
  {
    if(isEmpty()) return preorder_const_iterator(*this, superRoot);
    return preorder_const_iterator(*this, superRoot->leftChild);
  }

  preorder_const_iterator preorder_cend() const
  {
    return preorder_const_iterator(*this, superRoot);
  }

  preorder_const_iterator preorder_begin() const
  {
    return preorder_cbegin();
  }

  preorder_const_iterator preorder_end() const
  {
    return preorder_cend();
  }
};

template <typename KeyType, typename ValueType>
class TreeMap<KeyType, ValueType>::ConstIterator
{
public:
  using reference = typename TreeMap::const_reference;
  using iterator_category = std::bidirectional_iterator_tag;
  using value_type = typename TreeMap::value_type;
  using pointer = const typename TreeMap::value_type*;

  using Node = typename TreeMap::Node;

 protected:
  const TreeMap& handledTreeMap;
  const Node* currentNode;

 public:

  explicit ConstIterator(const TreeMap& tm, const Node* const n) : handledTreeMap(tm), currentNode(n) {}

  ConstIterator(const ConstIterator& other) : handledTreeMap(other.handledTreeMap), currentNode(other.currentNode) {}

  ConstIterator& operator++()
  {
    if(!currentNode || *this == handledTreeMap.end()) throw std::out_of_range("Iterator is out of range");

    if (currentNode->rightChild) currentNode = TreeMap::getLeftMost(currentNode->rightChild);
    else currentNode = TreeMap::getRightMostParent(currentNode);

    return *this;
  }

  ConstIterator operator++(int)
  {
    const auto copied = *this;
    ++(*this);
    return copied;
  }

  ConstIterator& operator--()
  {
    if(!currentNode || *this == handledTreeMap.begin()) throw std::out_of_range("Iterator is out of range");

    if (currentNode->leftChild) currentNode = TreeMap::getRightMost(currentNode->leftChild);
    else currentNode = TreeMap::getLeftMostParent(currentNode);

    return *this;
  }

  ConstIterator operator--(int)
  {
    const auto copied = *this;
    --(*this);
    return copied;
  }

  reference operator*() const
  {
    if(!currentNode || *this == handledTreeMap.end()) throw std::out_of_range("Iterator is out of range");
    return currentNode->getData();
  }

  pointer operator->() const
  {
    return &this->operator*();
  }

  bool operator==(const ConstIterator& other) const
  {
    return currentNode == other.currentNode;
  }

  bool operator!=(const ConstIterator& other) const
  {
    return !(*this == other);
  }

  const Node* getCurrentNode() const { return currentNode; }
};

template <typename KeyType, typename ValueType>
class TreeMap<KeyType, ValueType>::Iterator : public TreeMap<KeyType, ValueType>::ConstIterator
{
public:
  using reference = typename TreeMap::reference;
  using pointer = typename TreeMap::value_type*;

  explicit Iterator(const TreeMap& tm, Node* const n) : ConstIterator(tm, n) {}

  explicit Iterator(const ConstIterator& other) : ConstIterator(other) {}

  Iterator& operator++()
  {
    ConstIterator::operator++();
    return *this;
  }

  Iterator operator++(int)
  {
    auto result = *this;
    ConstIterator::operator++();
    return result;
  }

  Iterator& operator--()
  {
    ConstIterator::operator--();
    return *this;
  }

  Iterator operator--(int)
  {
    auto result = *this;
    ConstIterator::operator--();
    return result;
  }

  pointer operator->() const
  {
    return &this->operator*();
  }

  reference operator*() const
  {
    // ugly cast, yet reduces code duplication.
    return const_cast<reference>(ConstIterator::operator*());
  }

  Node* getCurrentNode() const { return const_cast<Node*>(ConstIterator::getCurrentNode()); }
};

template <typename KeyType, typename ValueType>
class TreeMap<KeyType, ValueType>::PreOrderConstIterator : public TreeMap<KeyType, ValueType>::ConstIterator
{
public:

  explicit PreOrderConstIterator(const TreeMap& tm, const Node* const n) : ConstIterator(tm, n) {}

  PreOrderConstIterator(const PreOrderConstIterator& other) : PreOrderConstIterator(other.handledTreeMap, other.currentNode) {}

  explicit PreOrderConstIterator(const ConstIterator& other) : ConstIterator(other) {}

  PreOrderConstIterator& operator++()
  {
    if(!this->currentNode || *this == this->handledTreeMap.preorder_end()) throw std::out_of_range("Iterator is out of range");

    if(this->currentNode->leftChild) this->currentNode = this->currentNode->leftChild;
    else if(!this->currentNode->leftChild && this->currentNode->rightChild) this->currentNode = this->currentNode->rightChild;
    else
    {
      this->currentNode = TreeMap::getRightMostParent(this->currentNode);
      this->currentNode = TreeMap::getRightMostChildUpwards(this->currentNode);
    }

    return *this;
  }

  PreOrderConstIterator operator++(int)
  {
    const auto copied = *this;
    ++(*this);
    return copied;
  }

  PreOrderConstIterator& operator--()
  {
    if(!this->currentNode || *this == this->handledTreeMap.preorder_begin()) throw std::out_of_range("Iterator is out of range");

    if(!this->currentNode->parent) this->currentNode = TreeMap::getRightMost(this->currentNode->leftChild);
    else if(!this->currentNode->parent->leftChild || this->currentNode->parent->leftChild == this->currentNode) this->currentNode = this->currentNode->parent;
    else this->currentNode = TreeMap::getRightMost(this->currentNode->parent->leftChild);

    return *this;
  }

  PreOrderConstIterator operator--(int)
  {
    const auto copied = *this;
    --(*this);
    return copied;
  }
};

template <typename KeyType, typename ValueType>
class TreeMap<KeyType, ValueType>::PreOrderIterator : public TreeMap<KeyType, ValueType>::PreOrderConstIterator
{
public:
  using reference = typename TreeMap::reference;
  using pointer = typename TreeMap::value_type*;

  explicit PreOrderIterator(const TreeMap& tm, Node* const n) : PreOrderConstIterator(tm, n) {}

  explicit PreOrderIterator(const PreOrderConstIterator& other) : PreOrderConstIterator(other) {}

  explicit PreOrderIterator(const ConstIterator& other) : PreOrderConstIterator(other) {}

  explicit PreOrderIterator(const Iterator& other) : PreOrderIterator(other.handledTreeMap, other.currentNode) {}

  PreOrderIterator& operator++()
  {
    PreOrderConstIterator::operator++();
    return *this;
  }

  PreOrderIterator operator++(int)
  {
    auto result = *this;
    PreOrderConstIterator::operator++();
    return result;
  }

  PreOrderIterator& operator--()
  {
    PreOrderConstIterator::operator--();
    return *this;
  }

  PreOrderIterator operator--(int)
  {
    auto result = *this;
    PreOrderConstIterator::operator--();
    return result;
  }

  pointer operator->() const
  {
    return &this->operator*();
  }

  reference operator*() const
  {
    // ugly cast, yet reduces code duplication.
    return const_cast<reference>(PreOrderConstIterator::operator*());
  }

  Node* getCurrentNode() const { return const_cast<Node*>(PreOrderConstIterator::getCurrentNode()); }
};

}

#endif /* AISDI_MAPS_MAP_H */
