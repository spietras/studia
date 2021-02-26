#ifndef AISDI_LINEAR_LINKEDLIST_H
#define AISDI_LINEAR_LINKEDLIST_H

#include <cstddef>
#include <initializer_list>
#include <stdexcept>

namespace aisdi
{

    template <typename Type>
    class LinkedList
    {
    public:
        using difference_type = std::ptrdiff_t;
        using size_type = std::size_t;
        using value_type = Type;
        using pointer = Type*;
        using reference = Type&;
        using const_pointer = const Type*;
        using const_reference = const Type&;

        class ConstIterator;
        class Iterator;
        using iterator = Iterator;
        using const_iterator = ConstIterator;

    private:
        struct Node
        {
            pointer valuePointer;
            struct Node* next;
            struct Node* prev;

            Node(pointer v = nullptr) : valuePointer(v), next(this), prev(this) {} //if empty, next and prev point to this
            Node(pointer v, struct Node* n, struct Node* p) : valuePointer(v), next(n), prev(p) {}
            ~Node() { delete valuePointer; }
        };

        using Node = struct Node;

        Node* guard;
        size_type elementCount;

    public:
        LinkedList() : guard(new Node()), elementCount(0) { }

        LinkedList(std::initializer_list<Type> l) : LinkedList()
        {
            if(l.size() == 0) return;

            for(const auto& value : l) insert(end(), value);
        }

        LinkedList(const LinkedList& other) : LinkedList()
        {
            for (auto it = other.begin(); it != other.end(); it++) insert(end(), *it);
        }

        LinkedList(LinkedList&& other)
        {
            guard = other.guard;
            other.guard = nullptr;
            elementCount = other.elementCount;
        }

        ~LinkedList()
        {
            if(guard == nullptr) return; //corner case for move
            erase(begin(), end());
            delete guard;
        }

        LinkedList& operator=(const LinkedList& other)
        {
            if (this == &other) return *this; //don't self-assign

            erase(begin(), end());
            for (auto it = other.begin(); it != other.end(); it++) insert(end(), *it);
            return *this;
        }

        LinkedList& operator=(LinkedList&& other)
        {
            if (this == &other) return *this; //don't self-assign

            erase(begin(), end());
            delete guard;
            guard = other.guard; //steal the pointer
            other.guard = nullptr;
            elementCount = other.elementCount;

            return *this;
        }

        bool isEmpty() const
        {
            return elementCount == 0;
        }

        size_type getSize() const
        {
            return elementCount;
        }

        void append(const Type& item)
        {
            insert(end(), item);
        }

        void prepend(const Type& item)
        {
            insert(begin(), item);
        }

        void insert(const const_iterator& insertPosition, const Type& item)
        {
            if(!insertPosition.isOnList(*this)) throw std::out_of_range("Iterator is out of range");
            auto newNode = new Node(new value_type(item), insertPosition.getNodePointer(), insertPosition.getNodePointer()->prev);

            newNode->next->prev = newNode;
            newNode->prev->next = newNode;

            elementCount++;
        }

        Type popFirst()
        {
            if(isEmpty()) throw std::logic_error("You can't pop from empty collection");
            const auto firstElement = *begin();
            erase(begin());
            return firstElement;
        }

        Type popLast()
        {
            if(isEmpty()) throw std::logic_error("You can't pop from empty collection");
            const auto lastElement = *(--end());
            erase(--end());
            return lastElement;
        }

        void erase(const const_iterator& position)
        {
            if(!position.isOnList(*this)) throw std::out_of_range("Iterator is out of range");
            if(position == end()) throw std::out_of_range("Can't erase the guard");
            erase(position, position+1);
        }

        void erase(const const_iterator& firstIncluded, const const_iterator& lastExcluded)
        {
            if(!firstIncluded.isOnList(*this) || !lastExcluded.isOnTheSameList(firstIncluded) || lastExcluded < firstIncluded) throw std::out_of_range("Iterator is out of range");
            auto firstPrev = firstIncluded.getNodePointer()->prev;
            auto it = firstIncluded;

            while(it != lastExcluded) //go to the next element and delete previous
            {
                it++;
                delete it.getNodePointer()->prev;
                elementCount--;
            }

            lastExcluded.getNodePointer()->prev = firstPrev;
            firstPrev->next = lastExcluded.getNodePointer();
        }

        iterator begin()
        {
            return iterator(*this, guard->next);
        }

        iterator end()
        {
            return iterator(*this, guard);
        }

        const_iterator cbegin() const
        {
            return const_iterator(*this, guard->next);
        }

        const_iterator cend() const
        {
            return const_iterator(*this, guard);
        }

        const_iterator begin() const
        {
            return cbegin();
        }

        const_iterator end() const
        {
            return cend();
        }
    };

    template <typename Type>
    class LinkedList<Type>::ConstIterator
    {
    public:
        using iterator_category = std::bidirectional_iterator_tag;
        using value_type = typename LinkedList::value_type;
        using difference_type = typename LinkedList::difference_type;
        using pointer = typename LinkedList::const_pointer;
        using reference = typename LinkedList::const_reference;

    protected:
        using Node = typename LinkedList::Node;
        const LinkedList<Type>& handledList;
        Node* currentNode;

        bool isNullOrGuard() const { return currentNode == nullptr || *this == handledList.end(); }
        bool isNullOrBegin() const { return currentNode == nullptr || *this == handledList.begin(); }

    public:

        explicit ConstIterator(const LinkedList<Type>& l, Node* n) : handledList(l), currentNode(n)
        {}

        reference operator*() const
        {
            if(isNullOrGuard())
                throw std::out_of_range("Iterator is out of range");

            return *(currentNode->valuePointer);
        }

        ConstIterator& operator++()
        {
            if(isNullOrGuard())
                throw std::out_of_range("Iterator is out of range");

            currentNode = currentNode->next;
            return *this;
        }

        ConstIterator operator++(int)
        {
            if(isNullOrGuard())
                throw std::out_of_range("Iterator is out of range");

            const auto copied = *this;
            currentNode = currentNode->next;
            return copied;
        }

        ConstIterator& operator--()
        {
            if(isNullOrBegin())
                throw std::out_of_range("Iterator is out of range");

            currentNode = currentNode->prev;
            return *this;
        }

        ConstIterator operator--(int)
        {
            if(isNullOrBegin())
                throw std::out_of_range("Iterator is out of range");

            const auto copied = *this;
            currentNode = currentNode->prev;
            return copied;
        }

        ConstIterator operator+(difference_type d) const
        {
            auto copied = *this;
            const size_type absoluteD = (d > 0 ? d : -d); //we need positive value for loop
            for(size_type i = 0; i < absoluteD; i++)
            {
                if(d < 0) //go backward
                {
                    if(copied.isNullOrBegin()) throw std::out_of_range("Iterator is out of range");
                    copied.currentNode = copied.currentNode->prev;
                }
                else //go forward
                {
                    if(copied.isNullOrGuard()) throw std::out_of_range("Iterator is out of range");
                    copied.currentNode = copied.currentNode->next;
                }
            }

            return copied;
        }

        ConstIterator operator-(difference_type d) const
        {
            return *this + (-d);
        }

        bool operator==(const ConstIterator& other) const
        {
            return currentNode == other.currentNode;
        }

        bool operator!=(const ConstIterator& other) const
        {
            return currentNode != other.currentNode;
        }

        bool operator>(const ConstIterator& other) const
        {
            if(!isOnTheSameList(other)) return false;

            auto temp = other;
            while(temp != handledList.end())
            {
                if(temp == *this) return true;
                temp++;
            }

            return false;
        }

        bool operator>=(const ConstIterator& other) const
        {
            return *this == other || *this > other;
        }

        bool operator<(const ConstIterator& other) const
        {
            if(!isOnTheSameList(other)) return false;

            auto temp = other;
            while(temp != handledList.begin())
            {
                if(temp == *this) return true;
                temp--;
            }

            return false;
        }

        bool operator<=(const ConstIterator& other) const
        {
            return *this == other || *this < other;
        }

        bool isOnList(const LinkedList<Type>& checkedList) const { return checkedList.guard == handledList.guard; }
        bool isOnTheSameList(const ConstIterator& other) const { return isOnList(other.handledList); }

        Node* getNodePointer() const { return currentNode; }
    };

    template <typename Type>
    class LinkedList<Type>::Iterator : public LinkedList<Type>::ConstIterator
    {
    public:
        using pointer = typename LinkedList::pointer;
        using reference = typename LinkedList::reference;

        explicit Iterator(const LinkedList<Type>& l, Node* n) : ConstIterator(l, n)
        {}

        Iterator(const ConstIterator& other)
                : ConstIterator(other)
        {}

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

        Iterator operator+(difference_type d) const
        {
            return ConstIterator::operator+(d);
        }

        Iterator operator-(difference_type d) const
        {
            return ConstIterator::operator-(d);
        }

        reference operator*() const
        {
            // ugly cast, yet reduces code duplication.
            return const_cast<reference>(ConstIterator::operator*());
        }
    };

}

#endif // AISDI_LINEAR_LINKEDLIST_H
