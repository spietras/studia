#ifndef AISDI_LINEAR_VECTOR_H
#define AISDI_LINEAR_VECTOR_H

#include <cstddef>
#include <initializer_list>
#include <stdexcept>
#include <algorithm>

namespace aisdi {

    template <typename Type>
    class Vector {
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
        pointer dataArray;
        size_type elementCount;
        size_type currentCapacity;
        const size_type INIT_CAPACITY = 10;
        const size_type INCREASE_MULTIPLIER = 2;

        void initArray(size_type neededCapacity) //adjust capacity to needed space and allocate space for data array
        {
            if(neededCapacity < INIT_CAPACITY) neededCapacity = INIT_CAPACITY; //if needed capacity is low, allocate more, because otherwise there would probably be more reallocations when adding new elements

            dataArray = new value_type[neededCapacity];
            currentCapacity = neededCapacity;
            elementCount = 0;
        }

        void reset() //reset everything to zero (WARNING: doesn't erase the memory)
        {
            dataArray = nullptr;
            elementCount = currentCapacity = 0;
        }

        size_type minIndex() const { return 0; }
        size_type maxIndex() const { return elementCount == 0 ? 0 : elementCount - 1; } //maxIndex for 0 elements is 0, needed for correct iterators behaviour

        void increaseSize() //increase size when current size is not enough
        {
            const auto newCapacity = currentCapacity * INCREASE_MULTIPLIER; //multiply size by constant
            auto newDataArray = new value_type[newCapacity];
            std::copy(begin(), end(), newDataArray);
            currentCapacity = newCapacity;
            delete[] dataArray;
            dataArray = newDataArray;
        }

    public:

        Vector() : dataArray(nullptr), elementCount(0), currentCapacity(0) {} //don't need to allocate memory here, no elements

        Vector(std::initializer_list<Type> l) : Vector()
        {
            if(l.size() == 0) return;

            dataArray = new value_type[l.size()];
            currentCapacity = l.size();
            std::copy(l.begin(), l.end(), dataArray);
            elementCount = l.size();
        }

        Vector(const Vector& other) : elementCount(other.elementCount), currentCapacity(other.currentCapacity)
        {
            dataArray = new value_type[currentCapacity];
            std::copy(other.begin(), other.end(), begin());
        }

        Vector(Vector&& other) noexcept : dataArray(other.dataArray), elementCount(other.elementCount), currentCapacity(other.currentCapacity)
        {
            other.reset(); //just steal the pointer
        }

        ~Vector()
        {
            delete[] dataArray;
        }

        Vector& operator=(const Vector& other)
        {
            if(this == &other) //don't self-assign
                return *this;

            const auto oldData = dataArray;

            if(other.isEmpty())
            {
                reset();
                delete[] oldData;
                return *this;
            }

            dataArray = new value_type[other.currentCapacity];
            std::copy(other.begin(), other.end(), begin());
            elementCount = other.elementCount;
            delete[] oldData;
            return *this;
        }

        Vector& operator=(Vector&& other) noexcept
        {
            if(this == &other) //don't self-assign
                return *this;

            currentCapacity = other.currentCapacity;
            elementCount = other.elementCount;
            delete[] dataArray;
            dataArray = other.dataArray;
            other.dataArray = nullptr;
            return *this;
        }

        bool isEmpty() const
        {
            return (int)elementCount == 0;
        }

        size_type getSize() const
        {
            return elementCount;
        }

        void append(const Type& item) {
            insert(end(),item);
        }

        void prepend(const Type& item) {
            insert(begin(), item);
        }

        void insert(const const_iterator& insertPosition, const Type& item)
        {
            if(!insertPosition.isOnVector(*this) || !insertPosition.isAtPossiblePosition()) throw std::out_of_range("Insert position is out of range");

            if(currentCapacity == 0) initArray(1); //when vector hasn't been used yet
            if(elementCount == currentCapacity) increaseSize(); //if more space is needed, increase it

            std::copy_backward(iterator(insertPosition), end(), dataArray + elementCount + 1); //shift to the right
            dataArray[insertPosition.getIndex()] = item; //insert new element
            elementCount++;
        }

        Type popFirst() {
            if (isEmpty())
                throw std::logic_error("You can't pop from empty collection");

            const auto firstElement = *begin();
            std::copy(++begin(), end(), begin()); //shift to the right
            elementCount--;
            return firstElement;
        }

        Type popLast() {
            if (isEmpty())
                throw std::logic_error("You can't pop from empty collection");

            const auto lastElement = *(--end());
            elementCount--;
            return lastElement;
        }

        void erase(const const_iterator& position) {
            erase(position, position+1);
        }

        void erase(const const_iterator& firstIncluded, const const_iterator& lastExcluded) {
            if(!firstIncluded.isOnVector(*this) || !lastExcluded.isOnTheSameVector(firstIncluded) ||
               !firstIncluded.isAtPossiblePosition() || firstIncluded == end() || !lastExcluded.isAtPossiblePosition() ||
               lastExcluded < firstIncluded)
                throw std::out_of_range("Can't erase what is out of range");

            std::copy(lastExcluded, cend(), iterator(firstIncluded));
            elementCount -= lastExcluded.getIndex() - firstIncluded.getIndex();
        }

        iterator begin() {
            return iterator(*this, minIndex());
        }

        iterator end() {
            return iterator(*this, elementCount == 0 ? 0 : maxIndex() + 1);
        }

        const_iterator cbegin() const {
            return const_iterator(*this, minIndex());
        }

        const_iterator cend() const {
            return const_iterator(*this, elementCount == 0 ? 0 : maxIndex() + 1);
        }

        const_iterator begin() const {
            return cbegin();
        }

        const_iterator end() const {
            return cend();
        }
    };

    template <typename Type>
    class Vector<Type>::ConstIterator {
    public:
        using iterator_category = std::bidirectional_iterator_tag;
        using value_type = typename Vector::value_type;
        using difference_type = typename Vector::difference_type;
        using pointer = typename Vector::const_pointer;
        using reference = typename Vector::const_reference;

    protected:
        const Vector<Type>& handledVector;
        size_type currentIndex;

        bool willBeInsideIndexBounds(const difference_type offset) const
        {
            if(handledVector.elementCount == 0) return false;
            else return currentIndex + offset >= handledVector.minIndex() && currentIndex + offset <= handledVector.maxIndex();
        }
        bool willBeAtPossiblePosition(const difference_type offset) const
        {
            if(handledVector.elementCount == 0 && currentIndex == 0) return offset == 0;
            return willBeInsideIndexBounds(offset) || currentIndex + offset == handledVector.maxIndex() + 1;
        }
        bool isInsideIndexBounds() const { return willBeInsideIndexBounds(0); }

    public:

        explicit ConstIterator(const Vector<Type>& v, size_type i) : handledVector(v), currentIndex(i) {}

        reference operator*() const {
            if(!isInsideIndexBounds())
                throw std::out_of_range("Iterator is out of range");

            return handledVector.dataArray[currentIndex];
        }

        ConstIterator& operator++() {
            if(!willBeAtPossiblePosition(1))
                throw std::out_of_range("Iterator is out of range");

            currentIndex++;
            return *this;
        }

        ConstIterator operator++(int) {
            if(!willBeAtPossiblePosition(1))
                throw std::out_of_range("Iterator is out of range");

            const auto copied = *this;
            currentIndex++;
            return copied;
        }

        ConstIterator& operator--() {
            if(!willBeAtPossiblePosition(-1))
                throw std::out_of_range("Iterator is out of range");

            currentIndex--;
            return *this;
        }

        ConstIterator operator--(int) {
            if(!willBeAtPossiblePosition(-1))
                throw std::out_of_range("Iterator is out of range");

            const auto copied = *this;
            currentIndex++;
            return copied;
        }

        ConstIterator operator+(difference_type d) const {
            if(!willBeAtPossiblePosition(d))
                throw std::out_of_range("Iterator is out of range");

            auto copied = *this;
            copied.currentIndex += d;
            return copied;
        }

        ConstIterator operator-(difference_type d) const {
            if(!willBeAtPossiblePosition(-d))
                throw std::out_of_range("Iterator is out of range");

            auto copied = *this;
            copied.currentIndex -= d;
            return copied;
        }

        bool operator==(const ConstIterator& other) const {
            return currentIndex == other.currentIndex;
        }

        bool operator!=(const ConstIterator& other) const {
            return currentIndex != other.currentIndex;
        }

        bool operator>(const ConstIterator& other) const
        {
            return isOnTheSameVector(other) && currentIndex > other.currentIndex;
        }

        bool operator>=(const ConstIterator& other) const
        {
            return *this == other || *this > other;
        }

        bool operator<(const ConstIterator& other) const
        {
            return isOnTheSameVector(other) && currentIndex < other.currentIndex;
        }

        bool operator<=(const ConstIterator& other) const
        {
            return *this == other || *this < other;
        }

        bool isAtPossiblePosition() const { return willBeAtPossiblePosition(0); }

        bool isOnVector(const Vector<Type>& checkedVector) const { return checkedVector.dataArray == handledVector.dataArray && checkedVector.elementCount >= currentIndex; }
        bool isOnTheSameVector(const ConstIterator& other) const { return isOnVector(other.handledVector); }

        size_type getIndex() const { return currentIndex; }
    };

    template <typename Type>
    class Vector<Type>::Iterator : public Vector<Type>::ConstIterator
    {
    public:
        using pointer = typename Vector::pointer;
        using reference = typename Vector::reference;

        explicit Iterator(Vector<Type>& v, int i) : ConstIterator(v, i)
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

#endif // AISDI_LINEAR_VECTOR_H
