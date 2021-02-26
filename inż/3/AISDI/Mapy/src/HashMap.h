#ifndef AISDI_MAPS_HASHMAP_H
#define AISDI_MAPS_HASHMAP_H

#include <cstddef>
#include <initializer_list>
#include <stdexcept>
#include <utility>
#include <list>
#include <algorithm>
#include <iterator>

namespace aisdi
{

    template <typename KeyType, typename ValueType>
    class HashMap
    {
    public:
        using key_type = KeyType;
        using mapped_type = ValueType;
        using value_type = std::pair<const key_type, mapped_type>;
        using size_type = std::size_t;
        using reference = value_type&;
        using const_reference = const value_type&;

        using valueList = std::list<value_type>;
        using valueListIterator = typename std::list<value_type>::iterator;
        using valueListConstIterator = typename std::list<value_type>::const_iterator;

        class ConstIterator;
        class Iterator;
        using iterator = Iterator;
        using const_iterator = ConstIterator;

    private:
        const size_type TABLE_SIZE = 100000;
        valueList** table;
        size_type elementsCount;

        size_type hashFunction(const key_type& key) const //converts key to index
        {
          return std::hash<key_type>{}(key) % TABLE_SIZE;
        }

        size_type findFirstNonEmptyList() const
        {
          for(size_type i = 0; i < TABLE_SIZE; i++)
            if(table[i]) return i;

          return TABLE_SIZE;
        }

        size_type findLastNonEmptyList() const
        {
          for(auto i = TABLE_SIZE; i-- > 0;)
            if(table[i]) return i;

          return TABLE_SIZE;
        }

        void clear()
        {
          for(size_type i = 0; i < TABLE_SIZE; i++)
          {
            delete table[i];
            table[i] = nullptr;
          }

          elementsCount = 0;
        }

    public:

        ~HashMap()
        {
          clear();
          delete table[TABLE_SIZE];
          delete[] table;
        }

        HashMap() : table(new valueList*[TABLE_SIZE + 1]), elementsCount(0)
        {
          for(size_type i = 0; i < TABLE_SIZE; i++) table[i] = nullptr;
          table[TABLE_SIZE] = new valueList();
        }

        HashMap(std::initializer_list<value_type> l) : HashMap()
        {
          for(const auto& value : l)
            (*this)[value.first] = value.second;
        }

        HashMap(const HashMap& other) : HashMap()
        {
          auto elementsLeft = other.elementsCount;
          for(size_type i = 0; i < TABLE_SIZE || elementsLeft > 0; i++)
          {
            if(!other.table[i]) continue;
            table[i] = new valueList();
            for(const auto& value : *other.table[i])
            {
              table[i]->push_back(value);
              elementsLeft--;
            }
          }
          elementsCount = other.elementsCount;
        }

        HashMap(HashMap&& other) : HashMap()
        {
          std::swap(table, other.table);
          std::swap(elementsCount, other.elementsCount);
        }

        HashMap& operator=(const HashMap& other)
        {
          return *this = HashMap(other); //copy and swap
        }

        HashMap& operator=(HashMap&& other)
        {
          clear();
          std::swap(table, other.table);
          std::swap(elementsCount, other.elementsCount);
          return *this;
        }

        bool isEmpty() const
        {
          return elementsCount == 0;
        }

        mapped_type& operator[](const key_type& key)
        {
          const auto it = find(key);

          if(it != end()) return it->second; //if found, return value

          const auto index = hashFunction(key);
          if(!table[index]) table[index] = new valueList();
          table[index]->push_back(value_type(key, {})); //if not found add new pair to the end of list
          elementsCount++;
          return iterator(*this, index, --(table[index]->end()))->second;
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
          const auto index = hashFunction(key);
          if(!table[index]) return end();
          for(auto it = table[index]->begin(); it != table[index]->end(); it++)
            if(it->first == key) return const_iterator(*this, index, it);

          return end();
        }

        iterator find(const key_type& key)
        {
          const auto index = hashFunction(key);
          if(!table[index]) return end();
          for(auto it = table[index]->begin(); it != table[index]->end(); it++)
            if(it->first == key) return iterator(*this, index, it);

          return end();
        }

        void remove(const key_type& key)
        {
          remove(find(key));
        }

        void remove(const const_iterator& it)
        {
          if(it == end()) throw std::out_of_range("Iterator is out of range");

          table[it.getCurrentIndex()]->erase(it.getCurrentListIterator()); //erase it from its list
          if(table[it.getCurrentIndex()]->empty())
          {
            delete table[it.getCurrentIndex()];
            table[it.getCurrentIndex()] = nullptr;
          }
          elementsCount--;
        }

        size_type getSize() const
        {
          return elementsCount;
        }

        bool operator==(const HashMap& other) const
        {
          if(other.getSize() != getSize()) return false;
          for(auto it = other.begin(); it != other.end(); it++) //two hashmaps are equal if they contain the same elements
          {
            const auto found = find(it->first);
            if(found == end() || found->second != it->second) return false;
          }
          return true;
        }

        bool operator!=(const HashMap& other) const
        {
          return !(*this == other);
        }

        iterator begin()
        {
          if(isEmpty()) return end();

          const auto firstIndex = findFirstNonEmptyList();
          return iterator(*this, firstIndex, table[firstIndex]->begin());
        }

        iterator end()
        {
          return iterator(*this, TABLE_SIZE, table[TABLE_SIZE]->end());
        }

        const_iterator cbegin() const
        {
          if(isEmpty()) return cend();

          const auto firstIndex = findFirstNonEmptyList();
          return const_iterator(*this, firstIndex, table[firstIndex]->begin());
        }

        const_iterator cend() const
        {
          return const_iterator(*this, TABLE_SIZE, table[TABLE_SIZE]->end());
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

    template <typename KeyType, typename ValueType>
    class HashMap<KeyType, ValueType>::ConstIterator
    {
    public:
        using reference = typename HashMap::const_reference;
        using iterator_category = std::bidirectional_iterator_tag;
        using value_type = typename HashMap::value_type;
        using pointer = const typename HashMap::value_type*;

        using size_type = typename HashMap::size_type;
        using valueListIterator = typename HashMap::valueListConstIterator;

    protected:
        const HashMap<KeyType, ValueType>& handledHashMap;
        size_type currentTableIndex;
        valueListIterator currentListIterator;

        size_type findNextNonEmptyListIndex(const size_type currentIndex) const
        {
          for(auto i = currentIndex + 1; i < handledHashMap.TABLE_SIZE; i++)
            if(handledHashMap.table[i]) return i;

          return handledHashMap.TABLE_SIZE;
        }

        size_type findPreviousNonEmptyListIndex(const size_type currentIndex) const
        {
          for(auto i = currentIndex; i-- > 0;)
            if(handledHashMap.table[i]) return i;

          return handledHashMap.TABLE_SIZE;
        }

    public:

        explicit ConstIterator(const HashMap<KeyType, ValueType>& hm, const size_type i, const valueListIterator& it) : handledHashMap(hm), currentTableIndex(i), currentListIterator(it)
        { }

        ConstIterator(const ConstIterator& other) : ConstIterator(other.handledHashMap, other.currentTableIndex, other.currentListIterator)
        {}

        ConstIterator& operator++()
        {
          if(*this == handledHashMap.end()) throw std::out_of_range("Iterator is out of range");

          //if iterator is inside of list (there is at least one element to the right)
          if(handledHashMap.table[currentTableIndex] &&
             handledHashMap.table[currentTableIndex]->size() >= 2 &&
             currentListIterator != handledHashMap.table[currentTableIndex]->end() &&
             currentListIterator != --(handledHashMap.table[currentTableIndex]->end()))
          {
            ++currentListIterator;
            return *this;
          }

          currentTableIndex = findNextNonEmptyListIndex(currentTableIndex);
          currentListIterator = handledHashMap.table[currentTableIndex]->begin(); //if no elements found, iterator will be set to begin of last list (it's empty)
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
          if(*this == handledHashMap.begin()) throw std::out_of_range("Iterator is out of range");

          //if iterator is inside of list (there is at least one element to the left)
          if(handledHashMap.table[currentTableIndex] &&
             currentListIterator != handledHashMap.table[currentTableIndex]->begin())
          {
            --currentListIterator;
            return *this;
          }

          currentTableIndex = findPreviousNonEmptyListIndex(currentTableIndex); //iterator is not at begin(), so there has to be some previous non-empty list
          currentListIterator = --handledHashMap.table[currentTableIndex]->end();
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
          if(*this == handledHashMap.end()) throw std::out_of_range("Iterator is out of range");

          return *currentListIterator;
        }

        pointer operator->() const
        {
          return &this->operator*();
        }

        bool operator==(const ConstIterator& other) const
        {
          return currentListIterator == other.currentListIterator; //iterators from std::list are equal if they point to the same element in the same list
        }

        bool operator!=(const ConstIterator& other) const
        {
          return !(*this == other);
        }

        size_type getCurrentIndex() const { return currentTableIndex; }
        valueListIterator getCurrentListIterator() const { return currentListIterator; }
    };

    template <typename KeyType, typename ValueType>
    class HashMap<KeyType, ValueType>::Iterator : public HashMap<KeyType, ValueType>::ConstIterator
    {
    public:
        using reference = typename HashMap::reference;
        using pointer = typename HashMap::value_type*;
        using valueListIterator = typename HashMap::valueListIterator;

        explicit Iterator(const HashMap<KeyType, ValueType>& hm, const size_type i, const valueListIterator& it) : ConstIterator(hm, i, it)
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

        pointer operator->() const
        {
          return &this->operator*();
        }

        reference operator*() const
        {
          // ugly cast, yet reduces code duplication.
          return const_cast<reference>(ConstIterator::operator*());
        }

        valueListIterator getCurrentListIterator() const { return ConstIterator::getCurrentListIterator(); }
    };

}

#endif /* AISDI_MAPS_HASHMAP_H */
