#ifndef PROJEKT5_ITEMQUEUE_H
#define PROJEKT5_ITEMQUEUE_H

#include <queue>
#include <stdexcept>

template <typename T>
class ItemQueue
{
private:
    std::queue<const T*> queue;
    unsigned int size;
public:
    explicit ItemQueue(const unsigned int s) : size(s) {}
    ~ItemQueue() { while(!queue.empty()) delete pop(); } //for safety, but

    bool isFull() const { return queue.size() >= size; }
    bool isEmpty() const { return queue.size() == 0; }

    void push(const T* item)
    {
        if(isFull()) throw std::logic_error("Can't push to full queue");

        queue.push(item);
    }

    const T* pop()
    {
        if(isEmpty()) throw std::logic_error("Can't pop from empty queue");

        const auto item = queue.front();
        queue.pop();
        return item;
    }

    void printQueue(std::ostream& os) const
    {
        std::queue<const T*> copy = queue; //copy so we can iterate through the queue

        while (!copy.empty())
        {
            const auto item = copy.front();
            if(item) os << *item << " ";
            else os << "nullptr ";
            copy.pop();
        }
        os << std::endl;
    }
};

#endif //PROJEKT5_ITEMQUEUE_H
