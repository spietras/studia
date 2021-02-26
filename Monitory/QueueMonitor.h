#ifndef PROJEKT5_QUEUEMONITOR_H
#define PROJEKT5_QUEUEMONITOR_H

#include <queue>
#include <stdexcept>
#include "Monitor.h"
#include "ItemQueue.h"
#include "PrintMonitor.h"

template <typename T>
class QueueMonitor : Monitor
{
private:
    const unsigned int id;
    ItemQueue<T> queue;
    Condition emptyQueue;
    PrintMonitor<T>& printMonitor;

public:
    QueueMonitor(const unsigned int i, const unsigned int s, PrintMonitor<T>& pm) : Monitor(), id(i), queue(s), printMonitor(pm) {}

    bool isFull() const { return queue.isFull(); }

    void pushItem(const T* const item)
    {
        enter();
        queue.push(item);
        printMonitor.printQueue(id, queue);
        signal(emptyQueue); //wake up consumer if he is waiting on empty queue
        leave();
    }

    const T* popItem(const bool& productionRunning)
    {
        enter();

        if(queue.isEmpty())
        {
            if(productionRunning) wait(emptyQueue); //if queue is empty and production still running, wait
            if(!productionRunning) //if queue is empty and production ended, end
            {
                leave();
                return nullptr;
            }
        }

        const auto item = queue.pop();
        printMonitor.printQueue(id, queue);

        leave();
        return item;
    }

    void printQueue(std::ostream& os) const
    {
        os << "Queue " << id << ": ";
        queue.printQueue(os);
    }

    void closeProduction() { signal(emptyQueue); }
};

#endif //PROJEKT5_QUEUEMONITOR_H
