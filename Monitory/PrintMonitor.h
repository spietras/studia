#ifndef PROJEKT5_PRINTMONITOR_H
#define PROJEKT5_PRINTMONITOR_H

#include "Monitor.h"
#include "ItemQueue.h"

template <typename T>
class PrintMonitor : Monitor
{
private:
    std::ostream& outputStream;
public:
    explicit PrintMonitor(std::ostream& os) : Monitor(), outputStream(os) {}

    void printQueue(const unsigned int id, const ItemQueue<T>& queue)
    {
        enter();
        outputStream << "Queue " << id << ": ";
        queue.printQueue(outputStream);
        leave();
    }
};

#endif //PROJEKT5_PRINTMONITOR_H
