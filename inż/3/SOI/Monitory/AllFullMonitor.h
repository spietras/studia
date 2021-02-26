#ifndef PROJEKT5_ALLFULLMONITOR_H
#define PROJEKT5_ALLFULLMONITOR_H

#include "QueueMonitor.h"

template <typename T>
class AllFullMonitor : Monitor
{
private:
    Condition allFull;
    std::vector<QueueMonitor<T>>& queueMonitors;

    bool areAllQueuesFull() const
    {
        for(auto& q : queueMonitors)
            if(!q.isFull()) return false;

        return true;
    }
public:
    explicit AllFullMonitor(std::vector<QueueMonitor<T>>& qm) : Monitor(), queueMonitors(qm) {}

    void increaseItems()
    {
        enter();
        if(areAllQueuesFull()) wait(allFull);
        leave();
    }

    void decreaseItems() { signal(allFull); }
};

#endif //PROJEKT5_ALLFULLMONITOR_H
