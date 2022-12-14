#ifndef PROJEKT5_CONSUMER_H
#define PROJEKT5_CONSUMER_H

#include "Runnable.h"

template <typename T>
class Consumer : public Runnable
{
private:
    QueueMonitor<T>& monitor; //consumer's queue
    AllFullMonitor<T>& allFullMonitor;
    const bool& productionRunning;

    void consumeItem(const T* item)
    {
        Runnable::threadSleep();
        delete item;
    }

public:
    explicit Consumer(const unsigned int s, QueueMonitor<T>& m, AllFullMonitor<T>& afm, const bool& p) : Runnable(s), monitor(m), allFullMonitor(afm), productionRunning(p) {}

    void run() override
    {
        while(true)
        {
            auto item = this->monitor.popItem(productionRunning);
            allFullMonitor.decreaseItems();
            if(!item) break;
            consumeItem(item);
        }
    }
};

#endif //PROJEKT5_CONSUMER_H
