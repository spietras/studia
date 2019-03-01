#ifndef PROJEKT5_PRODUCER_H
#define PROJEKT5_PRODUCER_H

#include <algorithm>
#include <random>
#include "Runnable.h"
#include "AllFullMonitor.h"

template <typename T>
class Producer : public Runnable
{
private:
    unsigned int producedQuantity;
    std::vector<QueueMonitor<T>>& queueMonitors; //all queues
    AllFullMonitor<T>& allFullMonitor;
    bool& productionRunning;
    std::mt19937 rng;

    const T* produceItem()
    {
        Runnable::threadSleep();
        return new T();
    }

public:
    explicit Producer(const unsigned int s, const unsigned int q, std::vector<QueueMonitor<T>>& qm, AllFullMonitor<T>& afm, bool& p) : Runnable(s), producedQuantity(q), queueMonitors(qm), allFullMonitor(afm), productionRunning(p), rng(std::random_device()()) {}

    void run() override
    {
        for(auto i = 0; i < producedQuantity; i++)
        {
            auto item = produceItem();
            allFullMonitor.increaseItems(); //if all are full sleep there, after that there is at least one non-full queue
            std::vector<int> indexes;
            indexes.reserve(queueMonitors.size());
            for(int j = 0; j < queueMonitors.size(); j++) indexes.push_back(j);
            std::shuffle(indexes.begin(), indexes.end(), rng); //random sequence of queues to check
            for(auto& index : indexes)
            {
                if(queueMonitors.at(index).isFull()) continue;

                queueMonitors.at(index).pushItem(item);
                break;
            }
        }

        productionRunning = false;
        for(unsigned int i = 0; i < queueMonitors.size(); i++)
            queueMonitors.at(i).closeProduction(); //wake consumers if they wait on empty queues
    }
};

template <>
const int* Producer<int>::produceItem()
{
    Runnable::threadSleep();
    return new int(rand() % 100);
}

#endif //PROJEKT5_PRODUCER_H
