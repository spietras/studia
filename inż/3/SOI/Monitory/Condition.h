#ifndef PROJEKT5_CONDITION_H
#define PROJEKT5_CONDITION_H

#include "Semaphore.h"

class Condition
{
private:
    Semaphore conditionSemaphore;
    unsigned int waitingThreadsCount;

public:
    Condition() : conditionSemaphore(0), waitingThreadsCount(0) {} //condition is initially 0, because wait should be called only when condition is met

    void wait() { conditionSemaphore.down(); } //wait when condition is met

    bool signal()
    {
        if(waitingThreadsCount == 0) return false; //no one is waiting

        --waitingThreadsCount;
        conditionSemaphore.up(); //release waiting thread
        return true;
    }

    void addNewThread() { ++waitingThreadsCount; }
};

#endif //PROJEKT5_CONDITION_H
