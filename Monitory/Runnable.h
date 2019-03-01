#ifndef PROJEKT5_RUNNABLE_H
#define PROJEKT5_RUNNABLE_H

#include <unistd.h>
#include "QueueMonitor.h"

class Runnable
{
protected:
    const unsigned int sleepTime;
    void threadSleep() const { sleep(sleepTime); }
public:
    explicit Runnable(const unsigned int s) : sleepTime(s) {}

    virtual void run() = 0;
};

#endif //PROJEKT5_RUNNABLE_H
