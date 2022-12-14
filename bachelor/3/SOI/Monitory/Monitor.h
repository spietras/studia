#ifndef PROJEKT5_MONITOR_H
#define PROJEKT5_MONITOR_H

#include "Semaphore.h"
#include "Condition.h"

class Monitor
{
private:
    Semaphore mutex;

public:
    Monitor() : mutex(1) {} //initially mutex is up, so first thread can enter monitor

    void enter() { mutex.down(); }

    void leave() { mutex.up(); }

    void wait(Condition& cond) //call when condition is met
    {
        cond.addNewThread();
        leave(); //leave mutex so other threads can enter
        cond.wait();
    }
    void signal(Condition& cond) //call when condition stops being met
    {
        if(cond.signal()) enter(); //if someone was waiting, let them in
    }
};

#endif //PROJEKT5_MONITOR_H
