#ifndef PROJEKT3_CONSUMER_H
#define PROJEKT3_CONSUMER_H

#include "global.h"

int getItem(const int queueNum)
{
    sem_wait(&fullQueue[queueNum]); //sleep if queue if empty
    sem_wait(&mutexes[queueNum]); //enter queue

    if(producerEnded && isQueueEmpty(&queues[queueNum])) //if producer ended and queue is empty we can end
    {
        sem_post(&mutexes[queueNum]);
        printf("Consumer %d ended!\n", queueNum);
        return -1;
    }

    int item = queuePop(&queues[queueNum]); //get item
    printf("Popping %d from queue %d\n", item, queueNum);

    sem_post(&mutexes[queueNum]); //leave queue
    sem_post(&totalEmpty); //increase empty slots

    return item;
}

void consumeItem(int* item)
{
    //consuming item takes some time
    if(CONSUMERS_SLEEP_RANGE_MIN > 0 && CONSUMERS_SLEEP_RANGE_MAX >= CONSUMERS_SLEEP_RANGE_MIN)
        sleep((unsigned int) (rand() % (CONSUMERS_SLEEP_RANGE_MAX + 1 - CONSUMERS_SLEEP_RANGE_MIN) + CONSUMERS_SLEEP_RANGE_MIN));
    else if(CONSUMERS_SLEEP_RANGE_MIN == 0 && CONSUMERS_SLEEP_RANGE_MAX > CONSUMERS_SLEEP_RANGE_MIN)
        sleep((unsigned int)(rand() % (CONSUMERS_SLEEP_RANGE_MAX + 1)));
}

void *consumer(void *arg)
{
    int consumerID = *((int *)arg); //convert passed argument

    while(true)
    {
        int item = getItem(consumerID);
        if(item == -1) return NULL;

        printQueues();

        consumeItem(&item);
    }
}

#endif //PROJEKT3_CONSUMER_H
