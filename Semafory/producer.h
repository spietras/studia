#ifndef PROJEKT3_PRODUCER_H
#define PROJEKT3_PRODUCER_H

#include "global.h"

static int produceItem()
{
    sleep(PRODUCER_SLEEP_TIME); //producing takes some time
    return rand() % ITEM_RANGE; //produce random number from 0 to ITEM_RANGE
}

static void insertItemToFirstNonEmptyQueue(const int *queuesSequence, const int* item)
{
    //we know that there is at least one empty slot, so we search for it with random sequence of queues
    for(int j = 0; j < CONSUMER_NUM; j++)
    {
        const int pickedQueue = queuesSequence[j];

        sem_wait(&mutexes[pickedQueue]); //enter queue

        if(isQueueFull(&queues[pickedQueue])) //if it's full, ignore it
        {
            sem_post(&mutexes[pickedQueue]); //leave queue
            continue;
        }

        printf("Pushing %d to queue %d\n", *item, pickedQueue);
        queuePush(&queues[pickedQueue], item); //add item to the queue

        sem_post(&mutexes[pickedQueue]); //leave queue
        sem_post(&fullQueue[pickedQueue]); //increase taken slots

        break;
    }
}

static void endProducer()
{
    printf("Producer ended!\n");
    producerEnded = true; //flag for when producer ended, checked by consumers when they want to end

    for(int i = 0; i < CONSUMER_NUM; i++)
        sem_post(&fullQueue[i]); //we have to raise semaphores blocking consumers waiting on empty queues, so they can check if they can end
}

void *producer(void *arg)
{
    for(int i = 0; i < PRODUCED_ITEMS; i++)
    {
        const int item = produceItem();
        int queuesSequenceArray[] = {0,1,2,3,4};
        shuffleArray(queuesSequenceArray, CONSUMER_NUM); //random sequence of queues

        sem_wait(&totalEmpty); //sleep if all queues are full

        insertItemToFirstNonEmptyQueue(queuesSequenceArray, &item);

        printQueues();
    }

    endProducer();

    return NULL;
}

#endif //PROJEKT3_PRODUCER_H
