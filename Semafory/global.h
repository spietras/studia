#ifndef PROJEKT3_DEFINE_H
#define PROJEKT3_DEFINE_H

#include <stdio.h>
#include <pthread.h>
#include <semaphore.h>
#include <stdlib.h>
#include <unistd.h>
#include "queue.h"

#define CONSUMER_NUM 5
#define PRODUCED_ITEMS 100
#define ITEM_RANGE 100
#define CONSUMERS_SLEEP_RANGE_MIN 3
#define CONSUMERS_SLEEP_RANGE_MAX 8
#define PRODUCER_SLEEP_TIME 1

Queue queues[CONSUMER_NUM];
sem_t mutexes[CONSUMER_NUM];
sem_t totalEmpty;
sem_t fullQueue[CONSUMER_NUM];
bool producerEnded;

void printQueues()
{
    //if we want current state of queues we have to block all of them
    for(int i = 0; i < CONSUMER_NUM; i++) sem_wait(&mutexes[i]);

    for(int i = 0; i < CONSUMER_NUM; i++)
    {
        printf("Queue %d: ", i);
        printQueue(&queues[i]);
    }

    for(int i = 0; i < CONSUMER_NUM; i++) sem_post(&mutexes[i]);
}

void shuffleArray(int *array, const int n)
{
    if (n > 1)
    {
        for (int i = 0; i < n - 1; i++)
        {
            int j = i + rand() / (RAND_MAX / (n - i) + 1);
            int t = array[j];
            array[j] = array[i];
            array[i] = t;
        }
    }
}

#endif //PROJEKT3_DEFINE_H
