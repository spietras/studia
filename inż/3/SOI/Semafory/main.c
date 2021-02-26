#include "queue.h"
#include "global.h"
#include "producer.h"
#include "consumer.h"

static void initialize()
{
    srand((unsigned int) time(NULL)); //seed for random

    for(int i = 0; i < CONSUMER_NUM; i++)
    {
        initQueue(&queues[i]);
        sem_init(&mutexes[i], 0, 1);
        sem_init(&fullQueue[i], 0, 0);
    }
    sem_init(&totalEmpty, 0, CONSUMER_NUM * QUEUE_CAPACITY); //initialized with number of total empty spaces (all are empty at the beginning)
    producerEnded = false;
}

static void terminate()
{
    for(int i = 0; i < CONSUMER_NUM; i++)
        sem_destroy(&mutexes[i]);
}

int main()
{
    initialize();

    pthread_t producer_t;
    pthread_t consumers_t[CONSUMER_NUM];
    int indexes[CONSUMER_NUM] = {0,1,2,3,4}; //necessary for passing as pointers to consumers

    pthread_create(&producer_t, NULL, producer, NULL); //create producer thread
    for(int i = 0; i < CONSUMER_NUM; i++)
        pthread_create(&consumers_t[i], NULL, consumer, (void *)&indexes[i]); //create consumer thread

    pthread_join(producer_t,NULL); //wait for producer thread to finish
    for(int i = 0; i < CONSUMER_NUM; i++)
        pthread_join(consumers_t[i], NULL); //wait for consumer thread to finish

    terminate();

    printf("Done.");
    return 0;
}