#ifndef PROJEKT3_QUEUE_H
#define PROJEKT3_QUEUE_H

#include <stdio.h>
#include <stdbool.h>

#define QUEUE_CAPACITY 10

typedef struct
{
    int data[QUEUE_CAPACITY];
    int head;
    int tail;
    int size;
} Queue;

bool initQueue(Queue* queue)
{
    if (!queue) return false;

    queue->head = 0;
    queue->tail = 0;
    queue->size = 0;
    return true;
}

int getQueueSize(const Queue* queue)
{
    if(!queue) return -1;
    return queue->size;
}

int isQueueEmpty(const Queue* queue)
{
    return queue && getQueueSize(queue) == 0;
}

int isQueueFull(const Queue* queue)
{
    return queue && getQueueSize(queue) == QUEUE_CAPACITY;
}

bool queuePush(Queue* queue, const int* item)
{
    if(!queue || isQueueFull(queue)) return false;

    queue->data[queue->tail] = *item;
    queue->tail = (queue->tail + 1) % QUEUE_CAPACITY; //cyclic queue so add modulo
    queue->size++;
    return true;
}

int queuePop(Queue* queue)
{
    if (!queue || isQueueEmpty(queue)) return -1;

    const int item = queue->data[queue->head];
    queue->head = (queue->head + 1) % QUEUE_CAPACITY; //cyclic queue so add modulo
    queue->size--;
    return item;
}

void printQueue(const Queue* queue)
{
    if(isQueueEmpty(queue))
    {
        printf("empty\n");
        return;
    }

    int index = queue->head;
    for(int i = 0; i < queue->size; i++)
    {
        printf("%d\t", queue->data[index]);
        index = (index + 1) % QUEUE_CAPACITY;
    }
    printf("\n");
}

#endif //PROJEKT3_QUEUE_H
