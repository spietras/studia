#include <iostream>
#include <thread>
#include "Runnable.h"
#include "Producer.h"
#include "Consumer.h"

void run(Runnable* runnable)
{
    runnable->run();
}

void setup(const unsigned int producerSleepTime,
           const unsigned int producedQuantity,
           const unsigned int consumersQuantity,
           const unsigned int queueSize,
           const std::vector<unsigned int>& consumersSleepTimes)
{
    PrintMonitor<int> printMonitor(std::cout); //monitor for printing
    std::vector<QueueMonitor<int>> queueMonitors; //monitors for queues
    queueMonitors.reserve(consumersQuantity);
    for(unsigned int i = 0; i < consumersQuantity; i++) queueMonitors.emplace_back(i, queueSize, printMonitor);
    AllFullMonitor allFullMonitor(queueMonitors); //monitor for when all queues are full
    bool productionRunning = true;

    //create one producer
    Producer<int> producer(producerSleepTime, producedQuantity, queueMonitors, allFullMonitor, productionRunning);

    //create consumersQuantity of consumers
    std::vector<Consumer<int>> consumers;
    consumers.reserve(consumersQuantity);
    for(unsigned int i = 0; i < consumersQuantity; i++)
        consumers.emplace_back(consumersSleepTimes.at(i), queueMonitors.at(i), allFullMonitor, productionRunning);

    //create consumers thread and run them
    std::vector<std::thread> consumerThreads;
    consumerThreads.reserve(consumersQuantity);
    for(int i = 0; i < consumersQuantity; i++) consumerThreads.emplace_back(run, &consumers[i]);

    //create producer thread and run it
    std::thread producerThread(run, &producer);

    //wait for all threads to finish
    producerThread.join();
    std::cout << "Producer ended" << std::endl;
    for(int i = 0; i < consumersQuantity; i++) consumerThreads[i].join();
}

int main()
{
    srand(time(nullptr));
    const auto producerSleepTime = 0;
    const auto consumersSleepTimeMin = 2, consumersSleepTimeMax = 6;
    const auto producedQuantity = 50;
    const auto consumersQuantity = 5;
    const auto queueSize = 5;

    std::vector<unsigned int> consumersSleepTimes;
    for(unsigned int i = 0; i < consumersQuantity; i++) //random consumer sleep time
        consumersSleepTimes.push_back(((unsigned int)rand() % (consumersSleepTimeMax - consumersSleepTimeMin)) + consumersSleepTimeMin);

    setup(producerSleepTime, producedQuantity, consumersQuantity, queueSize, consumersSleepTimes);

    return 0;
}