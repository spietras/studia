#ifndef PROJEKT5_SEMAPHORE_H
#define PROJEKT5_SEMAPHORE_H

#include <semaphore.h>

class Semaphore
{
private:
    sem_t sem;

public:
    explicit Semaphore( const unsigned int value ) { sem_init(&sem, 0, value); }

    ~Semaphore() { sem_destroy(&sem); }

    void down() { sem_wait(&sem); }

    void up() { sem_post( & sem ); }
};

#endif //PROJEKT5_SEMAPHORE_H
