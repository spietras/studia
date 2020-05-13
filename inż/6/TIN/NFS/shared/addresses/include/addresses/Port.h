#ifndef MYNFS_PORT_H
#define MYNFS_PORT_H


#include <cstdint>

class Port
{
    uint16_t port = 0;

public:
    /**
     * Empty default constructor for creating default empty objects
     */
    Port() = default;

    Port(uint16_t port);

    /**
     * TODO:
     * getters but i don't know the exact form of them yet
     */

    uint16_t toNetworkOrder() const;

    uint16_t toHostOrder() const;
};


#endif //MYNFS_PORT_H
