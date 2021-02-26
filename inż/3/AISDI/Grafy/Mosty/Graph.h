#ifndef MOSTY_GRAPH_H
#define MOSTY_GRAPH_H

#include <cstdlib>
#include <vector>

class Graph
{
private:
    size_t vertexCount;
    std::vector<std::pair<size_t, size_t>> edges;
    std::vector<std::vector<size_t>> adjacencyVector;

    bool isComplete();
    bool isExtensiveBridge(size_t firstVertex, size_t secondVertex);
    size_t DepthFirstTraversalCount(size_t currentVertex, std::vector<bool>& visited);
public:
    explicit Graph(size_t vertexQuantity);
    void addEdge(size_t firstVertex, size_t secondVertex);
    std::vector<std::pair<size_t, size_t>> findAllExtensiveBridges();
    void printAllExtensiveBridges(std::ostream& out);

    size_t getVertexCount();
    size_t getEdgesCount();
};

#endif //MOSTY_GRAPH_H
