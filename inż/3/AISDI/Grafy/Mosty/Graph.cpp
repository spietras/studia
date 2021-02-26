#include <stdexcept>
#include <iostream>
#include "Graph.h"

bool Graph::isComplete()
{
    return 2*getEdgesCount() == getVertexCount()*(getVertexCount() - 1); //warunek na graf pelny
}

bool Graph::isExtensiveBridge(const size_t firstVertex, const size_t secondVertex)
{
    //usuwamy firstVertex i secondVertex i liczymy ilosc pozostalych wierzcholkow w grafie

    size_t DfsStartVertex = 0; //zaczynamy dfs od wierzcholka 0 (jesli graf ma jakies wierzcholki to wierzcholek 0 na pewno istnieje)
    if(DfsStartVertex == firstVertex) //jesli startowy wierzcholek jest tym usunietym, to zaczynamy od innego
        DfsStartVertex++;
    if(DfsStartVertex == secondVertex) //tak samo jak wyzej tylko dla drugiego wierzcholka
        DfsStartVertex++;

    std::vector<bool> visited(getVertexCount()); //tworze tutaj, zebym mogl podawac przez referencje (domyslnie wszystko ustawi sie na false)
    visited.at(firstVertex) = true; //ustawiamy usuniete wierzcholki jako odwiedzone
    visited.at(secondVertex) = true;
    const auto countedVertexes = DepthFirstTraversalCount(DfsStartVertex, visited);

    return countedVertexes != getVertexCount() - 2; //jezeli DFS zliczyl wszystkie wierzcholki to graf jest spojny
}

size_t Graph::DepthFirstTraversalCount(const size_t currentVertex, std::vector<bool>& visited)
{
    size_t counted = 1; //zawsze liczy siebie jako 1
    visited.at(currentVertex) = true; //nowy wierzcholek oznaczamy jako odwiedzony

    for(const auto adjacentVertex : adjacencyVector.at(currentVertex)) //przechodzimy po wszystkich sasiadach tego wierzcholka
    {
        if(!visited.at(adjacentVertex)) //jezeli sasiad nie byl odwiedzany i nie jest usuniety
            counted += DepthFirstTraversalCount(adjacentVertex, visited); //to liczymy dalej
    }

    return counted; //zwracamy to co bylo policzone ta sciezka
}

Graph::Graph(const size_t vertexQuantity)
{
    vertexCount = vertexQuantity;
    adjacencyVector.resize(vertexQuantity); //tworzymy wektory sasiadow dla wszystkich wierzcholkow
}

void Graph::addEdge(const size_t firstVertex, const size_t secondVertex)
{
    if(firstVertex == secondVertex) //nie mozna laczyc ze soba
        throw std::logic_error("Can't connect vertex with itself");
    if(firstVertex >= getVertexCount() || secondVertex >= getVertexCount()) //zakladamy, ze indeksy wierzcholkow sa mniejsze niz ich ilosc
        throw std::out_of_range("Vertex index exceeds vertex count");

    edges.emplace_back(firstVertex, secondVertex); //zrob pare i osadz ja bez tworzenia kopii
    adjacencyVector.at(firstVertex).push_back(secondVertex); //dodaj jako sasiada
    adjacencyVector.at(secondVertex).push_back(firstVertex);
}

std::vector<std::pair<size_t, size_t>> Graph::findAllExtensiveBridges()
{
    std::vector<std::pair<size_t, size_t>> found;

    if(getVertexCount() <= 3) //graf z co najwyzej 3 wierzcholkami nie moze miec mostu rozleglego, bo po usunieciu koncow krawedzi zostanie albo pusty albo jeden wierzcholek
        return found;
    if(isComplete()) //jesli graf jest pelny, to nie moze miec mostu rozleglego, bo po usunieciu koncow krawedzi pozostale wierzcholki i tak beda polaczone
        return found;
    for (const auto &edge : edges) //przechodzimy po kazdej krawedzi
    {
        if(isExtensiveBridge(edge.first, edge.second)) //sprawdzamy czy jest mostem rozleglym
            found.emplace_back(edge.first, edge.second); //jesli tak to dodajemy do listy znalezionych
    }

    return found;
}

void Graph::printAllExtensiveBridges(std::ostream &out)
{
    const auto foundExtensiveBridges = findAllExtensiveBridges();

    for(const auto& bridge : foundExtensiveBridges)
        out << bridge.first << " " << bridge.second << std::endl;
}

size_t Graph::getVertexCount()
{
    return vertexCount;
}

size_t Graph::getEdgesCount()
{
    return edges.size();
}
