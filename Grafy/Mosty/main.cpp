#include <iostream>
#include "Graph.h"

int main()
{
    Graph g(4);
    g.addEdge(0, 1);
    g.addEdge(0, 2);
    g.addEdge(0, 3);
    g.addEdge(1, 2);
    g.addEdge(1, 3);
    g.addEdge(2, 3);

    g.printAllExtensiveBridges(std::cout);

    /*size_t a, b;
    std::cin >> a; //zakladam ze jesli graf bedzie pusty to bedzie wpisana liczba wierzcholkow jako 0
    Graph myGraph(a);

    while(std::cin >> a >> b) //bedzie falsz jesli cos sie zle wczytalo (czyli na przyklad koniec pliku)
        myGraph.addEdge(a, b);

    myGraph.printAllExtensiveBridges(std::cout);*/

    return 0;

}