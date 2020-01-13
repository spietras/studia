# AAL-Chemikalia

Autor: Sebastian Pietras

## Temat projektu

Projekt nr 8: Chemikalia

Zakłady chemiczne produkują n różnych substancji, z których każda składowana jest w jednym z dwóch magazynów. 
Ze względu na niebezpieczne opary, które mogą ze sobą reagować, 
niektóre substancje nie mogą znajdować się w tym samym magazynie. 
Napisać algorytm, który w bezpieczny sposób (o ile to możliwe) 
rozmieszcza każdą z substancji, mając na wejściu dane o parach,
które nie mogą być przechowywane razem.

Przykładowe dane wejściowe:
```
1 4
2 4
3 5
```
Jedno z poprawnych rozwiązań:
```
1 2 3
4 5
```

## Uruchomienie

Istnieją trzy tryby wykonania:

1. Rozwiązanie problemu ze standardowego wejścia

    ```
    python3 program.py m1 n << i
    ```

    * n - liczba substancji
    * i - restrykcje

2. Rozwiązanie wygenerowanego problemu

    Rozwiązywalnego:
    ```
    python3 program.py m2 solvable [-d] [-f] n
    ```
   
    Nierozwiązywalnego:
    ```
    python3 program.py m2 unsolvable [-d] n
    ```
   
    * n - liczba substancji
    * d - zagęszczenie restrykcji (ułamek maksymalnych)
    * f - liczba substancji w jednym z magazynów

3. Pomiar i testowanie rozwiązania dla rosnących rozmiarów problemów

    ```
    python3 program.py m3 [-ins] [-r] [-v] n iterations step density
    ```

    * n - początkowa liczba substancji
    * iterations - ile razy zwiększyć rozmiar problemu
    * step - o ile zwiększyć ilość substancji w każdej iteracji
    * density - zagęszczenie restrykcji
    * ins - ile problemów wygenerować w każdej iteracji
    * r - ile razy powtarzać rozwiązanie każdego problemu
    * v - 0 nic nie wypisuje, 1 wypisuje nagłówek z podstawowymi informacjami, 2 wypisuje informacje po każdej iteracji
    
## Wejście/Wyjście

Wejściem powinny być niepowtarzające się pary liczb odpowiadających substancjom, 
które nie mogą być ze sobą w magazynie, oddzielone spacją.
Kolejne pary powinny być oddzielone znakiem nowej linii.

Na przykład:

```
1 4
2 4
3 5
```

Liczby odpowiadające substancjom powinny być naturalne i nie przekraczać liczby wszystkich substancji.
Jedyne dopuszczalne znaki na wejściu to: cyfry, spacja i znak nowej linii.


Na wyjściu otrzymamy pary liczb odpowiadających substancjom, 
znajdującym się w tym samym magazynie, oddzielone spacją 
oraz informację o tym, jaki to magazyn.
Magazyny zostaną oddzielone znakiem nowej linii.

Na przykład:

```
Warehouse A: 1 2 3
Warehouse B: 4 5
```

Jeżeli rozłożenie nie będzie możliwe, wypisana zostanie wadliwa restrykcja i aktualnie znalezione rozłożenie.

Na przykład:

```
Can't place substances. Problematic restriction:  (4, 5)
Current warehouse A: 1 2 3
Current warehouse B: 4 5
```

W przypadku wybrania trzeciego trybu uruchomienia zostanie wygenerowana tabela
porównująca pomiary z teoretyczną złożonościa.

Na przykład:

```

Algorytm z asymptotą O(n + m) dla m = 0.5 * n^2 / 4

n         	t(n)[ms]  	q(n)      
100       	0.65900   	0.73332   
200       	2.98892   	0.86348   
300       	6.12982   	0.79727   
400       	13.39467  	0.98638   
500       	20.62933  	0.97608   
600       	30.44341  	1.00293   
700       	40.27294  	0.97659   
800       	53.29207  	0.99082   
900       	67.94188  	0.99917   
1000      	84.37375  	1.00596
```

## Rozwiązanie

Reprezentacja danych jako graf w postaci listy sąsiedztwa, na przykład:

```
1: {4}
2: {4}
3: {5}
4: {1, 2}
5: {3}
```

Aby stworzyć tę strukturę wystarczy utworzyć zbiory dla każdego wierzchołka
i dla każdej krawędzi z wejścia dodawać odpowiedni wierzchołek do odpowiedniego zbioru.

Aby znaleźć rozwiązanie trzeba pokolorować graf na dwa kolory. 
Można to zrobić za pomocą algorytmu BFS.

Zauważmy, że nasz graf nie musi być spójny, może mieć wiele składowych. 
W szczególności może na przykład nie mieć żadnych krawędzi. 
Algorytm BFS za jednym razem jest w stanie przeszukać jedynie jedną spójną składową grafu. 
Z tego powodu trzeba pamiętać, które wierzchołki zostały już przeszukane. 
Wtedy pomijając te wierzchołki trzeba wykonywać algorytm tak długo, 
aż nie przeszuka wszystkich.

Działanie algorytmu BFS oparte jest na kolejce. 
Pobieramy wierzchołek z kolejki i dodajemy do kolejki wszystkich sąsiadów, 
którzy nie są jeszcze pokolorowani. 
Od razu kolorujemy ich innym kolorem niż kolor pobranego wierzchołka. 
W ten sposób wszystkie wierzchołki w kolejce są pokolorowane 
(z tym, że na początku trzeba pokolorować wierzchołek startowy algorytmu i wrzucić go do kolejki). 
Jeśli któryś z sąsiadów pobranego wierzchołka jest pokolorowany na ten sam kolor co wierzchołek, 
to znaczy że nie można pokolorować grafu na dwa kolory i nie można rozdzielić substancji na dwa magazyny. 
Możemy w tym przypadku zwrócić wadliwą krawędź i dotychczasowe rozłożenie na magazyny. 
Algorytm powtarzamy dopóki kolejka nie będzie pusta. 
Na końcu zwracamy zbiory wierzchołków pokolorowanych na te same kolory 
i dodajemy jeden z nich do jednego magazynu a drugi do drugiego.

Zbiory kolorów zaimplementowane są jako ```set``` a kolejka jako ```deque```,
co pozwala potrzebnym operacjom mieć średni koszt jednostkowy.

## Dekompozycja

* ```program.py``` - główny skrypt uruchamiający
* ```algorthm.py``` - implementacja algorytmu
* ```generator.py``` - implementacja generatora danych
* ```graph.py``` - definicja grafu
* ```measuring.py``` - pomocnicze funkcje do mierzenia czasu wykonania