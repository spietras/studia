-- Game scenario definitions

module Def(start, welcome, help, quit, badInput, availableCommands) where

import           System.Exit

import           Map
import           Types
import           Utils

questCompleted :: GameState -> PlaceId -> Bool
questCompleted (GameState map) placeId = mapKeyExists map placeId

questsCompleted :: GameState -> [PlaceId] -> Bool
questsCompleted state [] = True
questsCompleted state (x:xs) = (questCompleted state x) && (questsCompleted state xs)

dormQuest :: Quest
dormQuest state = if (questCompleted state 0)
                      then do putStrLn "Quest already completed."
                              return Nothing
                      else do putStrLn "dormQuest"
                              return (Just 10)

dorm :: Place
dorm = Place 0
             "Dorm"
             "quest"
             dormQuest
             [("F", faculty), ("P", project)]
             []

projectQuest :: Quest
projectQuest state = if (questCompleted state 1)
                     then do putStrLn "Dostałeś już ocenę z projektu. Chcesz go przechodzić jeszcze raz? Zapomnij! Może za semestr."
                             return Nothing
                     else do score <- projectQuest2
                             putStrLn "Zakończyłeś projekt!"
                             putStr "Uzyskany wynik: "
                             print score
                             return (Just score)

projectQuest2 :: IO Int
projectQuest2 = do putStrLnLn "GODZINA 20:00. 12 GODZIN DO ODDANIA PROJEKTU."
                   putStrLn "Nadszedł ten moment. Wiadomo, wszystko zleciało szybko i nieubłaganie, trzy miesiące od rozdania projektów minęło Ci niepostrzeżenie, nie było czasu."
                   putStrLn "Ale jednak masz chęć zdać ten projekt. Zawsze to kilka stówek więcej w kieszeni na wsparcie lokalnych hodowców chmielu, żyta i ziemniaków."
                   putStrLn "Niechętnie zabierasz się za projekt. Czytasz temat: zamodelować dźwig w OpenGL."
                   putStrLn "Chyba jednak masz wątpliwości, czy jest sens brać się za to."
                   putStrLnLn "Pisać projekt, czy iść spać?"
                   input <- getLine
                   putLn
                   score <- case input of
                                "PISZ PROJEKT" -> projectQuest3_1
                                "IDŹ SPAĆ" -> projectQuest3_2
                                _ -> do putStrLnLn "Możliwe opcje do wyboru: PISZ PROJEKT, IDŹ SPAĆ"
                                        projectQuest2
                   return score

projectQuest3_1 :: IO Int
projectQuest3_1 = do putStrLn "OK, próbujesz kontaktować się z pozostałymi czterema członkami drużyny. Jeden odpowiada i deklaruje pomoc."
                     putStrLnLn "Odpalacie sesję udostępniania ekranu i próbujecie ogarnąć OpenGLa."
                     putStrLnLn "GODZINA 22:00. 10 GODZIN DO ODDANIA PROJEKTU."
                     putStrLn "Dochodzicie do wniosku, że z materiałów dostarczonych przez prowadzącego niczego się nie nauczycie. Trzeba znaleźć inne źródło."
                     putStrLn "Jesteś z drugiej strony już mocno znużony, koncepcja możliwości szybkiego zaśnięcia w łóżku, które stoi zaraz obok, jest niezwykle kusząca."
                     putStrLnLn "Pisać projekt, czy jednak iść spać?"
                     input <- getLine
                     putLn
                     score <- case input of
                                "PISZ PROJEKT" -> do res <- projectQuest4_1 True
                                                     return (res + 5)
                                "IDŹ SPAĆ" -> projectQuest4_2
                                _ -> do putStrLnLn "Możliwe opcje do wyboru: PISZ PROJEKT, IDŹ SPAĆ"
                                        projectQuest3_2
                     return score

projectQuest3_2 :: IO Int
projectQuest3_2 = do putStrLn "Brawo. Tak zachowuje się poważny człowiek. Studia, studiami, ale o zdrowie trzeba dbać."
                     putStrLnLn "No ale jeszcze nie jest tak późno. Odpalasz Netfliksa, oglądasz jeden odcinek serialu, drugi, trzeci..."
                     putStrLnLn "GODZINA 23:00. 9 GODZIN DO ODDANIA PROJEKTU."
                     putStrLn "Przebudzony przez kolegę z piętra wyżej, któremu akurat wtedy musiał spaść garnek na podłogę, wstajesz z odciśniętą klawiaturą na czole i znów masz wątpliwości."
                     putStrLn "Żal byłoby nie zdać, projekt wydaje się prosty - myślisz."
                     putStrLnLn "Pisać projekt, czy jednak iść spać?"
                     input <- getLine
                     putLn
                     score <- case input of
                                "PISZ PROJEKT" -> projectQuest4_1 False
                                "IDŹ SPAĆ" -> projectQuest4_2
                                _ -> do putStrLnLn "Możliwe opcje do wyboru: PISZ PROJEKT, IDŹ SPAĆ"
                                        projectQuest3_2
                     return score

projectQuest4_1 :: Bool -> IO Int
projectQuest4_1 learningSession = do    case learningSession of
                                                True -> putStrLn "Wychodzisz do toalety, wracasz, i Twój kolega nagle się rozłączył. Wygląda na to, że resztę nocy z projektem spędzisz sam."
                                                False -> putStrLn "No nic. Włączasz swoje ulubione IDE i bierzesz się do pracy."
                                        putStrLnLn "Znajdujesz jakąś gadającą głowę, która bez zbędnych komentarzy koślawym angielskim opowiada, jak tworzyć obiekty w OpenGLu. Oglądasz z prędkością x2 cały tutorial."
                                        putStrLnLn "GODZINA 00:00. 8 GODZIN DO ODDANIA PROJEKTU."
                                        putStrLn "Masz jakieś zaczątki działającego programu, ale do dźwigu jeszcze dłuuuga droga."
                                        putStrLn "Dochodzisz do wniosku, że sam tego nie napiszesz."
                                        putStrLn "Wtem zauważasz, że kolega, który zdawał ten przedmiot rok temu, jest dostępny. Jest już trochę późno, więc wpadasz też na pomysł poszukania jakiegoś projektu na GitHubie."
                                        putStrLnLn "Pisać do kolegi, czy zerżnąć gotowy projekt z GitHuba?"
                                        input <- getLine
                                        putLn
                                        score <- case input of
                                                        "PISZ DO KOLEGI" -> projectQuest5_1
                                                        "ZERŻNIJ Z GITHUBA" -> projectQuest5_2
                                                        _ -> do putStrLnLn "Możliwe opcje do wyboru: PISZ DO KOLEGI, ZERŻNIJ Z GITHUBA"
                                                                projectQuest4_1 learningSession
                                        return score

projectQuest4_2 :: IO Int
projectQuest4_2 = do putStrLn "No cóż, projekt nie jest najważniejszą rzeczą w Twoim życiu. Po co się męczyć w nocy?"
                     putStrLn "Myjesz ząbki i idziesz grzecznie spać."
                     putStrLn "Już w łóżku, przed snem, za kultową czołówką Na dobre i na złe, podśpiewujesz:"
                     putStrLnLn "Nie planuję, nie obliczam..." 
                     putStrLn "Z przedmiotu dostajesz 2, a miesiąc później, w USOSie czeka na Ciebie drobny rachuneczek do opłacenia."
                     putStrLnLn "Cóż, ciężkie jest życie studenta..."
                     return 0

projectQuest5_1 :: IO Int
projectQuest5_1 = do putStrLn "Twój kolega Janusz służy Ci pomocą. Udostępnia Ci projekt na ten sam temat, który musisz napisać!"
                     putStrLnLn "Niestety, nie powiedział Ci, jak uruchomić ten projekt, musisz dojść do tego sam..."
                     putStrLnLn "GODZINA 01:00. 7 GODZIN DO ODDANIA PROJEKTU."
                     putStrLn "Masz powoli dość. Po długich zmaganiach udało Ci się odpalić projekt, ale nie masz bladego pojęcia, jak działa."
                     putStrLnLn "Zrozumieć kod, czy iść spać?"
                     input <- getLine
                     putLn
                     score <- case input of
                                        "ZROZUM KOD" -> do putStrLnLn "Zasiadasz więc przed kodem i próbujesz z trudem go zrozumieć, a nuż się zapyta co i jak."
                                                           putStrLnLn "GODZINA 04:00. 4 GODZINY DO ODDANIA PROJEKTU."
                                                           putStrLn "Z sukcesem kończysz rozumienie kodu. Za oknem już dnieje, a pierwsze kruki dają o sobie znać."
                                                           putStrLnLn "Wykonałeś wszystko, co mogłeś. Zasypiasz jak długi w swoim łóżku."
                                                           putStrLnLn "ODDANIE PROJEKTU."
                                                           putStrLn "Na oddanie projektu przyszli oczywiście wszyscy członkowie Twojej grupy."
                                                           putStrLn "Śpiewająco zaprezentowałeś całe swoje (no, powiedzmy) rozwiązanie. Prowadzący miał nieco wątpliwości, czy nie widział już czegoś podobnego, ale udało Ci się odwieść go od tej myśli."
                                                           putStrLnLn "Zaliczacie projekt z dobrym wynikiem!"
                                                           return 43
                                        "IDŹ SPAĆ" -> do putStrLnLn "Koniec zabawy z dźwigiem. Zasypiasz."
                                                         putStrLnLn "ODDANIE PROJEKTU."
                                                         putStrLn "Na oddanie projektu przyszli oczywiście wszyscy członkowie Twojej grupy."
                                                         putStrLn "Zmusili Cię do prezentacji tego projektu. Ty, niestety, nie wiedziałeś, jak działa shader, który odpowiada za oświetlenie punktowe sceny."
                                                         putStrLnLn "Prowadzący, podejrzewając, że nie napisaliście tego projektu sami, daje Wam niewiele więcej punktów niż to, co potrzeba, żeby zdać projekt."
                                                         return 11
                                        _ -> do putStrLnLn "Możliwe opcje do wyboru: ZROZUM KOD, IDŹ SPAĆ"
                                                projectQuest5_1
                     return score

projectQuest5_2 :: IO Int
projectQuest5_2 = do putStrLn "Po długim napastowaniu wyszukiwarki GitHuba udaje Ci się znaleźć coś podobnego."
                     putStrLnLn "Nie jest to do końca to, bo to niby taśmociąg, ale kształt nieco podobny..."
                     putStrLnLn "GODZINA 07:00. GODZINA DO ODDANIA PROJEKTU."
                     putStrLn "Zadanie poprawy taśmociągu w dźwig Cię przerosło."
                     putStrLnLn"Nie wygląda to dobrze. Myślisz - i tak lepsze to niż nic, zresztą wszystko Ci jedno, siedziałeś przecież całą noc..."
                     putStrLnLn "ODDANIE PROJEKTU."
                     putStrLn "Na oddanie projektu przyszli oczywiście wszyscy członkowie Twojej grupy."
                     putStrLn "Prowadzący, co ciekawe, nie zauważył większych problemów z Twoim dźwigiem. Zastanowiły go jednak linie w kodzie, które traktowały o taśmociągu."
                     putStrLn "Mocno naciskał, więc się przyznajesz do prawdy."
                     putStrLnLn "Widząc wory pod Twoimi oczami, prowadzący lituje się nad Tobą i daje Waszej drużynie nawet dobry wynik!"
                     return 25

project :: Place
project = Place 1
                "Witaj w pokoju, w którym przeżyjesz noc pełną wrażeń!\nW czasie projektu nauczysz się efektywnie planować czas, konsultować pomysły z grupą i korzystać z zewnętrznych źródeł.\nZAJĄĆ SIĘ PROJEKTEM? Czy wrócić do akademika?"
                "ZAJMIJ SIĘ PROJEKTEM"
                projectQuest
                [("WRACAJ DO AKADEMIKA", dorm)]
                []

labQuest :: Quest
labQuest state = if (questCompleted state 2)
                     then do putStrLn "Niestety, do laboratorium mogłeś podejść tylko raz. Niech to będzie dla Ciebie nauczka na przyszłość!"
                             return Nothing
                     else do score <- labQuest2
                             putStrLn "Właśnie ukończyłeś moduł laboratorium!"
                             putStr "Uzyskany wynik: "
                             print score
                             return (Just score)

labQuest2 :: IO Int
labQuest2 = do putStrLn "Witaj, dzielny adepcie Technik Sygnałów i Informacji!"
               putStrLn "Właśnie dziś będziesz miał szansę przekonać się o tym, z jakimiż to problemami musimy się mierzyć, dyskretyzując sygnał w czasie."
               putStrLn "Ale, ale! Nie tak szybko! Na laboratorium musisz wykonać pracę domową, a zostało Ci tylko trzy godziny!"
               putStrLn "Na dodatek akurat trwają ćwiczenia z tego właśnie przedmiotu, a prowadzący zajęcia zwraca szczególną uwagę na to, co w czasie zajęć robią jego studenci."
               putStrLnLn "Pisać pracę domową, czy uważać? (i tak będziesz tylko udawał, nie oszukujmy się)"
               input <- getLine
               putLn
               score <- case input of
                    "PISZ PRACĘ DOMOWĄ" -> do res <- labQuest3 True
                                              return (res + 5)
                    "UWAŻAJ" -> labQuest3 False
                    _ -> do putStrLnLn "Możliwe opcje do wyboru: PISZ PRACĘ DOMOWĄ, UWAŻAJ"
                            labQuest2
               return score

labQuest3 :: Bool -> IO Int
labQuest3 hw = do if hw == True
                    then do putStrLn "Prowadzący spostrzegł, że zajmujesz się czymś zdecydowanie mniej ciekawym niż prowadzone przez niego ćwiczenia, ale uszło Ci to płazem."
                            putStrLnLn "Udaje Ci się spisać zadanie od koleżanki z ławki - pamiętaj, masz u niej dług wdzięczności!"
                            score <- labQuest4 hw
                            return score
                    else do putStrLnLn "Zieeew..."
                            score <- labQuest4 hw
                            return score

labQuest4 :: Bool -> IO Int
labQuest4 hw = do putStrLn "No dobrze, ćwiczenia się skończyły, laboratorium już za godzinę, a Tobie burczy w brzuchu..."
                  putStrLnLn "A na laboratorium trzeba się jeszcze nauczyć na wejściówkę! I co teraz?"
                  input <- getLine
                  putLn
                  score <- case input of
                        "IDŹ JEŚĆ" -> labQuest5_1 True
                        "UCZ SIĘ" -> labQuest5_2
                        _ -> do putStrLnLn "Możliwe opcje do wyboru: IDŹ JEŚĆ, UCZ SIĘ"
                                labQuest4 hw
                  return score

labQuest5_1 :: Bool -> IO Int
labQuest5_1 hw = do putStrLn "Myślisz sobie - dość tego! Idę na spaghetti alla carbonara!"
                    putStrLn "Najadłeś się, ale myślisz sobie, że w sumie to byś wrócił do pokoju i się przespał... W końcu zdrowie jest najważniejsze!"
                    putStrLnLn "Jaka decyzja?"
                    input <- getLine
                    putLn
                    score <- case input of
                            "IDŹ SPAĆ" -> do labQuest6_1
                                             let ret = case hw of
                                                        True -> (-5)
                                                        False -> 0
                                             return ret
                                          
                            "IDŹ NA LABORATORIUM" -> labQuest6_2 False True
                            _ -> do putStrLnLn "Możliwe opcje do wyboru: IDŹ SPAĆ, IDŹ NA LABORATORIUM"
                                    labQuest5_1 hw
                    return score

labQuest5_2 :: IO Int
labQuest5_2 = do putStrLn "Ty pracusiu! Wiele się nie nauczyłeś, ale zawsze lepsze to niż nic."
                 putStrLn "Zostało Ci jeszcze trochę czasu, myślisz - może warto napisać sobie ściągę... Czemu nie? Co tam uczciwość!"
                 putStrLnLn "Pisać ściągi na wejściówkę?"
                 input <- getLine
                 putLn
                 score <- case input of
                        "TAK" -> labQuest6_2 True False
                        "NIE" -> labQuest6_2 False False
                        _ -> do putStrLnLn "Możliwe opcje do wyboru: TAK, NIE"
                                labQuest5_2
                 return score


labQuest6_1 :: IO ()
labQuest6_1 = do putStrLn "Wiadomo, opuszczenie jednego laboratorium to nie koniec świata, a mała sjesta po wyżerce zawsze się przyda..."
                 putStrLn "Spędzasz błogo popołudnie ze swoim najlepszym czworonożnym przyjacielem, podczas gdy inni cierpią katusze na labce!"

labQuest6_2 :: Bool -> Bool -> IO Int
labQuest6_2 cheats food = do putStrLn "Docierasz na labkę. Wyjeżdżasz windą na piętro dwie minutki przed, wychodzisz, a tam masa ludzi tli się przy drzwiach wejściowych..."
                             putStrLn "Z kolei tu, gdzie jesteś, przy windzie, jest całkiem sporo miejsca!"
                             putStrLnLn "Jesteś skonfundowany. Co teraz - iść pod drzwi czy zostać pod windą?"
                             input <- getLine
                             putLn
                             score <- case input of
                                 "IDŹ POD DRZWI" -> labQuest7 cheats food True
                                 "ZOSTAŃ POD WINDĄ" -> labQuest7 cheats food False
                                 _ -> do putStrLnLn "Możliwe opcje do wyboru: IDŹ POD DRZWI, ZOSTAŃ POD WINDĄ"
                                         labQuest6_2 cheats food
                             return score

labQuest7 :: Bool -> Bool -> Bool -> IO Int
labQuest7 cheats food door = do putStrLn "Drzwi się otwierają, i przypominasz sobie znajomą twarz - ostatnio ten prowadzący dał Ci popalić na labce..."
                                putStrLn "Wszyscy idą do pierwszej sali, gdzie prowadzi ktoś nowy, wygląda na spoko gościa!"
                                putStrLnLn "Czy spróbować iść do nowego prowadzącego?"
                                input <- getLine
                                putLn
                                score <- case input of 
                                            "TAK" -> case door of
                                                        True -> labQuest8_1
                                                        False -> labQuest8_2 cheats food
                                            "NIE" -> labQuest8_2 cheats food
                                            _ -> do putStrLnLn "Możliwe opcje do wyboru: TAK, NIE"
                                                    labQuest7 cheats food door
                                return score

labQuest8_1 :: IO Int
labQuest8_1 = do
    putStrLn "Brawo! Udało się! Nowy prowadzący to rzeczywiście złoty człowiek."
    putStrLn "Z łatwością (i z pomocą kolegi po lewej) maksujesz wejściówkę, a z labek ucina Ci tylko trochę (dla przykładu)."
    return 34

labQuest8_2 :: Bool -> Bool -> IO Int
labQuest8_2 cheats food = do    putStrLn "Niestety, trafiłeś do swojego starego, (ekhm) dobrego znajomego. Wejściówka oczywiście zbiła Cię z tropu."
                                putStrLn "Na dodatek Was porozsadzał, nie masz od kogo ściągnąć..."
                                score <- case food of
                                        True -> do putStrLn "Nie masz zielonego pojęcia jak rozwiązać zadania. Zerujesz wejściówkę."
                                                   putStrLn "Z labami też wyszło tak sobie, dobrze, że chociaż Twój kolega coś umiał."
                                                   putStrLn "Najważniejsze, że wychodzisz najedzony."
                                                   return 13
                                        False -> do putStrLn "Na jedno pytanie udaje Ci się odpowiedzieć."
                                                    score <- case cheats of
                                                        True -> do putStrLnLn "Ale masz ściągawki! Skorzystać z nich?"
                                                                   ret <- labQuest9
                                                                   return ret
                                                        False -> do putStrLn "Nie masz ściągawek, zostajesz z tym co masz, tyle wyszło z Twojej uczciwości..."
                                                                    return 0 
                                                    putStrLn "Na szczęście na samej labce wiedza nie poszła w las. Nawet dobrze Ci poszło to laboratorium!"
                                                    return (score + 24)
                                return score       

labQuest9 :: IO Int
labQuest9 = do input <- getLine
               case input of
                "TAK" -> do putStrLn "Niestety, prowadzący nakrył Cię na ściąganiu! Z wejściówki dostajesz okrągłe zero! I tyle z Twojej nauki!"
                            return (-5)
                "NIE" -> do putStrLn "No nic, zostajesz z tym, co pamiętałeś."
                            return 0
                _ -> do putStrLnLn "Możliwe opcje do wyboru: TAK, NIE"
                        labQuest9

lab :: Place
lab = Place 2
            "Witaj przed salą laboratoryjną! Właśnie tutaj odbywają się laboratoria z Technik Sygnałów i Informacji.\nPODJĄĆ WYZWANIE? A może wrócić na wydział albo na kolokwium?"
            "PODEJMIJ WYZWANIE"
            labQuest
            [("WRACAJ NA WYDZIAŁ", faculty), ("WRACAJ NA KOLOKWIUM", test)]
            []

testQuest :: Quest
testQuest state = if (questCompleted state 3)
                      then do putStrLn "Quest already completed."
                              return Nothing
                      else do putStrLn "testQuest"
                              return (Just 10)

test :: Place
test = Place 3
             "Test"
             "quest"
             testQuest
             [("F", faculty), ("L", lab)]
             []


facultyQuest :: Quest
facultyQuest state = if not (questsCompleted state [0, 1, 2, 3])
                         then do putStrLn "Complete all the other quests first."
                                 return Nothing
                         else do putStrLn "facultyQuest"
                                 putStrLn "Game Over"
                                 exitSuccess

faculty :: Place
faculty = Place 4
                "Faculty"
                "quest"
                facultyQuest
                [("D", dorm), ("T", test), ("L", lab)]
                []


start :: Place
start = dorm

welcome :: String
welcome = "Welcome to the game"

help :: Action
help = "HELP"

quit :: Action
quit = "QUIT"

badInput :: String
badInput = "Bad input."

availableCommands :: [Action] -> String
availableCommands cmds = "Available commands: " ++ (prettyStrings cmds)
