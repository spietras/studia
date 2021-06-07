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
                         then do putStrLn "Quest already completed."
                                 return Nothing
                         else do putStrLn "projectQuest"
                                 return (Just 10)

project :: Place
project = Place 1
                "Project"
                "quest"
                projectQuest
                [("D", dorm)]
                []

labQuest :: Quest
labQuest state = if (questCompleted state 2)
                     then do putStrLn "Niestety, do laboratorium mogłeś podejść tylko raz. Niech to będzie dla Ciebie nauczka na przyszłość!"
                             return Nothing
                     else do score <- labQuest2
                             putStrLnLn "Właśnie ukończyłeś moduł laboratorium!"
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


facultyQuestPrintScore :: ScoreMap -> IO ()
facultyQuestPrintScore scores = let sumScores []          = 0
                                    sumScores ((_, (QuestScore v)):xs) = v + (sumScores xs)
                                in do putStr "Wynik ze studiów: "
                                      print (sumScores scores)


facultyQuest :: Quest
facultyQuest state = if not (questsCompleted state [0, 1, 2, 3])
                         then do putStrLn "Nie ma dróg na skróty. Najpierw musisz zasmakować studenckiego życia.\n\
                                          \Wróć jak wykonasz wszystkie ważne czyności."
                                 return Nothing
                         else do putStrLnLn "Po długiej drodze przez mękę nadszedł czas opuścić mury uczelni.\n\
                                            \Ze łzami w oczach patrzysz prawdopodobnie ostatni raz na gmach ukochanego wydziału.\n\
                                            \Wspomnienia wszystkich pięknych chwil powracają. Czas jednak uczynić kolejny krok.\n\n\
                                            \Nagle ktoś kładzie ci dłoń na ramieniu.\n\
                                            \Odwracasz się. Oślepia cię blask nieznajomego, którego potęga jaśnieje jak tysiące słońc.\n\
                                            \Po chwili słyszysz aksamitny głos:\n\
                                            \\"Dzień dobry. Jestem Prezesem Firmy ZIOMARCH. Obserwowałem Pana.\n\
                                            \Wiem, że dzisiaj kończy Pan studia. Czy chce Pan rozpocząć przygodę w naszej Firmie?\""
                                 input <- getLine
                                 putLn
                                 case input of 
                                     "TAK" -> facultyQuest2_1 state
                                     "NIE" -> facultyQuest2_2 state
                                     _ -> do putStrLnLn "Możliwe opcje do wyboru: TAK, NIE"
                                             _ <- facultyQuest state
                                             return ()
                                 return Nothing

facultyQuest2_1 :: GameState -> IO ()
facultyQuest2_1 (GameState scores) = do putStrLnLn "Wybrałeś drogę pracownika korporacji. Dobrze.\n\
                                                   \Na twarzy Prezesa pojawia się szyderczy uśmiech.\n\
                                                   \\"Kolejny student do kolekcji\" - mówi Prezes, zakładając ci kajdany na nogi.\n\n\
                                                   \Wsiadacie do helikoptera Prezesa i odlatujecie do klimatyzowanego biura z open spacem.\n\n\
                                                   \Koniec gry."
                                        facultyQuestPrintScore scores
                                        exitSuccess

facultyQuest2_2 :: GameState -> IO ()
facultyQuest2_2 state = do putStrLnLn "Bez słowa odchodzisz, wiedząc, że czeka na ciebie lepszy los.\n\
                                      \W głowie pojawiają ci się dwa pomysły.\n\
                                      \Możesz wspinać się dalej po akademickiej drabinie i zostać doktorantem.\n\
                                      \Albo możesz założyć własny startup.\n\
                                      \Co robisz?"
                           input <- getLine
                           putLn
                           case input of 
                               "ZOSTAŃ DOKTORANTEM" -> facultyQuest3_1 state
                               "ZAŁÓŻ STARTUP" -> facultyQuest3_2 state
                               _ -> do putStrLnLn "Możliwe opcje do wyboru: ZOSTAŃ DOKTORANTEM, ZAŁÓŻ STARTUP"
                                       facultyQuest2_2 state


facultyQuest3_1 :: GameState -> IO ()
facultyQuest3_1 state = do putStrLnLn "Podążasz za głosem serca i wybierasz karierę akademicką.\n\
                                      \Może wyrośnie nam kolejny Einstein!\n\
                                      \Najpierw jednak musisz odpowiedzieć na pytanie kwalifikacyjne:\n\
                                      \\"Ile to 2+2?\""
                           input <- getLine
                           putLn
                           case input of 
                               "4" -> facultyQuest4_1 state
                               _ -> facultyQuest4_2 state

facultyQuest4_1 :: GameState -> IO ()
facultyQuest4_1 (GameState scores) = do putStrLnLn "Dobrze! Zostajesz przyjęty!\
                                                   \Już w pierwszym miesiącu udaje ci się udowodnić, że P != NP.\n\
                                                   \Dzięki temu zyskujesz wieczną chwałę dla siebie i wydziału.\n\n\
                                                   \Koniec gry"
                                        facultyQuestPrintScore scores
                                        exitSuccess

facultyQuest4_2 :: GameState -> IO ()
facultyQuest4_2 (GameState scores) = do putStrLnLn "Niestety nie jest to poprawna odpowiedź.\n\
                                                   \Od absolwenta tego wydziału oczekuje się przynajmniej podstaw arytmetyki.\n\
                                                   \Twój dyplom zostaje unieważniony. Musisz powtórzyć studia.\n\
                                                   \Najlepiej zaczynając od szkoły podstawowej...\n\n\
                                                   \Koniec gry"
                                        facultyQuestPrintScore scores
                                        exitSuccess

facultyQuest3_2 :: GameState -> IO ()
facultyQuest3_2 state = do putStrLnLn "Postanawiasz zaryzykować i założyć startup.\n\
                                      \Widzisz jak z nieba już sypią się pieniądze od inwestorów.\n\
                                      \Najpierw jednak musisz zdecydować czy kupisz za nie Playstation czy Xboxa do game roomu.\n\
                                      \Co wybierasz?"
                           input <- getLine
                           putLn
                           case input of 
                              "PLAYSTATION" -> facultyQuest4_3 state
                              "XBOX" -> facultyQuest4_4 state
                              _ -> do putStrLnLn "Możliwe opcje do wyboru: PLAYSTATION, XBOX"
                                      facultyQuest3_2 state

facultyQuest4_3 :: GameState -> IO ()
facultyQuest4_3 (GameState scores) = do putStrLnLn "Playstation! Dobry wybór. Widać, że jesteś człowiekiem kultury.\n\
                                                   \Twój biznes działa, produkt sprzedaje się w ogromnych nakładach.\n\
                                                   \Inwestorzy próbują się do ciebie dobić drzwiami i oknami.\n\
                                                   \Dorobiłeś się fortuny i nie musisz już pracować nawet dnia w swoim życiu.\n\n\
                                                   \Koniec gry"
                                        facultyQuestPrintScore scores
                                        exitSuccess

facultyQuest4_4 :: GameState -> IO ()
facultyQuest4_4 (GameState scores) = do putStrLnLn "Xbox? OK, każdy ma prawo się mylić.\n\
                                                   \Twój startup upada i lądujesz na ulicy. Całe studia na marne...\n\
                                                   \W poszukiwaniu jedzenia przeglądasz pobliskie śmietniki.\n\
                                                   \Jedyne co znajdujesz to stare Playstation.\n\
                                                   \Na zawsze będzie ci już ono przypominało o twojej porażce.\n\n\
                                                   \Koniec gry"
                                        facultyQuestPrintScore scores
                                        exitSuccess

faculty :: Place
faculty = Place 4
                "Elka... Przypominają ci się wszystkie zarwane noce, stres, brak życia i niespełnione ambicje, przez co prawie mdlejesz.\n\
                \Coś jednak pozwala ci przetrwać i iść dalej. Może to dobra kawa z automatów, może koledzy, a może po prostu syndrom sztokholmski?\n\
                \Jesteś przed salą wykładową. W oddali widać grono studentów czekających przy sali laboratoryjnej.\n\
                \Przechodzisz obok kosza na śmieci, który wygląda inaczej niż zwykle. Stawiasz plecak z laptopem i siadasz na kanapie.\n\
                \Myślisz sobie: \"Może najwyższy czas skończyć te studia?\""
                "ZAKOŃCZ STUDIA"
                facultyQuest
                [("WRÓĆ DO AKADEMIKA", dorm), ("WEJDŹ DO SALI WYKŁADOWEJ", test), ("PODEJDŹ DO SALI LABORATORYJNEJ", lab)]
                [("OTWÓRZ KOSZ", "Kosz na śmieci otwiera się bezdotykowo. A więc to na to idą pieniądze z warunków?"),
                 ("WYJMIJ LAPTOPA", "Wyjmujesz laptopa i udajesz że się uczysz.\nZbyt wiele to nie daje, bo wiadomo, że robisz to jedynie dla szpanu.")]


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
