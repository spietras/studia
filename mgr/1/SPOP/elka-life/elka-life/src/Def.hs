-- Game scenario definitions

module Def(start, help, quit, badInput, availableCommands) where

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

facultyQuest :: Quest
facultyQuest state = if not (questsCompleted state [0, 1, 3, 4])
                         then do putStrLn "Complete all the other quests first."
                                 return Nothing
                         else do putStrLn "facultyQuest"
                                 putStrLn "Game Over"
                                 exitSuccess

faculty :: Place
faculty = Place 2
                "Faculty"
                "quest"
                facultyQuest
                [("D", dorm), ("T", test), ("L", lab)]
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

labQuest :: Quest
labQuest state = if (questCompleted state 4)
                     then do putStrLn "Niestety, do laboratorium mogłeś podejść tylko raz. Niech to będzie dla Ciebie nauczka na przyszłość!"
                             return Nothing
                     else do score <- questLab2
                             putStrLn "Właśnie ukończyłeś moduł laboratorium!"
                             putStr "Uzyskany wynik: "
                             print score
                             return (Just score)

questLab2 :: IO Int
questLab2 = do putStrLn "\nWitaj, dzielny adepcie Technik Sygnałów i Informacji!"
               putStrLn "Właśnie dziś będziesz miał szansę przekonać się o tym, z jakimiż to problemami musimy się mierzyć, dyskretyzując sygnał w czasie."
               putStrLn "Ale, ale! Nie tak szybko! Na laboratorium musisz wykonać pracę domową, a zostało Ci tylko trzy godziny!"
               putStrLn "Na dodatek akurat trwają ćwiczenia z tego właśnie przedmiotu, a prowadzący zajęcia zwraca szczególną uwagę na to, co w czasie zajęć robią jego studenci."
               putStrLn "Piszesz pracę domową, czy uważasz? (i tak będziesz tylko udawał, nie oszukujmy się)"
               input <- getLine
               score <- case input of
                    "PISZĘ PRACĘ DOMOWĄ" -> do res <- questLab3 True
                                               return (res + 5)
                    "UWAŻAM" -> questLab3 False
                    _ -> do putStrLn "\nMożliwe opcje do wyboru: PISZĘ PRACĘ DOMOWĄ, UWAŻAM"
                            questLab2
               return score

questLab3 :: Bool -> IO Int
questLab3 hw = do if hw == True
                    then do putStrLn "\nProwadzący spostrzegł, że zajmujesz się czymś zdecydowanie mniej ciekawym niż prowadzone przez niego ćwiczenia, ale uszło Ci to płazem."
                            putStrLn "Udaje Ci się spisać zadanie od koleżanki z ławki - pamiętaj, masz u niej dług wdzięczności!"
                            score <- questLab4 hw
                            return score
                    else do putStrLn "\nZieeew..."
                            score <- questLab4 hw
                            return score

questLab4 :: Bool -> IO Int
questLab4 hw = do putStrLn "\nNo dobrze, ćwiczenia się skończyły, laboratorium już za godzinę, a Tobie burczy w brzuchu..."
                  putStrLn "A na laboratorium trzeba się jeszcze nauczyć na wejściówkę! I co teraz?"
                  input <- getLine
                  score <- case input of
                        "IDĘ JEŚĆ" -> questLab5_1 True
                        "UCZĘ SIĘ" -> questLab5_2
                        _ -> do putStrLn "\nMożliwe opcje do wyboru: IDĘ JEŚĆ, UCZĘ SIĘ"
                                questLab4 hw
                  return score

questLab5_1 :: Bool -> IO Int
questLab5_1 hw = do putStrLn "\nMyślisz sobie - dość tego! Idę na spaghetti alla carbonara!"
                    putStrLn "Najadłeś się, ale myślisz sobie, że w sumie to byś wrócił do pokoju i się przespał... W końcu zdrowie jest najważniejsze!"
                    putStrLn "Jaka decyzja?"
                    input <- getLine
                    score <- case input of
                            "IDĘ SPAĆ" -> do questLab6_1
                                             let ret = case hw of
                                                        True -> (-5)
                                                        False -> 0
                                             return ret
                                          
                            "IDĘ NA LABORATORIUM" -> questLab6_2 False True
                            _ -> do putStrLn "\nMożliwe opcje do wyboru: IDĘ SPAĆ, IDĘ NA LABORATORIUM"
                                    questLab5_1 hw
                    return score

questLab5_2 :: IO Int
questLab5_2 = do putStrLn "\nTy pracusiu! Wiele się nie nauczyłeś, ale zawsze lepsze to niż nic."
                 putStrLn "Zostało Ci jeszcze trochę czasu, myślisz - może warto napisać sobie ściągę... Czemu nie? Co tam uczciwość!"
                 putStrLn "Piszesz ściągi na wejściówkę?"
                 input <- getLine
                 score <- case input of
                        "TAK" -> questLab6_2 True False
                        "NIE" -> questLab6_2 False False
                        _ -> do putStrLn "\nMożliwe opcje do wyboru: TAK, NIE"
                                questLab5_2
                 return score


questLab6_1 :: IO ()
questLab6_1 = do putStrLn "\nWiadomo, opuszczenie jednego laboratorium to nie koniec świata, a mała sjesta po wyżerce zawsze się przyda..."
                 putStrLn "Spędzasz błogo popołudnie ze swoim najlepszym czworonożnym przyjacielem, podczas gdy inni cierpią katusze na labce!"

questLab6_2 :: Bool -> Bool -> IO Int
questLab6_2 cheats food = do putStrLn "\nDocierasz na labkę. Wyjeżdżasz windą na piętro dwie minutki przed, wychodzisz, a tam masa ludzi tli się przy drzwiach wejściowych..."
                             putStrLn "Z kolei tu, gdzie jesteś, przy windzie, jest całkiem sporo miejsca!"
                             putStrLn "Jesteś skonfundowany. Co wybierasz - idziesz pod drzwi czy zostajesz pod windą?"
                             input <- getLine
                             score <- case input of
                                 "IDĘ" -> questLab7 cheats food True
                                 "ZOSTAJĘ" -> questLab7 cheats food False
                                 _ -> do putStrLn "\nMożliwe opcje do wyboru: IDĘ, ZOSTAJĘ"
                                         questLab6_2 cheats food
                             return score

questLab7 :: Bool -> Bool -> Bool -> IO Int
questLab7 cheats food door = do putStrLn "\nDrzwi się otwierają, i przypominasz sobie znajomą twarz - ostatnio ten prowadzący dał Ci popalić na labce..."
                                putStrLn "Wszyscy idą do pierwszej sali, gdzie prowadzi ktoś nowy, wygląda na spoko gościa!"
                                putStrLn "Próbujesz iść do lepszego prowadzącego?"
                                input <- getLine
                                score <- case input of 
                                            "TAK" -> case door of
                                                        True -> questLab8_1
                                                        False -> questLab8_2 cheats food
                                            "NIE" -> questLab8_2 cheats food
                                            _ -> do putStrLn "\nMożliwe opcje do wyboru: TAK, NIE"
                                                    questLab7 cheats food door
                                return score

questLab8_1 :: IO Int
questLab8_1 = do
    putStrLn "\nBrawo! Udało się! Nowy prowadzący to rzeczywiście złoty człowiek."
    putStrLn "Z łatwością (i z pomocą kolegi po lewej) maksujesz wejściówkę, a z labek ucina Ci tylko trochę (dla przykładu)."
    return 34

questLab8_2 :: Bool -> Bool -> IO Int
questLab8_2 cheats food = do    putStrLn "\nNiestety, trafiłeś do swojego starego, (ekhm) dobrego znajomego. Wejściówka oczywiście zbiła Cię z tropu."
                                putStrLn "Na dodatek Was porozsadzał, nie masz od kogo ściągnąć..."
                                score <- case food of
                                        True -> do putStrLn "\nNie masz zielonego pojęcia jak rozwiązać zadania. Zerujesz wejściówkę."
                                                   putStrLn "Z labami też wyszło tak sobie, dobrze, że chociaż Twój kolega coś umiał."
                                                   putStrLn "Dobrze, że przynajmniej wychodzisz najedzony."
                                                   return 13
                                        False -> do putStrLn "\nNa jedno pytanie udaje Ci się odpowiedzieć."
                                                    score <- case cheats of
                                                        True -> do putStrLn "Ale masz ściągawki! Korzystasz z nich?"
                                                                   ret <- questLab9
                                                                   return ret
                                                        False -> do putStrLn "Nie masz ściągawek, zostajesz z tym co masz, tyle wyszło z Twojej uczciwości..."
                                                                    return 0 
                                                    putStrLn "\nNa szczęście na samej labce wiedza nie poszła w las. Nawet dobrze Ci poszło to laboratorium!"
                                                    return (score + 24)
                                return score       

questLab9 :: IO Int
questLab9 = do input <- getLine
               case input of
                "TAK" -> do putStrLn "\nNiestety, prowadzący nakrył Cię na ściąganiu! Z wejściówki dostajesz okrągłe zero! I tyle z Twojej nauki!"
                            return (-5)
                "NIE" -> do putStrLn "\nNo nic, zostajesz z tym, co pamiętałeś."
                            return 0
                _ -> do putStrLn "\nMożliwe opcje do wyboru: TAK, NIE"
                        questLab9

lab :: Place
lab = Place 4
            "\nWitaj przed salą laboratoryjną! Właśnie tutaj odbywają się laboratoria z Technik Sygnałów i Informacji.\nPODEJMUJESZ WYZWANIE? A może wracasz na wydział albo na kolokwium?"
            "PODEJMUJĘ WYZWANIE"
            labQuest
            [("WRACAM NA WYDZIAŁ", faculty), ("WRACAM NA KOLOKWIUM", test)]
            []


start :: Place
start = dorm

help :: Action
help = "HELP"

quit :: Action
quit = "QUIT"

badInput :: String
badInput = "Bad input."

availableCommands :: [Action] -> String
availableCommands cmds = "Available commands: " ++ (prettyStrings cmds)
