{-# LANGUAGE BlockArguments #-}
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

-- testQuest :: Quest
-- testQuest state = if (questCompleted state 3)
--                       then do putStrLn "Quest already completed."
--                               return Nothing
--                       else do putStrLn "testQuest"
--                               return (Just 10)

-- test :: Place
-- test = Place 3
--              "Test"
--              "quest"
--              testQuest
--              [("F", faculty), ("L", lab)]
--              []


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


-- PARTY START
partyQuest :: Quest
partyQuest state = if (questCompleted state 4)
                     then do putStrLn "Niestety, nie możesz tylko chodzić na imprezy. Życie studenta to nie tylko przyjemności!"
                             return Nothing
                     else do score <- labQuest2
                             putStrLnLn "Właśnie ukończyłeś imprezę!"
                             putStr "Uzyskany wynik: "
                             print score
                             return (Just score)

partyQuest2 :: IO Int
partyQuest2 = do putStrLn "Witaj, dzisiaj długo wyczekiwany piątek! Czekałeś na ten dzień cąły dłuuugi tydzień - już dzisiaj latynoska impreza w Remoncie"
                 putStrLn "Po całym tygodniu zajęć studentowi też należy się coś od życia"
                 putStrLn "Pamiętaj tylko aby bawić się rozważnie i nie przesadzać z alkoholem!"
                 putStrLn "No to coż.. zaczynamy..."
                 putStrLnLn "No.. chyba że nie chcesz - imprezy w przeciwieństwie do kolokwiów nie są obowiązkowe"
                 input <- getLine
                 putLn
                 score <- case input of
                      "LECĘ NA IMPREZĘ" -> return 10
                      "ZOSTAJĘ POUCZYĆ SIĘ NA KOLOKWIUM" -> partyQuest3
                      _ -> do putStrLnLn "Możliwe opcje do wyboru: LECĘ NA IMPREZĘ, ZOSTAJĘ POUCZYĆ SIĘ NA KOLOKWIUM"
                              labQuest2
                 return score


partyQuest3 = do putStrLn "No dobrze, do imprezy już tylko godzina, pora wybrać jakieś ubranie..."
                 putStrLnLn "Impreza ma być w stylu latino - ale czy chce mi się bawić w przebieranki?"
                 input <- getLine
                 putLn
                 score <- case input of
                       "TAK ZAKŁADAM HAWAJSKĄ KOSZULE" -> partyQuest4_1
                       "ZOSTAJE W BLUZIE" -> partyQuest4_2
                       _ -> do putStrLnLn "Możliwe opcje do wyboru: TAK ZAKŁADAM HAWAJSKĄ KOSZULE, ZOSTAJĘ W BLUZIE"
                               partyQuest3
                 return score

partyQuest4_1 :: IO Int
partyQuest4_1 = do putStrLn "WOW! Ta koszula jest serio cool!"
                   putStrLn "Kompletujesz resztę stroju - nakładasz mokasyny, lniane spodnie i kwiecisty naszyjnik"
                   putStrLn "Wyglądasz jak prawdziwy gringo! Czy jesteś gotowy na imprezę !?"
                   input <- getLine
                   putLn
                   score <- case input of
                           "TAK" -> partyQuest5 True
                           "JESZCZE JAK" -> partyQuest5 True
                           _ -> do putStrLnLn "Możliwe opcje do wyboru: TAK, JESZCZE JAK"
                                   partyQuest4_1
                   return score

partyQuest4_2 :: IO Int
partyQuest4_2 = do putStrLn "Kompletujesz resztę stroju, ale czujesz żenie pasuje do klimatu imprezy"
                   putStrLnLn "Co robisz?"
                   input <- getLine
                   putLn
                   score <- case input of
                           "NAKŁADAM KLIMATYCZNE UBRANIE" -> partyQuest5 True
                           "JEDNAK ZOSTAJE W BLUZIE" -> partyQuest5 False
                           _ -> do putStrLnLn "Możliwe opcje do wyboru: NAKŁADAM KLIMATYCZNE UBRANIE, JEDNAK ZOSTAJE W BLUZIE"
                                   partyQuest4_2
                   return score

partyQuest5 :: Bool -> IO Int
partyQuest5 gringo = do putStrLn "Przebieranki zajęły Ci troche czasu! Do imprezy zostało naprawdę niewiele czasu..."
                        putStrLn "Wybiegasz z pokoju i zdzwaniasz się ze znajomymi z kierunku."
                        putStrLn "Są już w kolejce, więc obiecujesz zaraz do nich dołączyć."
                        putStrLn "Na szczęście do Remontu nie masz daleko i już po chwili widzisz znajomych"
                        putStrLn "Po szybkim przywitaniu częstują Ciebie butelką z cieczą niewiadmoego pochodzenia"
                        putStrLnLn "Co robisz?"
                        input <- getLine
                        putLn
                        score <- case input of
                           "TO PEWNIE COLA - PIJĘ" -> partyQuest6 gringo True
                           "ODMAWIAM" -> partyQuest6 gringo False
                           _ -> do putStrLnLn "Możliwe opcje do wyboru: TO PEWNIE COLA - PIJĘ, ODMAWIAM"
                                   partyQuest5 gringo
                        return score


partyQuest6 :: Bool -> Bool -> IO Int
partyQuest6 gringo bootle = do case bootle of
                                    True -> putStrLn "Brrrr! Co to było.. Jeśli w piekle jest woda to smakuje właśnie tak"
                                            putStrLn "Czujesz że wypity płyn szybko zaczyna działać i wprowadza Ciebie w zabawny nastój"
                                    _ ->    putStrLn "Uff! Sądząc po reakcji znajomych chyba dobrze że odpuściłeś - napój wydaje się być bardzo mocny"
                               putStrLn "Kolejka przed klubem topnieje i kolejni studenci znikają klubie"
                               putStrLnLn "Już za chwilę wasza kolej! Bramkarz uważnie ogląda Ci się przygląda"
                               score <- do case gringo of
                                             True -> putStrLn "A po chwili dodaje *Kozaki strój - w takim wchodzisz dziś za darmo*"
                                                     putStrLn "Nieźle! Warto było zaszaleć - zawsze 20zł w kieszeni"
                                                     return partyQuest7 bootle
                                             False -> putStrLn "A po chwili mówi Ci że w takim stroju nie wejdziesz do klubu"
                                                      putStrLn "Protesty znajomych na nic się zdają i zmuszony jesteś wrócić do domu"
                                                      putStrLn "Tracisz ochotę na zabawę i decydujesz się wrócić do domu - szkoda :("
                                                      return 5

                               return score


partyQuest7 :: Bool-> IO Int
partyQuest7 bootle = do putStrLn "Brawo! W końcu jesteś w klubie"
                        putStrLn "Atmosfera jest na prawdę cudowna - muzyka, piękne dziewczny i duży asortyment baru"
                        putStrLn "Właśnie ten ostatni askept zainteresował twojego kolegę, który proponuję eksploarację baru"
                        putStrLnLn "Co robisz?"
                        input <- getLine
                        putLn
                        score <- case input of
                            "ZGADZAM SIĘ" -> partyQuest8 bootle True
                            "ODMAWIAM" -> partyQuest9
                            _ -> do putStrLnLn "Możliwe opcje do wyboru: ZGADZAM SIĘ, ODMAWIAM"
                                    partyQuest7 bootle
                        return score

partyQuest8 :: Bool -> Bool-> IO Int
partyQuest8 bootle more = do putStrLn "Na barze są świetne promocje. Kumpel z łatwością namawia Ciebie na zestaw 3 + 3"
                             putStrLn "Napoje są idealnie schłodzone i dobrze smakują"
                             putStrLn "Ze smakiem (i prędkością bliską prędkości światła) opróżniasz napoje"
                             score <- do case bootle of
                                             True -> putStrLn "Po chwili czujesz jak świat zaczyna wirować"
                                                     putStrLn "Uff.. chyba picie przed klubem nie było dobrym pomysłem"
                                                     putStrLnLn "Znajomi pomagają Ci wrócić do pokoju - trochę głupio co!?"
                                                     putStrLnLn "Nastepnym razem uważaj i nie mieszaj alkoholi!"
                                                     return 0
                                             False -> putStrLn "A po chwili czujesz że płyny zaczynaja działać"
                                                      putStrLn "Bardzo dobre - ale chyba już wystarczy"
                                                      putStrLn "Pora rozejrzeć się po klubie"
                                                      return partyQuest9

                             return score


partyQuest9 :: IO Int
partyQuest9 = do putStrLn "Czas na imprezie leci w zawrotnym tempie - nawet nie wiesz kiedy minął ten czas"
                 putStrLn "Powoli zaczynasz myśleć o powrocie, ale chciałbyś jeszcze trochę potańczyć"
                 putStrLnLn "Co robisz?"
                 input <- getLine
                 putLn
                 score <- case input of
                             "TAŃCZĘ SAM" -> partyQuest10 True
                             "PROSZĘ DO TAŃCA KOLEŻANKĘ" -> partyQuest10 False
                             _ -> do putStrLnLn "Możliwe opcje do wyboru: TAŃCZĘ SAM, PROSZĘ DO TAŃCA KOLEŻANKĘ"
                                     partyQuest9
                 return score


partyQuest10 :: Bool-> IO Int
partyQuest10 alone = do score <- do case alone of
                                             True -> putStrLn "Ehh tańczenie samemu jest fajne, ale szybko się nudzi"
                                                     putStrLn "Zresztą zdecydowanie lepiej idzie Ci przed lustrem"
                                                     putStrLn "Chyba pora już lecieć!"
                                                     return 20
                                             False -> putStrLn "Koleżanka od razu zgodziła się na taniec - chyba wpadłeś jej w oko"
                                                      putStrLn "Nic dziwnego - studenci EiTI to nielada przystajniaki"
                                                      putStrLn "Tańczy się wam naprawdę dobrze, a uśmiech nie schodzi z waszych twarzy."
                                                      putStrLn "Może to nie pierwszy raz kiedy tak tańczycie? Kto wie..."
                                                      putStrLn "Dzisiaj niestety musisz już lecieć, ale nie wychodzisz z pustymi rękami.."
                                                      putStrLnLn "W ręku masz kartkę z jej numerem - trochę oldschoolowe ale cóż - nie tylko twoje baterie są już rozładowane"
                                                      return 40

                        return score

party :: Place
party = Place 4
            "Witaj przed wejściem do Remontu! Właśnie tutaj odbywają się piątkowe imprezy dla studentów Politechniki.\nPODJĄĆ WYZWANIE? A może wrócić na wydział albo na kolokwium?"
            "PODEJMIJ WYZWANIE"
            partyQuest
            [("WRACAJ DO AKADEMIKA",dorm)]
            []

-- PARTY START

-- TEST START
testQuest :: Quest
testQuest state = if (questCompleted state 3)
                     then do putStrLn "Niestety, tego kolokwium nie da się poprawić!"
                             return Nothing
                     else do score <- testQuest2
                             putStrLnLn "Właśnie ukończyłeś moduł kolokwium!"
                             putStr "Uzyskany wynik: "
                             print score
                             return (Just score)

testQuest2 :: IO Int
testQuest2 = do putStrLn "Oo nie! To już 10:00.. A dzisiaj sądny dzień - czas na kolokwium z Analizy Matematycznej 2"
                putStrLn "Na szczęście kolokwium jest o 12:15, więc masz jeszcze ponad 2 godziny"
                putStrLn "Co robisz?"
                input <- getLine
                putLn
                score <- case input of
                 "WSTAJĘ" -> testQuest3
                 "ŚPIĘ DALEJ" -> testQuest4 True False False
                 _ -> do putStrLn "Możliwe opcje do wyboru: WSTAJĘ, ŚPIĘ DALEJ"
                      testQuest2
                return score

testQuest3 :: IO Int
testQuest3 =  do putStrLn "Odważna decyzja - wstanie rano nie należy do łatwych, szczególnie gdy oddczuwasz jeszcze skutki weekendowych szaleństw."
                 putStrLn "Bierzesz do ręki zeszyt z ćwiczeń i zaczynasz analizowac poszczególne zadania"
                 putStrLn "Uświadamiasz sobie, że sporo już zapomniałeś"
                 putStrLn "Czy napewno zdążę powtórzyć cały materiał w 2 godziny? Może powinienem zrobić ściągawkę?"
                 input <- getLine
                 putLn
                 score <- case input of
                           "ROBIĘ ŚCIĄGAWKĘ" -> testQuest4 False False True
                           "UCZĘ SIĘ" -> testQuest4 False True False
                           _ -> do putStrLn "Możliwe opcje do wyboru: ROBIĘ ŚCIĄGAWKĘ, UCZĘ SIĘ"
                                   testQuest3
                 return score

testQuest4 :: Bool -> Bool -> Bool -> IO Int
testQuest4 sleep learn cheats = do case sleep of
                                     True -> putStrLn "Brrrr! Budzik przerywa Ci piękny sen..."
                                             putStrLn "Śniło Ci się że zamiast kolokwium Pani zabrała grupę na lody i wszystkim wpisała maksa."
                                             putStrLn "Niestety rzeczywistość jest zgoła inna - jeśli nie chcesz sie spóźnić to za 15min musisz być na wydziale"
                                     _ -> putStrLn "Już 12:00. Dobrze że spojrzałeś na zegarek bo pochłonięty pracą straciłeś poczucie czasu."
                                      putStrLn "No dobrze, ale teraz trzeba zadać sobie kluczowe pytanie..."
                                      putStrLn "Czy ja naprawdę chcę przystąpić do tego kolokwium?"
                                     input <- getLine
                                     putLn
                                     score <- case input of
                                      "WYCHODZĘ NA KOLOKWIUM" -> testQuest5 learn cheats
                                      "REZYGNUJĘ Z UDZIAŁU NA KOLOKWIUM" -> return 0
                                      _ -> do putStrLn "Możliwe opcje do wyboru: WYCHODZĘ NA KOLOKWIUM, REZYGNUJĘ Z UDZIAŁU NA KOLOKWIUM"
                                       testQuest4
                                     return score

testQuest5 :: Bool -> Bool -> IO Int
testQuest5 learn cheats = do  putStrLn "Ahh do odważnych świat należy. Szybko pakujesz plecak i idziesz w paszczę lwa..."
                              putStrLn "Do kolokwium pozostało tylko 10min, ale ty z oddali widzisz już wydział"
                              putStrLn "Po chwili twoim oczom ukazuje się miejsce egzek.. kolokwium, gdzie spoktykasz znajomych"
                              putStrLn "Punktualnie o 12:15 dostrzegasz w oddali po charakterystycznym kroku swoją ulubioną prowadzącą"
                              putStrLn "Trzyma ona w ręku kartki - od których może zależeć twoja przyszłość, a także klucz do sali"
                              putStrLn "Drzwi się otwierają, a ty stajesz przed arcytrudnym mornalnym dylematem..."
                              putStrLn "Czy startegiczne miejsce w sali jest ceniejsze niż wierność zasadom savoir vivre?"
                              input <- getLine
                              putLn
                              score <- case input of
                               "PRZEPUSZCZAM DZIEWCZYNY" -> testQuest6 learn cheats False
                               "PIERWSZY WCHODZĘ DO SALI" -> testQuest6 learn cheats True
                               _ -> do putStrLn "Możliwe opcje do wyboru: PRZEPUSZCZAM DZIEWCZYNY, PIERWSZY WCHODZĘ DO SALI"
                                testQuest5
                              return score


testQuest6 :: Bool -> Bool -> Bool -> IO Int
testQuest6 learn cheats place = do case place of
                                    True -> putStrLn "Ahh mama pewnie nie byłaby dumna, ale ukryte miejsce za filarem przy oknie było tego warte!"
                                    _ ->    putStrLn "Brawo gentelamnie! Chociaż miejsce w pierwszej ławce nie jest twoim wymarzonym..."
                                   putStrLn "Teraz jedynie pozostało czekać na kartkę z zadaniami."
                                   putStrLn "Zasady kolokwium są standardowe- dwa zestawy zadań skłądające się z trzech pytania - 10 punktów za każde"
                                   putStrLn "Pani rozdaje kartki z zadanami, a ty niecierpilwie rozglądasz się po sali"
                                   putStrLn "Wyostrzając wzrok dostrzegsz zadania na kartkach osób siedzących przed tobą"
                                   putStrLn "Grupa A wydaje się zdecydowanie łatwiejsza, jednak w twoich rękasz ląduje wersja oznaczona literką B. Cóż pech..."
                                   putStrLn "Dostrzegasz jednak szansę na podmiankę grup z sąsiadem"
                                   putStrLn "Co robisz?"
                                   input <- getLine
                                   putLn
                                   score <- case input of
                                       "PRÓBUJĘ ZMIENIĆ GRUPĘ" -> testQuest7 learn cheats place True
                                       "ZOSTAWIAM GRUPĘ A" -> testQuest7 learn cheats place False
                                       _ -> do putStrLn "Możliwe opcje do wyboru: PRÓBUJĘ ZMIENIĆ GRUPĘ, ZOSTAWIAM GRUPĘ A"
                                    testQuest4
                                   return score

testQuest7 :: Bool -> Bool -> Bool -> Bool -> IO Int
testQuest7 learn cheats place group = do case group of
                                          True -> putStrLn "Ahh próba zamiany grup to nie był dobry pomysł. Sąsiadowi też bardziej podobała się grupa B"
                                                  putStrLn "Zrobiłeś tylko niepotrzebne zamieszanie czym zwróciłeś uwagę prowadzącej."
                                          _ ->    putStrLn "Nie tylko Ci przemknęła przez myśl próba zamiany grupy."
                                                  putStrLn "Twoi znajomi siedzący po drugiej stronie sali również próbwali tego manewru."
                                                  putStrLn "Ich próba zakończyła się jendak fiaskiem, a prowadząca wydaje się od tego zdarzenia patrzeć im na ręce"
                                         putStrLn "Teraz jedyne na co możesz liczyć to na siebiei swoją wiedzę, albo..."
                                         input <- getLine
                                         putLn
                                         score <- case input of
                                             "PRÓBUJĘ ŚCIĄGAĆ" -> testQuest8_1 learn cheats place group
                                             "PISZĘ UCZCIWIE" -> testQuest8_2 learn
                                             _ -> do putStrLn "Możliwe opcje do wyboru: PRÓBUJĘ ŚCIĄGAĆ, PISZĘ UCZCIWIE"
                                          testQuest4
                                         return score

testQuest8_1 :: Bool -> Bool -> Bool -> Bool -> IO Int
testQuest8_1 learn cheats place group = do score <- 0
                                           if group
                                           then do
                                           putStrLn "Ściąganie po nieudanej próbie zmiany grup to nie był dobry pomysł"
                                              putStrLn "Prowadząca ceremonialnie wyprasza Cię z sali i na kolokwium zaznacza okrąglutkie 0..."
                                              return 0
                                           else if cheats
                                           then do
                                           putStrLn "Prowadząca nie zwraca uwagi na Ciebie uwagi i cały czas obserwuje kolegów którzy probówali zamienić się grupami"
                                           putStrLn "To twoja szansa."
                                           putStrLn "Wykorzytsujesz przygotowane rano ściągawki i uzyskujesz dobry wynik z kolokwium"
                                           putStrLn "Brawo spryciarzu! Nastepnym razem los może nie okazać się tak przychylny."
                                           return 23
                                           else if learn
                                             then do
                                              putStrLn "Rozglądasz się po kolegach, jednak wydaje Ci się że rozwiązują zadanie źle.."
                                              putStrLn "Decydujesz się polegać na swojej wiedzy - i słusznie. Poranna nauka nie poszła w las"
                                              putStrLn "Gdybyś nie tracił czasu na rozglądanie się byłoby jeszcz lepiej, ale i tak uzyskujesz bardzo dobry wynik"
                                              return 25
                                            else if place
                                             then do
                                           putStrLn "Rano zamiast spać mogłeś się pouczyć, albo chociaż przygotować jakieś sciągi..."
                                           putStrLn "Dobrze, że chociaż zająłeś dobre miejsce"
                                           putStrLn "Rozglądasz się po sąsiadach i wykorzystując okazję przepisujesz kawałki zadań"
                                           putStrLn "Niestety wszyscy piszą niewyraźnie i z trudem udaje Ci się przepisać zadania"
                                           putStrLn "Koledzy chyba średnio przyłożyli się do nauki."
                                           putStrLn "Mogłeś sam spróbować rozwiązać zadanie, a tak.. no cóż - bywało znacznie lepiej"
                                           return 10
                                           else
                                             putStrLn "Jednak rano mogłeś lepiej spożytkowac czas i powtórzyć materiał przed kolokwium"
                                             putStrLn "Mijce w pierwszym rzędzie również nie sprzyja ściąganiu. Tym razem musisz polegać na sobie."
                                             putStrLn "Uff chyba nie było aż tak źle. Obecność na ćwiczeniach popłaciła"
                                             putStrLn "Udaje Ci się przepołowić kolosa. Brawo!"
                                             return 16

                                           return score

testQuest8_2 :: Bool -> IO Int
testQuest8_2 learn = do putStrLn "Ahh wybór uczciwej ścieżki zasługuje na szacunek"
                        score <- 16
                        if learn
                         then do
                          putStrLn "Brawo! Udało się! Ciężka praca popłaca! W uczciwy sposób zaliczasz kolokwium ze świetnym wynikiem"
                          putStrLn "Mimo początkowych problemów rozwiązujesz wszystkie zadania i uzyskujesz świetny wynik"
                          return score + 13
                         else
                           putStrLn "Jednak rano mogłeś lepiej spożytkowac czas i powtórzyć materiał przed kolokwium"
                           putStrLn "Mimo wszystko udaje Ci się przepołowić kolosa. Brawo!"

                        return score


test :: Place
test = Place 3
            "Witaj przed salą laboratoryjną! Właśnie tutaj odbywają się laboratoria z Technik Sygnałów i Informacji.\nPODJĄĆ WYZWANIE? A może wrócić na wydział albo na kolokwium?"
            "PODEJMIJ WYZWANIE"
            labQuest
            [("WRACAJ NA WYDZIAŁ", faculty), ("WRACAJ NA LABOLATORIUM", lab)]
            []

-- TEST END
