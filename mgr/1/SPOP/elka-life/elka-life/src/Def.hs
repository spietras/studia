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
                      then do putStrLn "Niestety, nie możesz tylko chodzić na imprezy. Życie studenta to nie tylko przyjemności!"
                              return Nothing
                      else do score <- dormQuest2
                              putLnStrLnLn "Właśnie ukończyłeś imprezę!"
                              putStr "Uzyskany wynik: "
                              print score
                              return (Just score)

dormQuest2 :: IO Int
dormQuest2 = do putStrLn "Zanim pójdziesz, trzeba wybrać jakieś ubranie..."
                putStrLn "Z tego co wiesz, impreza miała być w stylu latino."
                putStrLnLn "Ale czy chce ci się bawić w przebieranki?"
                input <- getLine
                putLn
                case input of
                    "ZAŁÓŻ HAWAJSKĄ KOSZULĘ" -> do putStrLnLn "WOW! Ta koszula jest serio cool!\n\
                                                              \Kompletujesz resztę stroju - nakładasz mokasyny, lniane spodnie i kwiecisty naszyjnik."
                                                   dormQuest3 True
                    "ZOSTAŃ W BLUZIE" -> do putStrLnLn "Kompletujesz resztę stroju, ale czujesz że nie pasuje do klimatu imprezy. Trudno."
                                            dormQuest3 False
                    _ -> do putStrLnLn "Możliwe opcje do wyboru: ZAŁÓŻ HAWAJSKĄ KOSZULĘ, ZOSTAŃ W BLUZIE"
                            dormQuest2

dormQuest3 :: Bool -> IO Int
dormQuest3 gringo = do putStrLn "Przebieranki zajęły Ci troche czasu! Do imprezy zostało naprawdę niewiele czasu..."
                       putStrLn "Wybiegasz z pokoju i zdzwaniasz się ze znajomymi z kierunku."
                       putStrLn "Są już w kolejce, więc obiecujesz zaraz do nich dołączyć."
                       putStrLn "Na szczęście do Remontu nie masz daleko i już po chwili widzisz znajomych."
                       putStrLn "Po szybkim przywitaniu częstują Ciebie butelką z cieczą niewiadmoego pochodzenia."
                       putStrLnLn "Co robisz?"
                       input <- getLine
                       putLn
                       case input of
                           "PIJ" -> dormQuest4 gringo True
                           "ODMÓW" -> dormQuest4 gringo False
                           _ -> do putStrLnLn "Możliwe opcje do wyboru: PIJ, ODMÓW"
                                   dormQuest3 gringo

dormQuest4 :: Bool -> Bool -> IO Int
dormQuest4 gringo bootle = do case bootle of
                                  True -> do putStrLn "Brrrr! Co to było.. Jeśli w piekle jest woda to smakuje właśnie tak."
                                             putStrLn "Czujesz że wypity płyn szybko zaczyna działać i wprowadza Ciebie w zabawny nastój."
                                  _ ->       putStrLn "Uff! Sądząc po reakcji znajomych chyba dobrze że odpuściłeś - napój wydaje się być bardzo mocny."
                              putStrLn "Kolejka przed klubem topnieje i kolejni studenci znikają klubie."
                              putStrLnLn "Już za chwilę wasza kolej! Bramkarz uważnie Ci się przygląda."
                              case gringo of
                                  True -> do putStrLn "A po chwili dodaje *Kozacki strój - w takim wchodzisz dziś za darmo*"
                                             putStrLn "Nieźle! Warto było zaszaleć - zawsze 20zł w kieszeni."
                                             dormQuest5 bootle
                                  False -> do putStrLn "A po chwili mówi Ci że w takim stroju nie wejdziesz do klubu."
                                              putStrLn "Protesty znajomych na nic się zdają i zmuszony jesteś wrócić do domu."
                                              putStrLn "Tracisz ochotę na zabawę i decydujesz się wrócić do domu - szkoda :("
                                              return 5


dormQuest5 :: Bool-> IO Int
dormQuest5 bootle = do putStrLn "Brawo! W końcu jesteś w klubie"
                       putStrLn "Atmosfera jest na prawdę cudowna - muzyka, piękne dziewczny i duży asortyment baru."
                       putStrLn "Właśnie ten ostatni aspekt zainteresował twojego kolegę, który proponuję eksplorację baru."
                       putStrLnLn "Co robisz?"
                       input <- getLine
                       putLn
                       case input of
                           "ZGÓDŹ SIĘ" -> dormQuest6 bootle True
                           "ODMÓW" -> dormQuest7
                           _ -> do putStrLnLn "Możliwe opcje do wyboru: ZGÓDŹ SIĘ, ODMÓW"
                                   dormQuest5 bootle

dormQuest6 :: Bool -> Bool-> IO Int
dormQuest6 bootle more = do putStrLn "Na barze są świetne promocje. Kumpel z łatwością namawia Ciebie na zestaw 3 + 3."
                            putStrLn "Napoje są idealnie schłodzone i dobrze smakują."
                            putStrLn "Ze smakiem (i prędkością bliską prędkości światła) opróżniasz napoje."
                            case bootle of
                                True -> do putStrLn "Po chwili czujesz jak świat zaczyna wirować."
                                           putStrLn "Uff... chyba picie przed klubem nie było dobrym pomysłem."
                                           putStrLnLn "Znajomi pomagają Ci wrócić do pokoju - trochę głupio co!?"
                                           putStrLn "Nastepnym razem uważaj i nie mieszaj alkoholi!"
                                           return 0
                                False -> do putStrLn "A po chwili czujesz że płyny zaczynaja działać."
                                            putStrLn "Bardzo dobre - ale chyba już wystarczy."
                                            putStrLn "Pora rozejrzeć się po klubie."
                                            dormQuest7


dormQuest7 :: IO Int
dormQuest7 = do putStrLn "Czas na imprezie leci w zawrotnym tempie - nawet nie wiesz kiedy minął ten czas."
                putStrLn "Powoli zaczynasz myśleć o powrocie, ale chciałbyś jeszcze trochę potańczyć."
                putStrLnLn "Co robisz?"
                input <- getLine
                putLn
                case input of
                    "TAŃCZ SAM" -> dormQuest8 True
                    "POPROŚ DO TAŃCA KOLEŻANKĘ" -> dormQuest8 False
                    _ -> do putStrLnLn "Możliwe opcje do wyboru: TAŃCZ SAM, POPROŚ DO TAŃCA KOLEŻANKĘ"
                            dormQuest7


dormQuest8 :: Bool-> IO Int
dormQuest8 alone = do case alone of
                           True -> do putStrLn "Ehh tańczenie samemu jest fajne, ale szybko się nudzi."
                                      putStrLn "Zresztą zdecydowanie lepiej idzie Ci przed lustrem."
                                      putStrLn "Chyba pora już lecieć!"
                                      return 20
                           False -> do putStrLn "Koleżanka od razu zgodziła się na taniec - chyba wpadłeś jej w oko."
                                       putStrLn "Nic dziwnego - studenci EiTI to nielada przystajniaki."
                                       putStrLn "Tańczy się wam naprawdę dobrze, a uśmiech nie schodzi z waszych twarzy."
                                       putStrLn "Może to nie pierwszy raz kiedy tak tańczycie? Kto wie..."
                                       putStrLn "Dzisiaj niestety musisz już lecieć, ale nie wychodzisz z pustymi rękami..."
                                       putStrLn "W ręku masz kartkę z jej numerem - trochę oldschoolowe ale cóż - nie tylko twoje baterie są już rozładowane."
                                       return 40

dorm :: Place
dorm = Place 0
             "Jesteś w swoim pokoju w akademiku. Gdzieś w oddali słyszysz imprezujących studentów.\n\
             \W jednym kącie pokoju znajduje się łóżko. Niezbyt wygodne, ale swoje.\n\
             \W drugim kącie pokoju stoi twoje biurko. Masz na nim komputer, a obok butelkę piwa."
             "IDŹ NA IMPREZĘ"
             dormQuest
             [("IDŹ NA WYDZIAŁ", faculty), ("USIĄDŹ PRZY BIURKU", project)]
             [("ŚPIJ", "Udało ci się zasnąć. Nie wiesz, który jest dzień ani która godzina, ale czujesz się wyspany."),
              ("PIJ PIWO", "Ahhhh... Chęć do życia do ciebie wróciła.")]

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
                     then do putStrLn "Niestety, tego kolokwium nie da się poprawić!"
                             return Nothing
                     else do score <- testQuest2
                             putLnStrLnLn "Właśnie ukończyłeś moduł kolokwium!"
                             putStr "Uzyskany wynik: "
                             print score
                             return (Just score)

testQuest2 :: IO Int 
testQuest2 = do putStrLn "Do kolokwium zostało jeszcze trochę czasu."
                putLnStrLn "Możesz się pouczyć albo się zrelaksować."
                input <- getLine
                putLn
                case input of
                         "UCZ SIĘ" -> testQuest3 True
                         "ODPOCZNIJ" -> testQuest3 False
                         _ -> do putStrLnLn "Możliwe opcje do wyboru: UCZ SIĘ, ODPOCZNIJ"
                                 testQuest2


testQuest3 :: Bool -> IO Int
testQuest3 learn = do putStrLn "Punktualnie o 12:15 dostrzegasz w oddali po charakterystycznym kroku swoją ulubioną prowadzącą."
                      putStrLn "Trzyma ona w ręku kartki - od których może zależeć twoja przyszłość, a także klucz do sali."
                      putStrLn "Drzwi się otwierają, a ty stajesz przed arcytrudnym mornalnym dylematem..."
                      putStrLnLn "Czy startegiczne miejsce w sali jest ceniejsze niż wierność zasadom savoir vivre?"
                      input <- getLine
                      putLn
                      case input of
                               "PRZEPUŚĆ INNYCH" -> do putStrLnLn "Brawo gentlemanie! Chociaż miejsce w pierwszej ławce nie jest twoim wymarzonym..."
                                                       testQuest4 learn False
                               "WEPCHNIJ SIĘ" -> do putStrLnLn "Ahh mama pewnie nie byłaby dumna, ale ukryte miejsce za filarem przy oknie było tego warte!"
                                                    testQuest4 learn True
                               _ -> do putStrLn "Możliwe opcje do wyboru: PRZEPUŚĆ INNYCH, WEPCHNIJ SIĘ"
                                       testQuest3 learn

testQuest4 :: Bool -> Bool -> IO Int
testQuest4 learn place = do putStrLn "Teraz jedynie pozostało czekać na kartkę z zadaniami."
                            putStrLn "Zasady kolokwium są standardowe - dwa zestawy zadań skłądające się z trzech pytań - 10 punktów za każde."
                            putStrLn "Pani rozdaje kartki z zadanami, a ty niecierpilwie rozglądasz się po sali."
                            putStrLn "Wyostrzając wzrok, dostrzegsz zadania na kartkach osób siedzących przed tobą."
                            putStrLn "Grupa A wydaje się zdecydowanie łatwiejsza, jednak w twoich rękach ląduje wersja oznaczona literką B. Cóż, pech..."
                            putStrLn "Dostrzegasz jednak szansę na podmianę grup z sąsiadem."
                            putStrLnLn "Co robisz?"
                            input <- getLine
                            putLn
                            case input of
                                     "ZMIEŃ GRUPĘ" -> do putStrLnLn "Ahh próba zamiany grup to nie był dobry pomysł. Sąsiadowi też bardziej podobała się grupa B.\n\
                                                                    \Zrobiłeś tylko niepotrzebne zamieszanie czym zwróciłeś uwagę prowadzącej."
                                                         testQuest5 learn place True
                                     "ZOSTAW GRUPĘ" -> do putStrLnLn "Nie tylko Ci przemknęła przez myśl próba zamiany grupy.\n\
                                                                     \Twoi znajomi siedzący po drugiej stronie sali również próbowali tego manewru.\n\
                                                                     \Ich próba zakończyła się jendak fiaskiem, a prowadząca wydaje się od tego zdarzenia patrzeć im na ręce."
                                                          testQuest5 learn place False
                                     _ -> do putStrLn "Możliwe opcje do wyboru: ZMIEŃ GRUPĘ, ZOSTAW GRUPĘ"
                                             testQuest4 learn place

testQuest5 :: Bool -> Bool -> Bool -> IO Int
testQuest5 learn place group = do putStrLnLn "Teraz jedyne na co możesz liczyć to na siebie i swoją wiedzę, albo..."
                                  input <- getLine
                                  putLn
                                  case input of
                                           "ŚCIĄGAJ" -> testQuest6_1 learn place group
                                           "PISZ UCZCIWIE" -> testQuest6_2 learn
                                           _ -> do putStrLn "Możliwe opcje do wyboru: ŚCIĄGAJ, PISZ UCZCIWIE"
                                                   testQuest5 learn place group

testQuest6_1 :: Bool -> Bool -> Bool -> IO Int
testQuest6_1 learn place group = do if group
                                        then do putStrLn "Ściąganie po nieudanej próbie zmiany grup to nie był dobry pomysł."
                                                putStrLn "Prowadząca ceremonialnie wyprasza Cię z sali i na kolokwium zaznacza okrąglutkie 0..."
                                                return 0
                                        else if learn
                                                    then do putStrLn "Rozglądasz się po kolegach, jednak wydaje Ci się że rozwiązują zadanie źle.."
                                                            putStrLn "Decydujesz się polegać na swojej wiedzy - i słusznie. Nauka nie poszła w las."
                                                            putStrLn "Gdybyś nie tracił czasu na rozglądanie się byłoby jeszcz lepiej, ale i tak uzyskujesz bardzo dobry wynik."
                                                            return 25
                                                    else if place
                                                            then do putStrLn "Zamiast odpoczywać mogłeś się pouczyć, albo chociaż przygotować jakieś sciągi..."
                                                                    putStrLn "Dobrze, że chociaż zająłeś dobre miejsce."
                                                                    putStrLn "Rozglądasz się po sąsiadach i wykorzystując okazję przepisujesz kawałki zadań."
                                                                    putStrLn "Niestety wszyscy piszą niewyraźnie i z trudem udaje Ci się przepisać zadania."
                                                                    putStrLn "Koledzy chyba średnio przyłożyli się do nauki."
                                                                    putStrLn "Mogłeś sam spróbować rozwiązać zadanie, a tak... no cóż - bywało znacznie lepiej."
                                                                    return 10
                                                            else do putStrLn "Jednak mogłeś lepiej spożytkować czas i powtórzyć materiał przed kolokwium."
                                                                    putStrLn "Miejsce w pierwszym rzędzie również nie sprzyja ściąganiu. Tym razem musisz polegać na sobie."
                                                                    putStrLn "Twoje dobre maniery wprawiają Cię w dobre samopoczucie, przez co przypominasz sobie część materiału."
                                                                    putStrLn "Udaje Ci się przepołowić kolosa. Brawo!"
                                                                    return 16

testQuest6_2 :: Bool -> IO Int
testQuest6_2 learn = do putStrLn "Ahh wybór uczciwej ścieżki zasługuje na szacunek."
                        if learn
                            then do putStrLn "Brawo! Udało się! Ciężka praca popłaca! W uczciwy sposób zaliczasz kolokwium ze świetnym wynikiem."
                                    putStrLn "Mimo początkowych problemów rozwiązujesz wszystkie zadania i uzyskujesz świetny wynik."
                                    return 29
                            else do putStrLn "Jednak mogłeś lepiej spożytkować czas i powtórzyć materiał przed kolokwium."
                                    putStrLn "Mimo wszystko udaje Ci się przepołowić kolosa. Brawo!"
                                    return 16


test :: Place
test = Place 3
             "Podchodzisz pod salę wykładową. Grupka studentów dziwnie na ciebie patrzy.\n\
             \Może dzisiaj jest kolokwium?"
             "NAPISZ KOLOKWIUM"
             testQuest
             [("WRÓĆ NA KANAPĘ", faculty), ("IDŹ NA LABOLATORIUM", lab)]
             [("POROZMAWIAJ ZE STUDENTAMI", "Próbujesz porozmawiać z grupą studentów. Nic nie rozumiesz, więc albo są z innego kierunku albo nic nie nauczyłeś się na kolokwium.")]

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
