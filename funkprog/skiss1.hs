sirSimulate :: (Num a, Num b, Num c, Num d, (Num x, Num y, Num z), [(Num x, Num y, Num z)]) => a -> b -> c -> d -> (x, y, z) -> [(x, y, z)]
# Första ekvationen räknar antalet mottagliga individer vid tidpunkten t+1.
# Andra ekvationen räknar antalet smittade vid tidpunkt t+1
# tredje ekvationen räknar antalet återhämtade vid tidpunkt t+1.
# De vi vill beräkna är antalet mottagliga, smittade, återhämtade vid varje tidssteg. Alright

sirSimulate beta gamma n steps initialState = intialState : (zip3 susceptible diseased recovered)
                        where susceptible = (initialState 0) (-) (beta (*) (((initialState 0) * (initialState 1))) (/) n)
                              diseased = (initialState 1) + ((beta (*) (((initialState 0) * (initialState 1))) (/) n) (-) (gamma (*) initialState 1)
                              recovered = (initialState 0) (+) (gamma (*) (intialState 0))

recursive' [(x, y, z)] 
    | steps == length tupleList = tupleList
    | otherwise = (calculate' x y z):tupleList





accessElement :: (a, a, a) -> Int -> a
accessElement (x, y, z) index
  | index == 0 = x
  | index == 1 = y
  | index == 2 = z

# Vi ska returnera en lista av tuples. Vi kan få detta genom att zipa ihop tre listor. susceptible diseased and recovered. 
# Vi kan också skapa tuples vid varje iteration och sen lägga det i en lista.
# vi kan concanetea den tuplelistan med initialstate för att få alla states genom alla iterationer.
# Hur ska vi spara våra värden? Jo vi har en function för susceptible, diseased and recovered.
# Denna funktionen måste beräkna baserat på tidigare värde ett nytt värde och lägga till de i en lista.
# Detta gäller för samtliga listor.
# Detta låter väldigt mycket som rekursion. faktisk 
# läs på rekurs
det känns ganska lätt att räkna x y och z sen lägga de in i en tuple som läggs till i en lista.
Japp
# sen andra iterationen är ju liksom då vi måste beräkna x y z. när vi väl har fått x y z så lägger vi till de i tuplelist
# och kallar på rekursiva delen igen med x y z som initialState? då behövs ej steps eller? finns flera sätt o göra de nt så viktigt
# hur stoppar vi rekursionen? Vi stoppar då steps = length tupleList då returnerar vi tuplelist. Najs.

sirSimulate beta gamma n steps initialState 
sirSimulate beta gamma n 0 (x, y, z) =  [(x, y ,z)] ++ (sirSimulate beta gamma n 1 (x, y, z)) 
sirSimulate beta gamma n steps (x, y, z)
        | steps = length 

sirSimulate _ _ _ steps (x, y, z) 
    | steps
sirSimulate beta gamma n steps initialState = 