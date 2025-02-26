data Fruct
  = Mar String Bool
  | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala "Tarocco" _) = True
ePortocalaDeSicilia (Portocala "Moro" _) = True
ePortocalaDeSicilia (Portocala "Sanguinello" _) = True
ePortocalaDeSicilia _ = False

test_ePortocalaDeSicilia1 =
    ePortocalaDeSicilia (Portocala "Moro" 12) == True

test_ePortocalaDeSicilia2 =
    ePortocalaDeSicilia (Mar "Ionatan" True) == False

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia l = sum 
                    [felii | Portocala s felii <- l, ePortocalaDeSicilia (Portocala s felii)]

test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52

nrMereViermi :: [Fruct] -> Int
nrMereViermi fructe = length [() | Mar _ areViermi <- fructe, areViermi]


test_nrMereViermi = nrMereViermi listaFructe == 2

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

vorbeste :: Animal -> String
vorbeste (Pisica _ ) = "Meow!"
vorbeste (Caine _ _) = "Woof!"

rasa :: Animal -> Maybe String
rasa (Caine _ rasa) = Just rasa
rasa (Pisica _)     = Nothing

data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show

sumaLinie :: Linie -> Int
sumaLinie (L xs) = sum xs

verifica :: Matrice -> Int -> Bool
verifica (M linii) n = foldr (\linie acc -> sumaLinie linie == n && acc) True linii



test_verif1 = verifica (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 10 == False
test_verif2 = verifica (M[L[2,20,3], L[4,21], L[2,3,6,8,6], L[8,5,3,9]]) 25 == True

estePozitiva :: [Int] -> Bool
estePozitiva = all (> 0)

doarPozN :: Matrice -> Int -> Bool
doarPozN (M linii) n = foldr (\(L xs) acc -> (length xs /= n || estePozitiva xs) && acc) True linii

testPoz1 = doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == True

testPoz2 = doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == False

lungimeLinie :: Linie -> Int
lungimeLinie (L xs) = length xs

corect :: Matrice -> Bool
corect (M []) = True 
corect (M (l:ls)) = all (\linie -> lungimeLinie linie == lungimeLinie l) ls 

testcorect1 = corect (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) == False
testcorect2 = corect (M[L[1,2,3], L[4,5,8], L[3,6,8], L[8,5,3]]) == True
