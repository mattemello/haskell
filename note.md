# note di haskell

## liste
bisogna controllare che vadano bene queste due proprietà:
- (x : xs) -> deve avere almeno un elemento
- (x : y : xs) -> deve avere almeno due elementi
con tipo [] = ... o [_] = ...


{_ come golang}

## classi
ci sono molte classi, e si utilizza la paternità, le sottoclassi prende le proprietà delle classi sopra di loro.

## tipi delle funzioni
spiegazione dell'algoritmo che capisce il tipo delle delle funzioni

i vincoli hanno una direzione, se no in alcuni casi è possibile che creino cicli infiniti

gli alberi si formano con:

foglie -> x : (alfa) || c : Bool ̉

            \x : (alfa) -> (tao)
             |
             t : (tao) 
