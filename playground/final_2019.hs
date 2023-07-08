-- 1. Se representan secuencias mediante árboles binarios dados por el siguiente tipo de datos:
data BTree a = E | L a | N Int (BTree a) (Btree a)
-- donde el recorrido inorder del árbol da el orden de los elementos de la secuencia y el valor entero guardado en los nodos representa la longitud
-- Definir en Haskell, de manera eficiente, la función 
--, que elimine el sufijo dado en una secuencia, devolviendo Nothing si la secuencia no termina con el sufijo dado o Just xs donde xs es la secuencia que queda antes del sufijo.

size :: BTree a -> Int
size E = 0
size (L _) = 1
size (N n _ _) = n

split :: BTree a -> Int -> (BTree a, BTree a)
split t 0 = (t, E) 
split (N n l r) m = case compare (size r) m of
                      EQ -> (l, r)
                      LT -> let (rl, nl) = split l (m - (size r))
                            in (rl, (N n nl r))
                      GT -> let (nr, rr) = split r m
                            in ((N n l nr), rr)

stripSufix :: Eq a => BTree a -> BTree a -> Maybe (BTree a)
stripSufix E _ = Nothing
stripSufix t E = Just t
stripSufix l@(L a) (L b) | a == b = Just E
                         | otherwise = Nothing
stripSufix (N n l r) (N n' l' r') | n < n' = Nothing 
stripSufix (N n l r) (N n' l' r') = 
  case compare (size r) n' of
    LT -> ()
