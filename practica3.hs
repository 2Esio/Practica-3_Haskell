-- Practica 3

import System.IO

-- Ejercicios:

{-
1. implicacion :: Bool -> Bool -> Bool
	Implicación: a -> b
-}

implicacion :: Bool -> Bool -> Bool
implicacion True False = False
implicacion _ _ = True

{-
2. negacion :: Bool -> Bool
	Negación: not a
-}

negacion :: Bool -> Bool
negacion True = False
negacion False = True

{-
3. disyuncion :: Bool -> Bool -> Bool
	Disyunción: a || b
-}

disyuncion :: Bool -> Bool -> Bool
disyuncion True _ = True
disyuncion _ True = True
disyuncion _ _ = False

{-
4. conjuncion :: Bool -> Bool -> Bool
	Conjunción: a && b
-}

conjuncion :: Bool -> Bool -> Bool
conjuncion True True = True
conjuncion _ _ = False

{-
5. dobleImplicacion :: Bool -> Bool -> Bool
	Doble implicación: a <-> b
-}

dobleImplicacion :: Bool -> Bool -> Bool
dobleImplicacion a b = (implicacion a b) && (implicacion b a)

{-
6. modusTollens :: Bool -> Bool -> Bool -> Bool
	Modus Tollens: (a -> b) -> ¬b -> ¬a
-}

modusTollens :: Bool -> Bool -> Bool -> Bool
modusTollens a b c = if b == False then True else (negacion c) && (negacion a)

{-
7. modusPonens :: Bool -> Bool -> Bool -> Bool
	Modus Ponens: (a -> b) -> a -> b
-}

modusPonens :: Bool -> Bool -> Bool -> Bool
modusPonens a b c = if a == True && c == True then b else True


--Extra:

{-
Lograr implementarlo en un programa con un menu interactivo donde pueda ver el
funcionamiento de cada punto.
-}

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "¡Bienvenido al programa de lógica booleana!"
    menu

menu :: IO ()
menu = do
    putStrLn "\nElige una opción:"
    putStrLn "1. Implicación"
    putStrLn "2. Negación"
    putStrLn "3. Disyunción"
    putStrLn "4. Conjunción"
    putStrLn "5. Doble Implicación"
    putStrLn "6. Modus Tollens"
    putStrLn "7. Modus Ponens"
    putStrLn "8. Salir"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Introduce dos valores booleanos (True o False):"
            a <- readLn
            b <- readLn
            putStrLn $ "La implicación de " ++ show a ++ " -> " ++ show b ++ " es: " ++ show (implicacion a b)
            menu
        "2" -> do
            putStrLn "Introduce un valor booleano (True o False):"
            a <- readLn
            putStrLn $ "La negación de " ++ show a ++ " es: " ++ show (negacion a)
            menu
        "3" -> do
            putStrLn "Introduce dos valores booleanos (True o False):"
            a <- readLn
            b <- readLn
            putStrLn $ "La disyunción de " ++ show a ++ " || " ++ show b ++ " es: " ++ show (disyuncion a b)
            menu
        "4" -> do
            putStrLn "Introduce dos valores booleanos (True o False):"
            a <- readLn
            b <- readLn
            putStrLn $ "La conjunción de " ++ show a ++ " && " ++ show b ++ " es: " ++ show (conjuncion a b)
            menu
        "5" -> do
            putStrLn "Introduce dos valores booleanos (True o False):"
            a <- readLn
            b <- readLn
            putStrLn $ "La doble implicación de " ++ show a ++ " <-> " ++ show b ++ " es: " ++ show (dobleImplicacion a b)
            menu
        "6" -> do
            putStrLn "Introduce tres valores booleanos (True o False):"
            a <- readLn
            b <- readLn
            c <- readLn
            putStrLn $ "El resultado de aplicar Modus Tollens con " ++ show a ++ ", " ++ show b ++ ", y " ++ show c ++ " es: " ++ show (modusTollens a b c)
            menu
        "7" -> do
            putStrLn "Introduce tres valores booleanos (True o False):"
            a <- readLn
            b <- readLn
            c <- readLn
            putStrLn $ "El resultado de aplicar Modus Ponens con " ++ show a ++ ", " ++ show b ++ ", y " ++ show c ++ " es: " ++ show (modusPonens a b c)
            menu
        "8" -> putStrLn "¡Hasta luego!"
        _   -> do
            putStrLn "Opción no válida, por favor selecciona una opción válida."
            menu



