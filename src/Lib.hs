import Text.Show.Functions
import Data.List
import Data.Tuple

--Tp
{- 
¡Excelsior! ¡Encontramos la manera de convertir la tierra en oro mágicamente! O eso creían los alquimistas, aunque en realidad no era magia, si no que era ciencia, química para ser exactos.
Hoy en día, las sustancias químicas abundan, y prácticamente todos los días se están descubriendo sustancias nuevas.
Es por ello que, para llevar un mejor recuento de las sustancias ya existentes y las nuevas creadas, nos decidimos a hacer un sistema con dicho fin, en Haskell.
En nuestro análisis, nos encontramos con que las sustancias pueden clasificarse en dos tipos: compuestas o sencillas. 
Las sustancias sencillas son aquellas que se corresponden directamente a un elemento de la tabla periódica, de allí su otro nombre, elemento.
De los elementos conocemos su nombre, símbolo químico y número atómico.
Las sustancias compuestas, o simplemente compuestos, son aquellas que tienen una serie de componentes. Un componente es un par formado por una sustancia y la cantidad de moléculas de esa sustancia.
La sustancia del componente puede ser un elemento o un compuesto. Además los compuestos, al igual que las sustancias simples, tienen un nombre, pero no número atómico.
También poseen un símbolo o fórmula química, la cual no nos interesa conocer en todo momento, ya que es deducible a partir de las sustancias que la componen.
Ah, nos olvidábamos, también sabemos que todas las sustancias poseen un grupo o especie, que puede ser metal, no metal, halógeno o gas noble.
-}

--SustanciasSencillas || Elementos:
--Tupla de 4 -> Pq tiene elementos de distinto tipo
{-
        nombre : "String",
        símboloQuímico : "String",
        númeroAtómico : "Int" [1...103 U 121],
        especie: noMetal|metal|halógeno|gasNoble (String)
-}
--Tupla de n elementos + 2 
--SustanciasCompuestas || Compuestos:
{-
        Sustancia: Compuesto || Elemento,
        cantdidadDeMoleculas: Int,
        nombre : String,
-}

{-data Elemento = Elemento {
    nombre :: String,
    simbolo :: String,
    nAtomico :: Int
} deriving (Show)-}

{-
        data Elemento = Elemento {
    nombre :: String,
    simbolo :: [Char, Char],
    nAtomico :: Int
} deriving (Show)

data Componente = Componente {
    sustancia :: Compuesto | Elemento,
    cantMoleculas :: Int
} deriving (Show) where 

data Compuesto = Compuesto {
    nombre :: String,
    componente :: [Componente],
    grupo :: String
} deriving (Show)
-}



{-
data Sustancia = Elemento String String Int  | Compuesto String [Componente] String deriving Show --Sólo existe el type Sustancia, pero tiene 2 constructores -> Elemento (crea una tupla de 3 valores) y Compuesto (crea una tupla de 3 valores)

data Componente = Componente Sustancia Int deriving Show
-}


data Grupo = Metal | NoMetal | Halógeno | GasNoble deriving (Show, Eq)

data Sustancia = Elemento String String Int Grupo | Compuesto String [Componente] Grupo   deriving Show

data Componente = Componente Sustancia Int deriving Show



--1)
hidrogeno :: Sustancia
hidrogeno = Elemento "Hidrógeno" "H" 1 NoMetal

oxigeno :: Sustancia
oxigeno = Elemento "Oxígeno" "O" 8 NoMetal

{-
    agua :: Sustancia
    agua = Compuesto "Agua" (map (\(nombre, cantidad) -> Componente nombre cantidad) [(hidrogeno, 2), (oxigeno, 1)]) "no metal"
-} 
agua :: Sustancia
agua = Compuesto "agua"  [Componente hidrogeno 2, Componente oxigeno 1] NoMetal --Ésta es más declarativa

dobleAgua = Compuesto "dobleAgua" [Componente agua 2] NoMetal

--2)

--Elementos y Compuestos Metálicos

calcio :: Sustancia
calcio = Elemento "Calcio" "Ca" 20 Metal

potasio :: Sustancia
potasio = Elemento "Potasio" "K" 19 Metal

compuestoMetalico :: Sustancia
compuestoMetalico = Compuesto "CompuestoMetalico" [Componente calcio 1, Componente potasio 1] Metal

--Elementos y Compuestos Gas Noble

helio :: Sustancia
helio = Elemento "Helio" "He" 2 GasNoble

compuestoGasNoble :: Sustancia
compuestoGasNoble = Compuesto "CompuestoGasNoble" [Componente helio 3] GasNoble

--Elementos y Compuestos Halógenos

fluor :: Sustancia
fluor = Elemento "Fluor" "F" 9 Halógeno

compuestoHalogeno :: Sustancia
compuestoHalogeno = Compuesto "CompuestoHalogeno" [Componente fluor 3] Halógeno

--Función De Conducción Bien o Mal Según un Criterio

conduceBien :: Sustancia -> String -> Bool
conduceBien (Compuesto _ _ Metal) _ = True
conduceBien (Compuesto _ _ Halógeno) "calor" = True
conduceBien (Elemento _ _ _ GasNoble) "electricidad" = True
conduceBien (Elemento _ _ _ Metal) _ = True
conduceBien _ _ = False


--3)

vocales :: [Char]
vocales = ['a', 'e', 'i', 'o', 'u']

esVocal :: Char -> Bool
esVocal unaLetra = elem unaLetra vocales --elem te dice si un elemento está en una lista
{- Version de esVocal vieja
    esVocal :: Char -> Bool
    esVocal 'a' = True
    esVocal 'e' = True
    esVocal 'i' = True
    esVocal 'o' = True
    esVocal 'u' = True
    esVocal _ = False
-}

formarNombreDeUnionDeUnNombre :: String -> String
formarNombreDeUnionDeUnNombre nombre  | (not . esVocal . last) nombre = nombre ++ "uro"
                                        | otherwise = (formarNombreDeUnionDeUnNombre . take ((length nombre) - 1)) nombre 

obtenerNombreDeUnionDeUnElementoOCompuesto :: Sustancia -> String
obtenerNombreDeUnionDeUnElementoOCompuesto (Elemento nombre _ _ _) = formarNombreDeUnionDeUnNombre nombre
obtenerNombreDeUnionDeUnElementoOCompuesto (Compuesto nombre _ _) = formarNombreDeUnionDeUnNombre nombre

--4)
{-combinarUnNombreDeUnionConUnNombre :: Sustancia -> Sustancia -> String
combinarUnNombreDeUnionConUnNombre (Elemento nombreDelPrimerElemento _ _ _) (Elemento nombreDelSegundoElemento _ _ _) = (flip (++) (" de " ++ nombreDelSegundoElemento) . formarNombreDeUnionDeUnElemento) nombreDelPrimerElemento -}

combinarNombreDeUnionConOtroNombre :: String -> String -> String
combinarNombreDeUnionConOtroNombre nombreDelPrimerElemento nombreDelSegundoElemento = (formarNombreDeUnionDeUnNombre nombreDelPrimerElemento) ++ " de " ++ nombreDelSegundoElemento

--5)

mezclarComponentes :: [Componente] -> Sustancia
mezclarComponentes componentes = Compuesto ((formarNombreDeLaMezclaDeComponente . map (obtenerNombreSustancia . obtenerSustanciaDeComponente)) componentes) componentes NoMetal

formarNombreDeLaMezclaDeComponente :: [String] -> String
formarNombreDeLaMezclaDeComponente componentes | length componentes == 1 = head componentes
                                               | otherwise = combinarNombreDeUnionConOtroNombre (head componentes) (formarNombreDeLaMezclaDeComponente(tail componentes))

--6)

concatenacionDeLasFormulasDeLosComponentesDeUnCompuesto :: [Componente] -> String
concatenacionDeLasFormulasDeLosComponentesDeUnCompuesto = ((++)"(" . flip (++)")". concat . (map construirFormulaDeComponenteDeUnaSustanciaCompuesta)) -- Poca expresividad (Por eso no se la usa)

construirFormulaDeComponenteDeUnaSustanciaCompuesta :: Componente -> String
construirFormulaDeComponenteDeUnaSustanciaCompuesta (Componente sustancia 1) = formulaDeUnaSustancia sustancia
construirFormulaDeComponenteDeUnaSustanciaCompuesta (Componente sustancia cantidadDeLaSustancia) = formulaDeUnaSustancia sustancia ++ show cantidadDeLaSustancia

formulaDeUnaSustancia :: Sustancia -> String
formulaDeUnaSustancia (Elemento _ simboloQuimico _ _) = simboloQuimico
formulaDeUnaSustancia (Compuesto _ componentes _) = "(" ++ concatMap construirFormulaDeComponenteDeUnaSustanciaCompuesta componentes ++ ")"  --No necesitas agregar un show acá pq la otra función ya convierte todos los ints

--Extras

obtenerNombreSustancia :: Sustancia -> String
obtenerNombreSustancia (Compuesto nombre lista grupo) = nombre
obtenerNombreSustancia (Elemento nombre _ _ _) = nombre

obtenerSustanciaDeComponente :: Componente -> Sustancia
obtenerSustanciaDeComponente (Componente sustancia cantidadDeLaSustancia) = sustancia


