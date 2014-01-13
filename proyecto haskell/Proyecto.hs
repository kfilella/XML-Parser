import System.IO
import Data.Char(toUpper)
import Data.List
import Data.Char
import Data.List.Split

{- Función que me permite obtener la información de un archivo

-}

data Device = Device { id_device :: String, 
                       user_agent :: String, 
                       fall_back :: String
                     } deriving (Eq,Show,Read)
					 
data Group = Group { id_group :: String
                   } deriving (Eq,Show,Read)

data Capability = Capability { name :: String,
					value :: String
                     } deriving (Eq,Show,Read)
				   

	
cargararchivo :: FilePath -> IO [String]
cargararchivo arch = do	
				codigo <- readFile arch
				return (lines codigo)
				
--printflistanueva [] = return()
--printflistanueva (x:xs) = do
			--	let listp= splitOneOf("<>= \\\"") x     --AQUI YA C SEPARAN LOS DATOS Y C GUARDAN EN UNA LISTA
				--imprimir listp
				
				--printflistanueva xs
				
imprimir []=return()
imprimir (x:xs) = do
			putStrLn x
			imprimir xs
			
printDevice :: Device -> String
printDevice(Device id user fall)="ID: "++id++" User Agent: "++user++" Fall Back: "++fall

espacios ::[String]->[String]
espacios []=[]
espacios (x:xs)= do
	if x==""
	then []++espacios xs
	else [x]++espacios xs
		
device [] =return()
device x = do
		 let l = splitOneOf("<>=/ \\\"") x --AQUI YA C SEPARAN LOS DATOS Y C GUARDAN EN UNA LISTA
		 let listsinespacio= espacios l
		 imprimir listsinespacio
	     
listdevic [] =return()
listdevic (x:xs) = do
				if isInfixOf "<group" x then do
					device x
				else if isInfixOf "<product" x then do
						device x
				else if isInfixOf "<capability" x then 
						device x
				else putStrLn ""
				listdevic xs
				
				

lista [] = return ()
lista (x:xs) = do
				if isInfixOf "<devices>" x then do
					listdevic xs
				else
					lista xs
				
				
main = do 
	putStrLn "Parseo de XML"
	putStrLn ""
	putStrLn ""
	putStrLn "**********Parser xml**********"
	putStrLn "*************************************"
	putStrLn "*Integrantes:                       *"
	putStrLn "*             Adrian Aguilar        *"
	putStrLn "*             Kevin Filella         *"
	putStrLn "*             Edison Sanchez        *"
	putStrLn "*                                   *"
	putStrLn "*************************************"
	putStrLn ""
	putStrLn ("Leyendo el archivo device.xml")
	xml <- cargararchivo "test1.xml"
	lista xml
	putStrLn "cargado de documento exitoso"
	--menu1 xml
	
	
	
--menu1 :: [String] -> IO ()
--menu1 [] = return ()
--menu1 (x:xs) = do
			--	if isInfixOf "<device" x 
			--	then do
				--		putStrLn ""
				--		
				--		putStrLn "1 -- Nombre(ID) del device "
				--		putStrLn "2 -- Capability del device"
				--		opcion <- getLine
				--		if opcion == "1"
				--		then do putStrLn "escriba el  Nombre(ID) del device "
				--		        nombre <- getLine
				--		        devicesConNombre nombre
				--		else if number == "2"
				--		then do putStrLn "escriba la capability del device"
				--		        capability <- getLine
				--				capabilityDevices capability
				--			   else do putStrLn "opcion no valida"
							 
						
						
				--else
				--	menu1 xs	