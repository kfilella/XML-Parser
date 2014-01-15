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
buscarDevice [] id= return ()
buscarDevice (a:b:c:d) id =do
				if a==id then do
					if c=="fall_back" then do
					putStrLn "ID:"++a++"user_agent:  fall_back:"++d 
					else putStrLn ""
						--putStrLn "ID:"++a++"user_agent:"++c++"fall_back:"++d
			    else putStrLn ""
				
				
imprimir [] car id []=return()
imprimir (x:y:z) car id sl=  do
			
			if x=="device"then do
				 if y== "id" then do
					 let ido = (head z)
					else print ""
				else print ""
				
			if x=="name" then do	
				if  y==car	then do	
				buscarDevice sl ido
				putStrLn  y				
				else imprimir (y++z) car ido sl
			else imprimir (y++z) car ido sl
			
printDevice :: Device -> String
printDevice(Device id user fall)="ID: "++id++" User Agent: "++user++" Fall Back: "++fall

espacios ::[String]->[String]
espacios []=[]
espacios (x:xs)= do
	if x==""
	then []++espacios xs
	else [x]++espacios xs
	
--buscaCar :: [String]->  String-> IO ()
--buscaCar [] car = putStrLn "no encontrado"
--buscaCar (x:xs) car	= do
		--	if (x==car) then do
		--	putStrLn "encontrado"			
		--	else buscaCar xs car
			
			
		
		
	
	
		
device [] car =return()
device x car= do
		 let l = splitOneOf("<>=/ \\\"") x --AQUI YA C SEPARAN LOS DATOS Y C GUARDAN EN UNA LISTA
		 let listsinespacio= espacios l
		 
		 imprimir listsinespacio car "00000" listsinespacio
		
		
	     
listdevic [] car = return()
listdevic (x:xs) car = do
				if isInfixOf "<device" x then do
					device x car
					listdevic xs car
				else if isInfixOf "<group" x then do
						device x car
						listdevic xs car
				else if isInfixOf "<capability" x then do
						device x car
						listdevic xs car
				else listdevic xs car
				
				
				
				
				
				

lista [] car = return ()
lista (x:xs) car = do
				if isInfixOf "<devices>" x then do
					listdevic xs car
				else
					lista xs car
				
				
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
	putStrLn ("Ingrese la caracteristica o capability que desea consultar")
	caracteristica <- getLine
	lista xml caracteristica
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