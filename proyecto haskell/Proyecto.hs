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
--buscarDevice [] id= return ()
--buscarDevice (a:b:c:d) id =do
--				if a==id then do
	--				if c=="fall_back" then do
		--			putStrLn "ID:"++a++"user_agent:  fall_back:"++d 
			--		else putStrLn ""
						--putStrLn "ID:"++a++"user_agent:"++c++"fall_back:"++d
			   -- else putStrLn ""
				
				
imprimir [] car  =return()
imprimir (x:xs) car  =  do
			if x=="name" then do	
				if  (head xs)==car	then do		
				--lc++1
				putStrLn  (head xs)				
				else imprimir (xs) car 
			else imprimir (xs) car 
			--putStrLn (length lc)
			 
			--	buscarDevice sl ido
		--	if x=="device"then do
		--		 if y== "id" then do
			--		 let ido = (head z)
			--		else print ""
			--	else print ""
				
			
			
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
			
			
creardevice :: [String] -> Device
creardevice [] = Device "" "" ""
creardevice (a:b:c:d:e) = do
			let k="none"
			if(d=="fall_back") then do
				Device b k (head e)
			else Device b d (last e)
			

crearcapability :: [String] -> Capability
crearcapability [] = Capability "" ""
crearcapability (a:b:c) = do 
		let k = "none"
		if((last c)=="value") then do
			Capability b k
		else  Capability b (last c)
			
			
creargroup :: [String] -> Group
creargroup [] = Group ""
creargroup a = do
		Group (last a)		
		
	
	
device1 :: String->String->Device
device1 [] car = Device "" "" ""
device1 x car = do
		 let l = splitOneOf("<>=/ \\\"") x --AQUI YA C SEPARAN LOS DATOS Y C GUARDAN EN UNA LISTA
		 let listsinespacio= espacios l
		 --imprimir listsinespacio car
		 if (head listsinespacio)=="device" then do
			creardevice(tail listsinespacio)
			else Device "" "" ""
			
device2 ::String->String->Group
device2 [] car = Group ""
device2 x car = do	
		 let l = splitOneOf("<>=/ \\\"") x --AQUI YA C SEPARAN LOS DATOS Y C GUARDAN EN UNA LISTA
		 let listsinespacio= espacios l
		 --imprimir listsinespacio car		
		 if (head listsinespacio)=="group" then do
			creargroup(tail listsinespacio)
			else Group ""
			
device3 ::String->String->Capability
device3 [] car = Capability "" ""
device3 x car = do	
		 let l = splitOneOf("<>=/ \\\"") x --AQUI YA C SEPARAN LOS DATOS Y C GUARDAN EN UNA LISTA
		 let listsinespacio= espacios l
		 --imprimir listsinespacio car
		 if (head listsinespacio)=="capability" then do
			crearcapability(tail listsinespacio)
			else Capability "" ""
		
	     
listdevic [] car = return()
listdevic (x:xs) car = do
				let listadevices = []
				let listagroups = []
				let listacapabilities = []
				if isInfixOf "<device" x then do
					listadevices++[device1  x car]
					listdevic xs car
				else if isInfixOf "<group" x then do
					listagroups++[device2 x car]
					listdevic xs car
				else if isInfixOf "<capability" x then do
					listacapabilities++[device3 x car]
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
	putStrLn "estos son todas las capabilities "	
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