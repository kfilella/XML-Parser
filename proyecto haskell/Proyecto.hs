import Data.List.Split
import Data.Char(toUpper)
import Data.List
import Data.Char
import System.IO
import System.Exit

--Estructuras para poder crear nuestra base de datos
data Device = Device { id_device :: String, 
                       user_agent :: String, 
                       fall_back :: String
                     } deriving (Eq,Show,Read)
					 
data Group = Group { id_group :: String
					} deriving (Eq,Show,Read)

data Capability = Capability { name :: String, 
                       value :: String
                     } deriving (Eq,Show,Read)
					 
data Grupo =Grupo {dev :: Device, 
                    gp :: Group, 
                     cp :: Capability
					}deriving (Eq,Show,Read)
					
--Función que carga el archivo xml y crea una lista de strings, cada linea del archivo es un string diferente
cargararchivo :: FilePath -> IO [String]
cargararchivo arch = do	
				codigo <- readFile arch
				return (lines codigo)
				
--Función que recibe un tipo de dato Device	y lo convierte en un string 			
impridevice :: Device -> String
impridevice devic = "Device-> id= "++(id_device devic)++" user_agent="++(user_agent devic)++" fall_back="++(fall_back devic)

--Función que recibe un tipo de dato Device y devuelve el id del device
iddevice:: Device -> String
iddevice devic =  (id_device devic)

--Función que recibe un tipo de dato Device y devuelve el fall_back del device
fallbackdevice:: Device -> String
fallbackdevice devic =  (fall_back devic)

--Función que recibe un tipo de dato Group y devuelve los datos en un string
imprigroup :: Group-> String
imprigroup grop= "Group: id="++(id_group grop)

--Función que recibe un tipo de dato Group y devuelve el id del grupo
idgroup:: Group-> String
idgroup grop= (id_group grop)

--Función que recibe un tipo de dato Capability y devuelve los datos en un string
impricapability :: Capability -> String
impricapability cap= "Capability: name= "++(name cap)++" value="++(value cap)

--Función que recibe un tipo de dato Capability y devuelve el nombre del Capability
namecapability:: Capability -> String
namecapability cap= (name cap)

--Función que recibe un tipo de dato Grupo y devuelve el Device que contiene
getDevice :: Grupo -> Device
getDevice grpo =do
			(dev grpo)
			
--Función que recibe un tipo de dato Grupo y devuelve el Group que contiene
getGroup :: Grupo -> Group
getGroup grpo =do
			(gp grpo)
			
--Función que recibe un tipo de dato Grupo y devuelve el Capability que contiene
getCapability :: Grupo -> Capability
getCapability grpo =do
			(cp grpo)

--Función que recibe la lista de string y devuelve una lista de string pero sin espacios vacios
espacios ::[String]->[String]
espacios []=[]
espacios (x:xs)= do
	if x==""
	then []++espacios xs
	else [x]++espacios xs
	
--Función que recibe la lista que contiene los datos de un Capability y devuelve una lista con los datos necesarios para crear el capability
listacapability[]=[]
listacapability (x:xs) = do
		if (x=="name") then do
			[head xs]++listacapability xs
		else if (x=="value") then do
			if (xs==[]) then do
				xs++["none"]
				else [head xs]++listacapability xs
		else listacapability xs

--Función que recibe la lista que contiene los datos de un Group y devuelve una lista con los datos necesarios para crear el Group
listagroup[]=[]
listagroup (x:xs) = do
		if (x=="id") then do
			[head xs]++listagroup xs
		else listagroup xs

--Función que recibe la lista que contiene los datos de un Device y devuelve una lista con los datos necesarios para crear el Device
listadevice ::[String] -> [String]	
listadevice[]=[]
listadevice (x:xs) = do
		if (x=="id") then do
			[head xs]++listadevice xs
		else if (x=="user_agent")then do
			if(head xs == "fall_back") then do
				"none": tail xs
			else [head xs]++listadevice xs
		else if (x=="fall_back") then do
			if(head xs == "fall_back") then do
				"none": tail xs
			else [head xs]++listadevice xs
		else listadevice xs
			
--Función que recibe una lista con los datos necesarios para crear un Device y devuelve un nuevo Device con los datos recibidos	
createdevice ::[String] -> Device
createdevice [] = Device "" "" ""
createdevice (x:y:zs) = do
	Device x y (head zs)

--Función que recibe una lista con los datos necesarios para crear un Capability y devuelve un nuevo Capability con los datos recibidos		
createcapability [] = Capability "" ""
createcapability (x:ys) = do
	Capability x (head ys)

--Función que recibe una lista con los datos necesarios para crear un Group y devuelve un nuevo Group con los datos recibidos	
creategroup [] = Group ""
creategroup (x:ys) = do
	Group x

--Función que recibe un Device,Group y Capability y devuelve un tipo de dato Grupo con los datos recibidos	
creategrupo ::Device->Group->Capability->Grupo
creategrupo (Device "" "" "") (Group "") (Capability "" "")= Grupo (Device "" "" "") (Group "") (Capability "" "")
creategrupo dev gp cp = do
	Grupo dev gp cp


--Función que crea un Capability	
funcioncapabi x  = do
			let list = listacapability x
			if(list/=[]) then createcapability list
			else Capability "" ""
--Función que crea un Group	
funciongroup x  = do
			let list = listagroup x
			if(list/=[]) then creategroup list
			else Group ""
--Función que crea un Device	
funciondevice::[String]	-> Device	
funciondevice	x  = do
			let list = listadevice x
			if(list/=[]) then createdevice list
			else Device "" "" ""
		
--Función que recibe la lista del archivo cargado y devuelve una lista de tipo de datos Grupo(nuestra base de datos)
prodt ::[String]->Device->Group->Capability->[Grupo]
prodt [] _ _ _ =[]
prodt (x:xs) devi grou capa = do
	let l = splitOneOf("<>=/ \\\"") x 
	let listsinespacio= espacios l 
	let lista=[]
	if (head listsinespacio) == "device" then do
			let dev = funciondevice $ tail listsinespacio
			let grupo = creategrupo dev grou capa
			[grupo]++prodt xs dev grou capa
	else if (head listsinespacio) == "group" then do
			let gro = funciongroup $ tail listsinespacio
			let grupo = creategrupo devi gro capa
			[grupo]++prodt xs devi gro capa
	else if (head listsinespacio) == "capability" then do
			let cap = funcioncapabi $ tail listsinespacio
			let grupo = creategrupo devi grou cap
			[grupo]++prodt xs devi grou cap
			
	else lista++prodt xs devi grou capa
	
--Función para hacer la consulta
imprimirgrupos []=return()
imprimirgrupos (x:xs)=do
			putStrLn $ impridevice $ getDevice x
			putStrLn $ imprigroup $ getGroup x
			putStrLn $ impricapability $ getCapability x
			--imprimirgrupos xs
--Función que imprime una lista 			
imprimir []=return()
imprimir (x:xs)=do
		putStrLn x
		imprimir xs
			
--Función para buscar un fall_back ingresado por el usuario
buscarfallbacklista _ []=[]			
buscarfallbacklista fall (x:xs)=do
			let iddev= iddevice $ getDevice x
			let fb= fallbackdevice $ getDevice x
			if(fall==fb) then do
				[iddev]++buscarfallbacklista fall xs
			else []++buscarfallbacklista fall xs


--Función para buscar un capability ingresado por el usuario
buscarcapabilitylista _ []=[]			
buscarcapabilitylista cap (x:xs)=do
			let iddev= iddevice $ getDevice x
			let cp= namecapability $ getCapability x
			if(cap==cp) then do
				[iddev]++buscarcapabilitylista cap xs
			else []++buscarcapabilitylista cap xs

--Función que elimina string repetido de la lista
eliminarrepetidos [s]=[s]
eliminarrepetidos (x:xs)=do
			if (x==(head xs)) then do
				eliminarrepetidos xs
			else [x]++eliminarrepetidos xs

			--Función que cuenta los string de la lista
contador []= 0
contador x= do
		length x

--Función pincipal para la busqueda de device
buscar str lis =do
		if str=="1" then do
		 putStrLn "Ingrese el Fall_back que desea buscar:"
		 opc <- getLine
		 putStrLn $ "los device con el fall_back "++opc++" son :"
		 let lsta= eliminarrepetidos $ buscarfallbacklista  opc lis
		 imprimir lsta
		 putStrLn ""
		 putStrLn ("el numero de dispositivos son: "++ show(contador lsta) )
		else if str=="2" then do
		 putStrLn "Ingrese el Capability que desea buscar:"
		 opc <- getLine
		 putStrLn $ "los device con el capability "++opc++" son :"
		 let lst = eliminarrepetidos $ buscarcapabilitylista  opc lis
		 imprimir lst 
		 putStrLn ""
		 putStrLn ("el numero de dispositivos son: "++ show(contador lst))
		
		else exitSuccess
				

--Función que elimina los datos "basura" como el encabezado,etc, dejando solo los datos importantes				
lista :: [String]->[Grupo]
lista (x:xs) = do
				if isInfixOf "<devices>" x then do
					prodt xs (Device "" "" "") (Group "") (Capability "" "")
				else
					lista xs

--Función principal					
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
	xml <- cargararchivo "device.xml"
	putStrLn "cargado de documento exitoso"
	let lis = lista xml
	imprimirgrupos lis
	putStrLn "que desea realizar?"
	putStrLn "1.-buscar device por fall Back"
	putStrLn "2.-buscar device por capability"
	putStrLn "3.-salir"
	opc <- getLine
	buscar opc lis
	
	
	

						
						
