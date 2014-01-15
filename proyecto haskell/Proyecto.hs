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
					 
data Group = Group { --id_device_ref :: String
						id_group :: String
					} deriving (Eq,Show,Read)

data Capability = Capability { --id_group_ref :: String,
					   name :: String, 
                       value :: String
                     } deriving (Eq,Show,Read)
					 
					 
				   
data Grupo =Grupo {dev :: Device, 
                    gp :: Group, 
                     cp :: Capability
					}deriving (Eq,Show,Read)

	
cargararchivo :: FilePath -> IO [String]
cargararchivo arch = do	
				codigo <- readFile arch
				return (lines codigo)
				
				
getIdDevice :: Device -> String
getIdDevice (Device id ua fb) = id
				
				
--printflistanueva [] = return()
--printflistanueva (x:xs) = do
			--	let listp= splitOneOf("<>= \\\"") x     --AQUI YA C SEPARAN LOS DATOS Y C GUARDAN EN UNA LISTA
				--imprimir listp
				
				--printflistanueva xs
				
imprimir []=return()
imprimir (x:xs) = do
			putStrLn x
			imprimir xs

impridevice :: Device -> String
impridevice (Device id u f) = "Device: id-> "++id++" user_agent->"++u++" fall_back->"++f

imprigroup :: Group-> String
imprigroup(Group id)= "Group: id="++id

impricapability :: Capability -> String
impricapability(Capability n v)= "Capability: name= "++n++" value="++v
	
espacios ::[String]->[String]
espacios []=[]
espacios (x:xs)= do
	if x==""
	then []++espacios xs
	else [x]++espacios xs
	
listacapability[]=[]
listacapability (x:xs) = do
		if (x=="name") then do
			[head xs]++listacapability xs
		else if (x=="value") then do
			if (xs==[]) then do
				xs++["none"]
				else [head xs]++listacapability xs
		else listacapability xs


listagroup[]=[]
listagroup (x:xs) = do
		if (x=="id") then do
			[head xs]++listagroup xs
		else listagroup xs
	
listadevice ::[String] -> [String]	
listadevice[]=[]
listadevice (x:xs) = do
		if (x=="id") then do
			[head xs]++listadevice xs
		else if (x=="user_agent")then do
		    [head xs]++listadevice xs
			--if(head xs == "fall_back") then do
			--	[x]++ ""
			--else [head xs]++listadevice xs
		else if (x=="fall_back") then do
			[head xs]++listadevice xs
		else listadevice xs
			
		
		
createdevice ::[String] -> Device
createdevice [] = Device "" "" ""
createdevice (x:y:zs) = do
	Device x y (head zs)
	
createcapability [] = Capability "" ""
createcapability (x:ys) = do
	Capability x (head ys)


creategroup [] = Group ""
creategroup (x:ys) = do
	Group x

	
creategrupo ::Device->Group->Capability->Grupo
creategrupo (Device "" "" "") (Group "") (Capability "" "") = Grupo (Device "" "" "") (Group "") (Capability "" "")
creategrupo dev gp cp = do
	Grupo dev gp cp

sacardevice ::Grupo -> Device
sacardevice (Grupo (Device "" "" "") (Group "") (Capability "" ""))= (Device "" "" "") 
sacardevice grp = do
	dev grp
	
sacargroup ::Grupo -> Group
sacargroup (Grupo (Device "" "" "") (Group "") (Capability "" ""))= (Group "") 
sacargroup grp = do
	gp grp

sacarcapab ::Grupo -> Capability
sacarcapab (Grupo (Device "" "" "") (Group "") (Capability "" ""))= (Capability "" "") 
sacarcapab grp = do
	cp grp
	
funcioncapabi x  = do
			let list = listacapability x
			if(list/=[]) then createcapability list
			else Capability "" ""

funciongroup x  = do
			let list = listagroup x
			if(list/=[]) then creategroup list
			else Group ""
	
--createdevice ::[String] -> Device	
funciondevice::[String]	-> Device	
funciondevice	x  = do
			let list = listadevice x
			if(list/=[]) then createdevice list
			else Device "" "" ""
			
devicedecapab :: Grupo -> String
devicedecapab grp = do
	impridevice (sacardevice grp)
	

	
		--AQUI SE DEBE HACER TODO
prodt ::[String]->Device->Group->Capability->[Grupo]
prodt [] _ _ _ =[]
prodt (x:xs) dev gp cp = do
	let l = splitOneOf("<>=/ \\\"") x --AQUI YA C SEPARAN LOS DATOS Y C GUARDAN EN UNA LISTA
	let listsinespacio= espacios l  --- lista de le linea sin espacios
	let lista=[]
	if (head listsinespacio) == "device" then do
			let devi = funciondevice (tail listsinespacio)
			--putStrLn (impridevice devi)
			[]++prodt xs dev gp cp
	else if (head listsinespacio) == "group" then do
			let grou = funciongroup (tail listsinespacio)
			--let idDe = getDevice dev
			--putStrLn (imprigroup grou)
			[]++prodt xs dev gp cp
	else if (head listsinespacio) == "capability" then do
			let capa = funcioncapabi (tail listsinespacio)
			let grupo = creategrupo dev gp capa
			--putStrLn (impricapability  capa)
			[grupo]++prodt xs dev gp cp
	else lista++prodt xs dev gp cp
	--putStrLn impridevice devi
	--imprimir listsinespacio
	     
{-listdevic [] =return()
listdevic (x:xs) = do
				let lista=[]
				if isInfixOf "<device" x then do
					lista ++ prodt x 
					
				else if isInfixOf "<group" x then do
					lista ++ prodt x 
					
				else if isInfixOf "<capability" x then 
					lista ++ prodt x 
					
				else putStrLn " "
				listdevic xs
-}
					
lista :: [String]->[Grupo]
lista (x:xs) = do
				if isInfixOf "<devices>" x then do
					prodt xs (Device "" "" "") (Group "") (Capability "" "")
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
	let lis = lista xml
	putStrLn "cargado de documento exitoso"

elimespacios ::[String]->[String]
elimespacios [] = []
elimespacios (x:xs)=do
	if x==""
	then []++elimespacios xs
	else [x]++elimespacios xs
	

						
						
