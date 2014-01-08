data Device = Device { id_device :: String, user_agent :: String, fall_back :: String} deriving (Eq,Show,Read)
					 
data Group = Group { id_group :: String} deriving (Eq,Show,Read)

data Capability = Capability { id_ref :: String, name :: String, value :: String} deriving (Eq,Show,Read)
					 