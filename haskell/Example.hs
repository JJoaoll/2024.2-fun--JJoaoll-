module Example where 

import Prelude 
import qualified ExList as L

data Client where 
  Client :: { getName  :: String 
            , getId    :: Integer 
            , getSaldo :: Double
            } -> Client
  deriving (Eq, Show)


c1 = Client "claudia" 1 359
c2 = Client "claudia" 1 356
c3 = Client "claudia" 1 35
c4 = Client "claudia" 1 30
c5 = Client "claudia" 1 30
c6 = Client "claudia" 1 35
c7 = Client "claudia" 1 30
c8 = Client "claudia" 1 30

clientes = [c1, c2, c3, c4, c5, c6, c7, c8]                                         
