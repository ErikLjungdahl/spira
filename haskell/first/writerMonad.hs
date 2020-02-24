import Control.Monad.Writer

multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b) test



a :: String
a = "Hello"

b :: String
b = " World!"




