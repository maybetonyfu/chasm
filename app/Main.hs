module Main where
import RIO hiding (Handler)
import Yesod
import Slice

data ChasmApp = ChasmApp

mkYesod "ChasmApp" [parseRoutes|
/ BindingsR GET
|]

instance Yesod ChasmApp where
    makeSessionBackend _ = return Nothing


getBindingsR :: Handler Html
getBindingsR = defaultLayout [whamlet|
Bindings for the program!
|]

main :: IO ()
main = warp 3000 ChasmApp
