import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withLoLLogsWebApp)
import Prelude              (IO)

main :: IO ()
main = defaultMain fromArgs withLoLLogsWebApp