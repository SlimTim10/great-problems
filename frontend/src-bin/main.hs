import Frontend
import Common.Route
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom

main :: IO ()
main = case checkEncoder fullRouteEncoder of
  Right validFullEncoder -> run $ runFrontend validFullEncoder frontend
  Left _ -> error "Invalid frontend encoder"
