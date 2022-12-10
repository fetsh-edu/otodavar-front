module About exposing (..)

import View.Helper

view _ =
    { title = "About"
    , body = [ View.Helper.container View.Helper.about ]
    }
