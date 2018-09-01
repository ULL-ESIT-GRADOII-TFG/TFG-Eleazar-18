{-# LANGUAGE QuasiQuotes #-}
module Compiler.Prelude.Github where

import           Compiler.Interpreter
import           Compiler.Interpreter.Th
import           Compiler.Prelude.Github.Auth
import           Compiler.Types


githubClassSC :: Interpreter Object
githubClassSC = [scriptflow|

    # Github base class
    class Github:
        fun __init__:
            self.url_base = "https://api.github.com/"
            self.auth = none
            self.user = none
            self.check_config()

        fun check_config:
            cfg = load_config("Github")
            if cfg.auth = none:
              print "Logging to get authorization token to use in future connections"
              username = get_line "User Name: "
              password = ask_password "*" "Password: "
              # __call__ ${requestLogin} username password
              # self.get_auth(username, password)

   |]
