{-# LANGUAGE QuasiQuotes #-}
module Compiler.Prelude.Github where

import           Compiler.Interpreter
import           Compiler.Interpreter.Th
-- import           Compiler.Prelude.Github.Auth
import           Compiler.Types


githubClassSC :: Interpreter Object
githubClassSC = [scriptflow| |]

{-
  # Github base class
  class Github:
    fun __init__:
      self.__gh = __call__ ${initGithub}

      cfg = load_config("Github")
      if cfg.auth == none:
        print "Logging to get authorization token to use in future connections"
        username = get_line "User Name: "
        password = ask_password "*" "Password: "
        self.__gh = __call__ ${requestLogin} self.__gh username password

    fun repos:
      __call__ ${getRepos} self.__gh Repositories

    fun logued_user:
      __call__ ${loguedUser} self.__gh User

    fun issues:
      __call__ ${userIssues} self.__gh Issue

    fun orgs:
      __call__ ${userOrgs} self.__gh Org

  class Repostories:
    fun __init__ name:
      self.name = name

    fun clone:
      __call__ ${clone} self.

    fun issues:
      __call__ ${issues} self.url

    fun __cd__:
       # get current config and use directory
       # Clone if it is necessary

  class Issue:
    fun __init__ dic:
      self.title = dic.title || ""
      self.message = dic.message || ""
      self.assignees = dic.assignees || []

   |]
-}
