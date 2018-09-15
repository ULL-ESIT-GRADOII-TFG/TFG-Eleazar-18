import           Distribution.Simple
import           Distribution.Simple.PreProcess
import           Distribution.Simple.Program
import           Distribution.Types.BuildInfo
import           Distribution.Types.ComponentLocalBuildInfo
import           Distribution.Types.LocalBuildInfo


main = defaultMainWithHooks (simpleUserHooks {
    hookedPreProcessors = [("x", ppAlex')]
    })

ppAlex' :: BuildInfo -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
ppAlex' _ lbi _ = pp { platformIndependent = True }
  where pp = standardPP lbi alexProgram (hcFlags hc)
        hc = compilerFlavor (compiler lbi)
        hcFlags GHC = ["-g", "--template=./templates"]
        -- hcFlags GHCJS = ["-g"]
        hcFlags _   = []

standardPP :: LocalBuildInfo -> Program -> [String] -> PreProcessor
standardPP lbi prog args =
  PreProcessor {
    platformIndependent = False,
    runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
      runDbProgram verbosity prog (withPrograms lbi)
                           (args ++ ["-o", outFile, inFile])
}
