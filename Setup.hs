import Data.Maybe
import Data.Version

import Control.Monad

import Text.ParserCombinators.ReadP

import Distribution.InstalledPackageInfo
import Distribution.Simple hiding (depends)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Simple.Utils
import Distribution.Verbosity

main = defaultMainWithHooks hooks
  where
    hooks = simpleUserHooks
        { confHook = localConfHook
        }

localConfHook a b = do
    lbi <- (confHook simpleUserHooks) a b

    when (isJust $ libraryConfig lbi) $ do
        let clbi = fromJust $ libraryConfig lbi
        deps <- foldM resolveDependencies [] $ componentPackageDeps clbi
        --mapM_ (printDependency 0 lbi) $ map fst $ componentPackageDeps clbi

    return lbi

-- Packages which are excluded from the lock file.
baseLibraries =
    [ "array"
    , "base"
    , "deepseq"
    , "ghc-prim"
    , "integer-gmp"
    , "old-locale"
    , "rts"
    ]

resolveDependencies :: [(InstalledPackageId, PackageId)] -> 
printDependency :: Int -> LocalBuildInfo -> InstalledPackageId -> IO ()
printDependency indent lbi ipid@(InstalledPackageId ipids) = do
    let mbipi = lookupInstalledPackageId (installedPkgs lbi) ipid

    when (isJust mbipi) $ do
        let ipi = fromJust mbipi
        let spid = sourcePackageId ipi
        let (PackageName name) = pkgName spid
        let version = showVersion $ pkgVersion spid
        if any (name==) baseLibraries
            then return ()
            else do
                let ver = (replicate indent ' ') ++ name ++ " " ++ version
                putStrLn ver
                -- putStrLn $ show $ fst $ last $ (readP_to_S parseVersion) version
                -- warn normal "warning hre"
                let deps = depends ipi
                mapM_ (printDependency (indent + 2) lbi) deps

    return ()
