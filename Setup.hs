import Data.Maybe
import Data.Version
import Data.List

import Control.Monad
import System.Directory

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
    haveLock <- doesFileExist ".cabal/lock"
    if haveLock
        then loadDependencies lbi
        else saveDependencies lbi


saveDependencies :: LocalBuildInfo -> IO LocalBuildInfo
saveDependencies lbi = do
    -- The entirety of dependencies this package has.
    let externalDeps = map fst $ externalPackageDeps lbi

    -- Recursively walk the dependencies and build up a list of them all.
    deps <- foldM (resolveDependencies lbi) [] externalDeps

    -- Turn [InstalledPackageId] into [PackageId]
    let packageIds = map sourcePackageId $ catMaybes $ map (lookupInstalledPackageId (installedPkgs lbi)) (nub deps)
    let deps = sort packageIds
    createDirectoryIfMissing True ".cabal"
    writeFile ".cabal/lock" $ Data.List.intercalate "\n" $ map show deps

    forM deps $ \spid -> do
        let (PackageName name) = pkgName spid
        let version = showVersion $ pkgVersion spid
        let a = read (show $ pkgVersion spid) :: Version
        if any (name==) baseLibraries
            then return ()
            else putStrLn $ show spid

    return lbi


loadDependencies :: LocalBuildInfo -> IO LocalBuildInfo
loadDependencies lbi = do
    fileContents <- readFile ".cabal/lock"
    let deps = map (read :: String -> PackageIdentifier) $ lines fileContents
    putStrLn $ show deps
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

resolveDependencies :: LocalBuildInfo -> [InstalledPackageId] -> InstalledPackageId -> IO [InstalledPackageId]
resolveDependencies lbi a b = do
    let mbipi = lookupInstalledPackageId (installedPkgs lbi) b
    if isJust mbipi
        then do
            let ipi = fromJust mbipi
            let spid = sourcePackageId ipi
            let (PackageName name) = pkgName spid
            let version = showVersion $ pkgVersion spid
            if any (name==) baseLibraries
                then return $ b : a
                else do
                    let deps = depends ipi
                    foldM (resolveDependencies lbi) (b : a) deps
        else return $ b : a
