{-# LANGUAGE CPP #-}

import Distribution.Simple
import Distribution.Simple.BuildPaths (autogenModulesDir)
import Distribution.Simple.InstallDirs as I
import Distribution.Simple.LocalBuildInfo as L
import qualified Distribution.Simple.Setup as S
import qualified Distribution.Simple.Program as P
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose, rewriteFile)
import Distribution.PackageDescription
import Distribution.Text

import System.FilePath ((</>), splitDirectories,isAbsolute)

-- -----------------------------------------------------------------------------
-- Make Commands

-- use GNU make on FreeBSD
#if defined(freebsd_HOST_OS) || defined(dragonfly_HOST_OS)
mymake = "gmake"
#else
mymake = "make"
#endif
make verbosity =
   P.runProgramInvocation verbosity . P.simpleProgramInvocation mymake


idrisRuntimeClean _ flags _ _ = do
        make verbosity [ "-C", "gorts", "clean", "IDRIS=idris" ]
    where
        verbosity = S.fromFlag $ S.cleanVerbosity flags

idrisRuntimeInstall verbosity copy pkg local = do
        installRTS
    where
        target = datadir $ L.absoluteInstallDirs pkg local copy
        installRTS = do
            putStrLn $ "Installing c++ runtime in " ++ target
            makeInstall "gorts" target
        makeInstall src target =
            make verbosity [ "-C", src, "install", "TARGET=" ++ target ]

idrisRuntimeBuild _ flags _ local = do
        buildGo
    where
        verbosity = S.fromFlag $ S.buildVerbosity flags
        buildGo = make verbosity ["-C", "gorts", "build"]

main = defaultMainWithHooks $ simpleUserHooks
    { postClean = idrisRuntimeClean
    , postBuild = idrisRuntimeBuild
    , postCopy = \_ flags pkg local ->
                   idrisRuntimeInstall (S.fromFlag $ S.copyVerbosity flags)
                                   (S.fromFlag $ S.copyDest flags) pkg local
    , postInst = \_ flags pkg local ->
                   idrisRuntimeInstall (S.fromFlag $ S.installVerbosity flags)
                                   NoCopyDest pkg local
    }
