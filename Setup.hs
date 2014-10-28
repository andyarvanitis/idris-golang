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


idrisCppClean _ flags _ _ = do
        make verbosity [ "-C", "cpprts", "clean", "IDRIS=idris" ]
    where
        verbosity = S.fromFlag $ S.cleanVerbosity flags

idrisCppInstall verbosity copy pkg local = do
        installCppRTS
    where
        target = datadir $ L.absoluteInstallDirs pkg local copy
        installCppRTS = do
            let target' = target </> "cpprts"
            putStrLn $ "Installing c++ runtime in " ++ target
            makeInstall "cpprts" target
        makeInstall src target =
            make verbosity [ "-C", src, "install", "TARGET=" ++ target ]

idrisCppBuild _ flags _ local = do
        buildCpp
    where
        verbosity = S.fromFlag $ S.buildVerbosity flags
        buildCpp = make verbosity ["-C", "cpprts", "build"]

main = defaultMainWithHooks $ simpleUserHooks
    { postClean = idrisCppClean
    , postBuild = idrisCppBuild
    , postCopy = \_ flags pkg local ->
                   idrisCppInstall (S.fromFlag $ S.copyVerbosity flags)
                                   (S.fromFlag $ S.copyDest flags) pkg local
    , postInst = \_ flags pkg local ->
                   idrisCppInstall (S.fromFlag $ S.installVerbosity flags)
                                   NoCopyDest pkg local
    }
