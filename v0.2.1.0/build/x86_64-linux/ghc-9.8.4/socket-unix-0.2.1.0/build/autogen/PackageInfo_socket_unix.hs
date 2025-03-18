{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_socket_unix (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "socket_unix"
version :: Version
version = Version [0,2,1,0] []

synopsis :: String
synopsis = "Unix domain sockets"
copyright :: String
copyright = "2017 Vyacheslav Hashov"
homepage :: String
homepage = "https://github.com/flip111/haskell-socket-unix#readme"
