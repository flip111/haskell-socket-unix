{-# LINE 1 "src/System/Socket/Family/Unix/Internal.hsc" #-}
-- |
-- Stability   :  experimental
-- Portability :  Linux, Unix

module System.Socket.Family.Unix.Internal
    ( Unix
    -- * Exceptions
    , eNoEntry
    ) where

import System.Socket (SocketException(..))




{-# LINE 18 "src/System/Socket/Family/Unix/Internal.hsc" #-}

-- | The [Unix domain socket]
-- (https://en.wikipedia.org/wiki/Unix_domain_socket)
data Unix


-- | > SocketException "No such file or directory"
eNoEntry :: SocketException
eNoEntry = SocketException (2)
{-# LINE 27 "src/System/Socket/Family/Unix/Internal.hsc" #-}

