[![Available on Hackage](https://img.shields.io/hackage/v/socket-unix.svg?dummy)](https://hackage.haskell.org/package/socket-unix)
[![License MIT](https://img.shields.io/badge/license-MIT-blue.svg?dummy)](https://github.com/flip111/haskell-socket-unix/blob/master/LICENSE)
[![Build Status](https://github.com/flip111/haskell-socket-unix/actions/workflows/ci.yml/badge.svg)](https://github.com/flip111/haskell-socket-unix/actions)
[![Stackage LTS](https://stackage.org/package/socket-unix/badge/lts)](https://stackage.org/package/socket-unix)
[![GitHub release](https://img.shields.io/github/release/flip111/haskell-socket-unix.svg)](https://github.com/flip111/haskell-socket-unix/releases)

# socket-unix
A Unix domain socket API for the [socket](https://github.com/lpeterse/haskell-socket) library.

This is a fork maintained at [GitHub](https://github.com/flip111/haskell-socket-unix).  
Please refer to this repository for the latest updates, issue tracking, and contributions.

## Usage
Creating the Unix domain socket:
```haskell
import System.Socket
import System.Socket.Type.Stream
import System.Socket.Family.Unix

s <- socket :: IO (Socket Unix Stream Unix)
```

Creating the address for binding/connecting
```haskell
address <- case socketAddressUnixPath "example.sock" of
             Just addr -> pure addr
             Nothing -> putStrLn "invalid pathname for socket"
```

### Symlinks
Binding to a socket with a filename creates a socket in the filesystem, but does not unlink it after `close` called. You should handle deleting links yourself.

## Portability
Linux and OS X are supported.
