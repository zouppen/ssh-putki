module Main where

import System.Environment
import Network.Socket
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Builder
import Data.Monoid
import System.IO
import Control.Monad
import Data.Word
import SendFd
import Control.Concurrent (threadDelay)

main = do
  [controlMasterPath, host, port] <- getArgs

  -- Open ControlMaster socket
  s <- socket AF_UNIX Stream defaultProtocol
  connect s $ SockAddrUnix controlMasterPath
  h <- socketToHandle s ReadWriteMode

  -- Say hello
  hPutBuilder h $ sshPacket hello
  serverHello <- getSshPacket h
  unless (serverHello == toLazyByteString hello) $ fail "Invalid protocol version"

  -- Stdio forwarding
  hPutBuilder h $ sshPacket $ stdioFwd host (read port)
--  sendFd s 0
--  sendFd s 1
  sendFd1 s stdin
  sendFd1 s stdout

  -- Wait for a moment to allow playing with the socket
  threadDelay 10000000

sshPacket :: Builder -> Builder
sshPacket b = let payload = toLazyByteString b
              in  word32BE (fromIntegral $ BL.length payload) <>
                  lazyByteString payload

hello :: Builder
hello = word32BE 0x00000001 <> -- MUX_MSG_HELLO
        word32BE 4

stdioFwd :: String -> Word32 -> Builder
stdioFwd host port =
  let hostRaw = toLazyByteString $ string7 host
  in  word32BE 0x10000008 <>                         -- MUX_C_NEW_STDIO_FWD
      word32BE 13 <>                                 -- request ID TODO
      word32BE 0 <>                                  -- reserved
      word32BE (fromIntegral $ BL.length hostRaw) <> -- host length
      lazyByteString hostRaw <>                      -- host
      word32BE port                                  -- port

getSshPacket :: Handle -> IO BL.ByteString
getSshPacket h = do
  len <- runGet getWord32be <$> BL.hGet h 4
  BL.hGet h $ fromIntegral len
