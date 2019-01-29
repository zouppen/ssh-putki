{-# LANGUAGE ForeignFunctionInterface #-}
module SendFd (sendFd1) where
import Foreign.C
import Network.Socket
import System.IO
import System.Posix.IO
import System.Posix.Types
 
foreign import ccall "send_fd" send_fd_raw :: CInt -> CInt -> IO CInt

-- |Sends given Handle to another process via SCM_RIGHTS. This differs
-- from sendFd from Network.Socket in a way it sends only 1 byte
-- payload and is SSH ControlMaster compatible.
sendFd1 :: Socket -> Handle -> IO ()
sendFd1 s h = do
  let sFd = fdSocket s
  Fd hFd <- handleToFd h
  send_fd_raw sFd hFd
  return ()
