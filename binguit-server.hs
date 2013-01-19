{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Char8 (ByteString)
import Data.Conduit.Network

import System.Random (getStdGen)
import System.Environment (getArgs)
import Control.Monad.IO.Class (liftIO)

import qualified BingoGame

main :: IO ()
main = do
  port:_ <- getArgs
  runTCPServer (ServerSettings (read port) HostAny) app

app :: Application IO
app src sink = src $$ conduit =$ sink

conduit :: Conduit ByteString IO ByteString
conduit = do
  g <- liftIO getStdGen
  CL.sourceList (BingoGame.chooseRandomNumbers g )
  =$= CL.map ( withNL . C8.pack . show )

withNL :: ByteString -> ByteString
withNL s = C8.append s "\n"
