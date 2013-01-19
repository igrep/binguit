{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Char8 (ByteString)
import Data.Conduit.Network

import Data.Char (isDigit)

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
  -- TODO: get Ints by promptInput before CL.map
  =$= CL.map ( withNL . C8.pack . show )
  =$= promptInput

promptInput :: Conduit ByteString IO ByteString
promptInput = do
  mbs <- await
  liftIO $ putStr "Press Enter, input a number (1..75) or Q to quit: "
  input <- liftIO getLine
  case input of
       ('Q':_) -> liftIO $ putStrLn "Bye."
       _ -> do
         if all isDigit input
           then yield $ C8.pack input
           else case mbs of
                     Just bs -> yield bs
                     Nothing -> do
                       liftIO $ putStrLn "No number given!"
                       promptInput

withNL :: ByteString -> ByteString
withNL s = C8.append s "\n"
