{-# LANGUAGE OverloadedStrings #-}

import Data.Conduit
import Data.Conduit.Network
import qualified Data.Conduit.Binary as CB

import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Char8 (ByteString)

import Control.Concurrent (threadDelay)

import System.Environment (getArgs)
import System.Random (getStdGen)
import Control.Monad.IO.Class

import qualified BingoGame
import BingoGame (BingoGame)

main :: IO ()
main = do
  host:port:_ <- getArgs
  g <- liftIO getStdGen
  let bingo = BingoGame.newGame g
  runTCPClient (ClientSettings (read port) host) (bingoClient bingo)

bingoClient :: BingoGame -> Application IO
bingoClient bingo src sink = do
  putStrLnL "Your Initial Bingo Sheet:"
  printL bingo
  src $$ CB.lines =$= readAsInts =$= binguit bingo =$ sink

binguit :: BingoGame -> Conduit Int IO ByteString
binguit bingo = do
  putStrLnL "Press Enter to get the next number, or enter 'Q' to quit"
  command <- liftIO C8.getLine
  if "Q" `C8.isPrefixOf` command
    then return ()
    else do
      mn <- await
      case mn of
           Nothing ->
             putStrLnL "Connection closed!"
           Just n -> do
             putStrLnL $ "Got " ++ show n ++ ". Can any cell marked...?"
             liftIO $ threadDelay sec
             let bingo' = BingoGame.markCell n bingo
             putStrLnL $ BingoGame.debug bingo'
             printL bingo'
             binguit bingo'
  where
    sec = ( 1000 )

putStrLnL :: MonadIO m => String -> m ()
putStrLnL = liftIO . putStrLn

printL :: (MonadIO m, Show a) => a -> m ()
printL = liftIO . print

readAsInts :: Conduit ByteString IO Int
readAsInts =
  awaitForever $ yield . read . C8.unpack
