module Client
    ( client
    ) where

import Control.Concurrent ( forkIO, newMVar, modifyMVar_ )
import           Control.Monad       (forever, unless, void)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Network.WebSockets  as WS
import Data.Map.Strict qualified as Map
import Data.Time.Clock (getCurrentTime, diffUTCTime)

space :: Text
space = " "

admin :: Text
admin = "Admin: "

app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"
    trips <- newMVar Map.empty
    let acknowledge = "Message received:"


    -- Fork a thread that writes WS data to stdout
    void $ forkIO $ forever $ do
        msg <- WS.receiveData conn
        endTime <- getCurrentTime
        if T.isPrefixOf acknowledge msg
            then do
              case T.take 10 <$> T.stripPrefix (acknowledge <> space) msg of
                Nothing -> TIO.putStrLn "Invalid key of roundtrip"
                Just key -> do
                  modifyMVar_ trips $ \t -> do
                    case Map.lookup key t of
                      Nothing -> do
                        TIO.putStrLn "There is not key in the roundtrips"
                        pure t
                      Just start -> do
                        TIO.putStrLn
                          $ "Roundtrip for message: "
                          <> key
                          <> "... is "
                          <> T.pack (show $ diffUTCTime endTime start)
                        pure $ Map.delete key t


            else do
              unless (T.isPrefixOf admin msg) $ do
                WS.sendTextData conn $ acknowledge <> space <> msg
              TIO.putStrLn $ "Server: " <> msg
        -- WS.sendTextData conn $ "Message received: " <> msg

    -- Read from stdin and write to WS
    let loop = do
            line <- TIO.getLine
            unless (T.null line) $ do
              WS.sendTextData conn line
              startTime <- getCurrentTime
              modifyMVar_ trips $ pure . Map.insert (T.take 10 line) startTime
              loop

    loop
    WS.sendClose conn ("Bye!" :: Text)


client :: IO ()
client = withSocketsDo $ WS.runClient "127.0.0.1" 9160 "/" app
