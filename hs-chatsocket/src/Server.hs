module Server (server) where

import Data.Char (isPunctuation, isSpace)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forever, void, when)
import Data.Maybe (isJust)
import Control.Concurrent ( MVar, newMVar, modifyMVar_, modifyMVar, readMVar, forkIO )
import  Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Control.Monad.Extra (whenJustM)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import qualified Network.WebSockets as WS

-- We represent a client by their username and a `WS.Connection`. We will see how we
-- obtain this `WS.Connection` later on.

type Client = (Text, WS.Connection)

-- The state kept on the server is simply a list of connected clients. We've added
-- an alias and some utility functions, so it will be easier to extend this state
-- later on.

type ServerState = Map Text WS.Connection

-- Create a new, initial state:

newServerState :: ServerState
newServerState = Map.empty

-- Get the number of active clients:

numClients :: ServerState -> Int
numClients = Map.size

lookupConn :: Text -> ServerState -> Maybe WS.Connection
lookupConn = Map.lookup

-- Check if a user already exists (based on username):

clientExists :: Client -> ServerState -> Bool
clientExists (user, _) = isJust . lookupConn user

-- Add a client (this does not check if the client already exists, you should do
-- this yourself using `clientExists`):

addClient :: Client -> ServerState -> ServerState
addClient (user, conn) clients = case Map.lookup user clients of
  Nothing -> Map.insert user conn clients
  Just _ -> clients

-- Remove a client:

removeClient :: Client -> ServerState -> ServerState
removeClient (user,_) = Map.delete user

-- Send a message to all clients, and log it on stdout:

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    TIO.putStrLn message
    void $ flip Map.traverseWithKey clients $ \_ conn -> WS.sendTextData conn message

-- The server function first creates a new state for the server, then spawns the
-- actual server. For this purpose, we use the simple server provided by
-- `WS.runServer`.

server :: IO ()
server = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state

-- Our main application has the type:

application :: MVar ServerState -> WS.ServerApp

-- Note that `WS.ServerApp` is nothing but a type synonym for
-- `WS.PendingConnection -> IO ()`.

-- Our application starts by accepting the connection. In a more realistic
-- application, you probably want to check the path and headers provided by the
-- pending request.

-- We also fork a pinging thread in the background. This will ensure the connection
-- stays alive on some browsers.

application state pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) $ do

-- When a client is succesfully connected, we read the first message. This should
-- be in the format of "Hi! I am Jasper", where Jasper is the requested username.

        msg <- WS.receiveData conn
        clients <- readMVar state
        case msg of

-- Check that the first message has the right format:

            _   | not (prefix `T.isPrefixOf` msg) ->
                    WS.sendTextData conn ("Wrong announcement of new client!" :: Text)

-- Check the validity of the username:

                | any ($ fst client)
                    [T.null, T.any isPunctuation, T.any isSpace] ->
                        WS.sendTextData conn ("Name cannot " <>
                            "contain punctuation or whitespace, and " <>
                            "cannot be empty" :: Text)

-- Check that the given username is not already taken:

                | clientExists client clients ->
                    WS.sendTextData conn ("User already exists" :: Text)

-- All is right! We're going to allow the client, but for safety reasons we *first*
-- setup a `disconnect` function that will be run when the connection is closed.

                | otherwise -> flip finally disconnect $ do

-- We send a "Welcome!", according to our own little protocol. We add the client to
-- the list and broadcast the fact that he has joined. Then, we give control to the
-- 'talk' function.

                   modifyMVar_ state $ \s -> do
                       let s' = addClient client s
                       WS.sendTextData conn $
                           "Admin: Welcome! Users: " <>
                           T.intercalate ", " (Map.keys s')
                       broadcast ("Admin: " <> fst client <> " joined") s'
                       return s'
                   talk client state
             where
               prefix     = "Hi! I am "
               client     = (T.drop (T.length prefix) msg, conn)
               disconnect = do
                   -- Remove client and return new state
                   s <- modifyMVar state $ \s -> do
                       let s' = removeClient client s
                       pure (s', s')
                   broadcast (fst client <> " disconnected") s

-- The talk function continues to read messages from a single client until he
-- disconnects. All messages are broadcasted to the other clients.

space :: Text
space = " "

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = do
  st <- readMVar state
  trips <- newMVar Map.empty
  let acknowledge = "Message received:"

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
          WS.sendTextData conn $ acknowledge <> space <> msg
          TIO.putStrLn $ user <> ": " <> msg

    -- Read from stdin and write to WS
  let loop = do
        line <- TIO.getLine
        whenJustM (parseInput line st) $ \(c, l) -> do
          WS.sendTextData c l
          startTime <- getCurrentTime
          modifyMVar_ trips $ pure . Map.insert (T.take 10 l) startTime
          loop

  loop


parseInput :: Text -> ServerState -> IO (Maybe (WS.Connection, Text))
parseInput txt st = do
  let user = T.strip $ T.takeWhile (/= '#') txt
  let message = T.strip $ T.dropWhile (== '#') $ T.dropWhile (/= '#') txt
  if T.null message
    then TIO.putStrLn "Empty message" >> pure Nothing
    else case lookupConn user st of
      Nothing -> TIO.putStrLn "Invalid user's name" >> pure Nothing
      Just conn -> pure $ Just (conn, message)


