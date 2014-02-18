import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Char
import Data.IORef
import Data.List
import qualified Data.Map as Map
import Data.Time
import Network
import Prelude hiding (catch)
import System.Exit
import System.IO
import Text.Printf

data Bot = Bot { socket :: Handle, 
                 lastActTime :: IORef (Map.Map String UTCTime) }

type Net = ReaderT Bot IO

server = "changeme"
port = 6667
home = "#tumbleweed"
nick = "tumbleweed"
waittime = 604800

main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st = runReaderT run st

connect :: IO Bot
connect = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h LineBuffering
    noTime <- newIORef Map.empty
    return (Bot h noTime)
  where
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a

run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :a tumbleweed")
    joinChan home Nothing
    bot <- ask
    liftIO $ forkIO $ runReaderT checkToss bot
    asks socket >>= listen

checkToss :: Net ()
checkToss = forever $ do
    liftIO $ threadDelay 1000000
    timesRef <- asks lastActTime
    times <- liftIO $ readIORef timesRef
    now <- liftIO $ getCurrentTime
    mapM_ toss $ Map.keys $ 
        Map.filter (\t -> diffUTCTime now t > fromInteger waittime) times

toss :: String -> Net ()
toss chan = do
    action chan "rolls by"
    updateTime chan

joinChan :: String -> Maybe String -> Net ()
joinChan chan key = do
    maybe (write "JOIN" chan) (\k -> write "JOIN" (chan ++ k)) key
    updateTime chan

partChan :: String -> Net ()
partChan chan = do
    write "PART" $ chan
    removeTime chan

updateTime :: String -> Net ()
updateTime chan = do
    current <- liftIO getCurrentTime
    times <- asks lastActTime
    liftIO $ atomicModifyIORef times $ (\t -> (Map.insert chan current t, ()))

removeTime :: String -> Net ()
removeTime chan = do
    times <- asks lastActTime
    liftIO $ atomicModifyIORef times $ (\t -> (Map.delete chan t, ()))

listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` liftIO (hGetLine h)
    liftIO (putStrLn s)
    if ping s then pong s else eval (clean s)
  where
    clean = \s -> (
        takeWhile (/=' ') $ drop 1 $ dropWhile (/=' ') s,
        takeWhile (/=' ') $ drop 1 $ dropWhile (/=' ') $ 
            drop 1 $ dropWhile (/=' ') s,
        drop 1 $ dropWhile (/=':') $ drop 1 s)
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)

eval :: (String, String, String) -> Net ()
eval (action, chan, message) 
    | action == "PRIVMSG" && "&join " `isPrefixOf` message = 
        joinChan (takeWhile (/=' ') $ drop 6 message) $ 
            if ' ' `elem` message 
            then Just $ dropWhile (/=' ') $ drop 6 message
            else Nothing
    | action == "PRIVMSG" && "&part " `isPrefixOf` message = 
        partChan $ drop 6 message
    | action == "PRIVMSG" && "&times" `isPrefixOf` message = do
        tellTime chan
    | action == "PRIVMSG" = updateTime chan
    | otherwise = return ()

tellTime :: String -> Net ()
tellTime chan = do
    timesRef <- asks lastActTime
    times <- liftIO $ readIORef timesRef
    now <- liftIO $ getCurrentTime
    mapM_ (privmsg chan) $ 
        map (\(channel,time) -> channel ++ " " ++ show (diffUTCTime now time)) $
        Map.toList times

action :: String -> String -> Net ()
action chan message = privmsg chan $ [chr 1] ++ "ACTION " ++ message ++ [chr 1]

privmsg :: String -> String -> Net ()
privmsg chan message = write "PRIVMSG" (chan ++ " :" ++ message)

write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    liftIO $ hPrintf h "%s %s\r\n" s t
    liftIO $ printf "> %s %s\n" s t
