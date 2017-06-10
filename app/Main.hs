module Main where

import           Control.Monad.State.Lazy
import           Data.Char                (isDigit, toLower)
import           Data.List
import           System.IO
import           System.Console.ANSI

type TodoList = [String]

data Command = Quit
             | Help
             | List
             | Add { item :: Maybe String }
             | Remove { index :: Maybe Int }
             | Save { filePath :: Maybe String }
             | Load { filePath :: Maybe String }
             deriving (Show, Eq)

-- Splits word when conditions are met
--
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
    case dropWhile p s of
      "" -> []
      s' -> w : wordsWhen p s''
            where (w, s'') = break p s'

-- Checks if string only contains digits
strOnlyDigits :: String -> Maybe Int
strOnlyDigits s = if all isDigit s then Just (read s :: Int) else Nothing

-- Maybe Tail
maybeTail :: String -> Maybe String
maybeTail s = if null s then Nothing else Just (tail s)

-- Tokenize input
tokenize :: String -> Maybe Command
tokenize s = cmd'
    where (cmd, args) = break (==':') s
          -- Gets rid of the colon (if any)
          args' = maybeTail args
          cmd' = case map toLower cmd of
                   "quit"   -> Just Quit
                   "help"   -> Just Help
                   "remove" -> Just Remove { index = args' >>= strOnlyDigits }
                   "add"    -> Just Add { item = args' }
                   "list"   -> Just List
                   "save"   -> Just Save { filePath = args' }
                   "load"   -> Just Load { filePath = args' }
                   _        -> Nothing

-- Removes index from todolist
rmTDLItem :: Int -> TodoList -> TodoList
rmTDLItem i x = take i x ++ drop (i+1) x

-- TodoList to String
--
tdl2Str :: Int -> TodoList -> String
tdl2Str i [] = []
tdl2Str i x = if null x' then tdl2Str i xs' else (show i ++ ": " ++ x' ++ "\n") ++ tdl2Str (i + 1) xs'
    where x'  = head x
          xs' = tail x

-- Prompt User correct command
--
promptCorrectCommand :: String -> IO ()
promptCorrectCommand s = putStrLn $ "Command needs to be in format " ++ s ++ ""


-- Indefinitely queries until
-- user types 'quit'
indefiniteParse :: StateT TodoList IO ()
indefiniteParse = do
    -- Gets the previous todo list
    tdl <- get

    -- Gets user input
    c <- liftIO getLine

    -- Clears screen
    -- sets cursor to 0,0
    liftIO clearScreen
    liftIO $ setCursorPosition 0 0

    case tokenize c of
      (Just Quit)       -> return ()
      (Just List)       -> liftIO $ putStrLn (tdl2Str 0 tdl)
      (Just (Remove i)) -> case i of
                             Nothing   -> liftIO $ promptCorrectCommand "`remove:<index>`"
                             (Just i') -> do put $ rmTDLItem i' tdl
                                             liftIO $ putStrLn $ "removed item: " ++ show i'
      (Just (Add s))    -> case s of
                             Nothing   -> liftIO $ promptCorrectCommand "`add:<string>`"
                             (Just s') -> do put $ tdl ++ [s']
                                             liftIO $ putStrLn $ "added item: " ++ s'
      (Just (Save s))   -> case s of
                             Nothing   -> liftIO $ promptCorrectCommand "`save:<filepath>`"
                             (Just s') -> do liftIO $ writeFile s' (intercalate "\n" tdl)
                                             liftIO $ putStrLn $ "saved file to: " ++ s'
      (Just (Load s))   -> case s of
                             Nothing   -> liftIO $ promptCorrectCommand "`load:<filepath>`"
                             (Just s') -> do tdl' <- liftIO $ readFile s'
                                             put $ wordsWhen (=='\n') tdl'
      (Just Help)       -> do liftIO $ putStrLn "quit"
                              liftIO $ putStrLn "help"
                              liftIO $ putStrLn "remove:<index>"
                              liftIO $ putStrLn "add:<string>"
                              liftIO $ putStrLn "list"
                              liftIO $ putStrLn "save:<filepath>"
                              liftIO $ putStrLn "load:<filepath>"
      Nothing           -> liftIO $ putStrLn "Unrecognized command, type help for more info"

    if tokenize c == Just Quit then return () else indefiniteParse

main :: IO ()
main = do
    -- Clear screen and allow
    -- the use of arrow keys
    -- and backspace
    hSetBuffering stdin LineBuffering
    clearScreen
    setCursorPosition 0 0
    putStrLn "--- Welcome to Todo List, in Haskell v0.0.1, type 'help' for help ---"
    void $ runStateT indefiniteParse ([""] :: TodoList)
