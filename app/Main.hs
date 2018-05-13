module Main where

import Item

main :: IO ()
main = do
  putStrLn "TODO app"
  let initialList = []
  interactWithUser initialList

data Command
  = Quit
  | DisplayItems
  | AddItem String
  | RemoveItem Int
  | Help

parseInt :: String -> Maybe Int
parseInt str = 
    if all (\c -> elem c "0123456789") str
      then Just (read str)
      else Nothing

head' xs = case xs of 
    [] -> error "No head for empty lists!"  
    (x:_) -> x  

parseCommand :: String -> Either String Command
parseCommand line = case words line of
  ["quit"] -> Right Quit
  ["list"] -> Right DisplayItems
  ["help"] -> Right Help
  "add" : item -> Right (AddItem (unwords item))
  ["remove", idxStr] ->
    case parseInt idxStr of Nothing -> Left "Invalid index."
                            Just idx -> Right (RemoveItem idx)
  _ -> Left "Unknown command."

interactWithUser :: Items -> IO ()
interactWithUser items = do
  line <- getLine
  case parseCommand line of
    Right DisplayItems -> do
      putStrLn "The List of items is:"
      putStrLn (displayItems items)
      interactWithUser items

    Right (AddItem item) -> do
      let newItems = addItem (Item item) items
      putStrLn "Item added."
      interactWithUser newItems

    Right Help -> do
      putStrLn "Commands: help, quit, items, add - <item to add>"
      interactWithUser items

    Right Quit -> do
      putStrLn "Bye!"
      pure ()

    Left errMsg -> do
      putStrLn ("Error: " ++ errMsg)
      interactWithUser items