module Item where

data Item = Item String
type Items = [Item]

-- Returns a new list of Items with the new item in it
addItem :: Item -> Items -> Items
addItem item items = item : items

-- Returns a string representation of the items
displayItems :: Items -> String
displayItems items =
  let
    displayItem index (Item itemStr) = show index ++ " - " ++ itemStr
    reversedList = reverse items
    displayedItemsList = zipWith displayItem [1..] reversedList
  in
    unlines displayedItemsList

-- Returns a new list of items or an error message if the index is out of bounds
removeItem :: Int -> Items -> Either String Items
removeItem reverseIndex allItems =
    impl (length allItems - reverseIndex) allItems
  where
    impl index items =
      case (index, items) of
        (0, item : rest) ->
          Right rest
        (n, []) ->
          Left "Index out of bounds."
        (n, item : rest) ->
          case impl (n - 1) rest of
            Right newItems ->
              Right (item : newItems)
            Left errMsg ->
              Left errMsg

