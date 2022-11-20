-- | A Bot Behavior for managing lists
module CofreeBot.Bot.Behaviors.Lists
  ( listsBot,
  )
where

-- TODO: I want a bot combinator ala Sessionize that allows you take
-- any bot and save its state to disk on updates.

--------------------------------------------------------------------------------

import CofreeBot.Bot
import CofreeBot.Utils (indistinct)
import Control.Applicative
import Control.Monad (void)
import Data.Attoparsec.ByteString.Char8
  ( isSpace,
  )
import Data.Attoparsec.Text
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Profunctor
import Data.Text qualified as T

--------------------------------------------------------------------------------

data ListItemAction = Insert T.Text | Modify Int T.Text | Remove Int

data ListAction = CreateList T.Text | ModifyList T.Text ListItemAction | DeleteList T.Text | ShowList T.Text

listItemBot :: Monad m => Bot m (IntMap T.Text) ListItemAction T.Text
listItemBot = Bot $ \s -> \case
  Insert todo ->
    let k = freshKey s in pure ("Entry added", IntMap.insert k todo s)
  Modify k todo -> pure ("Entry updated", IntMap.insert k todo s)
  Remove k -> pure ("Entry deleted", IntMap.delete k s)

freshKey :: IntMap a -> Int
freshKey state = case IntMap.lookupMax state of
  Nothing -> 0
  Just (k, _) -> k + 1

listsBot' :: Monad m => Bot m (Map T.Text (IntMap T.Text)) ListAction T.Text
listsBot' = Bot $ \s -> \case
  CreateList name -> pure ("List Created", Map.insert name mempty s)
  ModifyList name action -> do
    let t = fromMaybe IntMap.empty $ Map.lookup name s
    t' <- fmap snd $ runBot listItemBot t action
    pure ("List Updated", Map.insert name t' s)
  DeleteList name -> pure ("List deleted", Map.delete name s)
  ShowList name -> pure (T.pack $ show $ Map.lookup name s, s)

listsBot :: Monad m => Bot m (Map T.Text (IntMap T.Text)) T.Text T.Text
listsBot = dimap (parseOnly parseListAction) indistinct $ emptyBot \/ listsBot'

parseListAction :: Parser ListAction
parseListAction =
  parseCreateList
    <|> parseDeleteList
    <|> parseAddListItem
    <|> parseRemoveListItem
    <|> parseUpdateListItem
    <|> parseShowList
  where
    parseName = ("📝" <|> "list") *> skipSpace *> takeTill isSpace
    parseCreateList = do
      name <- parseName
      skipSpace
      void $ "➕" <|> "create"
      skipSpace
      pure (CreateList name)
    parseDeleteList = do
      name <- parseName
      skipSpace
      void $ "➖" <|> "delete"
      skipSpace
      pure (DeleteList name)
    parseShowList = do
      name <- parseName
      pure (ShowList name)
    parseAddListItem = do
      name <- parseName
      skipSpace
      void ("📝" <|> "add")
      skipSpace
      item <- takeText
      pure (ModifyList name (Insert item))
    parseRemoveListItem = do
      name <- parseName
      skipSpace
      void ("✔️" <|> "remove")
      skipSpace
      key <- decimal
      pure (ModifyList name (Remove key))
    parseUpdateListItem = do
      name <- parseName
      skipSpace
      void "update"
      skipSpace
      key <- decimal
      skipSpace
      item <- takeText
      pure (ModifyList name (Modify key item))
