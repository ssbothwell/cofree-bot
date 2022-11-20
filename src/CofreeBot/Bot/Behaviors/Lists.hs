-- | A Bot Behavior for managing lists
module CofreeBot.Bot.Behaviors.Lists
  ( listsBot,
  )
where

--------------------------------------------------------------------------------

import CofreeBot.Bot
import CofreeBot.Utils (indistinct)
import Control.Applicative
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

data ListAction = CreateList T.Text | ModifyList T.Text ListItemAction | RemoveList T.Text | List T.Text

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
  RemoveList name -> pure ("List deleted", Map.delete name s)
  List name -> pure (T.pack $ show $ Map.lookup name s, s)

listsBot :: Monad m => Bot m (Map T.Text (IntMap T.Text)) T.Text T.Text
listsBot = dimap (parseOnly parseListAction) indistinct $ emptyBot \/ listsBot'

parseListAction :: Parser ListAction
parseListAction =
  parseCreateList <|> parseModifyList <|> parseRemoveList <|> parseListList
  where
    parseCreateList = "create" *> skipSpace *> fmap CreateList (takeTill isSpace)
    parseModifyList =
      "add"
        *> skipSpace
        *> fmap ModifyList (takeTill isSpace)
        <*> fmap Insert takeText
    parseRemoveList =
      "remove"
        *> skipSpace
        *> fmap ModifyList (takeTill isSpace)
        <*> fmap Remove decimal
    parseListList = "show" *> skipSpace *> fmap List takeText
