-- | A Bot Behavior for managing todo lists
module CofreeBot.Bot.Behaviors.Lists
  ( listBot,
    TodoAction (..),
  )
where

--------------------------------------------------------------------------------

import CofreeBot.Bot
import CofreeBot.Utils (indistinct)
import Control.Applicative
import Data.Attoparsec.Text
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Profunctor
import Data.Text qualified as T

--------------------------------------------------------------------------------

data TodoAction = Create T.Text | Modify Int T.Text | Remove Int | List

listBot' :: Monad m => Bot m (IntMap T.Text) TodoAction T.Text
listBot' = Bot $ \s -> \case
  Create todo ->
    let k = freshKey s in pure ("Entry added", IntMap.insert k todo s)
  Modify k todo -> pure ("Entry updated", IntMap.insert k todo s)
  Remove k -> pure ("Entry deleted", IntMap.delete k s)
  List -> pure (T.pack $ show s, s)

freshKey :: IntMap a -> Int
freshKey state = case IntMap.lookupMax state of
  Nothing -> 0
  Just (k, _) -> k + 1

listBot :: Monad m => Bot m (IntMap T.Text) T.Text T.Text
listBot = dimap (parseOnly parseAction) indistinct $ emptyBot \/ listBot'

parseAction :: Parser TodoAction
parseAction = parseAdd <|> parseRemove <|> parseModify <|> parseList
  where
    parseAdd = "create-todo:" *> skipSpace *> fmap Create takeText
    parseRemove = "remove-todo:" *> skipSpace *> fmap Remove decimal
    parseModify =
      "modify-todo:" *> skipSpace *> fmap (Modify) decimal <* space <*> takeText
    parseList = "list-todos" *> pure List
