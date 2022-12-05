{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module CofreeBot.Bot
  ( -- * Bot
    KBot,
    Bot (..),
    invmapBot,
    mapMaybeBot,
    nudge,
    nudgeLeft,
    nudgeRight,
    (/\),
    (/.\),
    (/+\),
    (\/),
    pureStatelessBot,
    emptyBot,
    hoistBot,
    liftEffect,

    -- * Behavior
    Behavior (..),
    fixBot,
    fixBotPersistent,
    batch,

    -- * Env
    Env (..),
    fixEnv,

    -- * Server
    Server (..),
    annihilate,
    loop,

    -- * Matrix Bot
    MatrixBot,
    matrix,
    simplifyMatrixBot,
    liftSimpleBot,

    -- * Text Bot
    TextBot,
    repl,
    readState,
    saveState,
  )
where

--------------------------------------------------------------------------------

import CofreeBot.Utils
import CofreeBot.Utils.ListT
import Control.Arrow qualified as Arrow
import Control.Exception
import Control.Lens
  ( ifolded,
    view,
    (^.),
    _Just,
  )
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (Bifunctor (..))
import Data.Fix (Fix (..))
import Data.Foldable (asum)
import Data.Functor ((<&>))
import Data.Kind
import Data.Map.Strict qualified as Map
import Data.Profunctor
import Data.Profunctor.Traversing
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.These
import Network.Matrix.Client
import Network.Matrix.Client.Lens
import System.Directory (createDirectoryIfMissing)
import System.Directory.Internal.Prelude (isDoesNotExistError)
import System.FilePath
import System.IO
import System.Random

--------------------------------------------------------------------------------

type KBot = (Type -> Type) -> Type -> Type -> Type -> Type

-- | A 'Bot' maps from some input type 'i' and a state 's' to an
-- output type 'o' and a state 's'
type Bot :: KBot
newtype Bot m s i o = Bot {runBot :: s -> i -> ListT m (o, s)}
  deriving
    (Functor, Applicative, Monad, MonadState s, MonadReader i, MonadIO)
    via StateT s (ReaderT i (ListT m))

instance Functor f => Profunctor (Bot f s) where
  dimap f g (Bot bot) = do
    Bot $ \s i -> fmap (Arrow.first g) $ bot s (f i)

instance Functor f => Strong (Bot f s) where
  first' (Bot bot) = Bot $ \s (a, c) -> fmap (Arrow.first (,c)) $ bot s a

--------------------------------------------------------------------------------

-- | The fixed point of a 'Bot'.
--
-- Notice that the @s@ parameter has disapeared. This allows us to
-- hide the state threading when interpreting a 'Bot' with some 'Env'.
--
-- See 'annihilate' for how this interaction occurs in practice.
newtype Behavior m i o = Behavior {runBehavior :: i -> ListT m (o, (Behavior m i o))}

instance Functor m => Profunctor (Behavior m) where
  dimap f g (Behavior b) = Behavior $ dimap f (fmap (bimap g (dimap f g))) b

instance Monad m => Choice (Behavior m) where
  left' (Behavior b) =
    Behavior $
      either
        (fmap (bimap Left left') . b)
        (pure . (,left' (Behavior b)) . Right)

instance Functor m => Strong (Behavior m) where
  first' (Behavior b) = Behavior $ \(a, c) -> fmap (bimap (,c) first') (b a)

instance Monad m => Traversing (Behavior m) where
  -- TODO: write wander instead for efficiency

  traverse' b = Behavior $ \is ->
    fmap (uncurry (,) . fmap traverse') $
      flip runStateT b $
        traverse
          ( \i -> StateT $ \(Behavior b') ->
              fmap (\(responses, nextState) -> (responses, nextState)) $ b' i
          )
          is

-- | Generate the fixed point of @Bot m s i o@ by recursively
-- construction an @s -> Behavior m i o@ action and tupling it with
-- the output @o@ from its parent action.
fixBot :: forall m s i o. Functor m => Bot m s i o -> s -> Behavior m i o
fixBot (Bot b) = go
  where
    go :: s -> Behavior m i o
    go s = Behavior $ \i -> second go <$> b s i

-- TODO: A bi-parser typeclass
type Serializable a = (Read a, Show a)

fixBotPersistent :: forall m s i o. (MonadIO m, Serializable s) => FilePath -> Bot m s i o -> s -> IO (Behavior m i o)
fixBotPersistent cachePath (Bot bot) initialState = do
  saveState cachePath initialState
  pure go
  where
    go :: Behavior m i o
    go = Behavior $ \i ->
      liftIO (readState cachePath) >>= \case
        Nothing -> error "ERROR: Failed to read Bot State from disk."
        Just oldState -> do
          (output, newState) <- bot oldState i
          liftIO $ saveState cachePath newState
          pure (output, go)

readState :: Read s => String -> IO (Maybe s)
readState cachePath = do
  s <- readFileMaybe $ cachePath </> "state"
  pure $ fmap (read . T.unpack) s

saveState :: Show s => String -> s -> IO ()
saveState cachePath state' = do
  createDirectoryIfMissing True cachePath
  writeFile (cachePath </> "state") (show state')

-- | Batch process a list of inputs @i@ with a single 'Behavior',
-- interleaving the effects, and collecting the resulting outputs @o@.
batch :: Monad m => Behavior m i o -> Behavior m [i] o
batch (Behavior b) = Behavior $ fmap (fmap batch) . asum . fmap b

--------------------------------------------------------------------------------

-- | The dual to a 'Bot'.
-- Given the state @s@, 'Env' produces an input for a 'Bot' and given
-- the 'Bot's output @o@ will produce an updated state @s@.
--
-- Given a @Bot m s i o@, an @Env m s o i@ and an initial state @s@ we
-- can thus carry on a dialog:
--
-- 1. Initialize the 'Env' with the state @s@ to produce an input @i@
-- for the 'Bot'.
-- 2. Feeding @i@ and @s@ into the 'Bot' to produce a new state @s'@
-- and an output @o@.
-- 3. Use the new state @s'@ to produce the next bot input @i'@ and the
-- prior 'Bot' output @o@ to produce the new state @s'@.
-- 4. Repeat from step 2 with @s'@ and @i'@.
type Env :: KBot
newtype Env m s o i = Env {runEnv :: s -> m (i, [o] -> s)}
  deriving (Functor)

instance Functor m => Profunctor (Env m s) where
  dimap f g (Env env) = Env $ fmap (fmap (bimap g (lmap (fmap f)))) env

--------------------------------------------------------------------------------

-- | The fixed point of an 'Env'. Like in 'Behavior' we have factored
-- out the @s@ parameter to hide the state threading.
--
-- See 'annihilate' for how this interaction occurs in practice.
newtype Server m o i = Server {runServer :: m (i, [o] -> Server m o i)}
  deriving (Functor)

instance Functor m => Profunctor (Server m) where
  dimap f g (Server serve) =
    Server $ fmap (bimap g (dimap (fmap f) (dimap f g))) serve

-- | Generate the fixed point of @Env m s o i@ by recursively
-- construction an @s -> Server m o i@ action and tupling it with
-- the output @i@ from its parent action.
fixEnv :: forall m s o i. Functor m => Env m s o i -> s -> Server m o i
fixEnv (Env b) = go
  where
    go :: s -> Server m o i
    go s = Server $ fmap (fmap (fmap go)) $ b s

--------------------------------------------------------------------------------
-- Operations

-- | 'Bot' is an invariant functor on @s@ but our types don't quite
-- fit the @Invariant@ typeclass.
invmapBot :: Functor m => (s -> s') -> (s' -> s) -> Bot m s i o -> Bot m s' i o
invmapBot f g (Bot b) = Bot $ \s i -> (b (g s) i) <&> bimap id f

-- | Given the sum of two bots, produce a bot who receives the sum of
-- the inputs to the input bots and produces a wedge product of their
-- outputs.
nudge ::
  Monad m => Bot m s i o \/ Bot m s i' o' -> Bot m s (i \/ i') (o \*/ o')
nudge =
  either
    ( \(Bot b) -> Bot $ \s ->
        either
          (fmap (fmap (Arrow.first (Just . Left))) $ b s)
          (const $ pure $ (,) Nothing s)
    )
    ( \(Bot b) -> Bot $ \s ->
        either
          (const $ pure $ (,) Nothing s)
          (fmap (fmap (Arrow.first (Just . Right))) $ b s)
    )

-- | Nudge a bot into the left side of a bot with a summed input and
-- wedge product output.
nudgeLeft :: Monad m => Bot m s i o -> Bot m s (i \/ i') (o \*/ o')
nudgeLeft = nudge . Left

-- | Nudge a bot into the right side of a bot with a summed input and
-- wedge product output.
nudgeRight :: Monad m => Bot m s i' o' -> Bot m s (i \/ i') (o \*/ o')
nudgeRight = nudge . Right

-- | Tuple the states and outputs of two bots who operate on the same
-- input @i@.
infixr 9 /\

(/\) :: Monad m => Bot m s i o -> Bot m s' i o' -> Bot m (s /\ s') i (o /\ o')
(/\) (Bot b1) (Bot b2) = Bot $ \(s, s') i -> do
  (nextState, responses) <- b1 s i
  (nextState', responses') <- b2 s' i
  pure $ (,) (nextState, nextState') (responses, responses')

-- | Runs two bots on the same input and then interleaves their output.
infixr 9 /+\

(/+\) ::
  Monad m => Bot m s i o -> Bot m s' i o' -> Bot m (s /\ s') i (o /+\ o')
(/+\) (Bot b1) (Bot b2) = Bot $ \(s, s') i -> do
  interleaveListT (b1 s i) (b2 s' i) <&> \case
    This (o, _s) -> (This o, (s, s'))
    That (o', _s') -> (That o', (s, s'))
    These (o, _s) (o', _s') -> (These o o', (s, s'))

-- | Runs two bots on the same input and then interleaves their
-- output, sequencing if they both return an output for the same
-- input.
infixr 9 /.\

(/.\) :: Monad m => Bot m s i o -> Bot m s' i o -> Bot m (s /\ s') i o
(/.\) (Bot b1) (Bot b2) = Bot $ \(s1, s2) i -> do
  interleaveListT (b1 s1 i) (b2 s2 i) >>= \case
    This (o, s1') -> pure (o, (s1', s2))
    That (o', s2') -> pure (o', (s1, s2'))
    These (o, s1') (o', s2') -> toListT [(o, (s1', s2)), (o', (s1', s2'))]

-- | Sum the inputs and outputs of two bots who operate on the same
-- state @s@.
--
-- This allows us to combine the behaviors of two bots such that only
-- one or the other bot will be executed depending on the input
-- provided.
infixr 9 \/

(\/) :: Monad m => Bot m s i o -> Bot m s i' o' -> Bot m s (i \/ i') (o \/ o')
(\/) (Bot b1) (Bot b2) =
  Bot $ \s -> either (fmap (first Left) . b1 s) (fmap (first Right) . b2 s)

-- | Construct a 'Bot' which maps from @i@ to @o@ without using its
-- state @s@ or monadic action @m@.
pureStatelessBot :: Monad m => (i -> o) -> Bot m s i o
pureStatelessBot f = Bot $ \s i -> pure $ (,) (f i) s

mapMaybeBot :: Applicative m => (i -> Maybe i') -> Bot m s i' o -> Bot m s i o
mapMaybeBot f (Bot bot) = Bot $ \s i -> maybe emptyListT (bot s) (f i)

-- | Lift the @Monoid o@ unit value into @Bot m s i o@.
-- TODO: Remove monoid and produce an empty listT Revist all 'Monoid o' decisions.
emptyBot :: Monad m => Bot m s i o
emptyBot = Bot $ \_ _ -> emptyListT

-- | Lift a monad morphism from @m@ to @n@ into a monad morphism from
-- @Bot m s i o@ to @Bot n s i o@
hoistBot :: Functor n => (forall x. m x -> n x) -> Bot m s i o -> Bot n s i o
hoistBot f (Bot b) = Bot $ \s i -> hoistListT f $ b s i

-- | Lift a monadic effect @m o@ into a @Bot m s i o@.
liftEffect :: Monad m => m o -> Bot m s i o
liftEffect m = Bot $ \s _ -> ListT $ do
  o <- m
  pure $ ConsF (o, s) (ListT $ pure NilF)

--------------------------------------------------------------------------------

type MatrixBot m s = Bot m s (RoomID, Event) (RoomID, Event)

liftMatrixIO :: (MonadIO m, MonadError MatrixError m) => MatrixIO x -> m x
liftMatrixIO m = liftEither =<< liftIO m

readFileMaybe :: String -> IO (Maybe T.Text)
readFileMaybe path =
  fmap Just (T.readFile path)
    `catch` \e -> if isDoesNotExistError e then pure Nothing else throwIO e

-- | A Matrix 'Server' for connecting a 'Bot' to the Matrix protocol.
matrix ::
  forall m.
  (MonadError MatrixError m, MonadIO m) =>
  ClientSession ->
  FilePath ->
  Server m (RoomID, Event) [(RoomID, Event)]
matrix session cache = Server $ do
  -- Setup cache
  liftIO $ createDirectoryIfMissing True cache
  since <- liftIO $ readFileMaybe $ cache <> "/since_file"

  -- Log in
  userId <- liftMatrixIO $ getTokenOwner session
  filterId <- liftMatrixIO $ createFilter session userId messageFilter

  -- Start looping
  runServer $ go filterId since
  where
    go :: FilterID -> Maybe T.Text -> Server m (RoomID, Event) [(RoomID, Event)]
    go filterId since = Server $ do
      -- Get conversation events
      syncResult <-
        liftMatrixIO $
          sync session (Just filterId) since (Just Online) (Just 1000)

      -- Unpack sync result
      let newSince :: T.Text
          newSince = syncResult ^. _srNextBatch

          roomsMap :: Map.Map T.Text JoinedRoomSync
          roomsMap = syncResult ^. _srRooms . _Just . _srrJoin . ifolded

          roomEvents :: Map.Map T.Text [RoomEvent]
          roomEvents = roomsMap <&> view (_jrsTimeline . _tsEvents . _Just)

          events :: [(RoomID, Event)]
          events =
            Map.foldMapWithKey
              (\rid es -> fmap ((RoomID rid,) . view _reContent) es)
              roomEvents

      pure $
        (events,) $ \outputs -> Server $ do
          -- Send the bot's responses
          gen <- newStdGen
          let txnIds = TxnID . T.pack . show <$> randoms @Int gen
          liftIO $ zipWithM_ (uncurry $ sendMessage session) outputs txnIds

          -- Update since file
          liftIO $ writeFile (cache <> "/since_file") (T.unpack newSince)

          -- Do it again
          runServer $ go filterId (Just newSince)

-- | Map the input and output of a 'MatrixBot' to allow for simple
-- 'T.Text' I/O.
simplifyMatrixBot :: Monad m => MatrixBot m s -> TextBot m s
simplifyMatrixBot (Bot bot) = Bot $ \s i -> do
  (responses, nextState) <- bot s (RoomID mempty, mkMsg i)
  pure (viewBody $ snd responses, nextState)

liftSimpleBot :: Functor m => TextBot m s -> MatrixBot m s
liftSimpleBot (Bot bot) = Bot $ \s (rid, i) ->
  fmap (\(i', s') -> ((rid, mkMsg i'), s')) $ bot s (viewBody i)

viewBody :: Event -> T.Text
viewBody = view (_EventRoomMessage . _RoomMessageText . _mtBody)

mkMsg :: T.Text -> Event
mkMsg msg =
  EventRoomMessage $ RoomMessageText $ MessageText msg TextType Nothing Nothing

--------------------------------------------------------------------------------
-- Text Bot

-- | A 'SimpleBot' maps from 'Text' to '[Text]'. Lifting into a
-- 'SimpleBot' is useful for locally debugging another bot.
type TextBot m s = Bot m s T.Text T.Text

-- | A repl-style 'Server' for interacting with a 'Bot'.
repl :: Server IO T.Text T.Text
repl = Server $ do
  -- Read the user's input
  liftIO $ do
    putStr "<<< "
    hFlush stdout
  (T.pack -> input) <- liftIO getLine

  pure $
    (input,) $ \outputs -> Server $ do
      forM_ outputs $ \output -> do
        -- Print the bot's responses
        liftIO $ putStrLn $ T.unpack $ ">>> " <> output

      -- Do it again
      runServer repl

-- | Collapse a @Server m o i@ with a @Bahavior m i o@ to create a
-- monadic action @m@.
annihilate :: Monad m => Server m o i -> Behavior m i o -> Fix m
annihilate (Server server) b@(Behavior botBehavior) = Fix $ do
  (i, nextServer) <- server
  xs <- fromListT $ botBehavior i
  let o = fmap fst $ xs
      server' = nextServer o
  pure $
    annihilate server' $ case xs of
      [] -> b
      _ -> snd $ last xs

loop :: Monad m => Fix m -> m x
loop (Fix x) = x >>= loop
