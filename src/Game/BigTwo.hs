{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Game.BigTwo where


import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors (Request(..))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Function (on)
import Game.Implement.Card.Standard
import Numeric.Natural


import Game.BigTwo.Card
import Game.BigTwo.Hand


import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M


-- The game state always represents the phase right before the turn player is
-- supposed to play their hand.
data GameState = GameState {
    _hands :: M.Map PlayerID [BTCard] -- Implicitly contains # remaining players
  , _trick :: [(HandType, [BTCard])] -- The current pile
  , _trickOwner :: PlayerID -- Current initiator of the trick
  , _players    :: [PlayerID] -- Players still active; head is current player
  , _finished :: [PlayerID] -- The list of players that've finished so far
  } deriving (Show, Eq, Ord)


nextPlayer :: GameState -> GameState
nextPlayer = undefined


type PlayerID = Natural


data GameEnv = GameEnv {
    _totalPlayers :: Natural
  }


data Move = Pass | Play (NE.NonEmpty [BTCard])
  deriving (Show, Eq, Ord)


data IllegalMoveError =
    NotAPokerHand String
  | NotHighEnough
  | BadHandType 
  deriving (Show, Eq, Ord)


maybeDone :: GameState -> Maybe [PlayerID]
maybeDone GameState{..} = if alone _players then Just _finished else Nothing
  where
    alone [_] = True
    alone _   = False

--
-- The damn engine.
--


-- Game monad transformer.
-- XXX: GameM m a ~ s -> m (Either e (r -> a), s)
newtype GameM m a =
  GameM { unGameM :: ReaderT GameEnv
                        (ExceptT IllegalMoveError
                          (StateT GameState m)) a }
  deriving (Functor, Applicative, Monad, MonadReader GameEnv,
            MonadState GameState, MonadIO, MonadError IllegalMoveError)


-- XXX: This wasn't derivable for some reason...?
instance MonadTrans GameM where
  lift = GameM . lift . lift . lift


-- XXX: 'v' for 'view'
-- Coroutine monad transformer, for suspendable/resumable games.
type GameC v m a = Coroutine (Request v Move) (GameM m) a


-- The game result is the list of players who finished, in reverse order
-- (most recent is at head)
type GameResult = [PlayerID]


allPlayGame :: GameC v m GameResult
allPlayGame = do
  undefined


runGameM :: Monad m =>
  GameEnv ->
  GameState ->
  GameM m a ->
  m (Either IllegalMoveError a, GameState)
runGameM env initState gameM =
  flip runStateT initState $ runExceptT $ flip runReaderT env $ unGameM gameM


runGameC :: Monad m =>
  (v -> m Move) ->
  GameC v m a ->
  GameM m a
runGameC getMove = pogoStick (respondWithM (lift . getMove))


respondWithM :: Monad m =>
  (req -> m resp) ->
  Request req resp (Coroutine (Request req resp) m a) ->
  Coroutine (Request req resp) m a
respondWithM getResp (Request req cont) = lift (getResp req) >>= cont
