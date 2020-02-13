{-# LANGUAGE TupleSections #-}
import qualified Control.Monad
import qualified Data.List                     as L
import qualified Data.Map                      as M
import qualified Data.Maybe                    as Maybe

newtype StateM st a = StateM (st -> (st,a))

instance Functor (StateM st) where
    fmap = Control.Monad.liftM

instance Applicative (StateM st) where
    pure  = Control.Monad.return
    (<*>) = Control.Monad.ap

instance Monad (StateM st) where
    return a = StateM (, a)
    StateM run >>= action = StateM run'
      where
        run' st =
            let (st', a)     = run st
                StateM run'' = action a
            in  run'' st'


getStateM :: StateM state state
getStateM = StateM (\state -> (state, state))

putStateM :: st -> StateM st ()
putStateM new = StateM (const (new, ()))

runStateM :: StateM st a -> st -> (st, a)
runStateM (StateM f) = f

data ComputerState = Done | WaitingForInput | Running deriving (Show, Eq)

data Computer = Computer {
    _getM :: M.Map Int Int,
    _getInstr :: Int,
    _getInputs :: [Int],
    _getOutputs :: [Int],
    _getState :: ComputerState,
    _getRelativeBase :: Int
} deriving (Show)

getM = _getM <$> getStateM
getInstr = _getInstr <$> getStateM
getInputs = _getInputs <$> getStateM
getOutputs = _getOutputs <$> getStateM
getState = _getState <$> getStateM
getRelativeBase = _getRelativeBase <$> getStateM







-- [getM, getInstr, getInput, getOutputs, getState, getRelativeBase] =
--     Control.Monad.sequence
--         [_getM, _getInstr, _getInputs, _getOutputs, _getState, _getRelativeBase]
--         (\x -> getStateM >>= return . x)

defaultComputer = Computer { _getInstr        = 0
                           , _getInputs       = []
                           , _getOutputs      = []
                           , _getState        = Running
                           , _getRelativeBase = 0
                           , _getM            = M.empty
                           }

data Mode = Position | Immediate | Relative deriving (Enum, Show, Eq)


getVal x m = Maybe.fromMaybe 0 $ M.lookup x m

getCurrInstr :: StateM Computer Int
getCurrInstr = do
    m <- getM
    i <- getInstr
    return $ getVal i m

parseOpcode opcode = (op, modes)
  where
    op = opcode `mod` 100
    modes_as_int =
        map read $ reverse $ map (: []) $ show $ opcode `div` 100 :: [Int]
    modes = map (\x -> toEnum x :: Mode) modes_as_int

getOpcode = parseOpcode <$> getCurrInstr

getInstrInputs modes n = do
    start <- (+) 1 <$> getInstr
    seq   <- getM
    rel   <- getRelativeBase
    let get_val v mode = case mode of
            Position  -> getVal v seq
            Immediate -> v
            Relative  -> getVal (v + rel) seq
    return
        $ [ get_val v mode
          | (v, mode) <- zip
              (map (\i -> getVal i seq) [start .. (n + start - 1)])
              (modes ++ repeat Position)
          ]

getWritePos pos mode = do
    m <- getM
    r <- getRelativeBase
    case mode of
        Position  -> return $ getVal pos m
        Relative  -> return $ (getVal pos m) + r
        otherwise -> error "immediate mode for writing parameter doesn't work"

performBinaryOpcode modes op = do
    i <- getInstr
    m <- getM
    let write_mode = (modes ++ repeat Position) !! 2
    write_at     <- getWritePos (i + 3) write_mode
    vs <- getInstrInputs modes 2
    let [v1,v2] = vs
    c            <- getStateM
    putStateM $ c { _getM     = M.insert write_at (fromEnum (op v1 v2)) m
                  , _getInstr = i + 4
                  }


initCom = putStateM

main = do
    print 1
