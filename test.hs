{-# LANGUAGE TupleSections #-}
import qualified Data.List                     as L
import qualified Control.Monad

-- lets implement a state monad
type State st a = st -> (st, a)

returnState :: a -> State st a
returnState a st = (st, a)

bindState :: State st a -> (a -> State st b) -> State st b
bindState m k st =
    let (st', a) = m st
        m'       = k a
    in  m' st'

addOneAndCountCalls :: Num a => a -> State Int a
addOneAndCountCalls x ctr = returnState (x + 1) (ctr + 1)

transformElementsM :: (a -> State st b) -> [a] -> State st [b]
transformElementsM f []       = returnState []
transformElementsM f (a : as) = f a `bindState` \b ->
    transformElementsM f as `bindState` \res -> returnState (b : res)

type State' = Int

transformElements
    :: (a -> State' -> (State', b)) -> [a] -> State' -> (State', [b])
transformElements _ [] st = (st, [])
transformElements f (a : as) st =
    let (st', b) = f a st
    in  let (st'', res) = transformElements f as st' in (st'', b : res)


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

addOneAndCountCallsM :: Num a => a -> StateM Int a
addOneAndCountCallsM a = do
    getStateM >>= return . (+) 1 >>= putStateM
    return (a + 1)

transformElementsM_ :: Monad m => (a -> m b) -> [a] -> m [b]
transformElementsM_ f []       = return []
transformElementsM_ f (a : as) = do
    b   <- f a
    res <- transformElementsM_ f as
    return $ b : res

getStateM :: StateM state state
getStateM = StateM (\state -> (state, state))

putStateM :: st -> StateM st ()
putStateM new = StateM (const (new, ()))

runStateM :: StateM st a -> st -> (st, a)
runStateM (StateM f) = f

main = do
    let x          = [1, 2, 3] :: [Int]
    let (st, newx) = transformElements addOneAndCountCalls x 0
    print st
    print newx

    let (st, newx) = transformElementsM addOneAndCountCalls x 0
    print st
    print newx

    let out = runStateM (transformElementsM_ addOneAndCountCallsM x) 0
    print out

    transformElementsM_ print x
