{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Lenses where
import RIO
import Types
import Language.Haskell.Exts
import SAT.MiniSat (Formula)

class HasBottle a where
  bottleL :: Lens' a (IORef [Bottle])

class HasBasicInfo a where
  basicInfoL :: Lens' a BasicInfo

class HasAST a where
  astL :: Lens' a (IORef (Maybe (Module SrcSpanInfo)))

class HasLoad a where
  loadL :: Lens' a (IORef [Load])

class HasSliceCounter a where
  sliceCounterL :: Lens' a (IORef Int)

class HasSlices a where
  slicesL :: Lens' a (IORef [Slice])

class HasTargetName a where
  targetNameL :: Lens' a (IORef Text)

class HasTypeVarCounter a where
  typeVarCounterL :: Lens' a (IORef Int)

class HasConstraintCounter a where
  constraintCounterL :: Lens' a (IORef Int)

class HasConstraints a where
  constraintsL :: Lens' a (IORef [Constraint])


class HasMarcoSeed a where
  marcoSeedL :: Lens' a (IORef [Constraint])

class HasMarcoMap a where
  marcoMapL :: Lens' a (IORef [Formula Int])

readIORefFromLens :: Lens' env (IORef b) -> RIO env b
readIORefFromLens l = do
  handle <- view l
  readIORef handle
