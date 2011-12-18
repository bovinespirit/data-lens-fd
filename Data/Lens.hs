-- | This module defines actions that allow the modification of individual
-- fields in a 'MonadState'.
module Data.Lens
  ( module Data.Lens.Common
  -- * State API
  , access         -- getter -- :: MonadState a m => Lens a b -> m b
  , (~=), (!=)     -- setter -- :: MonadState a m => Lens a b -> b -> m b
  , (%=), (!%=)    -- modify -- :: MonadState a m => Lens a b -> (b -> b) -> m b
  , (%%=), (!%%=)  -- modify -- :: MonadState a m => Lens a b -> (b -> (c, b)) -> m c
  , (+=), (!+=)    -- modify -- :: (MonadState a m, Num b) => Lens a b -> b -> m b
  , (-=), (!-=)    -- modify -- :: (MonadState a m, Num b) => Lens a b -> b -> m b
  , (*=), (!*=)    -- modify -- :: (MonadState a m, Num b) => Lens a b -> b -> m b
  , (//=), (!/=)   -- modify -- :: (MonadState a m, Fractional b) => Lens a b -> b -> m b
  , (&&=), (!&&=)  -- modify -- :: MonadState a m => Lens a Bool -> Bool -> m Bool
  , (||=), (!||=)  -- modify -- :: MonadState a m => Lens a Bool -> Bool -> m Bool
  , focus
  ) where

import Control.Comonad.Trans.Store
import Control.Monad.State
import Data.Functor.Identity
import Data.Lens.Common
import Data.Lens.Lazy (focus)

-- * State actions

-- | Get the value of a lens from state
--
-- >> value <- access field_name
access :: MonadState a m => Lens a b -> m b
access (Lens f) = gets (pos . f)
{-# INLINE access #-}

infixr 4 ~=, !=

-- | Set a value using a lens into state
--
-- >> new_value <- field_name ~= value
(~=) :: MonadState a m => Lens a b -> b -> m b
Lens f ~= b = do
  modify (peek b . f)
  return b
-- | Set a value using a lens into state strictly
--
-- >> new_value <- field_name !~= value
(!=) :: MonadState a m => Lens a b -> b -> m b
Lens f != b = do
  StoreT (Identity h) _ <- gets f
  put (h $! b)
  return b

infixr 4 %=, !%=

-- | Infix modification of a value through a lens into state
--
-- >> new_value <- field_name %= function
(%=)  :: MonadState a m => Lens a b -> (b -> b) -> m b
Lens f %= g = do
  StoreT (Identity h) b <- gets f
  let b' = g b
  put (h b')
  return b'
-- | Strict Infix modification of a value through a lens into state
--
-- >> new_value <- field_name !%= function
(!%=) :: MonadState a m => Lens a b -> (b -> b) -> m b
Lens f !%= g = do
  StoreT (Identity h) b <- gets f
  let b' = g b
  b' `seq` put (h b')
  return b'

infixr 4 %%=, !%%=

-- | Infix modification of a value through a lens into state
-- with a supplemental response
--
-- >> other_value <- field_name %%= function
(%%=) :: MonadState a m => Lens a b -> (b -> (c, b)) -> m c
Lens f %%= g = do
  StoreT (Identity h) b <- gets f
  let (c, b') = g b
  put (h b')
  return c
-- | Strict infix modification of a value through a lens into state
-- with a supplemental response
--
-- >> other_value <- field_name !%%= function
(!%%=) :: MonadState a m => Lens a b -> (b -> (c, b)) -> m c
Lens f !%%= g = do
  StoreT (Identity h) b <- gets f
  let (c, b') = g b
  b' `seq` put (h b')
  return c

infixr 4 +=, !+=, -=, !-=, *=, !*=

(+=), (!+=), (-=), (!-=), (*=), (!*=) :: (MonadState a m, Num b) => Lens a b -> b -> m b
f += b = f %= (+ b)
f -= b = f %= subtract b
f *= b = f %= (* b)
f !+= b = f !%= (+ b)
f !-= b = f !%= subtract b
f !*= b = f !%= (* b)

infixr 4 //=, !/=

(//=), (!/=) :: (MonadState a m, Fractional b) => Lens a b -> b -> m b
f //= b = f %= (/ b)
f !/= b = f !%= (/ b)

infixr 4 &&=, !&&=, ||=, !||=

(&&=), (||=), (!&&=), (!||=) :: MonadState a m => Lens a Bool -> Bool -> m Bool
f &&= b = f %= (&& b)
f ||= b = f %= (|| b)
f !&&= b = f !%= (&& b)
f !||= b = f !%= (|| b)
