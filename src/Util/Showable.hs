{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}

module Util.Showable where

import Text.Printf
import Data.Map(Map)
import qualified Data.Map as M

class ShowItemClass a where
  showI       :: a -> ShowItem
  showIString :: a -> String
  showIString = showiToString . showI


data ShowItem = Component String Bool [ShowItem]  -- bool: indicates
                                                  -- whether to indent
                                                  -- sub-groups
              | SingleLine String



--  we want a Map to be Showable, but the best it can be is ShowListable. how
--  do we deal with (Map Loc (Map String [Mark]))? Well we can say the top
--  level map has keys that are "show string able", and the inner map is
--  showlistable... provided that its keys are show stringable and 



class ShowListClass a where
  showL :: a -> [ShowItem]


type ShowListType a = [a]


instance ShowItemClass a => ShowListClass (ShowListType a) where
  showL = map showI
                        

class ShowStringClass a where
  showS :: a -> String


instance ShowStringClass String where
  showS = id


class ShortString m where
  showSh :: m -> String




{-
type NamedListable a = (String,a)


instance ShowListClass a => ShowItemClass (NamedListable a) where
  showI (name,xs) = Component name True (showL xs)
-}

{-

data MapMap a b c = MapMap (Map a (Map b c))

instance (ShortString a, ShortString b, ShowItemClass c) =>
   ShowItemClass (MapMap a b c) where
  showI (MapMap m) = Component "" True (map g $ M.toAscList m)
    where
      -- g :: (a,Map b c) -> ShowItem
      g (x,m2) = Component (showSh x) True (map h $ M.toAscList m2)
        where
          h (y,z) = Component (showSh y) True [showI z]

data Map1 a b = Map1 (Map a b)


instance (ShortString a, ShowItemClass b) => ShowItemClass (Map1 a b) where
  showI (Map1 m) = Component "" True (map g $ M.toAscList m)
    where
      g (k,v) = Component (showSh k) True [showI v]

-}

----------------------------------------------------------------------
-- ShowMap instance


data ShowMap l s = ShowMap (Map l s)


instance (ShowStringClass l, ShowItemClass s) =>
  ShowItemClass (ShowMap l s) where
    showI (ShowMap m) = Component "" True (map g $ M.toAscList m)
      where
        g (x,y) = case showI y of
          Component _ _ vs -> Component (showS x) True vs
          l                -> l

{-    
data WrappedMap a b = WrappedMap (Map a b)


instance (ShowStringClass a, ShowListClass b) =>
  ShowListClass (WrappedMap a b) where
  showL (WrappedMap m) =
    map (\(x,y) -> Component (showS x) True (showL y)) $ M.toAscList m
-}


----------------------------------------------------------------------
-- ShowList instance


data ShowList a = ShowList [a]


instance ShowItemClass a => ShowItemClass (ShowList a) where
  showI = error "f9834"
  

-- okay we need to have two ways of responding to maps. apparently its not
-- going to decide depending on what conditions match. a map is a map. we
-- could make a hairpin "ShowListable" but then it wouldn't. how about we have
-- some kind of tuple wrapper

data SingleLineTup a b = SingleLineTup Int Int (a,b)

{-

            removed 2019

instance (ShowStringClass a, ShowStringClass b) =>
         ShowItemClass (SingleLineTup a b) where
  showI (SingleLineTup x y (a,b)) = SingleLine $ printf "%*s%*s" x (showS a)
        y (showS b)




instance ShowStringClass a => ShowStringClass (Maybe a) where
  showS Nothing = "Nothing"
  showS (Just x) = "Just " ++ showS x


instance ShowStringClass Int where
  showS = show

-}


{-
type NamedShowItem = (String,ShowItem)


instance Showable a => Showable (NamedShowItem a) where
  showi (name,x) = Component name True [showi x]
  

type NamedList a = (String,[a])


instance Showable a => Showable (NamedList a) where
  showi (name,xs) = Component name True (map showi xs)


type NamedMap a b = (String,Map a b)


instance NamedMap 
-}
  

{-
type ShowPair a b = (a,b)


instance (Show1 a,Show1 b) => Show1 (ShowPair a b) where
  show1 (x,y) = printf "%s: %s" (show1 x) (show1 y)
-}


showiToString :: ShowItem -> String
showiToString = showItems' 0
  where showItems' :: Int -> ShowItem -> String
        showItems' i (SingleLine s) = replicate i ' ' ++ s ++ "\n"
        showItems' i (Component  title flag items) = 
          replicate i ' ' ++ title ++ "\n" ++ rest
          where
            newIndent = if flag then 2 else 0
            rest = concatMap (showItems' (i+newIndent)) items


showMaybe :: (a -> String) -> Int -> Maybe a -> String
showMaybe f n m = case m of
  Nothing -> lpad n "Nothing"
  Just x  -> lpad n ("Just " ++ f x)
  where
      lpad m s =
        let len = length s
        in replicate (m-len) ' ' ++ s

{-
showMap2 :: (Ord k) => (k -> String) -> (a -> String) -> Map k a -> [ShowItem]
showMap2 showKey showElem = map g . M.toList
  where g (k,a) = Component (showKey k) True [SingleLine $ showElem a]



-- show map in which each entry is one line
showMapShort :: (Ord a,ShowShort a,ShowShort b) => String -> Map a b ->
                ShowItem
showMapShort name = Component name True . map g . M.toAscList
  where g (x,y) = SingleLine $ printf "%s: %s" (showShort x) (showShort y)


showMapLShort :: (Ord a,ShowShort a,ShowShort b) => String -> Map a [b] ->
ShowItem
showMapLShort name = Component name True . map g . M.toAscList
  where g (x,ys) = Component (showShort x) True $
                   map (SingleLine . showShort) ys


showMapMShort :: (Ord a,Ord b,ShowShort a,ShowShort b,ShowShort c) =>
                 String -> Map a (Map b c) -> ShowItem
showMapMShort name = Component name True . map g . M.toAscList
  where g (x,m2) = showMapShort (showShort x) m2
        
-}

