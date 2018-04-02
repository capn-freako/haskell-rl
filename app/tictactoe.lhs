```include
other/header.md
```

haskell-rl : Tic-Tac-Toe Ex. 1
===

[ghc options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
---

\begin{code}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
\end{code}

[pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
---

\begin{code}
-- doctest doesn't look at the cabal file, so you need pragmas here
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
\end{code}

[libraries](https://www.stackage.org/)
---

- [protolude](https://www.stackage.org/package/protolude)
- [optparse-generic](https://www.stackage.org/package/optparse-generic)

\begin{code}
import Protolude
import Options.Generic
\end{code}

code
---

- [hoogle](https://www.stackage.org/package/hoogle)

\begin{code}
-- Command line options defintions.
data Opts w = Opts
    { number :: w ::: Maybe Integer <?> "The number you want to product to"
    }
    deriving (Generic)

instance ParseRecord (Opts Wrapped)

-- Playing board state definition.
data CellState = Empty
               | X
               | O

type BoardState     = VS.Vector 3 (VS.Vector 3 CellState)
type BoardStateEnum = Finite 19683  -- 3^9
type WinProbs       = VS.Vector 19683 Double

data Winner = None
            | Dummy
            | Learner

winner :: BoardState -> Winner
winner bs =
  if winnerX
    then Learner
    else
      if winnerO
        then Dummy
        else Nome
 -- where winnerX = any (all (== X)) bs
 --               | any (all (== X)) (transpose bs)
 --               | any (all (== X)) (diagonals bs)
 -- where winnerX = any $ map (any (all (== X))) [bs, transpose bs, diagonals bs]
 --       winnerO = any $ map (any (all (== O))) [bs, transpose bs, diagonals bs]
 where [winnerX, winnerO] = map (\cs -> any $ map (VS.any (VS.all (== cs))) [bs, transpose bs, diagonals bs]) [X, O]

diagonals :: BoardState -> VS.Vector 2 (VS.Vector 3 CellState)
diagonals bs = VS.fromList
  [ VS.fromList [bs VS.!! n VS.!! n     | n <- [0..2]]
  , VS.fromList [bs VS.!! n VS.!! (2-n) | n <- [0..2]]
  ]

winProb :: WinProbs -> BoardState -> Double
winProb wps bs = wps VS.!! (stateToIndex bs)

stateToIndex :: BoardState -> Finite 19683
-- stateToIndex bs = Finite $ foldl' (\ acc nxt -> acc * 3 + enumCell nxt) 0 $ concat $ map VS.toList $ VS.toList bs
-- stateToIndex bs = Finite $ foldl' (\ acc -> (acc * 3 +) . enumCell) 0 $ concat $ map VS.toList $ VS.toList bs
stateToIndex = Finite . foldl' (\ acc -> (acc * 3 +) . enumCell) 0 . concat . map VS.toList . VS.toList

enumCell :: CellState -> Integer
enumCell Empty = 0
enumCell X     = 1
enumCell O     = 2

--        h = (V.foldl' . curry) u (Finite 0) . (V.reverse . V.generate_)
--          where u (Finite acc, Finite m) = Finite $ acc * natValAt @m + m

-- Playing board state manipulation.
-- play :: Policy -> WinProbs -> (Winner, WinProbs, [BoardState])
-- play :: Policy -> State WinProbs BoardState
play :: Policy -> WinProbs -> [BoardState]
play p wps = unfold step $ VS.replicate (VS.replicate Empty))
  where step :: BoardState -> Maybe (BoardState, BoardState)
        step bs = ( let bs' = move p wps bs
                     in if winner bs == None then Just (bs', bs')
                                                else Nothing
                  )

-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

move :: Policy -> WinProbs -> BoardState -> BoardState
move p wps bs = if winner bs == None and length (emptyCells bs) > 0
                  then getPolicy p bs wp
                  else bs

nextPossibleStates :: BoardState -> [BoardState]
nextPossibleStates bs = bs VS.//
  [ ( i, bs VS.!! i VS.// [(j, X)] )
  | (i, j) <- emptyCells bs
  ]

emptyCells :: BoardState -> [(Int,Int)]
emptyCells bs = [(i,j) | i <- [0..2], j <- [0..2], bs VS.!! i VS.!! j == Empty]

-- Policy definitions.
newtype Policy = Policy {getPolicy :: BoardState -> WinProbs -> BoardState}

dummy = Policy ( \ bs wps ->
               )

greedy = Policy ( \ bs wps ->
                    maximumBy (uncurry max . curry (toBoth $ winProb wps))
                              (nextPossibleStates bs)
                )

-- Misc.
toBoth :: (a -> b) -> (a,a) -> (b,b)
toBoth f (x1, x2) = (f x1, f x2)

main :: IO ()
main = do
    o :: Opts Unwrapped <- unwrapRecord "an example app for readme-lhs"
    let n = fromMaybe 10 (number o)
    let answer = product [1..n]
    putStrLn (show answer <> " 👍" :: Text)
    writeFile "other/answer.md"
        ("$\\prod_{i=1}^{" <> show n <> "} i = " <>
         show answer <> "$")
\end{code}

output
---

```include
other/answer.md
```

tests
---

- [doctest](https://www.stackage.org/package/doctest)

\begin{code}
-- | doctests
-- >>> let n = 10
-- >>> product [1..n]
-- 3628800
\end{code}

***

<div class="footer">
Powered by [haskell](https://haskell-lang.org/), [stack](https://docs.haskellstack.org/en/stable/README/) and [pandoc](http://pandoc.org/).
</div>
