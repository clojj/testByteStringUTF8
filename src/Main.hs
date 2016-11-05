-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-@ LIQUID "--notermination"           @-}

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (splitAt, length, foldl, break, drop, take)
import Data.ByteString.UTF8 hiding (foldr)
import qualified Data.ByteString as B
import Data.Monoid

-- import Debug.Trace

data Pos = Pos Int Int deriving (Eq, Show)

type Token        = ((Pos, Pos), String)
type Acc          = (Pos, ByteString)
type Advancement  = (Int, Int)

-- TODO type Line = Int
-- TODO type Column = Int
-- TODO type Offset = Int

main :: IO ()
main =
  let content = fromString $ unlines
                [" aaa Zeile1 end1",
                 "Zeile 2 Ze",
                 "ile3 33 333 end3",
                 "Zeile4 Gaaaaanz l",
                 "aaaanges",
                 " Token  ENDE"]

      tokens = [
        ((Pos 1 2, Pos 1 2), "special"),
        ((Pos 1 2, Pos 1 5), "token_aaa"),
        ((Pos 1 6, Pos 1 12), "token_Zeile1"),
        ((Pos 2 9, Pos 3 5), "token_Zeile3"),
        ((Pos 4 8, Pos 6 7), "token_laaang"),
        ((Pos 6 9, Pos 6 13), "token_ENDE")]

      expectedLines =
        "[SPAN]WS[:] [SPAN][SPAN]special[SPAN][SPAN]token_aaa[:]aaa[SPAN][SPAN]WS[:] [SPAN][SPAN]token_Zeile1[:]Zeile1[SPAN][SPAN]WS[:] end1\n\
         \Zeile 2 [SPAN][SPAN]token_Zeile3[:]Ze\n\
         \ile3[SPAN][SPAN]WS[:] 33 333 end3\n\
         \Zeile4 [SPAN][SPAN]token_laaang[:]Gaaaaanz l\n\
         \aaaanges\n\
         \ Token[SPAN][SPAN]WS[:]  [SPAN][SPAN]token_ENDE[:]ENDE[SPAN][SPAN]WS[:]\n\
         \[SPAN]"

  in do
    r <- loopOver content tokens (Pos 1 1, mempty)
    let resultString = toString r
    putStrLn resultString
    putStrLn expectedLines
    print $ show (expectedLines == resultString)

loopOver :: ByteString -> [Token] -> Acc -> IO ByteString
loopOver bs tokens (currentPos, result) =
  case tokens of

    [] -> return $ result <> sepPart <> fromString "WS" <> sep <> bs <> sepPart

    ((pos1@(Pos l1 c1), pos2@(Pos l2 c2)), tname) : tokenTail ->
      -- putStrLn ("currentPos " ++ show currentPos ++ " pos1 " ++ show pos1 ++ " advanceLinesAndColumns " ++ show (advanceLinesAndColumns currentPos pos1)) >>

      if currentPos == pos1 then
        if pos1 == pos2 then
          loopOver bs tokenTail (Pos l1 c1, result <> sepPart <> fromString tname <> sepPart)
        else
          let advancement     = advanceLinesAndColumns pos1 pos2
              (token, bsTail) = spanAdvancement advancement bs
              -- (token, bsTail) = spanLines advancement bs
          in loopOver bsTail tokenTail (Pos l2 c2, result <> sepPart <> fromString tname <> sep <> token <> sepPart)
      else
        let advancement  = advanceLinesAndColumns currentPos pos1
            (ws, bsTail) = spanAdvancement advancement bs
            -- (ws, bsTail) = spanLines advancement bs
        in loopOver bsTail tokens (Pos l1 c1, result <> sepPart <> fromString "WS" <> sep <> ws <> sepPart)

advanceLinesAndColumns :: Pos -> Pos -> Advancement
advanceLinesAndColumns p1@(Pos l1 c1) p2@(Pos l2 c2)
    | p1 == p2 = (0, 0)
    | l2 - l1 > 0 = (l2 - l1, c2)
    | otherwise = (0, c2 - c1)

sep :: ByteString
sep = fromString "[:]"

sepPart :: ByteString
sepPart = fromString "[SPAN]"

spanAdvancement :: Advancement -> ByteString -> (ByteString, ByteString)
spanAdvancement (l, c) bs
  | l == 0 = splitAt c bs
  -- | l == 0 = splitAt' (l, c - 1) bs
  -- | l > 0  = splitAt ((lineOffset l bs) + c - 1) bs
  | l > 0  = splitAt' (l, c - 1) bs
spanAdvancement (_, _) _ = undefined


-- LiquidHaskell: l > 0
{-@ splitAt' :: {v:_ | fst v > 0} -> B.ByteString -> (B.ByteString,B.ByteString) @-}

splitAt' :: Advancement -> B.ByteString -> (B.ByteString,B.ByteString)
splitAt' (ls, cs) bs = loop 0 (ls, cs) bs
  where loop a (l, c) _ | (l, c) == (0, 0) = B.splitAt a bs
        loop a (l, c) bs1 = case decode bs1 of
                         Just (ch,y) -> case ch of
                                          '\n' -> loop (a+y) (l-1, cs) (B.drop y bs1)
                                          _    -> loop (a+y) (l, c-1) (B.drop y bs1)
                         Nothing    ->  (bs, B.empty)


-- lineOffset :: Int -> ByteString -> Int
-- lineOffset l bs = snd $ foldlUntil f endCond (l, 0) bs
--
-- endCond :: (Int, Int) -> Bool
-- endCond (line, _) = line < 1
--
-- f :: (Int, Int) -> Char -> (Int, Int)
-- f (l, offset) ch
--   | l == 0     = (0, offset)
--   | ch == '\n' = (l - 1, offset + 1)
--   | otherwise  = (l, offset + 1)
--
-- -- foldl with early opt-out
-- foldlUntil :: (a -> Char -> a) -> (a -> Bool) -> a -> B.ByteString -> a
-- foldlUntil add optOut acc cs  = case uncons cs of
--                       Just (a,as) -> let v = add acc a
--                                      in seq v $ if optOut v
--                                                 then acc
--                                                 else foldl add v as
--                       Nothing     -> acc



-- spanLines :: Advancement -> ByteString -> (ByteString, ByteString)
-- spanLines (l, c) input =
--   if l == 0 then
--     splitAt c input
--   else
--     let (output, outputTail) = spans (mempty, input)
--         (lastOutput, tailOutput) = splitAt (c - 1) outputTail
--     in (output <> lastOutput, tailOutput)
--     where
--       spans = foldr (.) id (replicate l breakLines)
--       breakLines (acc, bs) =
--         let (linePart, bsTail) = break (== '\n') bs
--             (lp, lt) = trace ("break " ++ show (linePart, bsTail)) (linePart, bsTail)
--         in (acc <> lp <> take 1 lt, drop 1 lt)

