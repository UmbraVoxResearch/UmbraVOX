-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.LayoutGrid
    ( Bounds(..)
    , TrackSpec(..)
    , GridRegion(..)
    , resolveTrackSizes
    , resolveGridRegion
    ) where

data Bounds = Bounds
    { bMin :: Int
    , bMax :: Int
    } deriving stock (Eq, Show)

data TrackSpec = Fixed Int | Flexible Bounds
    deriving stock (Eq, Show)

data GridRegion = GridRegion
    { grRowStart :: Int
    , grRowSpan :: Int
    , grColStart :: Int
    , grColSpan :: Int
    } deriving stock (Eq, Show)

resolveTrackSizes :: Int -> [TrackSpec] -> [Int]
resolveTrackSizes total specs =
    let clampedTotal = max 0 total
        mins = map minSize specs
        maxs = map maxSize specs
        base = if sum mins <= clampedTotal then mins else shrinkToTotal clampedTotal mins
        grown = growToTotal clampedTotal base maxs
    in grown

resolveGridRegion :: [Int] -> [Int] -> GridRegion -> (Int, Int, Int, Int)
resolveGridRegion rowSizes colSizes region =
    let row0 = sum (take (max 0 (grRowStart region)) rowSizes)
        col0 = sum (take (max 0 (grColStart region)) colSizes)
        rowSpan = max 0 (grRowSpan region)
        colSpan = max 0 (grColSpan region)
        h = sum (take rowSpan (drop (max 0 (grRowStart region)) rowSizes))
        w = sum (take colSpan (drop (max 0 (grColStart region)) colSizes))
    in (row0, col0, h, w)

minSize :: TrackSpec -> Int
minSize (Fixed n) = max 0 n
minSize (Flexible b) = max 0 (bMin b)

maxSize :: TrackSpec -> Int
maxSize (Fixed n) = max 0 n
maxSize (Flexible b) = max (max 0 (bMin b)) (max 0 (bMax b))

shrinkToTotal :: Int -> [Int] -> [Int]
shrinkToTotal total xs = go (sum xs - max 0 total) xs
  where
    go over ys
        | over <= 0 = ys
        | otherwise =
            let positiveIx = [i | (i, v) <- zip [0..] ys, v > 0]
            in case positiveIx of
                [] -> ys
                _ ->
                    let (ys', over') = foldl step (ys, over) positiveIx
                    in if over' == over then ys else go over' ys'
    step (ys, over) i
        | over <= 0 = (ys, over)
        | otherwise =
            let v = ys !! i
            in if v > 0
                then (replaceAt i (v - 1) ys, over - 1)
                else (ys, over)

growToTotal :: Int -> [Int] -> [Int] -> [Int]
growToTotal total base maxs = go (max 0 (total - sum base)) base
  where
    caps = zipWith (\v m -> max 0 (m - v)) base maxs
    go remain xs
        | remain <= 0 = xs
        | otherwise =
            let growable = [i | (i, c) <- zip [0..] caps, c > usedAt i xs]
            in case growable of
                [] -> xs
                _ ->
                    let (xs', remain') = foldl step (xs, remain) growable
                    in if remain' == remain then xs else go remain' xs'
    step (ys, remain) i
        | remain <= 0 = (ys, remain)
        | usedAt i ys >= caps !! i = (ys, remain)
        | otherwise =
            let v = ys !! i
            in (replaceAt i (v + 1) ys, remain - 1)
    usedAt i ys = (ys !! i) - (base !! i)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs
