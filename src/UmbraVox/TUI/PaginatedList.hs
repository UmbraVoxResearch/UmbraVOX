-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.PaginatedList
    ( PageSlice(..)
    , pageMaxIndex
    , slicePage
    , pageItemBySlot
    ) where

data PageSlice a = PageSlice
    { psPage :: Int
    , psTotalPages :: Int
    , psItems :: [(Int, a)]
    } deriving stock (Eq, Show)

pageMaxIndex :: Int -> [a] -> Int
pageMaxIndex pageSize xs
    | pageSize <= 0 = 0
    | otherwise = max 0 ((length xs - 1) `div` pageSize)

slicePage :: Int -> Int -> [a] -> PageSlice a
slicePage pageSize requestedPage xs
    | pageSize <= 0 = PageSlice 0 1 []
    | otherwise =
        let totalPages = max 1 ((length xs + pageSize - 1) `div` pageSize)
            page = max 0 (min (totalPages - 1) requestedPage)
            start = page * pageSize
            visible = take pageSize (drop start xs)
        in PageSlice page totalPages (zip [0..] visible)

pageItemBySlot :: Int -> Int -> Int -> [a] -> Maybe a
pageItemBySlot pageSize requestedPage slot xs =
    lookup slot (psItems (slicePage pageSize requestedPage xs))
