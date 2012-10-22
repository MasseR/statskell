{-# Language DeriveGeneric, DefaultSignatures, TypeSynonymInstances #-}
module Store (
    empty
  , addElement
  , addArchive
  , addElementConsolidate
  , consolidateRRA
  , RRA(..)
)
where

import Data.List (sortBy, unfoldr, sort)
import Types


empty :: RRA
empty = RRA (Meta [] [])

addArchive :: Int -> Integer -> Consolidation -> RRA -> RRA
addArchive elements xff consolidation (RRA (Meta buffer archive)) = let
  newArchive = Archive elements xff consolidation []
  in RRA (Meta buffer (sortBy sortByXFF (newArchive : archive)))
  where sortByXFF (Archive _ axff _ _) (Archive _ bxff _ _) = axff `compare` bxff

consolidate :: [Buffer] -> [Archive] -> [Archive]
consolidate [] xs = xs
consolidate _ [] = []
consolidate buffers (archive:as) = let
  newBuffers = mergeBuffers buffers (archiveBuffers archive) (archiveInterval archive) consolFunc
  in archive{archiveBuffers=limit newBuffers} : consolidate buffers as
  where
    limit bs = let n = archiveElements archive in map (\b -> b{bufferElements = reverse $ take n (reverse $ bufferElements b)}) bs
    middleTime [] = error "Can't have empty time"
    middleTime [BufferElement time _] = time
    middleTime bs = let
          (firstTime:bs') = map bufferElementTime bs
          lastTime = last bs'
          in firstTime + ((lastTime - firstTime) `div` 2)
    consolFunc bs = BufferElement (middleTime bs) (consolidationFunc (archiveConsolidation archive) $ map bufferElementValue bs)

consolidateRRA :: RRA -> RRA
consolidateRRA (RRA (Meta buffers archives)) = RRA (Meta [] (consolidate buffers archives))

mergeBuffers :: [Buffer] -> [Buffer] -> Integer -> ([BufferElement] -> BufferElement) -> [Buffer]
mergeBuffers new old interval func = let
  new' = sort new
  old' = sort old
  in mergeBuffers' new old
  where
    mergeBuffers' [] [] = []
    mergeBuffers' [] ys = ys
    mergeBuffers' (Buffer newName newBuffers:xs) [] = Buffer newName (mergeBufferElements newBuffers) : mergeBuffers' xs []
    mergeBuffers' (Buffer newName newBuffers:xs) (Buffer oldName oldBuffers:ys)
      | newName == oldName = Buffer newName (mergeBufferElements (oldBuffers ++ newBuffers)) : mergeBuffers' xs ys
      | otherwise = Buffer oldName oldBuffers : mergeBuffers' (Buffer newName newBuffers:xs) ys
    mergeBufferElements buffers = let
      intervals = groupInterval interval $ sort buffers
      in [func i | i <- intervals]

groupInterval :: Integer -> [BufferElement] -> [[BufferElement]]
groupInterval interval buffer = go buffer Nothing [] []
  where
    calculateInterval (BufferElement now _) = maybe 0 (\(BufferElement _then _) -> now - _then)
    go [] comparison intacc acc = reverse $ map reverse (intacc : acc)
    go (b:bs) comparison [] acc = go bs (Just b) [b] acc
    go (b@(BufferElement time value):bs) comparison intacc acc
      | calculateInterval b comparison >= interval = go (b:bs) Nothing [] (intacc : acc)
      | otherwise = go bs comparison (b:intacc) acc

addElement :: Text -> Integer -> Double -> RRA -> RRA
addElement name when value (RRA (Meta buffer archives)) = RRA (Meta (updateBuffer name when value buffer) archives)

addElementConsolidate :: Text -> Integer -> Double -> RRA -> RRA
addElementConsolidate name when value old = consolidateRRA $ addElement name when value old

updateBuffer :: Text -> Integer -> Double -> [Buffer] -> [Buffer]
updateBuffer name when value oldBuffer = go oldBuffer []
  where
    go [] acc = reverse (Buffer name [BufferElement when value] : acc)
    go (b@(Buffer name' elements) : bs) acc
          | name == name' = (Buffer name (BufferElement when value : elements) : acc) ++ bs
          | otherwise = go bs (b:acc)
