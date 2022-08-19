module Main where


import           Control.Monad
import           Control.Monad.State
import qualified Data.Array             as A
import           Data.Time.Clock
import           System.CPUTime


import           IloCplex
import           LP

import           Design
import           Stats

import           Algorithm

import           Confs
import           Zone_RA
import           Tests
import System.Environment



main = do
  args <- getArgs
  env <- newIloEnv
  case args of
    ["AP",confstyle,i,ins,logfile] -> do
      let conf= if confstyle == "twostage" then confMaxHV else monoConf confMaxHV
      dom@(n,p,_,_,_) <- readVC ins env
      (retN,total) <- time $ mainloop conf env dom
      appendFile logfile (show n ++ ";" ++ show p ++ ";" ++ i ++";"++ _cName conf ++";" ++ show total ++ ";" ++  show (_raStats retN) ++  "\t#" ++ ins++"\n")
      writeFile "ndpts" $ show $ _raNDPT_List retN
    ["KP",confstyle,i,ins,logfile] -> do
      let conf= if confstyle == "twostage" then confMaxHV else monoConf confMaxHV
      dom@(n,p,_,_,_) <- readKS ins env
      (retN,total) <- time $ mainloop conf env dom
      appendFile logfile (show n ++ ";" ++ show p ++ ";" ++ i ++";"++ _cName conf ++";" ++ show total ++ ";" ++  show (_raStats retN) ++  "\t#" ++ ins++"\n")
      writeFile "ndpts" $ show $ _raNDPT_List retN
    ["MODO",confstyle,i,ins,logfile] -> do
      let conf= if confstyle == "twostage" then confMaxHV else monoConf confMaxHV
      dom@(n,p,_,_,_) <- readDomain ins env
      (retN,total) <- time $ mainloop conf env dom
      appendFile logfile (show n ++ ";" ++ show p ++ ";" ++ i ++";"++ _cName conf ++";" ++ show total ++ ";" ++  show (_raStats retN) ++  "\t#" ++ ins++"\n")
      writeFile "ndpts" $ show $ _raNDPT_List retN
    ["SparseMODO",confstyle,i,ins,logfile] -> do
      let conf= if confstyle == "twostage" then confMaxHV else monoConf confMaxHV
      dom@(n,p,_,_,_) <- readSparseDomain ins env
      (retN,total) <- time $ mainloop conf env dom
      appendFile logfile (show n ++ ";" ++ show p ++ ";" ++ i ++";"++ _cName conf ++";" ++ show total ++ ";" ++  show (_raStats retN) ++  "\t#" ++ ins++"\n")
      writeFile "ndpts" $ show $ _raNDPT_List retN




    otherwise -> putStrLn $ "syntax: ./main KP/AP/MODO confstyle i instance logfile"

