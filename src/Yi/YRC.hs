{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  Yi.YRC
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Entry point to and the heart of YRC
module Yi.YRC where

import Control.Concurrent
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Reader
import Control.Lens
import Data.Binary
import Data.Default
import Data.Typeable
import Network.SimpleIRC
import System.IO.Unsafe
import Yi.Core
import Yi.Utils

{-# NOINLINE commChannel #-}
commChannel :: MVar IrcMessage
commChannel = unsafePerformIO newEmptyMVar

data YRCState = YRCState { session :: Maybe MIrc
                         , config :: IrcConfig
                         , comCh :: MVar IrcMessage
                         } deriving Typeable

-- We don't serialise the IO String action required for CTCP,
-- instead we deserialise without it and plug in something reasonable there.
-- Unfortunately this means that we can't really customise this response for
-- now.
--
-- Similarly, we don't serialise IrcEvent however there's not much loss there
-- as we're not making an IRC bot. It'd be nice to have to override default
-- behaviour of functions but for now we'll stick with defaults.
instance Binary IrcConfig where
  put (IrcConfig ad po ni pa us re' ch _ cv _ pi') =
    put ad >> put po >> put ni >> put pa >> put us >> put re'
    >> put ch >> put cv >> put pi'
  get = do
    ad <- get
    po <- get
    ni <- get
    pa <- get
    us <- get
    re' <- get
    ch <- get
    cv <- get
    pi' <- get
    return $ IrcConfig ad po ni pa us re' ch
                       [] cv (return "about five o'clock") pi'

instance Binary YRCState where
  put (YRCState _ c _) = put c
  get = get >>= \c -> return $ YRCState Nothing c commChannel
    where

instance Default IrcConfig where
  def = IrcConfig "irc.freenode.net" 6667 "YRC_default" Nothing
                  "YRCuser" "YRC realname" ["#fuuzetsu"] defaultEvents
                  "YRC 0.1.0.0"
                  (return "always five o'clock")
                  (350 * 10 ^ (6 :: Int))

instance Default YRCState where
  def = YRCState Nothing def commChannel

instance YiVariable YRCState

defaultEvents :: [IrcEvent]
defaultEvents = [ Privmsg $ c2 (putStrLn "Privmsg")
                , Numeric $ c2 (putStrLn "Numeric")
                , Ping $ c2 (putStrLn "Ping")
                , Join $ c2 (putStrLn "Join")
                , Part $ c2 (putStrLn "Part")
                , Network.SimpleIRC.Mode $ c2 (putStrLn "Mode")
                , Topic $ c2 (putStrLn "Topic")
                , Invite $ c2 (putStrLn "Invite")
                , Kick $ c2 (putStrLn "Kick")
                , Quit $ c2 (putStrLn "Quit")
                , Nick $ c2 (putStrLn "Nick")
                , Notice $ c2 (putStrLn "Notice")
                , RawMsg $ c2 (putStrLn "Raw")
                , Disconnect . const $ putStrLn "Disconnect"
                ]
  where
    c2 f _ e = f >> putMVar commChannel e

modYRCState :: (YRCState -> YRCState) -> YiM ()
modYRCState f = withBuffer $ bufferDynamicValueA %= f

handleEvent :: MVar IrcMessage -> YiM ()
handleEvent m = do
--  s <- withBuffer $ use bufferDynamicValueA
  e <- io . takeMVar $ m
  msgEditor $ show e

yrc :: YRCState -> YiM ()
yrc st@(YRCState (Just _) _ m) = modYRCState (const st)
yrc (YRCState Nothing conf _) = do
  msgEditor "yrc starting"

  io (connect conf True True) >>= \case
    Left _ -> msgEditor $ "YRC failed to connect."
    Right ms -> modYRCState (\_ -> YRCState (Just ms) conf commChannel)
