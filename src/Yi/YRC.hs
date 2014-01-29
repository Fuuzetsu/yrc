{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
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

import Control.Lens
import Data.Binary
import Data.Default
import Data.DeriveTH
import Data.Either
import Data.Typeable
import Network.SimpleIRC
import Yi.Core
import Yi.Utils
import Yi.Monad

data YRCState = YRCState { session :: Maybe MIrc
                         , config :: IrcConfig
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
  put (IrcConfig ad po ni pa us re ch ev cv ct pi) =
    put ad >> put po >> put ni >> put pa >> put us >> put re
    >> put ch >> put cv >> put pi
  get = do
    ad <- get
    po <- get
    ni <- get
    pa <- get
    us <- get
    re <- get
    ch <- get
    cv <- get
    pi <- get
    return $ IrcConfig ad po ni pa us re ch
                       [] cv (return "about five o'clock") pi

instance Binary YRCState where
  put (YRCState s c) = put c
  get = get >>= \c -> return $ YRCState Nothing c
    where

instance Default YRCState where
  def = YRCState Nothing conf
    where
      conf :: IrcConfig
      conf = IrcConfig "irc.freenode.net" 6667 "YRC_default" Nothing
               "YRCuser" "YRC realname" ["#yi"] defaultEvents
               "YRC 0.1.0.0"
               (return "always five o'clock") 200

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
    c2 f _ _ = f

modYRCState :: (YRCState -> YRCState) -> YiM ()
modYRCState f = withBuffer $ bufferDynamicValueA %= f

yrc :: YRCState -> YiM ()
yrc (YRCState ses conf) = do
  msgEditor "yrc starting"
  io (connect conf True True) >>= \case
    Left ioerr -> msgEditor $ "YRC failed to connect."
    Right ms -> modYRCState (\_ -> YRCState (Just ms) conf)
