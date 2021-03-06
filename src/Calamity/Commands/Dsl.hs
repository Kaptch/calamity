{-# LANGUAGE RecursiveDo #-}

-- | A DSL for generating commands and groups
module Calamity.Commands.Dsl
    ( command
    , command'
    , commandA
    , commandA'
    , help
    , requires
    , requires'
    , requiresPure
    , group
    , group'
    , groupA
    , groupA'
    , DSLState
    , raiseDSL
    , fetchHandler ) where

import           Calamity.Commands.AliasType
import           Calamity.Commands.Check
import           Calamity.Commands.Command     hiding ( help )
import           Calamity.Commands.CommandUtils
import           Calamity.Commands.Context     hiding ( command )
import           Calamity.Commands.Error
import           Calamity.Commands.Group       hiding ( help )
import           Calamity.Commands.Handler
import           Calamity.Internal.LocalWriter

import qualified Data.HashMap.Lazy             as LH
import qualified Data.Text                     as S
import qualified Data.Text.Lazy                as L

import qualified Polysemy                      as P
import qualified Polysemy.Fail                 as P
import qualified Polysemy.Tagged               as P
import qualified Polysemy.Fixpoint             as P
import qualified Polysemy.Reader               as P
import Data.List.NonEmpty (NonEmpty(..))

type DSLState r =
  ( LocalWriter (LH.HashMap S.Text (Command, AliasType))
      ': LocalWriter (LH.HashMap S.Text (Group, AliasType))
      ': P.Reader (Maybe Group)
      ': P.Reader (Context -> L.Text)
      ': P.Tagged "original-help" (P.Reader (Context -> L.Text))
      ': P.Reader [Check]
      ': P.Reader CommandHandler
      ': P.Fixpoint
      ': r
  )

raiseDSL :: P.Sem r a -> P.Sem (DSLState r) a
raiseDSL = P.raise . P.raise . P.raise . P.raise . P.raise . P.raise . P.raise . P.raise

-- | Given the command name and parameter names, @parser@ and @callback@ for a
-- command in the 'P.Sem' monad, build a command by transforming the Polysemy
-- actions into IO actions. Then register the command.
--
-- The parent group, checks, and command help are drawn from the reader context.
command'
  :: P.Member (P.Final IO) r
  => S.Text
  -> [S.Text]
  -> (Context -> P.Sem r (Either CommandError a))
  -> ((Context, a) -> P.Sem (P.Fail ': r) ())
  -> P.Sem (DSLState r) Command
command' name params parser cb = commandA' name [] params parser cb

-- | Given the command name, aliases, and parameter names, @parser@ and @callback@ for a
-- command in the 'P.Sem' monad, build a command by transforming the Polysemy
-- actions into IO actions. Then register the command.
--
-- The parent group, checks, and command help are drawn from the reader context.
commandA'
  :: P.Member (P.Final IO) r
  => S.Text -- ^ name
  -> [S.Text] -- ^ aliases
  -> [S.Text] -- ^ parameter names
  -> (Context -> P.Sem r (Either CommandError a))
  -> ((Context, a) -> P.Sem (P.Fail ': r) ())
  -> P.Sem (DSLState r) Command
commandA' name aliases params parser cb = do
  parent <- P.ask @(Maybe Group)
  checks <- P.ask @[Check]
  help' <- P.ask @(Context -> L.Text)
  cmd <- raiseDSL $ buildCommand' (name :| aliases) parent checks params help' parser cb
  ltell $ LH.singleton name (cmd, Original)
  ltell $ LH.fromList [(name, (cmd, Alias)) | name <- aliases]
  pure cmd

-- | Given the name of a command and a callback, and a type level list of
-- the parameters, build and register a command.
--
-- ==== Examples
--
-- Building a command that bans a user by id.
--
-- @
-- 'command' \@\'['Calamity.Commands.Parser.Named' "user" ('Calamity.Types.Snowflake' 'Calamity.Types.Model.User'),
--                'Calamity.Commands.Parser.Named' "reason" ('Calamity.Commands.Parser.KleeneStarConcat' 'S.Text')]
--    "ban" $ \ctx uid r -> case (ctx 'Control.Lens.^.' #guild) of
--      'Just' guild -> do
--        'Control.Monad.void' . 'Calamity.HTTP.invoke' . 'Calamity.HTTP.reason' r $ 'Calamity.HTTP.Guild.CreateGuildBan' guild uid
--        'Control.Monad.void' $ 'Calamity.Types.Tellable.tell' ctx ("Banned user `" '<>' 'TextShow.showt' uid '<>' "` with reason: " '<>' r)
--      'Nothing' -> 'void' $ 'Calamity.Types.Tellable.tell' @'L.Text' ctx "Can only ban users from guilds."
-- @
command :: forall ps r.
        ( P.Member (P.Final IO) r,
          TypedCommandC ps r)
        => S.Text
        -> (Context -> CommandForParsers ps r)
        -> P.Sem (DSLState r) Command
command name cmd = commandA @ps name [] cmd

-- | Given the name and aliases of a command and a callback, and a type level list of
-- the parameters, build and register a command.
--
-- ==== Examples
--
-- Building a command that bans a user by id.
--
-- @
-- 'commandA' \@\'['Calamity.Commands.Parser.Named' "user" ('Calamity.Types.Snowflake' 'Calamity.Types.Model.User'),
--                'Calamity.Commands.Parser.Named' "reason" ('Calamity.Commands.Parser.KleeneStarConcat' 'S.Text')]
--    "ban" [] $ \ctx uid r -> case (ctx 'Control.Lens.^.' #guild) of
--      'Just' guild -> do
--        'Control.Monad.void' . 'Calamity.HTTP.invoke' . 'Calamity.HTTP.reason' r $ 'Calamity.HTTP.Guild.CreateGuildBan' guild uid
--        'Control.Monad.void' $ 'Calamity.Types.Tellable.tell' ctx ("Banned user `" '<>' 'TextShow.showt' uid '<>' "` with reason: " '<>' r)
--      'Nothing' -> 'void' $ 'Calamity.Types.Tellable.tell' @'L.Text' ctx "Can only ban users from guilds."
-- @
commandA :: forall ps r.
        ( P.Member (P.Final IO) r,
          TypedCommandC ps r)
        => S.Text -- ^ name
        -> [S.Text] -- ^ aliases
        -> (Context -> CommandForParsers ps r)
        -> P.Sem (DSLState r) Command
commandA name aliases cmd = do
  parent <- P.ask @(Maybe Group)
  checks <- P.ask @[Check]
  help' <- P.ask @(Context -> L.Text)
  cmd' <- raiseDSL $ buildCommand @ps (name :| aliases) parent checks help' cmd
  ltell $ LH.singleton name (cmd', Original)
  ltell $ LH.fromList [(name, (cmd', Alias)) | name <- aliases]
  pure cmd'

-- | Set the help for any groups or commands registered inside the given action.
help :: P.Member (P.Reader (Context -> L.Text)) r
     => (Context -> L.Text)
     -> P.Sem r a
     -> P.Sem r a
help = P.local . const

-- | Add to the list of checks for any commands registered inside the given
-- action.
requires :: [Check]
         -> P.Sem (DSLState r) a
         -> P.Sem (DSLState r) a
requires = P.local . (<>)

-- | Construct a check and add it to the list of checks for any commands
-- registered inside the given action.
requires' :: P.Member (P.Final IO) r
          => S.Text
          -> (Context -> P.Sem r (Maybe L.Text))
          -> P.Sem (DSLState r) a
          -> P.Sem (DSLState r) a
requires' name cb m = do
  check <- raiseDSL $ buildCheck name cb
  requires [check] m

-- | Construct some pure checks and add them to the list of checks for any
-- commands registered inside the given action.
requiresPure :: [(S.Text, Context -> Maybe L.Text)]
             -> P.Sem (DSLState r) a
             -> P.Sem (DSLState r) a
requiresPure checks = requires $ map (uncurry buildCheckPure) checks

-- | Construct a group and place any commands registered in the given action
-- into the new group.
--
-- This also resets the @help@ function back to it's original value, use
-- 'group'' if you don't want that (i.e. your help function is context aware).
group :: P.Member (P.Final IO) r
         => S.Text
         -> P.Sem (DSLState r) a
         -> P.Sem (DSLState r) a
group name m = groupA name [] m

-- | Construct a group with aliases and place any commands registered in the given action
-- into the new group.
--
-- This also resets the @help@ function back to it's original value, use
-- 'group'' if you don't want that (i.e. your help function is context aware).
groupA :: P.Member (P.Final IO) r
         => S.Text -- ^ name
         -> [S.Text] -- ^ aliases
         -> P.Sem (DSLState r) a
         -> P.Sem (DSLState r) a
groupA name aliases m = mdo
  parent <- P.ask @(Maybe Group)
  checks <- P.ask @[Check]
  help'  <- P.ask @(Context -> L.Text)
  origHelp <- fetchOrigHelp
  let group' = Group (name :| aliases) parent commands children help' checks
  (children, (commands, res)) <- llisten @(LH.HashMap S.Text (Group, AliasType)) $
                                 llisten @(LH.HashMap S.Text (Command, AliasType)) $
                                 P.local @(Maybe Group) (const $ Just group') $
                                 P.local @(Context -> L.Text) (const origHelp) m
  ltell $ LH.singleton name (group', Original)
  ltell $ LH.fromList [(name, (group', Alias)) | name <- aliases]
  pure res

fetchOrigHelp :: P.Member (P.Tagged "original-help" (P.Reader (Context -> L.Text))) r => P.Sem r (Context -> L.Text)
fetchOrigHelp = P.tag P.ask

-- | Construct a group and place any commands registered in the given action
-- into the new group.
--
-- Unlike 'help' this doesn't reset the @help@ function back to it's original
-- value.
group' :: P.Member (P.Final IO) r
         => S.Text -- ^ name
         -> P.Sem (DSLState r) a
         -> P.Sem (DSLState r) a
group' name m = groupA' name [] m

-- | Construct a group with aliases and place any commands registered in the given action
-- into the new group.
--
-- Unlike 'help' this doesn't reset the @help@ function back to it's original
-- value.
groupA' :: P.Member (P.Final IO) r
         => S.Text -- ^ name
         -> [S.Text] -- ^ aliases
         -> P.Sem (DSLState r) a
         -> P.Sem (DSLState r) a
groupA' name aliases m = mdo
  parent <- P.ask @(Maybe Group)
  checks <- P.ask @[Check]
  help'  <- P.ask @(Context -> L.Text)
  let group' = Group (name :| aliases) parent commands children help' checks
  (children, (commands, res)) <- llisten @(LH.HashMap S.Text (Group, AliasType)) $
                                 llisten @(LH.HashMap S.Text (Command, AliasType)) $
                                 P.local @(Maybe Group) (const $ Just group') m
  ltell $ LH.singleton name (group', Original)
  ltell $ LH.fromList [(name, (group', Alias)) | name <- aliases]
  pure res

-- | Retrieve the final command handler for this block
fetchHandler :: P.Sem (DSLState r) CommandHandler
fetchHandler = P.ask
