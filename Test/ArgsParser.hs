{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, CPP  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Test.ArgsParser where

-- Based on https://github.com/BartMassey/parseargs

import Control.Exception
import Control.Monad
import Control.Monad.ST
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
import System.Environment
import System.IO

--
-- Provided datatypes.
--

-- |The description of an argument, suitable for
-- messages and for parsing.  The `argData` field
-- is used both for flags with a data argument, and
-- for positional data arguments.
-- 
-- There are two cases:
--
--     (1) The argument is a flag, in which case at least
--     one of `argAbbr` and `argName` is provided;
--
--     (2) The argument is positional, in which case neither
--     `argAbbr` nor `argName` are provided, but `argType` is.
-- 
-- If none of `argAbbr`, `argName`, or `argType` are
-- provided, this is an error. 
data Arg a where
  Arg ::
    (Ord a) =>
    { argIndex :: a              -- ^Connects the input description to the output argument.
    , argAbbr :: Maybe Char      -- ^One-character flag name.
    , argName :: Maybe String    -- ^\"Long name\" of flag.
    , argType :: Maybe Argtype   -- ^Datum type.
    } -> Arg a


-- |The types of an argument carrying data.  The constructor
-- argument is used to carry a default value.
data Argtype = ArgtypeString (Maybe String)
             | ArgtypeInteger (Maybe Integer)
             | ArgtypeInt (Maybe Int)
             | ArgtypeDouble (Maybe Double)
             | ArgtypeFloat (Maybe Float)
  deriving Show
--
-- Returned datatypes.
--

-- |The \"kinds of values\" an argument can have.
data Argval = ArgvalFlag   -- ^For simple present vs not-present flags.
            | ArgvalString String
            | ArgvalInteger Integer
            | ArgvalInt Int
            | ArgvalDouble Double
            | ArgvalFloat Float

-- |The type of the mapping from argument index to value.
newtype ArgRecord a = ArgRecord (Map.Map a Argval)

-- |The data structure `parseArgs` produces. There is a should-be-hidden
-- field that describes the parse.
data Args a where
  Args ::
    (Ord a) =>
    { __args :: ArgRecord a    -- ^The argument parse, only listed here to work around a Haddock bug. See <https://github.com/haskell/haddock/issues/456>.
    , argsProgName :: String   -- ^Basename of 0th argument.
    , argsUsage :: String      -- ^Full usage string.
    , argsRest :: [ String ]   -- ^Remaining unprocessed arguments.
    } -> Args a

--
-- Exception type.
--

-- |This exception is raised with an appropriate error message
-- when argument parsing fails.  The first argument is the usage
-- message, the second the actual error message from the parser.
data ParseArgsException = ParseArgsException String String deriving (Eq, Typeable)

instance Exception ParseArgsException

instance Show ParseArgsException where
  show (ParseArgsException usage msg) = msg ++ "\n" ++ usage

--
-- Implementation.
--

-- |True if the described argument is positional.
argPosn :: Arg a  -- ^Argument.
         -> Bool    -- ^True if argument is positional.
argPosn (Arg { argAbbr = Nothing,
                argName = Nothing }) = True
argPosn _ = False

-- |True if the described argument is a flag.
argFlag :: Arg a  -- ^Argument.
         -> Bool    -- ^True if argument is a flag.
argFlag a = not (argPosn a)

-- |True if the described argument is optional.
argOptional :: Arg a  -- ^Argument.
             -> Bool    -- ^False if argument is required to be present.
argOptional _ = True

argRequired :: Arg a  -- ^Argument.
             -> Bool    -- ^True if argument is required to be present.
argRequired a = not (argOptional a)

-- |Return the value of a defaulted argument.
argDefaultValue :: Arg a        -- ^Argument.
                  -> Maybe Argval  -- ^Optional default value.
argDefaultValue arg@(Arg { argType = Just da }) |
                             argOptional arg =
    defval da
    where
      defval (ArgtypeString (Just v)) = Just (ArgvalString v)
      defval (ArgtypeInteger (Just v)) = Just (ArgvalInteger v)
      defval (ArgtypeInt (Just v)) = Just (ArgvalInt v)
      defval (ArgtypeDouble (Just v)) = Just (ArgvalDouble v)
      defval (ArgtypeFloat (Just v)) = Just (ArgvalFloat v)
      defval _ = Nothing
argDefaultValue _ = Nothing

-- |There's probably a better way to do this.
perhaps :: Bool -> String -> String
perhaps b s = if b then s else ""

-- |Filter out the empty keys for a hash.
filterKeys :: [ (Maybe a, b) ]   -- ^List of (optional key, value) pairs.
            -> [ (a, b) ]         -- ^Pairs with actual keys.
filterKeys l = foldr check_key [] l
    where
      check_key (Nothing, _) rest = rest
      check_key (Just k, v) rest = (k, v) : rest

-- |Fail with an error if the argument description is bad
-- for some reason.
argdescError :: String   -- ^Error message.
              -> a        -- ^Bogus polymorphic result.
argdescError msg =
    error ("internal error: argument description: " ++ msg)

-- |Make a keymap.
keymapFromList :: (Ord k, Show k) =>
                    [ (k, a) ]    -- ^List of key-value pairs.
                                  -- Will be checked for duplicate keys.
                 -> Map.Map k a   -- ^Key-value map.
keymapFromList l =
    foldl add_entry Map.empty l
    where
      add_entry m (k, a) =
          case Map.member k m of
            False -> Map.insert k a m
            True -> argdescError ("duplicate argument description name " ++
                                   (show k))

-- |Make a keymap for looking up a flag argument.
makeKeymap :: (Ord k, Show k) =>
               (Arg a -> Maybe k)   -- ^Mapping from argdesc to flag key.
            -> [Arg a]              -- ^List of argdesc.
            -> Map.Map k (Arg a)    -- ^Map from key to argdesc.
makeKeymap f_field ads =
    (keymapFromList .
     filterKeys .
     map (\arg -> (f_field arg, arg))) ads

-- |The iteration function is given a state and a list, and
-- expected to produce a new state and list.  The function
-- is again invoked with the resulting state and list.  When
-- the supplied function returns the empty list, this
-- function returns the final state produced.
exhaust :: (s -> [e] -> ([e], s))   -- ^Function to iterate.
        -> s                        -- ^Initial state.
        -> [e]                      -- ^Initial list.
        -> s                        -- ^Final state.
exhaust _ s [] = s
exhaust f s l =
  let (l', s') = f s l
  in exhaust f s' l'

-- |Generate a usage error with the given supplementary message string.
parseError :: String    -- ^Usage message.
            -> String    -- ^Specific error message.
            -> a         -- ^Bogus polymorphic result.
parseError usage msg =
  throw (ParseArgsException usage msg)

-- |Given a description of the arguments, `parseArgs`
-- produces a map from the arguments to their \"values\" and
-- some other useful byproducts.  `parseArgs` requires that
-- the argument descriptions occur in the order 1) flag
-- arguments, then 2) positional arguments; otherwise a
-- runtime error will be thrown.
parseArgs :: (Show a, Ord a) =>
          [ Arg a ]      -- ^Argument descriptions.
          -> String         -- ^Full program pathname.
          -> [ String ]     -- ^Incoming program argument list.
          -> Args a        -- ^Outgoing argument parse results.
parseArgs argd pathname argv =
  runST (do
           check_argd
           let (flag_args, posn_args) = span argFlag argd
           let name_hash = makeKeymap argName flag_args
           let abbr_hash = makeKeymap argAbbr flag_args
           let prog_name = baseName pathname
           let usage = prog_name
           let (am, _, rest) = exhaust (parse usage name_hash abbr_hash)
                                (Map.empty, posn_args, [])
                                argv
           let required_args = filter (not . argOptional) argd
           let am' = foldl supply_defaults am argd
           return (Args { __args = ArgRecord am',
                          argsProgName = prog_name,
                          argsUsage = usage,
                          argsRest = rest }))
  where
    supply_defaults :: Map.Map a Argval -> Arg a -> Map.Map a Argval
    supply_defaults am ad@(Arg { argIndex = k }) =
        case Map.lookup k am of
          Just _ -> am
          Nothing -> case argDefaultValue ad of
                       Just v -> Map.insert k v am
                       Nothing -> am
    --- Check for various possible misuses.
    check_argd :: ST s ()
    check_argd = do
      --- Order must be flags, then posn args
      let (_, posns) = span argFlag argd
      unless (all argPosn posns)
             (argdescError "argument description mixes flags and positionals")
      --- No argument may be "nullary".
      when (any arg_nullary argd)
           (argdescError "bogus 'nothing' argument")
      where
        arg_nullary (Arg { argName = Nothing,
                           argAbbr = Nothing,
                           argType = Nothing }) = True
        arg_nullary _ = False
    --- simple recursive-descent parser
    parse _ _ _ av@(_, _, []) [] = ([], av)
    parse usage _ _ av [] = ([], av)
    parse usage name_hash abbr_hash (am, posn, rest) av@(aa : aas) =
        case aa of
          "--" -> ([], (am, posn, rest ++ aas))
          s@('-' : '-' : name)
            | isJust (Map.lookup name name_hash) ->
              case Map.lookup name name_hash of
                Just ad ->
                  let (args', am') = peel s ad aas in
                  (args', (am', posn, rest))
                Nothing -> (aas, (am, posn, rest ++ ["--" ++ name]))
          ('-' : abbr : abbrs)
            | isJust (Map.lookup abbr abbr_hash) ->
              case Map.lookup abbr abbr_hash of
                Just ad ->
                  let (args', am') = peel ['-', abbr] ad aas
                      state' = (am', posn, rest)
                  in case abbrs of
                    [] -> (args', state')
                    ('-' : _) -> parseError usage
                                 ("bad internal '-' in argument " ++ aa)
                    _ -> (('-' : abbrs) : args', state')
                Nothing -> (aas, (am, posn, rest ++ ['-' : abbr : abbrs]))
          _ ->
            case posn of
              (p : ps) ->
                let (_, req_posn) = partition argOptional posn in
                case length av - length req_posn of
                  n_extra | n_extra > 0 || (n_extra == 0 && argRequired p) ->
                    let (args', am') = peel (show $ fromJust $ argType p) p av in
                    (args', (am', ps, rest))
                  0 -> (av, (am, ps, rest))
                  _ -> parseError usage
                         "missing required positional argument(s)"
              [] -> ([], (am, [], rest ++ av))
        where
          add_entry s m (k, a) =
              case Map.member k m of
                False -> Map.insert k a m
                True -> parseError usage ("duplicate argument " ++ s)
          peel name (Arg { argType = Nothing, argIndex = index }) argl =
              let am' = add_entry name am (index, ArgvalFlag)
              in (argl, am')
          peel name (Arg { argType = Just {} }) [] =
              parseError usage (name ++ " is missing its argument")
          peel name (Arg { argType =
                                 Just atype,
                              argIndex = index })
              (a : argl) =
                let v = case atype of
                          ArgtypeString _ -> ArgvalString a
                          ArgtypeInteger _ -> read_arg ArgvalInteger
                                                       "an integer"
                          ArgtypeInt _ -> read_arg ArgvalInt "an int"
                          ArgtypeDouble _ -> read_arg ArgvalDouble "a double"
                          ArgtypeFloat _ -> read_arg ArgvalFloat "a float"
                        where
                          read_arg constructor kind =
                            case reads a of
                              [(val, "")] -> constructor val
                              _ -> parseError usage ("argument " ++
                                                     a ++ " to " ++ name ++
                                                     " is not " ++ kind)
                    am' = add_entry name am (index, v)
                in (argl, am')


-- |Most of the time, you just want the environment's
-- arguments and are willing to live in the IO monad.
-- This version of `parseArgs` digs the pathname and arguments
-- out of the system directly.
parseArgsIO :: (Show a, Ord a) =>
            [ Arg a ]     -- ^Argument descriptions.
            -> IO (Args a)  -- ^Argument parse results.
parseArgsIO argd = do
  argv <- getArgs
  pathname <- getProgName
  return (parseArgs argd pathname argv)


-- |Check whether a given optional argument was supplied. Works on all types.
gotArg :: (Ord a) =>
          Args a    -- ^Parsed arguments.
       -> a         -- ^Index of argument to be checked for.
       -> Bool      -- ^True if the arg was present.
gotArg (Args { __args = ArgRecord am }) k =
    case Map.lookup k am of
      Just _ -> True
      Nothing -> False

-- |Type of values that can be parsed by the argument parser.
class ArgType b where

    -- |Fetch an argument's value if it is present.
    getArg :: (Show a, Ord a)
           => Args a    -- ^Parsed arguments.
           -> a         -- ^Index of argument to be retrieved.
           -> Maybe b   -- ^Argument value if present.

getArgPrimitive :: Ord a => (Argval -> Maybe b) -> Args a -> a -> Maybe b
getArgPrimitive decons (Args { __args = ArgRecord am }) k =
  Map.lookup k am >>= decons

instance ArgType () where
  getArg =
      getArgPrimitive flagArg
      where
        flagArg ArgvalFlag = return ()
        flagArg _ = error "internal error: flag arg at wrong type"

instance ArgType ([] Char) where
  getArg =
      getArgPrimitive stringArg
      where
        stringArg (ArgvalString s) = return s
        stringArg _ = error "internal error: string arg at wrong type"

instance ArgType Integer where
  getArg =
      getArgPrimitive integerArg
      where
        integerArg (ArgvalInteger i) = return i
        integerArg _ = error "internal error: integer arg at wrong type"

instance ArgType Int where
  getArg =
      getArgPrimitive intArg
      where
        intArg (ArgvalInt i) = return i
        intArg _ = error "internal error: int arg at wrong type"

instance ArgType Double where
  getArg =
      getArgPrimitive doubleArg
      where
        doubleArg (ArgvalDouble d) = return d
        doubleArg _ = error "internal error: double arg at wrong type"

instance ArgType Float where
  getArg =
      getArgPrimitive floatArg
      where
        floatArg (ArgvalFloat f) = return f
        floatArg _ = error "internal error: float arg at wrong type"

---
--- Misc
---

-- |Return the filename part of a pathname.
-- Unnecessarily efficient implementation does a single
-- tail-call traversal with no construction.
baseName :: String   -- ^Pathname.
         -> String   -- ^Rightmost component of pathname.
baseName s =
    let s' = dropWhile (/= '/') s in
    if null s' then s else baseName (tail s')
