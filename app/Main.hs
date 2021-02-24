{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-fields -fno-warn-orphans #-}

-- | This is the preprocessor that extracts ? from the module, retaining
-- their positions, and then passes them to the compiler plugin.
--

-- © 2020 Sky Above Limited
-- © 2018 Mark Karpov

module Main (main) where

import qualified "ghc-lib-parser" ApiAnnotation
import "ghc-lib-parser"           BasicTypes
import                            Control.Concurrent
import                            Control.Monad
import                            Control.Monad.IO.Class
import                            Data.ByteString (ByteString)
import qualified                  Data.ByteString as S
import qualified                  Data.ByteString.Builder as SB
import qualified                  Data.ByteString.Char8 as S8
import                            Data.Conduit
import                            Data.Conduit.Filesystem
import qualified                  Data.Conduit.List as CL
import                            Data.Function
import                            Data.List (foldl')
import qualified                  Data.List as List
import                            Data.Maybe
import                            Data.Ord
import                            Data.Set (Set)
import qualified                  Data.Set as Set
import                            Data.Text (Text)
import qualified                  Data.Text as T
import qualified                  Data.Text.Encoding as T
import qualified                  Data.Text.IO as T
import "ghc-lib-parser"           DynFlags
import qualified "ghc-lib-parser" EnumSet as ES
import "ghc-lib-parser"           FastString
import "ghc-lib-parser"           FastString (mkFastString)
import "ghc-lib-parser"           GHC.LanguageExtensions
import qualified "ghc-lib-parser" Lexer as L
import                            Options.Applicative
import "ghc-lib-parser"           SrcLoc
import "ghc-lib-parser"           StringBuffer

import                            System.Environment
import                            UnliftIO

--------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  hSetBuffering stdout (BlockBuffering Nothing)
  dir:idents <- getArgs
  runConduitRes
    (sourceDirectoryDeep False dir .| CL.filter isHaskell .|
     CL.mapM_ (void . liftIO . forkIO . dump idents))

isHaskell = List.isSuffixOf ".hs"

dump :: [String] -> FilePath -> IO ()
dump idents fp = do
  bytes <- S.readFile fp
  case tokenizeHaskellLoc (T.decodeUtf8 bytes) of
    Just tokens -> do
        SB.hPutBuilder
          stdout
          (mconcat
             (mapMaybe
                (\(tokens@((_, Loc {line}):_)) ->
                   if null idents || all
                        (\ident -> any (List.isPrefixOf ident.show.fst) tokens)
                        idents
                     then pure
                            (fp' <> ":" <> SB.intDec line <> ": " <>
                             foldMap
                               (\(token, _) ->
                                  SB.byteString (S8.pack (show token)) <> " ")
                               tokens <>
                             "\n")
                     else Nothing)
                (List.groupBy (on (==) (line . snd)) tokens)))
    _ -> pure ()
  where
    fp' = SB.byteString (S8.pack fp)

--------------------------------------------------------------------------------
-- Lexing

isComment :: L.Token -> Bool
isComment =
  \case
    L.ITcomment_line_prag -> True
    L.ITdocCommentNext _ -> True
    L.ITdocCommentPrev _ -> True
    L.ITdocCommentNamed _ -> True
    L.ITdocSection _ _ -> True
    L.ITdocOptions _ -> True
    L.ITlineComment _ -> True
    L.ITblockComment _ -> True
    _ -> False

deriving instance Eq L.Token
data Loc = Loc
  { line, col :: !Int
  } deriving (Eq, Ord, Show)

tokenizeHaskellLoc :: Text -> Maybe [(L.Token, Loc)]
tokenizeHaskellLoc input =
  case L.unP pLexer parseState of
    L.PFailed {} -> Nothing
    L.POk _ x -> pure $ mapMaybe (\(x,y) -> fmap (x,) y) x
  where
    location = mkRealSrcLoc (mkFastString "") 1 1
    buffer = stringToStringBuffer (T.unpack input)
    parseState = L.mkPStatePure parserFlags buffer location
    parserFlags = L.mkParserFlags (foldl' xopt_set initialDynFlags enabledExts)
    initialDynFlags =
      DynFlags
        { warningFlags = ES.empty,
          generalFlags =
            ES.fromList
              [ Opt_Haddock,
                Opt_KeepRawTokenStream
              ],
          extensions = [],
          extensionFlags = ES.empty,
          safeHaskell = Sf_Safe,
          language = Just Haskell2010
        }

pLexer :: L.P [(L.Token, Maybe Loc)]
pLexer = go
  where
    go = do
      r <- L.lexer False return
      case r of
        L _ L.ITeof -> return []
        _ ->
          case fixupToken r of
            x -> (x :) <$> go

fixupToken :: Located L.Token -> (L.Token, Maybe Loc)
fixupToken (L srcSpan tok) = (tok,srcSpanToLoc srcSpan)

srcSpanToLoc :: SrcSpan -> Maybe Loc
srcSpanToLoc (RealSrcSpan rss) =
  let start = realSrcSpanStart rss
   in Just $
      Loc (srcLocLine start) (srcLocCol start)
srcSpanToLoc _ = Nothing

enabledExts :: [Extension]
enabledExts =
  [ ForeignFunctionInterface,
    InterruptibleFFI,
    CApiFFI,
    Arrows,
    TemplateHaskell,
    TemplateHaskellQuotes,
    ImplicitParams,
    OverloadedLabels,
    ExplicitForAll,
    BangPatterns,
    PatternSynonyms,
    MagicHash,
    RecursiveDo,
    UnicodeSyntax,
    UnboxedTuples,
    UnboxedSums,
    DatatypeContexts,
    TransformListComp,
    QuasiQuotes,
    LambdaCase,
    BinaryLiterals,
    NegativeLiterals,
    HexFloatLiterals,
    TypeApplications,
    StaticPointers,
    NumericUnderscores,
    StarIsType
  ]
