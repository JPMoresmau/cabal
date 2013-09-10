-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.ParseUtils
-- Copyright   :  (c) The University of Glasgow 2004
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Utilities for parsing 'PackageDescription' and 'InstalledPackageInfo'.
--
-- The @.cabal@ file format is not trivial, especially with the introduction
-- of configurations and the section syntax that goes with that. This module
-- has a bunch of parsing functions that is used by the @.cabal@ parser and a
-- couple others. It has the parsing framework code and also little parsers for
-- many of the formats we get in various @.cabal@ file fields, like module
-- names, comma separated lists etc.

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the University nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

-- This module is meant to be local-only to Distribution...

{-# OPTIONS_HADDOCK hide #-}
module Distribution.ParseUtils (
        LineNo, PError(..), PWarning(..), locatedErrorMsg, syntaxError, warning,
        runP, runE, ParseResult(..), catchParseError, parseFail, showPWarning,
        Field(..), fName, lineNo,
        FieldDescr(..), ppField, ppFields, readFields, readFieldsFlat,
        showFields, showSingleNamedField, showSimpleSingleNamedField,
        parseFields, parseFieldsFlat,
        parseFilePathQ, parseTokenQ, parseTokenQ',
        parseModuleNameQ, parseBuildTool, parsePkgconfigDependency,
        parseOptVersion, parsePackageNameQ, parseVersionRangeQ,
        parseTestedWithQ, parseLicenseQ, parseLanguageQ, parseExtensionQ,
        parseSepList, parseCommaList, parseOptCommaList,
        showFilePath, showToken, showTestedWith, showFreeText, parseFreeText,
        field, simpleField, listField, spaceListField, commaListField,
        optsField, liftField, boolField, parseQuoted,

        UnrecFieldParser, warnUnrec, ignoreUnrec,
  ) where

import Distribution.Compiler (CompilerFlavor, parseCompilerFlavorCompat)
import Distribution.License
import Distribution.Version
         ( Version(..), VersionRange, anyVersion )
import Distribution.Package     ( PackageName(..))
import Distribution.Dependency ( Dependency(..))
import Distribution.ModuleName (ModuleName)
import Distribution.Compat.ReadP as ReadP hiding (get)
import Distribution.Compat.ParseUtils
import Distribution.ReadE
import Distribution.Text
         ( Text(..) )
import Distribution.Simple.Utils
         ( comparing, intercalate, lowercase, normaliseLineEndings )
import Language.Haskell.Extension
         ( Language, Extension )

import Text.PrettyPrint hiding (braces)
import Data.Char (isSpace, toLower, isAlphaNum, isDigit)
import Data.Maybe       (fromMaybe)
import Data.Tree as Tree (Tree(..), flatten)
import qualified Data.Map as Map
import Control.Monad (foldM, ap) 
import Control.Applicative (Applicative(..))
import System.FilePath (normalise)
import Data.List (sortBy)

-- -----------------------------------------------------------------------------


runE :: LineNo -> String -> ReadE a -> String -> ParseResult a
runE line fieldname p s =
    case runReadE p s of
      Right a -> ParseOk (utf8Warnings line fieldname s) a
      Left  e -> syntaxError line $
        "Parse of field '" ++ fieldname ++ "' failed (" ++ e ++ "): " ++ s


optsField :: String -> CompilerFlavor -> (b -> [(CompilerFlavor,[String])])
             -> ([(CompilerFlavor,[String])] -> b -> b) -> FieldDescr b
optsField name flavor get set =
   liftField (fromMaybe [] . lookup flavor . get)
             (\opts b -> set (reorder (update flavor opts (get b))) b) $
        field name (hsep . map text)
                   (sepBy parseTokenQ' (munch1 isSpace))
  where
        update _ opts l | all null opts = l  --empty opts as if no opts
        update f opts [] = [(f,opts)]
        update f opts ((f',opts'):rest)
           | f == f'   = (f, opts' ++ opts) : rest
           | otherwise = (f',opts') : update f opts rest
        reorder = sortBy (comparing fst)



------------------------------------------------------------------------------



parseBuildTool :: ReadP r Dependency
parseBuildTool = do name <- parseBuildToolNameQ
                    ver <- betweenSpaces $
                           parseVersionRangeQ <++ return anyVersion
                    return $ Dependency name ver


-- pkg-config allows versions and other letters in package names,
-- eg "gtk+-2.0" is a valid pkg-config package _name_.
-- It then has a package version number like 2.10.13
parsePkgconfigDependency :: ReadP r Dependency
parsePkgconfigDependency = do name <- munch1
                                      (\c -> isAlphaNum c || c `elem` "+-._")
                              ver <- betweenSpaces $
                                     parseVersionRangeQ <++ return anyVersion
                              return $ Dependency (PackageName name) ver


parseVersionRangeQ :: ReadP r VersionRange
parseVersionRangeQ = parseQuoted parse <++ parse


parseTestedWithQ :: ReadP r (CompilerFlavor,VersionRange)
parseTestedWithQ = parseQuoted tw <++ tw
  where
    tw :: ReadP r (CompilerFlavor,VersionRange)
    tw = do compiler <- parseCompilerFlavorCompat
            version <- betweenSpaces $ parse <++ return anyVersion
            return (compiler,version)



-- urgh, we can't define optQuotes :: ReadP r a -> ReadP r a
-- because the "compat" version of ReadP isn't quite powerful enough.  In
-- particular, the type of <++ is ReadP r r -> ReadP r a -> ReadP r a
-- Hence the trick above to make 'lic' polymorphic.

parseLanguageQ :: ReadP r Language
parseLanguageQ = parseQuoted parse <++ parse

parseExtensionQ :: ReadP r Extension
parseExtensionQ = parseQuoted parse <++ parse



-- --------------------------------------------
-- ** Pretty printing


showTestedWith :: (CompilerFlavor,VersionRange) -> Doc
showTestedWith (compiler, version) = text (show compiler) <+> disp version


