{-# LANGUAGE OverloadedStrings #-}
module HaddockMd (plugin) where

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Lazy.Char8  as LBSC
import           Data.Generics.Uniplate.Data
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text
import           GHC.Hs
import           GhcPlugins                  hiding ((<>))
import           System.Exit
import           System.Process.Typed

-- | Optionally provide a pandoc format as a plugin argument (@-fplugin=HaddockMd:myformat@). The default is @markdown@.
plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = pluginAct
  }

pluginAct :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
pluginAct opts _ = convertParsedModule $ case opts of
  [format] -> format
  _ -> "markdown"

convertParsedModule :: String -> HsParsedModule -> Hsc HsParsedModule
convertParsedModule fmt mod = do
  converted <- transformBiM (convDocString fmt) (hpm_module mod)
  pure mod { hpm_module = converted }

remEscapeChars :: Text -> Text
remEscapeChars =
  Text.replace "\\'" "'"
  . Text.replace "\\\"" "\""
  . Text.replace "\\<" "<"

asUtf8 :: (Text -> Text) -> BS.ByteString -> BS.ByteString
asUtf8 f = Text.encodeUtf8 . f . Text.decodeUtf8

pandocShell :: String -> LBS.ByteString -> IO LBS.ByteString
pandocShell fmt inp = do
  (code, out, err) <- readProcess process
  case code of
    ExitSuccess   -> pure out
    ExitFailure _ -> fail $ "error parsing haddocks: " <> LBSC.unpack err
  where
    process =
      setStdin (byteStringInput inp)
      . setStdout byteStringOutput
      $ proc "pandoc" ["-f", fmt <> "-smart", "-t", "haddock"]

convDocString :: String -> HsDocString -> Hsc HsDocString
convDocString fmt =
  fmap (mkHsDocStringUtf8ByteString . asUtf8 remEscapeChars . LBS.toStrict)
  . liftIO
  . pandocShell fmt
  . LBS.fromStrict
  . hsDocStringToByteString
