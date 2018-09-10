{-|
Description: Executable to authorize Follow into Pocket.

"Follow.Digesters.Pocket" needs the user to grant privileges to our
app so that we can add articles to her account.

Run it with: @stack exec follow_pocket_auth@.

After running it, an access token is printed to stdout. This can be
used to run the pocket digester (see `Follow.Digesters.Pocket.digest`.
-}
module Main where

import           Data.Text               (Text)
import qualified Data.Text               as T (concat, unpack)
import           Follow.Digesters.Pocket

main :: IO ()
main = do
  (rToken, url) <- requestTokenStep
  putStrLn "Please, press ENTER after visiting:"
  putStrLn $ T.unpack url
  getChar
  aToken <- accessTokenStep rToken
  putStrLn "Your access token is:"
  putStrLn $ T.unpack aToken
