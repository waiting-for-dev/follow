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
import           Follow                  (Result, unwrapResult)
import           Follow.Digesters.Pocket

main :: IO ()
main =
  withResult requestTokenStep $ \(rToken, url) -> do
    putStrLn "Please, press ENTER after visiting:"
    putStrLn $ T.unpack url
    getChar
    withResult (accessTokenStep rToken) $ \aToken -> do
      putStrLn "Your access token is:"
      putStrLn $ T.unpack aToken

withResult :: Result a -> (a -> IO ()) -> IO ()
withResult result f = do
  unwrapped <- unwrapResult result
  case unwrapped of
    Left error -> print error
    Right x    -> f x
