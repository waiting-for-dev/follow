foo :: a -> Maybe b
bar :: a -> Maybe c

foobar :: a -> (a -> Maybe b) -> (a -> Maybe c) -> Either b c
foobar x f g =
  case f x of
    Nothing -> g x
    Just y  -> Just y
