
{- isMatch s p
   Matches the pattern p to the string s. 
   The '?' character matches any character. 
   The '*' character matches any sequence of characters.
   RETURNS: True if pattern matches the string, False otherwise.
   EXAMPLES: isMatch "" "" = True
             isMatch "hello"  "*el*" = True
             isMatch "hello" "el" = False
 -}
isMatch :: String -> String -> Bool

-- VARIANT: length s + length p
isMatch "" "" = True
isMatch "" "*" = True
isMatch "" (_ : []) = False
isMatch (s : []) "*" = True
isMatch (s : []) (p : ps) | p == '*' = isMatch (s : []) ps
                          | p == '?' = isMatch [] ps
                          | p == s = isMatch [] ps
                          | otherwise = False
isMatch (_ : _) ('*' : []) = True
isMatch (_ : _) (_ : []) = False
isMatch (s : ss) ('*': ps) = if isMatch (s : ss) ps then True else isMatch ss ('*' : ps)
isMatch (s : ss) (p : ps) = if (s == p) || (p == '?') then isMatch ss ps else False
isMatch _ _ = False
