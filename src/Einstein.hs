module Einstein where

{-
t < -readFile
tags = parseTags t
days = partitions (~== "<div class=\"field-day\">") tags
map (take 2 . map (head . drop 1) . partitions (~== "<p>")) days
-}
