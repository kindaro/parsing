open Grammar
open Chars
open Unicode

let cat_graphemes = sf "fence CAT house CAT CAT barn"

let more_cats = rule "CAT" "CAT CAT"

let cats_purr = rule "CAT CAT" "(two cats purr)"

let cat_meows = rule "CAT" "(cat meows)"

let cat_grammar: (grapheme, grapheme) grammar =
  {start = {source = Intermedial (char "S"); target = [cat_graphemes]}; rules = [more_cats; cats_purr; cat_meows]}
