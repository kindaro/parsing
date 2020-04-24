open Grammar
open Chars
open Unicode

let cat_graphemes = sf "fence CAT house CAT CAT garage"

let more_cats = rule "CAT" "CAT CAT"

let cats_purr = rule "CAT CAT" "(two cats purr)"

let cat_meows = rule "CAT" "(cat meows)"

let cat_grammar: (grapheme, grapheme) grammar = {start = cf_rules "S" ["CAT"]; rules = [more_cats; cats_purr; cat_meows]}
