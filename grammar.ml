type 'a terminal = Terminal of 'a

type 'b intermedial = Intermedial of 'b

type ('a, 'b) symbol = Terminary of 'a terminal | Intermediary of 'b intermedial

type ('a, 'b) rewrite_rule =
  { source: ('a, 'b) symbol * ('a, 'b) symbol list
  ; target: ('a, 'b) symbol list }

type ('a, 'b) context_free_rule_group =
  { source: 'a intermedial
  ; target: ('a, 'b) symbol list list }

type ('a, 'b) grammar =
  { rules: ('a, 'b) rewrite_rule list
  ; start: ('a, 'b) context_free_rule_group }

(*

This way to define a grammar is isomorphic to the traditional quadruple ⟨non-terminals, terminals, rules, start symbol⟩ but enforces the invariants on the type level, at the cost of some processing complexity.

* I am calling non-terminals _«intermedials»_ to add perceptual distinctiveness.
* There are two kinds of rules: rewrite and context-free. While the former are more general, the latter necessarily occur even in context-sensitive grammars — as transformations of the start symbol.
* For a rewrite rule, the source is a non-empty list, as opposed to a simple list.
* The source of a context-free rule is always a single intermediary. Context-free rules with the same source must be grouped.
* A grammar definition has exactly one dedicated context-free rule that defines the start symbol.

The sets of terminals and intermediaries can be derived by scanning the rule set. The start symbol is defined by the dedicated context-free rule group. The rule should then be added to the other rules.

*)
