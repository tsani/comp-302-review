# Induction on (syntax) trees

This document contains a number of theorems about transformations of syntax trees for you to prove
by induction.

Here is a simple syntax tree definition.

```ocaml
type exp =
  | Lit of int
  | Add of exp * exp
  | Var of string
```

These syntax trees can be evaluated, provided that we have an _environment_ mapping variable names
to integers.

```ocaml
type env = (string * int) list

let rec eval (env : env) : exp -> int = function
  | Var x -> List.assoc x env (* look up the name in the environment *)
  | Lit n -> n
  | Add (e1, e2) -> eval e1 + eval e2
```

## Basic simplification

However, even without an environment, it is possible to _simplify_ these trees by collapsing `Add`
nodes whose simplified children turn out to be `Lit`s.

```ocaml
let rec simplify1 : exp -> exp = function
  | Var x -> Var x
  | Lit n -> Lit n
  | Add (e1, e2) -> match simplify1 e1, simplify1 e2 with
    | Lit n1, Lit n2 -> Lit (n1 + n2)
    | e1, e2 -> Add (e1, e2)
```

**Theorem.** Simplification (1) preserves evaluation: `eval env e = eval env (simplify1 e)`

HINT: in the case for `Add`, you need to proceed by subcases on whether both `simplify e1 = Lit n2`
and `simplify e2 = Lit n2`, in parallel with the way the algorithm proceeds by pattern matching on
the results of the recursive calls.

## A more complex simplification

Notice that `simplify1` above cannot deal with certain situations, such as `Add (Var x, Lit 0)`,
which we could simplify to just `Var x`.

```ocaml
let rec simplify2 : exp -> exp = function
  | Var x -> Var x
  | Lit n -> Lit n
  | Add (e1, e2) -> match simplify2 e1, simplify2 e2 with
    | e1, Lit 0 -> e1
    | Lit 0, e2 -> e2
    | e1, e2 -> Add (e1, e2)
```

**Theorem.** Simplification (2) preserves evaluation: `eval env e = eval env (simplify2 e)`

HINT: this time there are three subcases according to the matching in the simplification algorithm.

## Substitution

A different way to evaluate these syntax trees would be to apply the environment as a substitution
and then use simplification. Notice that the susbtitution will replace all `Var x` nodes with `Lit
n` nodes, so the simplification will always be able to make progress.
These substitutions are a lot simpler than those seen in class, since the language contains no
binding constructs such as functions or let-expressions.

```ocaml
let rec subst (env : env) : exp -> exp = function
  | Var x -> Lit (List.assoc x env)
  | Lit n -> Lit n
  | Add (e1, e2) -> Add (subst env e1, subst env e2)
```

**Theorem.** Substitution followed by simplification performs evaluation:
    `simplify1 (subst env e) = Lit (eval env e)`

## Flattenings

The tree structure of the syntax tree above doesn't really give us anything due to the
associativity of the only operation in the language, namely `Add`. Instead, we could flatten the
tree into a list and sum the list.

```ocaml
type entry = V of string | I of int

let rec flatten (e : exp) (acc : entry list) : entry list = match e with
  | Lit n -> I n :: acc
  | Var x -> V x :: acc
  | Add (e1, e2) -> flatten e1 (flatten e2 acc)

let rec sum (env : env) : entry list -> int = function
  | [] -> 0
  | I n :: es -> n + sum env es
  | V x :: es -> List.assoc x env + sum env es
```

**Theorem.** Summing the flattened tree is the same as evaluating the tree:
    `sum env (flatten e []) = eval env e`

This theorem cannot be proven directly. It requires a generalization. Moreover, it's a tricky
generalization to find.

- We know we will need to change `[]` into `acc`, to give ourselves a more powerful IH.
- But now the LHS and RHS don't agree on what they compute!
    - the LHS flattens `e` and sticks it onto the front of `acc`, then adds up all of that
    - the RHS simply evaluates `e`, so it's missing `acc`'s contribution to the total sum.
- Think about what you need to add to the RHS to restore the equality.

## A more complex flattening

It's somewhat wasteful when performing the flattening to hold separate `I` entries in the list,
since we're going to sum those up later anyway. Instead, we can define a more efficient flat
representation of these trees:

```ocaml
type flat = int * string list
let empty = (0, []) (* an empty flat tree *)
```

This representation is a tuple consisting of the sum of all the Lit nodes in the tree and a list of
all the variable names that appear in the tree.
Next we define a function `flatten2` that computes such a representation from a given tree.

```ocaml
let rec flatten2 (e : exp) (k, vars : flat) : flat = match e with
  | Var x -> (k, x :: vars) (* extend the list of variables *)
  | Lit n -> (n + k, vars) (* increase the partial sum *)
  | Add (e1, e2) -> flatten2 e1 (flatten2 e2 (k, vars))
```

Now to sum _this_ flat representation, we need to map each variable to its integer value according
to an environment, then add up everything. We can treat the `int` part of the flat representation
as an accumulator for this sum.

```ocaml
let rec sum_flat env (k, vars : flat) : int = match vars with
  | [] -> k
  | x :: xs -> sum_flat env (List.assoc x env + k, xs)
```

Finally, we want to show that summing this flat representation is the same as evaluation.

**Theorem.** `sum_flat env (flatten e empty) = eval env e`

Again, this theorem cannot be proven directly and requires generalization.
