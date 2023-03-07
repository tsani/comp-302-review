# Induction

Here are a bunch of functions and statements of theorems about them to practice.
Some of these we did in class.

The collection of theorems is split into two parts:
* Theorems that can be proven immediately by induction.
* Theorems that require a _generalization_ in order to be proven.

## Functions

### First-order functions

```ocaml
(* `rev l` calculates the reverse of `l`. This algorithm is O(n^2) AND isn't tail-recursive. *)
let rec rev l = match l with
  | [] -> []
  | x :: xs -> app (rev xs) [x]

(* `rev_tr l acc` prepends the reverse of `l` onto the given `acc`.
   Calling it as `rev_tr l []` gives a more efficient `rev`. *)
let rec rev_tr l acc = match l with
  | [] -> acc
  | x :: xs -> rev_tr xs (x :: acc)

(* Computes the concatenation of l1 with l2. *)
let rec app l1 l2 = match l1 with
  | [] -> l2
  | x :: xs -> x :: app xs l2

(* Calculates the length of the given list. *)
let rec len l = match l with
  | [] -> 0
  | x :: xs -> 1 + len xs

(* Calculates the length of the given list tail-recursively using an accumulator. *)
let rec len_tr l acc = match l with
  | [] -> acc
  | x :: xs -> len_tr xs (1 + acc)

let rec sum l = match l with
  | [] -> 0
  | x :: xs -> x + sum xs

let rec sum_tr l acc = match l with
  | [] -> acc
  | x :: xs -> sum xs (x + acc)
```

### Higher-order functions

```ocaml
(* The ordinary implementation of map for lists. *)
let rec map f l = match l with
  | [] -> []
  | x :: xs -> f x :: map f xs

(* A tail-recursive version of map above using CPS. *)
let rec map_tr f l return = match l with
  | [] -> return []
  | x :: xs -> map_tr f xs (fun ys -> return (f x :: ys))

let rec fold_left (f : 'b -> 'a -> 'b) (acc : 'b) (l : 'a list) : 'b = match l with
  | [] -> acc
  | x :: xs -> fold_left f (f acc x) xs

let rec fold_right (f : 'a -> 'b -> 'b) (l : 'a list) (e : 'b) : 'b = match l with
  | [] -> e
  | x :: xs -> f x (fold_right f xs e)
```

## Theorems

THEOREM. (Append adds list lengths.) (Rank: `*`)
  For any l1, l2 : 'a list, we have:
    len (app l1 l2) = len l1 + len l2

PROOF. Seen in class on 20 February 2023.

THEOREM. (Map preserves length.) (Rank: `*`)
  For any f : 'a -> 'b and any l : 'a list, we have:
    len (map f l) = len l

PROOF. Seen in class on 20 February 2023.

Exercise to do in class 22 February 2023:

THEOREM. (Map distributes over append.) (Rank: `*`)
  For any l1, l2 : 'a list and any f : 'a -> 'b, we have:
    app (map f l1) (map f l2) = map f (app l1 l2)

PROOF. By induction on l1.

CASE. l1 = [].
    WTS: app (map f []) (map f l2) = map f (app [] l2)

    LHS = app (map f []) (map f l2)
        = app [] (map f l2)         -- by map
        = map f l2                  -- by app

    RHS = map f (app [] l2)
        = map f l2                  -- by app
        = LHS

CASE. l1 = x :: xs.
    WTS: app (map f (x :: xs)) (map f l2) = map f (app (x :: xs) l2)
    IH: app (map f xs) (map f l2) = map f (app xs l2)

    LHS = app (map f (x :: xs)) (map f l2)
        = app (f x :: map f xs) (map f l2)  -- by map
        = f x :: app (map f xs) (map f l2)  -- by app
        = f x :: map f (app xs l2)          -- by IH

    RHS = map f (app (x :: xs) l2)
        = map f (x :: app xs l2)            -- by app
        = f x :: map f (app xs l2)          -- by map
        = LHS

THEOREM. (Map and reverse commute.) (Rank: `*`)
    For any l : 'a list and any f : 'a -> 'b, we have:
        rev (map f l) = map f (rev l)

PROOF. Exercise.

THEOREM. (Append is associative.) (Rank: `*`)
    For any l1, l2, l3 : 'a list, we have:
        app l1 (app l2 l3) = app (app l1 l2) l3

PROOF. Exercise.

THEOREM. (Nil is the right identity of append.) (Rank: `*`)
    For any l : 'a list, we have:
        app l [] = l

It might be surprising that this needs a proof by induction. The reason is that `app` is defined by
recursion on the _first_ list, not the second one. By the definition of append, `app [] l = l`
is immediate, but whether `app l [] = l` will depend on `l`.

PROOF. Exercise.

THEOREM. (Reverse distributes backwards over append.)
    For any l1, l2 : 'a list, we have:
        rev (app l1 l2) = app (rev l2) (rev l1)

PROOF. Exercise.

First, trying to directly prove the equivalence `sum l = sum_tr l 0`.

THEOREM. (sum and sum_tr are equivalent)
    For any l : int list, we have:
        sum l = sum_tr l 0

PROOF. By induction l.

    CASE l = []. This will work.

    CASE l = x :: xs.
        WTS: sum (x :: xs) = sum_tr (x :: xs) 0
        IH: sum xs = sum_tr xs 0

        LHS = sum (x :: xs)
            = x + sum xs       -- by sum
            = x + sum_tr xs 0  -- by IH

        RHS = sum_tr (x :: xs) 0
            = sum_tr xs (0 + x)    -- by def sum
            = sum_tr xs x           -- by common sense

            ??????

        Now we're just stuck. We don't know that `x + sum_tr xs 0 = sum_tr xs x`.
        Need to generalize the theorem!

THEOREM. (sum and sum_tr are equivalent. Take 2.) (Rank: `**`)
    For any l : int list, and any acc : int, we have:
        acc + sum l = sum_tr l acc

How did we arrive at this generalization? First we observe that we had a _fixed constant_ 0 in the
statement of the theorem before. We change that to a variable:
    sum l = sum_tr l acc
But now the LHS and RHS of the statement don't agree!
For example, is sum [1;2;3] = sum_tr [1;2;3] 5? No!
`sum_tr` is actually itself a _generalization_ of `sum`, because it doesn't _just_ add the elements
of the list; it also tacks on whatever the initial `acc` was. So we need to _reflect_ this extra
work into the LHS of the theorem statement. That gives `acc + sum l = sum_tr l acc` -- we needed to
explicitly make the LHS "do this extra thing" so that its behaviour matches what the RHS is doing.

PROOF. By induction on l.

    CASE l = [].
        WTS: acc + sum [] = sum_tr [] acc

        LHS = acc + sum []
            = acc + 0           -- by def of sum
            = acc               -- by common sense

        RHS = sum_tr [] acc
            = acc               -- by def of sum_tr
            = LHS

    CASE l = x :: xs.
        WTS: acc + sum (x :: xs) = sum_tr (x :: xs) acc
        IH: for any acc : int, we have: acc + sum xs = sum_tr xs acc
            ^- this is crucial! The IH is more stronger than before, because of this "for any acc"
               part. It will allow us to choose `acc + x` to the instantiation of `acc`; see below.

        LHS = acc + sum (x :: xs)
            = acc + (x + sum xs)

        RHS = sum_tr (x :: xs) acc
            = sum_tr xs (acc + x)
            = (acc + x) + sum xs   -- by IH with acc := acc + x
                -- specialized IH: (acc + x) + sum xs = sum_tr xs (acc + x)
                -- ^ you don't need to write this specialized IH down on a test. But it can be
                --   helpful for you to write it down when you're practicing.
            = LHS   -- by common sense

let rec map_tr f l return = match l with
  | [] -> return []
  | x :: xs -> map_tr f xs (fun ys -> return (f x :: ys))

THEOREM. (map and map_tr are equivalent)
    For any f : 'a -> 'b, and any l : 'a list, we have:
        map f l = map_tr f l (fun x -> x)

We actually can't prove this theorem for the same reason we couldn't directly prove
`sum l = sum_tr l 0`. We need to generalize! Instead of using the constant choice for the
continuation `fun x -> x`, we need it to be a variable.

THEOREM. (map and map_tr are equivalent -- generalized) (Rank: `****`)
    For any f : 'a -> 'b, any any l : 'a list, and any return : 'b list -> 'r, we have:
        return (map f l) = map_tr f l return

PROOF. By induction on l.

CASE. l = [].
    WTS: return (map f []) = map_tr f [] return

    LHS = return (map f []) = return [] -- by map
    RHS = map_tr f [] return = return [] -- by map_tr
        = LHS

CASE. l = x :: xs.
    WTS: return (map f (x :: xs)) = map_tr f (x :: xs) return
    IH: for any return: return (map f xs) = map_tr f xs return

    LHS = return (map f (x :: xs))
        = return (f x :: map f xs) -- by map

    RHS = map_tr f (x :: xs) return
        = map_tr f xs (fun ys -> return (f x :: ys)) -- by map_tr
        = (fun ys -> return (f x :: ys)) (map f xs)
          -- ^ by IH with return := (fun ys -> return (f x :: ys))
        = return (f x :: map f xs)
          -- ^ by function application, substituting `map f xs` for `ys`.
        = return (map f (x :: xs)) -- by map, backwards
        = LHS

THEOREM. (sum_tr is a left fold.) (Rank: `*`)
    For any l : int list, and any `acc : int` we have:
        sum_tr l acc = fold_left (+) acc l

PROOF. Exercise.

THEOREM. (sum is a right fold.) (Rank: `*`)
    For any l : int list, we have:
        sum l = fold_right (+) l 0

PROOF. By induction on l.

CASE. l = [].
    LHS = sum [] = 0
    RHS = fold_right (+) [] 0 = 0 = LHS

CASE. l = x :: xs.
    WTS: sum (x :: xs) = fold_right (+) (x :: xs) 0
    IH: sum xs = fold_right (+) xs 0

    LHS = sum (x :: xs) = x + sum xs -- by sum
        = x + fold_right (+) xs 0    -- by IH

    RHS = fold_right (+) (x :: xs) 0
        = (+) x (fold_right (+) xs 0)  -- by fold_right
        = x + fold_right (+) xs 0      -- rewriting infix operator +
        = LHS

THEOREM. (Equivalence of fold_left and fold_right for associative operations.) (Rank: `***`)
    Suppose `f : 'a -> 'a -> 'a` is an associative operation, i.e. for any x, y, z : 'a, we have:
        f x (f y z) = f (f x y) z
    Suppose `e : 'a` is an identity of `f`, i.e. for any `x : 'a`, we have:
        f e x = f x e = x
    Then, for any l : 'a list, we have:
        fold_left f e l = fold_right f l e

    We can't prove this directly, so we prove a generalization:
    for any l : 'a list, and any acc : 'a, we have:
        fold_left f acc l = f acc (fold_right f l e)

PROOF. Exercise.

THEOREM. (sum_tr and sum are equivalent. Take 3.)
    For any l : int list, we have: sum l = sum_tr l 0.

PROOF. By previous theorems:
    sum l = fold_right (+) l 0    -- by equivalence of sum to a right fold
          = fold_left (+) 0 l     -- by equivalence of folds for associative operations
          = sum_tr l 0            -- by equivalence of sum_tr to a left fold

THEOREM. (length is equivalent to a sum of a constant map.) (Rank: `*`)
    for any l : 'a list, we have:
        len l = sum (map (fun _ -> 1) l)

PROOF. Exercise.

THEOREM. (Reverse is its own inverse.) (Rank: `**`)
    For any l : 'a list, we have:
        rev (rev l) = l

To prove this, you will need to justify one step using a previous theorem:
    LEMMA. for any l1, l2 : 'a list, we have: rev (app l1 l2) = app (rev l2) (rev l1)
