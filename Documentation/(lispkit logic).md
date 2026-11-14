# LispKit Logic

Library `(lispkit logic)` implements _miniKanren_ with _symbolic constraints_, an embedded domain-specific language for _relational programming_, i.e. logic programming in Scheme. miniKanren programs consist of _relations_, which are written as ordinary Scheme procedures that return _goals_. In contrast to functions, relations do not distinguish between inputs and outputs. A relation such as `appendo` can be used for appending two lists, for splitting a list into two parts, and for enumerating all the triples of lists for which the relation holds.

The core miniKanren language is small, with only three logical operators and one interface operator:

   - `==` unifies two terms (it _constrains_ them to be equal),
   - `fresh` introduces new logic variables,
   - `conde` expresses a choice between alternatives, and
   - `run` (and `run*`) is the interface to Scheme: it runs a query and returns the answers as a list.

Library `(lispkit logic)` adds to this core

   - the constraints `=/=` (disequality), `symbolo`, `numbero` (type constraints), and `absento` (a term does not occur in another term), which are tracked symbolically and show up in the answers of queries, and
   - the non-relational operators `conda` and `condu`, which commit to a choice and which can be used for cutting off the search.

```scheme
(import (lispkit base)
        (lispkit logic))
(run* (q) (== q 'tea))
  ⇒  (tea)
(run* (q) (conde ((== q 'tea)) ((== q 'coffee))))
  ⇒  (tea coffee)
(run* (q) (symbolo q) (=/= q 'tea))
  ⇒  ((_.0 (=/= ((_.0 tea))) (sym _.0)))
```

The implementation of the library is based on _miniKanren with symbolic constraints_ by Jason Hemann, William E. Byrd, and Daniel P. Friedman. The search strategy is _complete_: it interleaves the evaluation of alternatives, so that every answer is eventually found, even if some of the alternatives lead to infinite computations. The library is an alternative to library `(lispkit prolog)`, which provides Prolog-style logic programming with cuts and other impure features, and which searches depth-first.

## Tutorial

This tutorial introduces the programming model of miniKanren step by step. By convention, names of relations end in the letter `o`. Examples are written with an arrow `⇒` to the right of an expression to indicate its result. All examples build on each other and use the following imports:

```scheme
(import (lispkit base)
        (lispkit logic))
```

### Goals and queries

Logic programs are made of _goals_. A goal is a condition on a set of _logic variables_, which can either _succeed_ or _fail_. When a goal succeeds, it may do so in different ways, and, in the process, it may restrict the values the variables can have. There are two constant goals: `succeed` always succeeds, and `fail` always fails.

A _query_ asks for the values of a logic variable, the _query variable_, for which a sequence of goals succeeds. Queries are formulated with `run*`. The first argument of `run*` declares the query variable (the name `q` is a convention). The remaining arguments are goals, which all need to succeed for a value to be an answer.

```scheme
(run* (q) succeed)  ⇒  (_.0)
(run* (q) fail)     ⇒  ()
```

The result of a query is a list of _answers_. `run*` returns all of them. The second query has no answers, which is expressed by the empty list. The first query has a single answer, in which `q` has no particular value: any value of `q` makes the goal succeed. Variables that do not have a value are shown as `_.0`, `_.1`, etc. in the answers. They are called _reified_ variables.

### Unification

The most important goal is the _unification_ `==`, which succeeds if its two arguments can be made equal by giving values to their logic variables. Typically, one of the arguments is a variable, and unification gives this variable a value:

```scheme
(run* (q) (== q 1))             ⇒  (1)
(run* (q) (== 1 q))             ⇒  (1)
(run* (q) (== q 'a) (== q 'a))  ⇒  (a)
(run* (q) (== q 'a) (== q 'b))  ⇒  ()
```

The order of the arguments of `==` is irrelevant. Once a variable has a value, this value cannot change. Therefore, the last query has no answer: `q` cannot be both `a` and `b`. This is also the reason why all the goals of a query behave like a conjunction (a logical "and").

Values (_terms_) are atoms, such as symbols, numbers, strings, characters, booleans, and the empty list, or pairs of terms. Lists are terms as well. Unification of two pairs unifies their first and their second components. Quasiquotation is a convenient way to write terms with variables:

```scheme
(run* (q) (== `(1 ,q 3) '(1 2 3)))  ⇒  (2)
(run* (q) (== `(1 ,q 3) '(1 2 4)))  ⇒  ()
(run* (q) (== '(1 2 3) `(1 . ,q)))  ⇒  ((2 3))
(run* (q) (== q '(1 . 2)))          ⇒  ((1 . 2))
```

Atoms are compared with `equal?`. Strings are equal if they have the same characters, and `1` and `1.0` are not equal. The elements of vectors are not unified; vectors are treated as atoms and are only equal to other vectors with equal elements:

```scheme
(run* (q) (== "ab" (string #\a #\b)))      ⇒  (_.0)
(run* (q) (== 1 1.0))                      ⇒  ()
(run* (q) (== (vector 1 2) (vector 1 2)))  ⇒  (_.0)
(run* (q) (== (vector 1 q) (vector 1 2)))  ⇒  ()
```

### Fresh variables

Further logic variables are introduced by `fresh`. The form `(fresh (x y) goal ...)` creates new variables `x` and `y` that are visible in the goals, and which are treated like a conjunction.

```scheme
(run* (q) (fresh (x) (== x 5) (== q (list x 'b x))))  ⇒  ((5 b 5))
(run* (q) (fresh (x y) (== q (list x y))))            ⇒  ((_.0 _.1))
(run* (q) (fresh (x) (== q (list x x))))              ⇒  ((_.0 _.0))
```

The last two queries have a single answer which is a list with two elements. In the first of these, the elements are independent of each other. They are different variables, which is why they are shown as `_.0` and `_.1`. In the second one, the two elements are the same variable, and so the list has the form `(_.0 _.0)`. Reified variables are numbered in the order in which they appear in an answer, so the same variable may have a different name in a different answer.

Variables are lexically scoped. A variable of a `fresh` expression is unrelated to a variable with the same name in an enclosing expression:

```scheme
(run* (q) (fresh (q) (== q 5)))  ⇒  (_.0)
```

Queries can have more than one query variable. The answers are then lists of the values of the variables:

```scheme
(run* (x y) (== x 1) (== y x))   ⇒  ((1 1))
(run 1 (x y z) (== x z))         ⇒  ((_.0 _.1 _.0))
```

The query `(run* (x y) goal ...)` is a shorthand for `(run* (q) (fresh (x y) goal ... (== q (list x y))))`.

### Disjunction

Choices are expressed with `conde`. A `conde` expression consists of a number of _clauses_. Each clause is a sequence of goals, which are treated like a conjunction. A `conde` expression succeeds if one or more of its clauses succeed, and it contributes all of the answers of all of its clauses. The "e" of `conde` stands for "every": every clause that succeeds contributes. (Do not confuse it with `cond` of Scheme, which evaluates only one clause.)

```scheme
(run* (q) (conde ((== q 'tea)) ((== q 'coffee))))      ⇒  (tea coffee)
(run* (x y) (== x 1) (conde ((== y 'a)) ((== y 'b))))  ⇒  ((1 a) (1 b))
(run* (q) (conde ((== q 'a) (== q 'b)) ((== q 'c))))   ⇒  (c)
```

Each clause starts from the same bindings, and bindings made in one clause are not visible in the others. In the last example, the first clause fails because `q` cannot be both `a` and `b`, but the second clause succeeds.

It is not specified in which order the answers of a query appear. Programs should not depend on it.

### Limiting the number of answers

Queries can have infinitely many answers. The relation `natso` below describes natural numbers in Peano notation: a number is either `z` (zero), or a term `(s n)` where `n` is a number.

```scheme
(define (natso n)
  (conde ((== n 'z))
         ((fresh (m)
            (== n `(s ,m))
            (natso m)))))
```

For such queries, `run*` never terminates because it tries to collect all answers. The query form `(run n (q) goal ...)` returns the first `n` answers only, or all of them if there are fewer than `n`:

```scheme
(run 4 (q) (natso q))                       ⇒  (z (s z) (s (s z)) (s (s (s z))))
(run 1 (q) (natso q))                       ⇒  (z)
(run 10 (q) (conde ((== q 1)) ((== q 2))))  ⇒  (1 2)
(run 0 (q) (natso q))                       ⇒  ()
; (run* (q) (natso q))                      ⇒  never terminates
```

Note that `run*` is the same as `run` with an infinite bound, i.e. `(run* (q) goal ...)` is equivalent to `(run #f (q) goal ...)`.

The search is _fair_: it interleaves the alternatives of `conde` expressions, instead of exploring one alternative to the end before trying the next one. As a consequence, an alternative with infinitely many answers does not prevent an alternative with only a few answers from contributing. This is illustrated by the two relations `anyo`, which succeeds arbitrarily often if its argument succeeds, and `nevero`, which never succeeds:

```scheme
(define (anyo goal)
  (conde (goal)
         ((anyo goal))))

(define (nevero)
  (conde ((nevero))))

(run 5 (q) (anyo (== q 'x)))                     ⇒  (x x x x x)
(run 3 (q) (conde ((anyo (== q 1))) ((== q 2)))) ⇒  (2 1 1)
(run 1 (q) (conde ((nevero)) ((== q 1))))        ⇒  (1)
; (run 2 (q) (conde ((nevero)) ((== q 1))))      ⇒  never terminates
```

The last query has only one answer, but the search cannot know this, and it continues forever looking for a second one.

### Relations

A relation is a Scheme procedure that returns a goal. As in the case of `natso`, relations are typically defined recursively, using `conde` for distinguishing the cases and `fresh` for introducing the variables they need. The following relation `appendo` holds if appending lists `l` and `s` results in list `out`. It is a direct translation of the usual definition of list append: appending a list to the empty list gives the list; if the first list is a pair `(a . d)`, the result is a pair with the same first element, and the result of appending the rest of the first list to the second list.

```scheme
(define (appendo l s out)
  (conde ((== '() l) (== s out))
         ((fresh (a d res)
            (== `(,a . ,d) l)
            (== `(,a . ,res) out)
            (appendo d s res)))))
```

In its simplest use, `appendo` is used like the Scheme procedure `append`:

```scheme
(run* (q) (appendo '(a b) '(c) q))  ⇒  ((a b c))
```

But since `appendo` is a relation, it also works if other arguments are unknown. The following queries find the suffix and the prefix for a given result, and all the ways to split a list into two parts:

```scheme
(run* (q) (appendo '(a b) q '(a b c)))  ⇒  ((c))
(run* (q) (appendo q '(c) '(a b c)))    ⇒  ((a b))
(run* (x y) (appendo x y '(1 2)))       ⇒  ((() (1 2)) ((1) (2)) ((1 2) ()))
```

If none of the arguments is known, there are infinitely many answers. The following query asks for the first three triples of lists `x`, `y` and `z` for which `z` is the result of appending `y` to `x`. The second answer, for instance, says: if `x` is a list with one element, and `y` is anything, then `z` is a list that starts with the element of `x`, followed by `y`.

```scheme
(run 3 (x y z) (appendo x y z))
  ⇒  ((() _.0 _.0)
      ((_.0) _.1 (_.0 . _.1))
      ((_.0 _.1) _.2 (_.0 _.1 . _.2)))
```

Another typical relation is `membero`, which holds if `x` is an element of list `l`. It can be used for testing for membership, for enumerating the elements of a list, and for generating lists containing a given element. Note that the first query has only one answer, but the second one has two since `a` occurs twice in the list. The answer `_.0` does not constrain `q` in any way.

```scheme
(define (membero x l)
  (fresh (a d)
    (== l `(,a . ,d))
    (conde ((== a x))
           ((membero x d)))))

(run* (q) (membero 'b '(a b c)))  ⇒  (_.0)
(run* (q) (membero 'a '(a b a)))  ⇒  (_.0 _.0)
(run* (q) (membero q '(a b c)))   ⇒  (a b c)
(run 3 (q) (membero 'a q))        ⇒  ((a . _.0) (_.0 a . _.1) (_.0 _.1 a . _.2))
```

The last answers show lists with `a` at the first, second, and third position, followed by any other elements. Finally, here is addition of Peano numbers. Relation `pluso` relates two numbers with their sum. It can be used for adding numbers, but also for subtracting them, and for enumerating all the ways in which a number can be written as a sum.

```scheme
(define (pluso a b c)
  (conde ((== a 'z) (== b c))
         ((fresh (a1 c1)
            (== a `(s ,a1))
            (== c `(s ,c1))
            (pluso a1 b c1)))))

(run* (q) (pluso '(s (s z)) '(s z) q))
  ⇒  ((s (s (s z))))
(run* (q) (pluso '(s z) q '(s (s (s z)))))
  ⇒  ((s (s z)))
(run* (x y) (pluso x y '(s (s z))))
  ⇒  ((z (s (s z))) ((s z) (s z)) ((s (s z)) z))
```

**The order of goals matters for termination.** Relations should be written such that the unifications, which make the arguments more concrete, come before the recursive calls. The following definition of `appendo` is logically equivalent to the previous one, but starts with the recursive call, and the search does not terminate unless the number of answers is limited:

```scheme
(define (appendo2 l s out)
  (conde ((== '() l) (== s out))
         ((fresh (a d res)
            (appendo2 d s res)
            (== `(,a . ,d) l)
            (== `(,a . ,res) out)))))

(run 1 (q) (appendo2 '(a b) '(c) q))   ⇒  ((a b c))
; (run* (q) (appendo2 '(a b) '(c) q))  ⇒  never terminates
```

The order of goals does not change the set of answers of a query, but it influences how efficiently they are found and whether the search terminates.

### Disequality

The constraint `=/=` is the opposite of `==`. The goal `(=/= u v)` succeeds as long as `u` and `v` are not forced to be equal. If it is not clear yet whether they are equal, because they contain variables without a value, the constraint is recorded and checked whenever more values become known. Such constraints are part of the answers:

```scheme
(run* (q) (=/= q 1))           ⇒  ((_.0 (=/= ((_.0 1)))))
(run* (q) (=/= q 1) (== q 1))  ⇒  ()
(run* (q) (=/= q 1) (== q 2))  ⇒  (2)
(run* (q) (== q 1) (=/= q 1))  ⇒  ()
```

The first answer reads: `q` is unconstrained, except that `_.0` is not equal to `1`. The constraint does not depend on the order of the goals. The following example shows a disequality between two lists. The constraint `(=/= ((_.0 1) (_.1 2)))` says that it is not possible that both `_.0` is `1` _and_ `_.1` is `2`; everything else is allowed. The answers show the constraints after all the other goals have been processed, so they do not mention variables that have been given a value in the meantime:

```scheme
(run* (q) (fresh (x y) (=/= `(,x ,y) '(1 2)) (== q (list x y))))
  ⇒  (((_.0 _.1) (=/= ((_.0 1) (_.1 2)))))
(run* (x y) (=/= x y) (== x 1) (== y 1))
  ⇒  ()
(run* (x y) (=/= x y) (== x 1) (== y 2))
  ⇒  ((1 2))
```

Disequality makes it possible to write relations that distinguish cases that exclude each other. The following relation `rembero` removes the first occurrence of `x` from list `l`. The last clause only applies if the first element is different from `x`, which makes sure that only the first occurrence is removed and that the relation does not produce more than the intended answer:

```scheme
(define (rembero x l out)
  (conde ((== l '()) (== out '()))
         ((fresh (a d)
            (== l `(,a . ,d))
            (== a x)
            (== out d)))
         ((fresh (a d res)
            (== l `(,a . ,d))
            (=/= a x)
            (== out `(,a . ,res))
            (rembero x d res)))))

(run* (q) (rembero 'b '(a b c b) q))  ⇒  ((a c b))
```

### Types

Constraints `symbolo` and `numbero` restrict a variable to symbols and numbers, respectively. They correspond to the Scheme predicates `symbol?` and `number?`. They are _constraints_ as they can be applied to variables without a value, and they remain in effect for all values the variable might get later:

```scheme
(run* (q) (symbolo q))
  ⇒  ((_.0 (sym _.0)))
(run* (q) (symbolo q) (== q 'x))
  ⇒  (x)
(run* (q) (symbolo q) (== q 5))
  ⇒  ()
(run* (q) (symbolo q) (numbero q))
  ⇒  ()
(run* (q) (fresh (x) (== q `(a ,x)) (numbero x)))
  ⇒  (((a _.0) (num _.0)))
(run* (q) (symbolo q) (=/= q 'a) (=/= q 'b))
  ⇒  ((_.0 (=/= ((_.0 a)) ((_.0 b))) (sym _.0)))
```

The results are answers with constraints. For example, `(_.0 (sym _.0))` stands for all symbols. The second last answer shows a list of two elements, where the second one is a number. The last answer shows how two constraints are combined: `q` is any symbol that is different from `a` and from `b`. Constraints that become trivial are dropped. For instance, `(run* (q) (symbolo q) (=/= q 5))` does not mention the disequality, as a symbol can never be equal to `5`.

### Absence

The constraint `(absento u v)` expresses that term `u` does not occur in term `v`, neither as `v` itself, nor as a subterm. It is useful for excluding symbols from structured data. Like the other constraints, `absento` is checked when more values become known:

```scheme
(run* (q) (absento 'a q))
  ⇒  ((_.0 (absento (a _.0))))
(run* (q) (absento 'a q) (== q '(b c)))
  ⇒  ((b c))
(run* (q) (absento 'a q) (== q '(b (c a))))
  ⇒  ()
(run* (q) (absento 'a q) (== q `(b ,(+ 1 2))))
  ⇒  ((b 3))
```

Constraints can interact. If the compound term `(b . _.0)` is to be free of `a`, then it is enough that `_.0` is free of `a`:

```scheme
(run* (q) (fresh (x y) (absento 'a x) (== x `(b . ,y)) (== q (list x y))))
  ⇒  ((((b . _.0) _.0) (absento (a _.0))))
(run* (q) (fresh (x y) (absento 'tag x) (symbolo y) (== x (list y 1)) (== q (list x y))))
  ⇒  ((((_.0 1) _.0) (=/= ((_.0 tag))) (sym _.0)))
```

In the second query, the symbolic constraint on `y` allows the library to simplify `absento` to a disequality: `y` is a symbol, so it can only be `tag` itself, which is not allowed.

### Reading answers

Answers are always shown in a normalized form: unbound variables are replaced by `_.0`, `_.1`, etc., and the constraints that remain are listed after the value. If there are no constraints, the answer is just the value. Otherwise, it is a list consisting of the value followed by the constraints, in the following order:

| Constraint | Meaning |
|:-----------|:--------|
| `(=/= ((x1 v1) (x2 v2) ...) ...)` | For each group `((x1 v1) (x2 v2) ...)`, it is not the case that `x1` is `v1` and `x2` is `v2` and ... at the same time |
| `(num x ...)` | All of the variables `x ...` are numbers |
| `(sym x ...)` | All of the variables `x ...` are symbols |
| `(absento (u x) ...)` | For every pair `(u x)`, term `u` does not occur in `x` |

For instance, `(_.0 (=/= ((_.0 a)) ((_.0 b))) (sym _.0))` is the answer for a symbol that is neither `a` nor `b`: two groups with a single disequality each. Constraints about variables that do not appear in the value are not shown, unless they affect the value.

### Type inference

The following example combines unification, `conde`, `fresh`, and constraints in a relation `typeo`, which implements type inference for a small functional language with numbers, variables, procedures of one argument, and procedure application. An expression is either a number (of type `int`), a symbol (a variable that is looked up in an environment), a term `(lambda (x) body)`, or an application `(f a)`. The type of a procedure is `(-> t1 t2)`. The environment is an association list. The relation `lookupo` implements the lookup; `=/=` makes sure that the first binding of a variable is used.

```scheme
(define (lookupo x env t)
  (fresh (y v rest)
    (== `((,y . ,v) . ,rest) env)
    (conde ((== y x) (== v t))
           ((=/= y x) (lookupo x rest t)))))

(define (typeo expr env t)
  (conde ((symbolo expr) (lookupo expr env t))
         ((numbero expr) (== t 'int))
         ((fresh (x body tx tb)
            (== expr `(lambda (,x) ,body))
            (symbolo x)
            (== t `(-> ,tx ,tb))
            (typeo body `((,x . ,tx) . ,env) tb)))
         ((fresh (f a ta)
            (== expr `(,f ,a))
            (typeo f env `(-> ,ta ,t))
            (typeo a env ta)))))
```

The relation is used for inferring the type of an expression. Types can contain variables, which stand for any type. For instance, the identity procedure can be applied to arguments of any type. A procedure that applies a procedure to a value has a type with two type variables. Self-application is not type-correct, so there is no type for it:

```scheme
(run 1 (q) (typeo '(lambda (x) x) '() q))                  ⇒  ((-> _.0 _.0))
(run 1 (q) (typeo '(lambda (f) (lambda (x) (f x))) '() q)) ⇒  ((-> (-> _.0 _.1) (-> _.0 _.1)))
(run 1 (q) (typeo '((lambda (x) x) 5) '() q))              ⇒  (int)
(run 1 (q) (typeo '(lambda (x) (x x)) '() q))              ⇒  ()
```

Since `typeo` is a relation, it can also be used in the opposite direction. The following query generates three expressions of type `(-> int int)`. The answers show expressions with variables: `_.0` is a symbol (the parameter of a procedure), and `_.1` is a number. In other words, `(lambda (x) 42)` and `(lambda (x) x)` have the requested type.

```scheme
(run 3 (e) (typeo e '() '(-> int int)))
  ⇒  (((lambda (_.0) _.1) (num _.1) (sym _.0))
      ((lambda (_.0) _.0) (sym _.0))
      ((lambda (_.0) ((lambda (_.1) _.2) _.3)) (num _.2 _.3) (sym _.0 _.1)))
```

### Committing to choices

So far, all operators were _relational_: they do not depend on the order in which they are used, and they do not prune the search space. Sometimes it is useful or necessary to cut the search short for the sake of efficiency, or to express a negation. For these cases, there are two impure variants of `conde`:

   - **`conda`** commits to the first clause whose first goal (the _question_) succeeds. It discards all the other clauses, but it keeps all answers of the question that was chosen. It corresponds to a conditional expression: _if question then goals else try the next clause_.
   - **`condu`** is like `conda`, but it also keeps only the first answer of the question that was chosen. It corresponds to a _committed choice_.

```scheme
(run* (q) (conda ((== q 'a)) ((== q 'b))))                 ⇒  (a)
(run* (q) (conda (fail) ((== q 'b))))                      ⇒  (b)
(run* (q) (conda ((== q 'a) (== q 'b)) ((== q 'c))))       ⇒  ()
(run* (q) (conde ((== q 'a) (== q 'b)) ((== q 'c))))       ⇒  (c)
```

In the third query, the question `(== q 'a)` succeeds, so `conda` commits to the first clause, even though the remaining goal of this clause fails. With `conde`, the second clause still gets a chance. The difference between `conda` and `condu` is visible when the question has multiple answers:

```scheme
(run* (q) (conda ((membero q '(a b c)))))  ⇒  (a b c)
(run* (q) (condu ((membero q '(a b c)))))  ⇒  (a)
```

A frequent use of `condu` is a goal that succeeds at most once. It is called `onceo` in the literature. A negation can be defined with `conda`: the goal `(noto g)` fails if `g` succeeds, and it succeeds otherwise.

```scheme
(define (onceo goal)
  (condu (goal)))

(define (noto goal)
  (conda (goal fail)
         (succeed)))

(run* (q) (onceo (membero q '(a b c))))  ⇒  (a)
(run* (q) (noto (== 1 2)))               ⇒  (_.0)
(run* (q) (== q 1) (noto (== q 2)))      ⇒  (1)
```

**Negation via `conda` is not sound.** Since `noto` decides immediately whether `goal` succeeds, the result depends on how much is known about the variables at this point. The following two queries only differ in the order of the goals, and `noto` produces different answers. The constraint `=/=` does not have this problem and should be preferred:

```scheme
(run* (q) (noto (== q 1)) (== q 2))  ⇒  ()
(run* (q) (== q 2) (noto (== q 1)))  ⇒  (2)
(run* (q) (=/= q 1) (== q 2))        ⇒  (2)
(run* (q) (== q 2) (=/= q 1))        ⇒  (2)
```

Since `conda` and `condu` are not relational, relations that use them may no longer run in all directions, and the order of goals may change their answers. Use them sparingly.

### Tips

   - **Think of relations as predicates.** A relation `(appendo l s out)` holds for certain combinations of arguments. Think about which combinations are possible, not about inputs and outputs.
   - **Use `run n` while developing**, and `run*` only if you are certain that there are finitely many answers.
   - **Put unifications first.** Goals that make arguments more specific should be placed before recursive calls; goals that can fail early should be placed before goals that are expensive.
   - **An empty list of answers is not an error.** It just says that there is no solution. A mistake in a relation, e.g. in the name of a variable, can look like "no solutions"; test relations with simple queries first.
   - **A term can contain anything but boxes.** Logic variables are implemented with boxes. For this reason, `(run* (q) (== q (box 1)))` returns `(_.0)`. Hence, boxes are not usable as data in terms.
   - **Constraints are checked eagerly.** An unsatisfiable constraint makes a query fail as soon as it is detected, which prunes the search space. Constraints placed early in a relation can help with termination.

## API

### Goals

**succeed** <span style="float:right;text-align:rigth;">[goal]</span>  

A goal that always succeeds. It does not constrain any variable.

```scheme
(run* (q) succeed)          ⇒  (_.0)
(run* (q) succeed (== q 1)) ⇒  (1)
```

**fail** <span style="float:right;text-align:rigth;">[goal]</span>  

A goal that always fails. A query containing this goal has no answers.

```scheme
(run* (q) fail)             ⇒  ()
(run* (q) (== q 1) fail)    ⇒  ()
```

**(== _u v_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a goal that unifies the terms _u_ and _v_. The goal succeeds if _u_ and _v_ can be made equal by giving values to the logic variables in _u_ and _v_ consistently with all earlier goals and constraints. Otherwise, it fails. Pairs are unified component by component; atoms are unified if they are `equal?`. Vectors are atoms. The unification includes an occurs check, i.e. a variable cannot be unified with a term that contains the variable.

```scheme
(run* (q) (== q 'a))                   ⇒  (a)
(run* (q) (== '(1 2) `(1 ,q)))         ⇒  (2)
(run* (q) (fresh (x) (== x `(1 ,x))))  ⇒  ()
```

**(=/= _u v_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a goal that expresses the _disequality_ of the terms _u_ and _v_. It fails if _u_ and _v_ are equal already, or if they are bound to become equal. Otherwise, the goal succeeds and records a constraint that prevents _u_ and _v_ from becoming equal in the future.

```scheme
(run* (q) (=/= q 1))           ⇒  ((_.0 (=/= ((_.0 1)))))
(run* (q) (=/= q 1) (== q 1))  ⇒  ()
(run* (q) (=/= 1 1))           ⇒  ()
(run* (q) (=/= 1 2))           ⇒  (_.0)
```

**(symbolo _u_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a goal that constrains term _u_ to be a symbol. If _u_ is a variable without a value, a constraint is recorded. If _u_ has a value, the goal succeeds if it is a symbol and fails otherwise. A variable cannot be both a symbol and a number.

```scheme
(run* (q) (symbolo q))   ⇒  ((_.0 (sym _.0)))
(run* (q) (symbolo 'a))  ⇒  (_.0)
(run* (q) (symbolo 1))   ⇒  ()
```

**(numbero _u_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a goal that constrains term _u_ to be a number. If _u_ is a variable without a value, a constraint is recorded. If _u_ has a value, the goal succeeds if it is a number and fails otherwise.

```scheme
(run* (q) (numbero q))             ⇒  ((_.0 (num _.0)))
(run* (q) (numbero q) (== q 1.5))  ⇒  (1.5)
(run* (q) (numbero q) (== q 'a))   ⇒  ()
```

**(absento _u v_)** <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a goal that constrains term _v_ such that term _u_ does not occur in it, neither as _v_ itself, nor as a subterm of _v_ (the terms in the car or cdr of pairs, at any depth). If _v_ is not known sufficiently to decide this, a constraint is recorded.

```scheme
(run* (q) (absento 'a '(b c)))    ⇒  (_.0)
(run* (q) (absento 'a '(b (a))))  ⇒  ()
(run* (q) (absento 'a q))         ⇒  ((_.0 (absento (a _.0))))
```

### Binding and disjunction

**(fresh _(x ...) goal1 goal2 ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  

Returns a goal that introduces the new logic variables _x ..._ and then succeeds if all of the goals _goal1 goal2 ..._ succeed in order. The variables are visible in the goals. The list of variables may be empty. There needs to be at least one goal.

```scheme
(run* (q) (fresh (x y) (== x 1) (== y 2) (== q (list x y))))
  ⇒  ((1 2))
(run* (q) (fresh () (== q 1)))
  ⇒  (1)
```

**(conde _(goal ...) ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  

Returns a goal that is a disjunction of its clauses. Each clause is a conjunction of goals, i.e. a sequence of goals that have to succeed in order. The goal succeeds as many times as there are clauses succeeding. A clause has to have at least one goal. The search interleaves the evaluation of the clauses, so that infinite search in one clause does not prevent answers in other clauses from being found.

```scheme
(run* (q) (conde ((== q 1)) ((== q 2) (== q 2)) ((== q 3) fail)))
  ⇒  (1 2)
```

**(conda _(goal0 goal ...) ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  

Returns a goal that implements a _soft cut_. The clauses are tried in order. For the first clause whose first goal _goal0_ (its _question_) succeeds, `conda` commits to this clause: the remaining clauses are ignored, and the goal succeeds for all of the answers of the question that also satisfy the other goals of the clause. If the other goals of the committed clause fail, so does the entire goal; no other clause is tried. If there is no clause whose question succeeds, the goal fails.

```scheme
(run* (q) (conda ((== q 1)) ((== q 2))))       ⇒  (1)
(run* (q) (conda (fail) ((== q 2))))           ⇒  (2)
(run* (q) (conda ((== q 1) fail) ((== q 2))))  ⇒  ()
```

**(condu _(goal0 goal ...) ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  

Returns a goal that implements a _committed choice_. It behaves like `conda`, except that it only keeps the first answer of the question of the clause it commits to.

```scheme
(run* (q) (condu ((conde ((== q 1)) ((== q 2))))))  ⇒  (1)
(run* (q) (conda ((conde ((== q 1)) ((== q 2))))))  ⇒  (1 2)
```

**(ifa _(stream goal ...) clause ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(ifu _(stream goal ...) clause ..._)**  

Low-level forms that implement `conda` and `condu`. `ifa` and `ifu` are used inside of a procedure that takes the current state of the search, the _state_ `c`, and returns a _stream_ of states. Each clause is a list `(stream goal ...)` where _stream_ is an expression that evaluates to the stream of states that results from applying a goal to `c`, and the remaining elements are goals that are applied to every state in the chosen stream. `(conda (g0 g ...) (g1 g^ ...) ...)` is equivalent to a goal `(lambda (c) (ifa ((g0 c) g ...) ((g1 c) g^ ...) ...))`.

`ifa` and `ifu` make it possible to define new goal constructors, such as the following if-then-else goal, in which the last clause provides the alternative:

```scheme
(define (ifo test then else)
  (lambda (c) (ifa ((test c) then) ((succeed c) else))))

(run* (q) (ifo (== q 1) (== q 1) (== q 2)))  ⇒  (1)
(run* (q) (ifo (== 1 2) (== q 1) (== q 2)))  ⇒  (2)
```

### Queries

**(run _n (q) goal1 goal2 ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(run _n (q1 q2 q3 ...) goal1 goal2 ..._)**  

Runs a query for the logic variable _q_ (the query variable) and returns a list of at most _n_ answers. An answer is the value of _q_ for which all goals _goal1 goal2 ..._ succeed, with constraints that are not satisfied yet appended to it (see the section on reading answers of the tutorial). If _n_ is `#f`, all answers are returned, which does not terminate if there are infinitely many answers. At least one goal is required.

If more than one query variable is given, the query is equivalent to a query over a new variable _q_, bound to the list of the variables `(q1 q2 q3 ...)`. Each answer is then a list of the values of the variables. At least one query variable needs to be given.

```scheme
(run 1 (q) (conde ((== q 1)) ((== q 2))))             ⇒  (1)
(run 2 (x y) (conde ((== x 1) (== y 2)) ((== x 3))))  ⇒  ((1 2) (3 _.0))
(run 0 (q) succeed)                                   ⇒  ()
```

**(run* _(q) goal1 goal2 ..._)** <span style="float:right;text-align:rigth;">[syntax]</span>  
**(run* _(q1 q2 q3 ...) goal1 goal2 ..._)**  

Like `run`, but without a limit on the number of answers: `(run* (q) goal ...)` is the same as `(run #f (q) goal ...)`.

```scheme
(run* (q) (conde ((== q 'a)) ((== q 'b))))
  ⇒  (a b)
(run* (x y) (== x 1) (conde ((== y 'a)) ((== y 'b))))
  ⇒  ((1 a) (1 b))
```

## Notes

   - **Logic variables are boxes.** Logic variables are represented by boxes, so boxes cannot be part of terms. Do not use boxes in terms.
   - **Relations are procedures.** Goals are procedures. Relations are procedures that return goals, which are created when a relation gets applied. The arguments of a relation are ordinary Scheme values evaluated before the relation is applied; they are not evaluated lazily.
   - **Printing of variables.** Reified variables are symbols whose names start with `_.`. A query that returns the symbol `_.0` cannot be distinguished from an unbound variable.
   - **Completeness.** The search is complete for relations that are written with the care described in the tutorial: if an answer exists, `run*` will eventually find it. Left-recursive relations and the use of `conda` and `condu` can make the search incomplete.

## Further reading

   - Daniel P. Friedman, William E. Byrd, Oleg Kiselyov, Jason Hemann: _The Reasoned Schemer, Second Edition_. MIT Press, 2018. The book introduces miniKanren and relational programming step by step.
   - William E. Byrd: [The miniKanren interactive tutorial](http://io.livecode.ch/learn/webyrd/webmk). A short, interactive introduction to the core operators and the constraints `=/=`, `symbolo`, `numbero` and `absento`.
   - [minikanren.org](https://minikanren.org). The homepage of the miniKanren community, with links to books, tutorials, talks, papers, and many implementations.
   - Jason Hemann, Daniel P. Friedman: [microKanren: A Minimal Functional Core for Relational Programming](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf). Scheme Workshop, 2013.
   - Claire E. Alvis, Jeremiah J. Willcock, Kyle M. Carter, William E. Byrd, Daniel P. Friedman: [cKanren: miniKanren with Constraints](http://www.schemeworkshop.org/2011/papers/Alvis2011.pdf). Scheme Workshop, 2011.
   - [Introduction to Logic Programming with Clojure](https://github.com/frenchy64/Logic-Starter/wiki). A tutorial for Clojure's `core.logic`, which is closely related to miniKanren.
   - [miniKanren with symbolic constraints](https://github.com/webyrd/miniKanren-with-symbolic-constraints). The original implementation, including its test suite.
   - Library `(lispkit prolog)` for Prolog-style logic programming.

The implementation of this library is based on miniKanren with symbolic constraints by Jason Hemann, William E. Byrd, and Daniel P. Friedman; copyright © 2015 William E. Byrd, licensed under the MIT License. The adaptation to LispKit is copyright © 2018 Matthias Zenger.
