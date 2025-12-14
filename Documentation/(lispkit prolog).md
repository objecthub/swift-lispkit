# LispKit Prolog

Library `(lispkit prolog)` implements _Schelog_, an _embedding_ of Prolog-style logic programming in Scheme by Dorai Sitaram. This approach allows Prolog-style logic programming and Scheme-style functional programming to be combined. Schelog contains the full repertoire of Prolog features, including meta-logical and second-order ("set") predicates, leaving out only those features that could more easily and more efficiently be done with Scheme subexpressions.

## Simple Goals and Queries

Schelog objects are the same as Scheme objects. However, there are two subsets of these objects that are of special interest to Schelog: _goals_ and _predicates_. We will first look at some simple goals. The next section will introduce predicates and ways of making complex goals using predicates.

A _goal_ is an object whose truth or falsity we can check. A goal that turns out to be true is said to succeed. A goal that turns out to be false is said to fail. Two simple goals that are provided in Schelog are:

```scheme
%true
%fail
```

The goal `%true` always succeeds, the goal `%fail` always fails.

The names of all Schelog primitive objects start with `%`. This is to avoid clashes with the names of conventional Scheme objects of related meaning. User-created objects in Schelog are not required to follow this convention.

A Schelog user can _query_ a goal by wrapping it in a `%which`-form.  

```scheme
(%which () %true)
```

evaluates to `()`, indicating success, whereas:  

```scheme
(%which () %fail)
```

evaluates to `#f`, indicating failure.  

The second subexpression of the `%which`-form is the empty list `()`. Later we will see `%which` used with other lists as the second subform. The distinction between successful and failing goals relies on Scheme distinguishing between `#f` and `()`. We will use the annotation `()`<sup>true</sup> to signal that `()` is being used as a true value. Henceforth, we will use the notation:

&nbsp;&nbsp;&nbsp;&nbsp;E `=>` F

to say that E _evaluates to_ F. Thus,  

&nbsp;&nbsp;&nbsp;&nbsp;`(%which () %true)` => `()`<sup>true</sup>.

## Predicates

More interesting goals are created by applying a special kind of Schelog object called a _predicate_ (or _relation_) to other Schelog objects. Schelog comes with some primitive predicates, such as the arithmetic operators `%=:=` and `%<`, standing for arithmetic "equal" and "less than" respectively. For example, the following are some goals involving these predicates:

&nbsp;&nbsp;&nbsp;&nbsp;`(%which () (%=:= 1 1))`&nbsp;&nbsp;`=>` ()<sup>true</sup>  
&nbsp;&nbsp;&nbsp;&nbsp;`(%which () (%< 1 2))`&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`=>` ()<sup>true</sup>  
&nbsp;&nbsp;&nbsp;&nbsp;`(%which () (%=:= 1 2))`&nbsp;&nbsp;`=>` #f  
&nbsp;&nbsp;&nbsp;&nbsp;`(%which () (%< 1 1))`&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`=>` #f  

Other arithmetic predicates are `%>` ("greater than"), `%<=` ("less than or equal"), `%>=` ("greater than or equal"), and `%=/=` ("not equal").

Schelog predicates are not to be confused with conventional Scheme predicates (such as `<` and `=`). Schelog predicates, when applied to arguments, produce goals that may either succeed or fail. Scheme predicates, when applied to arguments, yield a boolean value. Henceforth, we will use the term "predicate" to mean Schelog predicates. Conventional predicates will be explicitly called "Scheme predicates".  

### Predicates introducing facts

Users can create their own predicates using the Schelog form `%rel`. For example, the following code defines a predicate `%knows`:

```scheme
(define %knows
  (%rel ()
    (('Odysseus 'TeX))
    (('Odysseus 'Scheme))
    (('Odysseus 'Prolog))
    (('Odysseus 'Penelope))
    (('Penelope 'TeX))
    (('Penelope 'Prolog))
    (('Penelope 'Odysseus))
    (('Telemachus 'TeX))
    (('Telemachus 'calculus))))
```

The expression has the expected meaning. Each _clause_ in the `%rel` establishes a _fact_: Odysseus knows TeX, Telemachus knows calculus, etc. In general, if we apply the predicate to the arguments in any one of its clauses, we will get a successful goal. Thus, since `%knows` has a clause that reads `(('Odysseus 'TeX))`, the goal `(%knows 'Odysseus 'TeX)` will be true.

We can now get answers for the following types of queries:

&nbsp;&nbsp;&nbsp;&nbsp;`(%which () (%knows 'Odysseus 'TeX))`&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;=> ()<sup>true</sup>  
&nbsp;&nbsp;&nbsp;&nbsp;`(%which () (%knows 'Telemachus 'Scheme))` => #f

### Predicates with rules

Predicates can be more complicated than the above recitation of facts. The predicate clauses can be _rules_, e.g.

```scheme
(define %computer-literate
  (%rel (person)
    ((person) (%knows person 'TeX)
              (%knows person 'Scheme))
    ((person) (%knows person 'TeX)
              (%knows person 'Prolog))))
```

This defines the predicate `%computer-literate` in terms of the predicate `%knows`. In effect, a person is defined as computer-literate if they know TeX and Scheme, _or_ TeX and Prolog.

Note that this use of `%rel` employs a local _logic variable_ called person. In general, a `%rel`-expression can have a list of symbols as its second subform. These name new logic variables that can be used within the body of the `%rel`. The following query can now be answered:

&nbsp;&nbsp;&nbsp;&nbsp;`(%which () (%computer-literate 'Penelope))`
&nbsp;&nbsp;&nbsp;&nbsp;`=> ()`<sup>true</sup>

Since Penelope knows TeX and Prolog, she is computer-literate.  

### Solving goals

The above queries are yes/no questions. Logic programming allows more: We can formulate a goal with _uninstantiated_ logic variables and then ask the querying process to provide, if possible, values for these variables that cause the goal to succeed. For instance, the query:  

```scheme
(%which (what)
  (%knows 'Odysseus what))  
```

asks for an instantiation of the logic variable `what` that satisfies the goal `(%knows 'Odysseus what)`. In other words, we are asking, "What does Odysseus know?".

Note that this use of `%which`, like `%rel` in the definition of `%computer-literate`, uses a local logic variable `what`. In general, the second subform of `%which` can be a list of local logic variables. The `%which`-query returns an answer that is a list of bindings, one for each logic variable mentioned in its second subform. Thus,  

```scheme
(%which (what)
  (%knows 'Odysseus what)) => ((what TeX))  
```

But that is not all that Odysseus knows. Schelog provides a zero-argument procedure called `%more` that _retries_ the goal in the last `%which`-query for a different solution.  

```scheme
(%more) => ((what Scheme))  
```

We can keep asking for more solutions:  

```scheme
(%more) => ((what Prolog))  
(%more) => ((what Penelope))  
(%more) => #f
```

The final `#f` shows that there are no more solutions. This is because there are no more clauses in the `%knows` predicate that list Odysseus as knowing anything else.

It is now clear why `()`<sup>true</sup> was the right choice for truth in the previous `%which`-queries that had no logic variables. `%which` returns a list of bindings for true goals: the list is empty when there are no variables.

### Asserting extra clauses

We can add more clauses to a predicate after it has already been defined via `%rel`. Schelog provides the `%assert` form for this purpose.

```scheme
(%assert %knows ()
  (('Odysseus 'archery)))
```

tacks on a new clause at the end of the existing clauses of the `%knows` predicate. Now, the query:  

```scheme
(%which (what)
  (%knows 'Odysseus what))  
```

gives TeX, Scheme, Prolog, and Penelope, as before, but a subsequent `(%more)` yields a new result: archery. The Schelog form `%assert-a` is similar to `%assert` but adds clauses _before_ any of the current clauses.

Both `%assert` and `%assert-a` assume that the variable they are adding to already names a predicate, presumably defined using `%rel`. In order to allow defining a predicate entirely through `%assert`, Schelog provides an empty predicate value `%empty-rel`. `%empty-rel` takes any number of arguments and always fails. Here is a typical use of the `%empty-rel` and `%assert` combination:

```scheme
(define %parent %empty-rel)
(%assert %parent ()
  (('Laertes 'Odysseus)))
(%assert %parent ()  
  (('Odysseus 'Telemachus))
  (('Penelope 'Telemachus)))
```

Schelog does not provide a predicate for _retracting_ assertions since we can keep track of older versions of predicates using conventional Scheme features such as `let` and `set!`.

### Local variables

The local logic variables of `%rel` and `%which`-expressions are in reality introduced by the Schelog syntactic form called `%let`. `%let` introduces new lexically scoped logic variables. Supposing, instead of  

```scheme
(%which (what)
  (%knows 'Odysseus what))  
```

we had asked

```scheme
(%let (what)
  (%which ()
    (%knows 'Odysseus what)))
```

This query, too, succeeds five times, since Odysseus knows five things. However, `%which` emits bindings only for the local variables that it introduces. Thus, this query emits `()`<sup>true</sup> five times before `(%more)` finally returns `#f`.  

## Using conventional Scheme expressions

The arguments of Schelog predicates can be any Scheme objects. In particular, composite structures such as lists, vectors and strings can be used, as also Scheme expressions using the full array of Scheme’s construction and decomposition operators. For instance, consider the following goal:

```scheme
(%member x '(1 2 3))  
```

Here, `%member` is a predicate, `x` is a logic variable, and `'(1 2 3)` is a structure. Given a suitably intuitive definition for `%member`, the above goal succeeds for `x` = 1, 2, and 3. Here is a possible definition of `%member`:

```scheme
(define %member
  (%rel (x y xs)
    ((x (cons x xs)))
    ((x (cons y xs))
      (%member x xs))))  
```

`%member` is defined with three local variables: `x`, `y`, `xs`. It has two clauses, identifying the two ways of determining membership. The first clause of `%member` states a fact: For any `x`, `x` is a member of a list whose head is also `x`. The second clause of `%member` is a rule: `x` is a member of a list if we can show that it is a member of the _tail_ of that list. In other words, the original `%member` goal is translated into a _sub goal_, which is also a `%member` goal.

Note that the variable `y` in the definition of `%member` occurs only once in the second clause. As such, it doesn’t need you to make the effort of naming it. Names help only in matching a second occurrence to a first. Schelog lets you use the expression `(_)` to denote an anonymous variable; i.e. `_` is a thunk that generates a fresh anonymous variable at each call. The predicate `%member` can be rewritten in the following way:

```scheme
(define %member  
  (%rel (x xs)  
    ((x (cons x (_))))  
    ((x (cons (_) xs))  
      (%member x xs))))
```

### Constructors

We can use constructors, i.e. Scheme procedures for creating structures, to simulate data types in Schelog. For instance, let’s define a natural-number data-type where `0` denotes zero, and `(succ x)` denotes the natural number whose immediate predecessor is `x`. The constructor `succ` can be defined in Scheme as:

```scheme
(define succ
  (lambda (x)
    (vector 'succ x)))
```

Addition and multiplication can be defined as:  

```scheme
(define %add  
  (%rel (x y z)  
    ((0 y y))  
    (((succ x) y (succ z))  
      (%add x y z))))
(define %times
  (%rel (x y z z1)
    ((0 y 0))
    (((succ x) y z)
      (%times x y z1)
      (%add y z1 z))))  
```

We can do a lot of arithmetic with this in place. For instance, the factorial predicate looks like:

```scheme
(define %factorial  
  (%rel (x y y1)  
    ((0 (succ 0)))  
    (((succ x) y)  
      (%factorial x y1)  
      (%times (succ x) y1 y))))  
```

### `%is`

The above is a very inefficient way to do arithmetic, especially when the underlying language Scheme offers excellent arithmetic facilities, including a comprehensive numeric tower and exact rational arithmetic. One problem with using Scheme calculations directly in Schelog clauses is that the expressions used may contain logic variables that need to be dereferenced. Schelog provides the predicate `%is` that takes care of this. The goal  

```scheme
(%is X E)
```

unifies `X` with the value of `E` considered as a Scheme expression. `E` can have logic variables, but usually they should at least be bound, as unbound variables may not be palatable values to the Scheme operators used in `E`. We can now directly use the numbers of Scheme to write a more efficient %factorial predicate:  

```scheme
(define %factorial
   (%rel (x y x1 y1)
     ((0 1))
     ((x y) (%is x1 (- x 1))
            (%factorial x1 y1)
            (%is y (* y1 x)))))
```

A price that this efficiency comes with is that we can use `%factorial` only with its first argument already instantiated. In many cases, this is not an unreasonable constraint. In fact, given this limitation, there is nothing to prevent us from using Scheme’s factorial directly:  

```scheme
(define %factorial  
  (%rel (x y)  
    ((x y)  
      (%is y (scheme-factorial x)))))  
```

or better yet, inline any calls to `%factorial` with `%is`-expressions calling scheme-factorial, where the latter is defined in the usual manner:  

```scheme
(define scheme-factorial  
  (lambda (n)  
    (if (= n 0)
        1  
        (\* n (factorial (- n 1))))))  
```

### Lexical scoping

One can use Scheme’s lexical scoping to enhance predicate definitions. Here is a list-reversal predicate defined using a hidden auxiliary predicate:

```scheme
(define %reverse
  (letrec ((revaux
             (%rel (x y z w)
               (('() y y))
               (((cons x y) z w)
                 (revaux y (cons x z) w)))))  
    (%rel (x y)
      ((x y) (revaux x '() y)))))
```

`(revaux X Y Z)` uses `Y` as an accumulator for reversing `X` into `Z`. `Y` starts out as `()`. Each head of `X` is consed on to `Y`. Finally, when `X` has wound down to `()`, `Y` contains the reversed list and can be returned as `Z`. Here, `revaux` is used purely as a helper predicate for `%reverse`, and so it can be concealed within a lexical contour. We use `letrec` instead of `let` because `revaux` is a recursive procedure.

### Type predicates

Schelog provides a couple of predicates that let the user probe the type of objects. The goal  

```scheme
(%constant X)  
```

succeeds if `X` is an _atomic_ object, i.e. not a list or vector. The predicate `%compound`, the negation of `%constant`, checks if its argument is indeed a list or a vector.

The above are merely the logic-programming equivalents of corresponding Scheme predicates. Users can use the predicate `%is` and Scheme predicates to write more type checks in Schelog. Thus, to test if `X` is a string, the following goal could be used:  

```scheme
(%is #t (string? X))
```

User-defined Scheme predicates, in addition to primitive Scheme predicates, can thus be imported.

## Backtracking

It is helpful to go into the following evaluation in a little more detail:

&nbsp;&nbsp;&nbsp;&nbsp;`(%which ()`  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`(%computer-literate 'Penelope))`  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`=> ()`<sup>true</sup>

The starting goal is:  

```scheme
G0 = (%computer-literate Penelope)  
```

Schelog tries to match this with the head of the first clause of `%computer-literate`. It succeeds, generating a binding `(person Penelope)`. But this means it now has two new goals — _subgoals_ — to solve. These are the goals in the body of the matching clause, with the logic variables substituted by their instantiations:  

```scheme
G1 = (%knows Penelope TeX)  
G2 = (%knows Penelope Scheme)  
```

For `G1`, Schelog attempts matches with the clauses of `%knows`, and succeeds at the fifth try. There are no subgoals in this case, because the bodies of these "fact" clauses are empty, in contrast to the "rule" clauses of `%computer`-literate. Schelog then tries to solve `G2` against the clauses of `%knows`, and since there is no clause stating that Penelope knows Scheme, it fails.

All is not lost though. Schelog now _backtracks_ to the goal that was solved just before: `G1`. It _retries_ G1, i.e. tries to solve it in a different way. This entails searching down the previously unconsidered `%knows` clauses for `G1`, i.e. the sixth onwards. Obviously, Schelog fails again, because the fact that Penelope knows TeX occurs only once.

Schelog now backtracks to the goal before `G1`, i.e. `G0`. We abandon the current successful match with the first clause-head of `%computer-literate`, and try the next clause-head. Schelog succeeds, again producing a binding `(person Penelope)`, and two new subgoals:

```scheme
G3 = (%knows Penelope TeX)  
G4 = (%knows Penelope Prolog)  
```

It is now easy to trace that Schelog finds both `G3` and `G4` to be true. Since both of `G0`’s subgoals are true, `G0` is itself considered true. And this is what Schelog reports. The interested reader can now trace why the following query has a different denouement:  

```scheme
(%which ()  
  (%computer-literate 'Telemachus))  
=> #f
```

## Unification

When we say that a goal matches with a clause-head, we mean that the predicate and argument positions line up. Before making this comparison, Schelog dereferences all already bound logic variables. The resulting structures are then compared to see if they are recursively identical. Thus, `1` unifies with `1`, and `(list 1 2)` with `'(1 2)`; but `1` and `2` do not unify, and neither do `'(1 2) and '(1 3)`.

In general, there could be quite a few uninstantiated logic variables in the compared objects. Unification will then endeavor to find the most natural way of binding these variables so that we arrive at structurally identical objects. Thus, `(list x 1)`, where `x` is an unbound logic variable, unifies with `'(0 1)`, producing the binding `(x 0)`.

Unification is thus a goal, and Schelog makes the unification predicate available to the user as `%=`, e.g.  

```scheme
(%which (x)
  (%= (list x 1) '(0 1))
=> ((x 0))
```

Schelog also provides the predicate `%/=`, the _negation_ of `%=`. `(%/= X Y)` succeeds if and only if `X` does _not_ unify with `Y`.

Unification goals constitute the basic subgoals that all Schelog goals devolve to. A goal succeeds because all the eventual unification subgoals that it decomposes to in at least one of its subgoal-branching succeeded. It fails because every possible subgoal-branching was thwarted by the failure of a crucial unification subgoal.

Going back to the example in the section on backtracking, the goal `(%computer-literate 'Penelope)` succeeds because (a) it unified with `(%computer-literate person)`; and then (b) with the binding `(person Penelope)` in place, `(%knows person 'TeX)` unified with `(%knows 'Penelope 'TeX)` and `(%knows person 'Prolog)` unified with `(%knows 'Penelope 'Prolog)`.

In contrast, the goal `(%computer-literate 'Telemachus)` fails because, with `(person Telemachus)`, the subgoals `(%knows person 'Scheme)` and `(%knows person 'Prolog)` have no facts they can unify with.  

### The "occurs check"

A robust unification algorithm uses the _occurs check_, which ensures that a logic variable isn’t bound to a structure that contains itself. Not performing the check can cause the unification to go into an infinite loop in some cases. On the other hand, performing the occurs check greatly increases the time taken by unification, even in cases that wouldn’t require the check.

Schelog uses the global variable `*schelog-use-occurs-check?*` to decide whether to use the occurs check. By default, this variable is `#f`, i.e. Schelog disables the occurs check. To enable the check,  

```scheme
(set! *schelog-use-occurs-check?* #t)
```

## Conjuctions and disjunctions

Goals may be combined using the forms `%and` and `%or` to form compound goals. For `%not`, see the section on "Negation as failure". Here is an example:

```scheme
(%which (x)
  (%and (%member x '(1 2 3))
        (%< x 3)))
```

gives solutions for `x` that satisfy both the argument goals of the `%and`, i.e. `x` should both be a member of `'(1 2 3)` _and_ be less than `3`. The first solution is  

```scheme
((x 1))
```

Typing `(%more)` gives another solution:  

```scheme
((x 2))
```

There are no more solutions, because `(x 3)` satisfies the first, but not the second goal. Similarly, the query

```scheme
(%which (x)  
  (%or (%member x '(1 2 3))  
       (%member x '(3 4 5))))  
```

lists all `x` that are members of either list.  

```scheme
           ((x 1))  
(%more) => ((x 2))  
(%more) => ((x 3))  
(%more) => ((x 3))  
(%more) => ((x 4))  
(%more) => ((x 5))  
```

Here, `((x 3))` is listed twice. We can rewrite the predicate `%computer-literate` from section "Predicates with rules" using `%and`and `%or`: 

```scheme
(define %computer-literate
  (%rel (person)  
    ((person)  
      (%or (%and (%knows person 'TeX)
                 (%knows person 'Scheme))  
           (%and (%knows person 'TeX)
                 (%knows person 'Prolog))))))
```

Or, more succinctly:

```scheme
(define %computer-literate  
  (%rel (person)  
    ((person)
      (%and (%knows person 'TeX)
            (%or (%knows person 'Scheme)
                 (%knows person 'Prolog))))))  
```

We can even dispense `%rel` altogether, turning `%computer-literate` into a conventional Scheme predicate definition:

```scheme
(define %computer-literate
  (lambda (person)
    (%and (%knows person 'TeX)
          (%or (%knows person 'Scheme)
               (%knows person 'Prolog)))))
```

## Manipulating logic variables

Schelog provides special predicates for probing logic variables, without risking them getting bound.

### Checking for variables

The goal

```scheme
(%== X Y)
```

succeeds if `X` and `Y` are _identical_ objects. This is not quite the unification predicate `%=`, for `%==` doesn’t touch unbound objects the way `%=` does. For instance, `%==` will not equate an unbound logic variable with a bound one, nor will it equate two unbound logic variables unless they are the _same_ variable.

The predicate `%/==` is the negation of `%==`.

The goal

```scheme
(%var X)
```

succeeds if `X` isn’t completely bound, i.e. it has at least one unbound logic variable in its innards.

The predicate `%nonvar` is the negation of `%var`.  

### Preserving variables

Schelog lets the user protect a term with variables from unification by allowing that term to be treated as a completely bound object. The predicates provided for this purpose are `%freeze`, `%melt`, `%melt-new`, and `%copy`.

The goal

```scheme
(%freeze S F)
```

unifies `F` to the frozen version of `S`. Any lack of bindings in `S` are preserved no matter how much you toss `F` about.

The goal  

```scheme
(%melt F S)  
```

retrieves the object frozen in `F` into `S`.

The goal  

```scheme
(%melt-new F S)  
```

is similar to `%melt`, except that when `S` is made, the unbound variables in `F` are replaced by brand-new unbound variables.

The goal  

```scheme
(%copy S C)  
```

is an abbreviation for `(%freeze S F)` followed by `(%melt-new F C)`.  

## The cut (!)

The cut `(called !)`` is a special goal that is used to prune backtracking options. Like the `%true` goal, the cut goal too succeeds, when accosted by the Schelog subgoaling engine. However, when a further subgoal down the line fails, and time comes to retry the cut goal, Schelog will refuse to try alternate clauses for the predicate in whose definition the cut occurs. In other words, the cut causes Schelog to commit to all the decisions made from the time that the predicate was selected to match a subgoal till the time the cut was satisfied.

For example, consider again the `%factorial` predicate, as defined in the section on `%is`:

```scheme
(define %factorial
  (%rel (x y x1 y1)  
    ((0 1))
    ((x y) (%is x1 (- x 1))
           (%factorial x1 y1)
           (%is y (\* y1 x)))))
```

Clearly,

&nbsp;&nbsp;&nbsp;&nbsp;`(%which ()`  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`(%factorial 0 1))`  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`=> ()`<sup>true</sup>  
&nbsp;&nbsp;&nbsp;&nbsp;`(%which (n)`  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`(%factorial 0 n))`  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`=> ((n 1))`

But what if we asked for `(%more)` for either query? Backtracking will try the second clause of `%factorial`, and sure enough the clause-head unifies, producing binding `(x 0)`. We now get three subgoals. Solving the first, we get `(x1 -1)`, and then we have to solve `(%factorial -1 y1)`. It is easy to see there is no end to this, as we fruitlessly try to get the factorials of numbers that get more and more negative.  

If we placed a cut at the first clause:  

```scheme
...
((0 1) !)
...
```

the attempt to find more solutions for `(%factorial 0 1)` is stopped immeditately.
  
Calling `%factorial` with a _negative_ number would still cause an infinite loop. To take care of that problem as well, we use another cut:

```scheme
(define %factorial  
  (%rel (x y x1 y1)  
    ((0 1) !)  
    ((x y) (< x 0) ! %fail)  
    ((x y) (%is x1 (- x 1))  
           (%factorial x1 y1)  
           (%is y (\* y1 x)))))  
```

Using _raw_ cuts as above can get very confusing. For this reason, it is advisable to use it hidden away in well-understood abstractions. Two such common abstractions are the conditional and negation.

### Conditional goals

An "if ... then ... else ..." predicate can be defined as follows  

```scheme
(define %if-then-else  
  (%rel (p q r)  
    ((p q r) p ! q)  
    ((p q r) r)))  
```

Note that for the first time we have predicate arguments that are themselves goals.

Consider the goal  

```scheme
G0 = (%if-then-else Gbool Gthen Gelse)  
```

We first unify `G0` with the first clause-head, giving `(p Gbool)`, `(q Gthen)`, `(r Gelse)`. `Gbool` can now either succeed or fail.

Case 1: If `Gbool` fails, backtracking will cause the `G0` to unify with the second clause-head. `r` is bound to `Gelse`, and so `Gelse` is tried, as expected.

Case 2: If `Gbool` succeeds, the cut commits to this clause of the `%if-then-else`. We now try `Gthen`. If `Gthen` should now fail — or even if we simply retry for more solutions — we are guaranteed that the second clause-head will not be tried. If it were not for the cut, `G0` would attempt to unify with the second clause-head, which will of course succeed, and `Gelse` _will_ be tried.

### Negation as failure

Another common abstraction using the cut is _negation_. The negation of goal `G` is defined as `(%not G)`, where the predicate `%not` is defined as follows:

```scheme
(define %not  
  (%rel (g)  
    ((g) g ! %fail)  
    ((g) %true)))  
```

Thus, `g`’s negation is deemed a failure if `g` succeeds, and a success if `g` fails. This is of course confusing goal failure with falsity. In some cases, this view of negation is actually helpful.  

## Set predicates

The goal  

```scheme
(%bag-of X G Bag)  
```

unifies with `Bag` the list of all instantiations of `X` for which `G` succeeds. Thus, the following query asks for all the things known, i.e. the collection of things such that someone knows them:

```scheme
(%which (things-known)
  (%let (someone x)
    (%bag-of x (%knows someone x) things-known)))
=> ((things-known  
      (TeX Scheme Prolog  
       Penelope TeX Prolog  
       Odysseus TeX calculus)))
```

This is the only solution for this goal:  

```scheme
(%more) =>#f  
```

Note that some things, e.g. TeX, are enumerated more than once. This is because more than one person knows TeX. To remove duplicates, use the predicate `%set-of` instead of `%bag-of`:

```scheme
(%which (things-known)
  (%let (someone x)
    (%set-of x (%knows someone x) things-known)))
=> ((things-known
      (TeX Scheme Prolog
       Penelope Odysseus calculus))) 
```

In the above, the free variable someone in the `%knows-goal` is used as if it were existentially quantified. In contrast, Prolog’s versions of `%bag-of` and `%set-of` fix it for each solution of the set-predicate goal. We can do it too with some additional syntax that identifies the free variable, for instance:

```scheme
(%which (someone things-known)
  (%let (x)
    (%bag-of x (%free-vars (someone)
                 (%knows someone x)) things-known)))
=> ((someone Odysseus)  
    (things-known  
      (TeX Scheme Prolog Penelope)))  
```

The bag of things known by _one_ someone is returned. That someone is Odysseus. The query can be retried for more solutions, each listing the things known by a different someone:  

```scheme
(%more) => ((someone Penelope)  
            (things-known
              (TeX Prolog Odysseus)))  
(%more) => ((someone Telemachus)  
            (things-known
              (TeX calculus)))  
(%more) => #f  
```

Schelog also provides two variants of these set predicates: `%bag-of-1` and `%set-of-1`. These act like `%bag-of` and `%set-of` but fail if the resulting bag or set is empty.  

## API

**(%/= _E1 E2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

`%/=` is the negation of predicate `%=`. The goal `(%/= E1 E2)` succeeds if `E1` can not be unified with `E2`.

**(%/== _E1 E2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  
 
`%/==` is the negation of predicate `%==`. The goal `(%/== E1 E2)` succeeds if `E1` and `E2` are not identical.  

**(%< _E1 E2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%< E1 E2)` succeeds if `E1` and `E2` are bound to numbers and `E1` is less than `E2`.

**(%<= _E1 E2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%<= E1 E2)` succeeds if `E1` and `E2` are bound to numbers and `E1` is less than or equal to `E2`.

**(%= _E1 E2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%= E1 E2)` succeeds if `E1` can be unified with `E2`. Any resulting bindings for logic variables are kept.  

**(%=/= _E1 E2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%=/= E1 E2)` succeeds if `E1` and `E2` are bound to numbers and `E1` is not equal to `E2`.

**(%=:= _E1 E2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%=:= E1 E2)` succeeds if `E1` and `E2` are bound to numbers and `E1` is equal to `E2`.

**(%== _E1 E2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%== E1 E2)` succeeds if `E1` is _identical_ to `E2`. They should be structurally equal. If containing logic variables, they should have the same variables in the same position. Unlike a `%=`-call, this goal will not bind any logic variables.

**(%> _E1 E2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%> E1 E2)` succeeds if `E1` and `E2` are bound to numbers and `E1` is greater than `E2`.

**(%>= _E1 E2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%>= E1 E2)` succeeds if `E1` and `E2` are bound to numbers and `E1` is greater than or equal to `E2`.

**(%and _G ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

The goal `(%and G ...)` succeeds if all the goals `G ...` succeed.  

**(%append _E1 E2 E3_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%append E1 E2 E3)` succeeds if `E3` is unifiable with the list obtained by appending `E1` and `E2`.  

**(%assert _Pname (V ...) C ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

The form `(%assert Pname (V ...) C ...)` adds the clauses _C ..._ to the end of the predicate that is the value of the Scheme variable _Pname_. The variables _V ..._ are local logic variables for _C ..._.

**(%assert-a _Pname (V ...) C ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

The form `(%assert-a Pname (V ...) C ...)` adds the clauses _C ..._ to the front of the predicate that is the value of the Scheme variable _Pname_. The variables _V ..._ are local logic variables for _C ..._.  

**(%bag-of _E1 G E2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%bag-of E1 G E2)` unifies with `E2` the _bag_ (multiset) of all the instantiations of `E1` for which goal `G` succeeds.

**(%bag-of-1 _E1 G E2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%bag-of E1 G E2)` unifies with `E2` the _bag_ (multiset) of all the instantiations of `E1` for which goal `G` succeeds. `%bag-of-1` fails if the bag is empty.

**(%compound _E_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%compound E)` succeeds if `E` is a non-atomic structure, i.e. a vector or a list.

**(%constant _E_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%constant E)` succeeds if `E` is an atomic object, i.e. not a vector and a list.

**(%copy _F S_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%copy F S)` unifies with `S` a copy of the frozen structure in `F`.

**(%empty-rel _E ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%empty-rel E ...)` always fails. The value `%empty-rel` is used as a starting value for predicates that can later be enhanced with `%assert` and `%assert-a`.

**%fail** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[goal]</span>  
 
The goal `%fail` always fails.  

**(%free-vars _(V ...) G_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

The form `(%free-vars (V ...) G)` identifies the occurrences of the variables `V ...` in goal `G` as free. It is used to avoid existential quantification in calls to set predicates such as `%bag-of`, `%set-of`, etc.

**(%freeze _S F_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%freeze S F)` unifies with `F` a new frozen version of the structure in `S`. Freezing implies that all the unbound variables are preserved. `F` can henceforth be used as _bound_ object with no fear of its variables getting bound by unification.

**(%if-then-else _G1 G2 G3_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%if-then-else G1 G2 G3)` tries `G1` first: if it succeeds, tries `G2`; if not, tries `G3`.

**(%is _E1 E2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%is E1 E2)` unifies with `E1` the result of evaluating `E2` as a Scheme expression. `E2` may contain logic variables, which are dereferenced automatically. Fails if `E2` contains unbound logic variables. Unlike other predicates, `%is` is implemented as syntax and not a procedure.

**(%let _(V ...) E ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

The form `(%let (V ...) E ...)` introduces `V ...` as lexically scoped logic variables to be used in `E ...`.

**(%melt _F S_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%melt F S)` unifies `S` with the thawed (original) form of the frozen structure in `F`.

**(%melt-new _F S_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%melt-new F S)` unifies `S` with a thawed _copy_ of the frozen structure in `F`. This means new logic variables are used for unbound logic variables in `F`.

**(%member _E1 E2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%member E1 E2)` succeeds if `E1` is a member of the list in `E2`.

**(%nonvar _E_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

`%nonvar` is the negation of `%var`. The goal `(%nonvar E)` succeeds if `E` is completely instantiated, i.e. it has no unbound variables in it.

**(%not _G_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%not G)` succeeds if `G` fails.

**(%more)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

The thunk `%more` produces more instantiations of the variables in the most recent `%which`-form that satisfy the goals in that `%which`-form. If no more solutions can be found, `%more` returns `#f`.

**(%or _G ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

The goal `(%or G ...)` succeeds if one of `G ...`, tried in that order, succeeds.

**(%rel (V ...) C ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

The form `(%rel (V ...) C ...)` creates a predicate object. Each clause `C` is of the form `((E ...) G ...)`, signifying that the goal created by applying the predicate object to anything that matches `(E ...)` is deemed to succeed if all the goals `G ...` can, in their turn, be shown to succeed.

**(%repeat)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%repeat)` always succeeds (even on retries). Used for failure-driven loops.

**\*schelog-use-occurs-check?\*** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[object]</span>  

If the global flag `*schelog-use-occurs-check?*` is false (the default), unification will not use the occurs check. If it is true, the occurs check is enabled.

**(%set-of _E1 G E2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%set-of E1 G E2)` unifies with `E2` the _set_ of all the instantiations of `E1` for which goal `G` succeeds.

**(%set-of-1 _E1 G E2_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%set-of-1 E1 G E2)` unifies with `E2` the _set_ of all the instantiations of `E1` for which goal `G` succeeds. The predicate fails if the set is empty.

**%true** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[goal]</span>  

The goal `%true` succeeds. Fails on retry.  

**(%var _E_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[predicate]</span>  

The goal `(%var E)` succeeds if `E` is not completely instantiated, i.e. it has at least one unbound variable in it.

**(%which _(V ...) G ..._)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[syntax]</span>  

The form `(%which (V ...) G ...)` returns an instantiation of the variables `V ...` that satisfies all of `G ...`. If `G ...` cannot be satisfied, `%which` returns `#f`. Calling the thunk `%more` produces more instantiations, if available.

**(\_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

A thunk that produces a new logic variable. Can be used in situations where we want a logic variable but don’t want to name it. `%let`, in contrast, introduces new lexical names for the logic variables it creates.

***

Copyright (c) 1993-2001, Dorai Sitaram.  
All rights reserved.

Permission to distribute and use this work for any purpose is hereby granted provided this copyright notice is included in the copy. This work is provided as is, with no warranty of any kind.
