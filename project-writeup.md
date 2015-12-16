% An Analysis of the Kleinberg-Tardos
  MaxSAT Approximation Algorithm
% Rolph Recto
% CS 6161 Final Project

\newcommand{\expect}[1]{\text{E} \left [ #1 \right ]}
\newcommand{\paren}[1]{\left ( #1 \right )}
\newcommand{\bracket}[1]{\left [ #1 \right ]}

The simple randomized algorithm presented in Chapter 13.4 of
Kleinberg-Tardos[^1] is one of the simplest for approximating SAT, yet
is also surprising effective, satisfying on average over 7/8 of clauses
for 3-CNF instances. In this paper we extend the analysis of this algorithm
(henceforth the "KT algorithm") and explore two research questions:

* What is the distribution of satisfied clauses if the number of literals
in a clause is random?

* What is the distribution of satisfied clauses relative to (1) the proportion
of variables assigned as true/false and (2) proportion of positive literals 
in each clause?

[^1]: Jon Kleinberg and Éva Tardos, *Algorithm Design* (Pearson, 2005).

## 0 - Algorithm Review

The algorithm works by randomly assigning either `True` or `False` to
variables. If we let $x_1, \ldots, x_n$ be the set of variables in a
formula, the KT algorithm assigns Bernoulli variables $b_1, \ldots, b_n$
to the variables in order to build a model $M$ where

$$
M(x_i) = \left\{
          \begin{array}{ll}
            \text{True} & b_i = 1 \\
            \text{False} & b_i = 0
          \end{array}
          \right.
$$

The discussion in Kleinberg-Tardos assumes that $P(b_i = 1) = 1/2$.


## 1 - Random number of literals

Given a CNF formula $f$ with $m$ clauses and $X$ is a random variable
that stands for the proportions of the clauses in $f$ satisfied by the
KT algorithm, we know from a previous analysis that

$$
\expect{X_f}= 1 - \dfrac{1}{2^l}
$$

where $l$ is the constant number of literals per clause. What happens
when the number of literals per clause is random?

Let $L_1 , \ldots , L_m$ be random variables that stand for the number
of literals in a clause. A natural distribution to assume for the
number of literals is

$$
\forall_{1 \leq i \leq m} \ldotp L_i \sim \text{Uniform}(1,n)
$$

for some $n > 1$. Then what is $\text{E} \left [ X_f \right ]$?

Let $C = \{ C_1, \ldots, C_m \}$ be indicator variables such that $C_i = 1$
if clause $i$ is satisfied, and $C_i = 0$ otherwise.
If we let $Y_{ij}$ be an indicator variable where $Y_{ij} = 1$
if the $j$th literal in clause $i$ is true, and $Y_{ij} = 0$ otherwise,
then we know that for any $C_i \in C$,

$$
\expect{C_i \mid L_i = l_i}
= P(C_i = 1 \mid L_i = l_i)
= 1 - P \left (\bigcap_{j=1}^{l_i} Y_{ij} = 0 \right ) \ldotp
$$

Since the set of $Y_{ij}$ for $C_i$ are mutually independent, and
we know that $P(Y_{ij} = 1) = \dfrac{1}{2}$, then

$$
P \left ( \bigcap_{j=1}^{l_i} Y_{ij} = 0 \right )
= \prod_{j=1}^{l_i} P(Y_{ij} = 0)
= \prod_{j=1}^{l_i} \dfrac{1}{2} = \dfrac{1}{2^{l_i}}
$$

and

$$
\expect{C_i \mid L_i = l_i} = 1 - \dfrac{1}{2^{l_i}} \ldotp
$$

By definition of $X_f$, we know that

$$
X_f = \dfrac{C_1 + \ldots + C_m}{m}
$$

and given that $C_1 + \ldots + C_m$ is $L$-measurable where
$L = \{L_1, \ldots, L_m\}$, $X_f$ is also $L$-measurable such that

$$
\expect{X_f \mid L}
= \expect{\dfrac{1}{m} \left ( C_1 + \ldots + C_m \right ) \mid L}
= \dfrac{1}{m} \sum_{i=1}^{m} \expect{C_i \mid L} \ldotp
$$

Since $C_i$ is conditionally independent to all $L_j$ where $j \neq i$,

$$
\dfrac{1}{m} \sum_{i=1}^{m} \expect{C_i \mid L}
= \dfrac{1}{m} \sum_{i=1}^{m} \expect{C_i \mid L_i}
$$

and thus

$$
\expect{X_f}
= \expect{\expect{X_f \mid L}}
= \expect{\dfrac{1}{m} \sum_{i=1}^{m} \expect{C_i \mid L_i}}
= \expect{\dfrac{1}{m} \sum_{i=1}^{m} \left ( 1 - \dfrac{1}{2^{L_i}} \right )}
\ldotp
$$

Since all $L_i \in L$ are uniformly distributed, by definition of expectation
we know that

$$
= \dfrac{1}{m} \sum_{i=1}^{m} \bracket{1 - \expect{\dfrac{1}{2^{L_i}}}}
= \dfrac{1}{m} \sum_{i=1}^{m}
  \bracket{1 - \sum_{j=1}^{n}\dfrac{1}{n}\paren{\dfrac{1}{2^j}}}
\ldotp
$$

Since we know the closed form of a partial sum for a geometric series,

$$
= \dfrac{1}{m} \sum_{i=1}^{m}
  \bracket{1 - \dfrac{1}{n}\paren{\frac{1}{2}}
    \paren{\dfrac{1 - 1/2^n}{1 - 1/2}}}
= \dfrac{1}{m} \sum_{i=1}^{m}
  \bracket{1 - \dfrac{1}{n}\paren{1 - \dfrac{1}{2^n}}}
$$

$$
= \dfrac{1}{m} \bracket{m
    \paren{1 - \dfrac{1}{n}\paren{1 - \dfrac{1}{2^n}}}}
= 1 - \dfrac{1}{n}\paren{1 - \dfrac{1}{2^n}} \ldotp
$$

Notice that this is a generalization of the previous analysis for
a fixed number of literals in each clause. If we let
$L_i = l$ for all $L_i \in L$, then

$$
\expect{X_f}
= \expect{\dfrac{1}{m} \sum_{i=1}^{m} \expect{C_i \mid L_i = l}}
= \expect{\dfrac{1}{m} \sum_{i=1}^{m} \paren{1 - \dfrac{1}{2^{l}}}}
= 1 - \dfrac{1}{2^{l}} \ldotp
$$

which is exactly the result of the previous analysis.

*Example*. If $n=3$ then

$$
E \left [ X_f \right ] = 1 - \dfrac{1}{3} \left ( 1 - \dfrac{1}{2^3} \right )
$$
$$
= 1 - \dfrac{1}{3} \left ( \dfrac{7}{8} \right )
= 1 - \dfrac{7}{24} = \dfrac{17}{24} \ldotp
$$

Compare this to the expected value for Max-3SAT instances
(i.e., when $L_i=3$ for all $L_i \in L$):

$$
\expect{X_f} = 1 - \dfrac{1}{2^3} = \dfrac{7}{8} = \dfrac{21}{24} \ldotp
$$

This makes sense intuitively, since clauses are "easier" (more likely) to
be satisfied when they have more clauses, and Max-3SAT instances
are likely to have more literals.

## 2 - Proportion of Assignments and Literals

Previous analyses of the KT algorithm made two assumptions:

* The algorithm uniformly assigns ``true`` or ``false`` to variables.
That is, if $p$ equals the probability that a variable is
assigned to ``true``, previous analyses assumed $p = 1/2$.

* The distribution of the "polarity" of literals[^2] is uniform. That is,
if $r$ equals the probability that a literal is positive,
previous analyses assumed $r = 1/2$.

[^2]: By the "polarity" of a literal we mean whether the literal is
negated (which makes it negative) or not (which makes it positive).

The natural question arises: how does the distribution of $\expect{X_f}$
change when $p \neq 1/2$ or $r \neq 1/2$?

Notice that in \S 1 we assumed that $P(Y_{ij} = 0) = 1/2$. In general,
we know that $Y_{ij} = 0$ when the either the literal is positive and
its variable is assigned ``false`` or the literal is negative and its
variable is assigned ``true``. That is,

$$
P(Y_{ij}) = P \paren{ \paren{T \cap \neg S} \cup \paren{\neg T \cap S} }
= P(T)P(\neg S) + P(\neg T)P(S)
$$

where $T$ is the event that the variable is assigned ``true``,
and $S$ is the event that the literal is positive. Then we know

$$
P(Y_{ij}) = p(1-r) + (1-p)r = p + r - 2pr
$$

and thus

$$
\expect{C_i \mid L_i = l_i} = 1 - P \paren{\bigcap_{j=1}^{l_i} Y_{ij} = 0}
= 1 - \prod_{j=1}^{l_i} P(Y_{ij} = 0)
$$
$$
= 1 - \prod_{j=1}^{l_i} \paren{p + r - 2pr}
= 1 - \paren{p + r - 2pr}^{l_i}
\ldotp
$$

Recalling the analysis for $\expect{X_f}$ in \S 1,

$$
\expect{X_f}
= \expect{\dfrac{1}{m}\sum_{i=1}^{m} \expect{C_i \mid L_i}}
= \expect{\dfrac{1}{m}\sum_{i=1}^{m} \paren{1 - \paren{p + r - 2pr}^{L_i}}}
$$

$$
= \dfrac{1}{m}\sum_{i=1}^{m} \paren{1 - \expect{\paren{p + r - 2pr}^{L_i}}}
= \dfrac{1}{m}\sum_{i=1}^{m} \paren{1 -
    \sum_{j=1}^{n} \dfrac{1}{n} \paren{p + r - 2pr}^{j}}
$$

$$
= \dfrac{1}{m}\sum_{i=1}^{m} \paren{1 -
    \dfrac{1}{n} \paren{p+r-2pr}
    \paren{\dfrac{1 - \paren{p+r-2pr}^n}{1 - \paren{p+r-2pr}}}}
$$

$$
= 1 - \dfrac{1}{n} \paren{p+r-2pr}
    \paren{\dfrac{1 - \paren{p+r-2pr}^n}{1 - \paren{p+r-2pr}}} \ldotp
$$

Notice that this is a generalization for the case when $p = r = 1/2$, since

$$
p + r - 2pr
= \dfrac{1}{2} + \dfrac{1}{2} - 2\paren{\dfrac{1}{2}}\paren{\dfrac{1}{2}}
= 1 - \dfrac{1}{2}
= \dfrac{1}{2}
$$

and thus

$$
1 - \dfrac{1}{n} \paren{p+r-2pr}
    \paren{\dfrac{1 - \paren{p+r-2pr}^n}{1 - \paren{p+r-2pr}}}
$$
$$
= 1 - \dfrac{1}{n} \paren{\dfrac{1}{2}}
    \paren{\dfrac{1 - 1/2^n}{1 - 1/2}}
= 1 - \dfrac{1}{n}\paren{1 - 1/2^n}
$$

which matches the result from \S 1.

Again, if we assumed that the number of literals in each clause is fixed
such that $L_i = l$ for all $L_i \in L$,

$$
\expect{X_f}
= \expect{\dfrac{1}{m}\sum_{i=1}^{m} \expect{C_i \mid L_i = l}}
$$
$$
= \expect{\dfrac{1}{m}\sum_{i=1}^{m} \paren{1 - \paren{p + r - 2pr}^{l}}}
= 1 - \paren{p + r - 2pr}^{l} \ldotp
$$

And if we set $p = r = 1/2$,

$$
\expect{X_f}
= 1 - \paren{p + r - 2pr}^{l}
= 1 - \dfrac{1}{2^l}
$$

which again matches the previous result.

What happens when we set only one of $p$ or $r$ to $1/2$? We get the
remarkable result:

**Theorem**. When $p = 1/2$ (resp. $r = 1/2$) and
$r \neq 1/2$ (resp. $p \neq 1/2$), then $\expect{X_f}$ does not depend
on $r$ (resp. $p$).

Notice that if we let $p=1/2$,

$$
p + r - 2pr
= \dfrac{1}{2} + r - 2\paren{\dfrac{1}{2}}\paren{r}
= \dfrac{1}{2} + r - r
= \dfrac{1}{2}
$$

and the same result follows from similar reasoning when $r=1/2$.
Thus 

$$
\expect{X_f}
= 1 - \dfrac{1}{n} \paren{p+r-2pr}
    \paren{\dfrac{1 - \paren{p+r-2pr}^n}{1 - \paren{p+r-2pr}}}
$$
$$
= 1 - \dfrac{1}{n} \paren{\dfrac{1}{2}}
    \paren{\dfrac{1 - 1/2^n}{1 - 1/2}}
= 1 - \dfrac{1}{n} \paren{1 - 1/2^n}
$$

which is the same result as when $p = r = 1/2$.

Intuitively, this means that if the KT algorithm uniformly assigned ``true``
or ``false`` to variables, the proportion of clauses satisfied
*is independent of the proportion of literals that are positive/negative*.
Conversely, if the polarity of literals is uniformly distributed over
positive/negative, the distribution of clauses satisfied
*is independent of the proportion of variables to which the KT algorithm
assigns* ``true``/``false``.

*Example*. Let $p=1$, $r=1/2$, and $n=3$. Then

$$
\expect{X_f}
= 1 - \dfrac{1}{3} \paren{1 - 1/2^3}
= 1 - \dfrac{1}{3}\paren{\dfrac{7}{8}}
= 1 - \dfrac{7}{24}
= \dfrac{17}{24} \ldotp
$$

Thus even when we assign ``true`` to *all* variables, truly the
most trivial algorithm for MaxSAT imaginable, we still satisfy over 70%
of clauses. This speaks to the ease with which SAT can be approximated,
even though it is NP-complete. Compare this to the Traveling Salesman
Problem, where it is both NP-complete and impossible to approximate
(unless $P = NP$).

## 3 - Experiments
We ran experiments to empirically verify our analyses above. The experiments
consisted of generating a CNF formula with 1000 clauses and running the
KT algorithm to generate a model for the formula[^3].
We modified several parameters, including (1) the number of literals per
clause ($n$), (2) whether the number of literals per clause is uniformly
random (from 1 to $n$) or constant (always $n$), (3) the proportion of
literals in the formula that are positive ($r$), and (4) the proportion of
variables assigned `True` by the KT algorithm ($p$).

[^3]: We implemented the experiments in Haskell.
Go to [github.com/rolph-recto/cs6161-project](github.com/rolph-recto/cs6161-project)
to see the code.


  Random literals?        n          r           p          % sat        expected
--------------------   -------   ---------   ---------   -----------   ------------
         Yes              3         0.5         0.5         0.7075        0.7083
         Yes              3         0.3         0.5         0.6996        0.7083
         Yes              5         0.5         0.5         0.8076        0.8063
         Yes              5         0.3         0.8         0.7010        0.7036
         No               3         0.5         0.5         0.8751        0.8750
         No               3         0.5         1.0         0.8741        0.8750
         No               5         0.5         0.5         0.9688        0.9688
         No               5         0.4         0.5         0.9616        0.9688


We ran 100 trials for each parameter setting and calculated the average
number of clauses satisfied in a formula. The table above summarizes
our results. The first four columns specify the parameter setting for that
experiment; the `% sat` column denotes the average proportion of clauses
satisfied in a formula over 100 trials. The `expected` column denotes the
predicted proportion of satisfied clauses by the analyses above. The results
strongly indicate wide agreement between the empirically observed results
and the predicted proportions, which vindicates the analyses above as correct.

