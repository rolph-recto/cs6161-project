## CS 6161 Final Project
This project explores approximation algorithms for MaxSAT. I will also
explore how well such algorithms generalize to arbitrary boolean formulae,
given that most of them require input in CNF form.

Algorithms currently implemented:

* Randomized algorithm from Kleinberg-Tardos 13.4
    * This is a very simple but effective algorithm. For CNF formulae
    with d literals in each clause, this algorithm satisfies
    (1-(1/2)^d)100% of clauses on average. For example, this algorithm
    satisfies 7/8 of clauses on average for Max-3SAT instances.

