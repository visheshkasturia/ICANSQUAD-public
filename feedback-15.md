CIS 552 Final Project, Fall 2021

Project name: Quadtrees
Github repo: https://github.com/cis552/project_15-project/
Group members: Vishesh Kasturia, Amithab Arumugam
Mentor: Nick Rioux

NOTE: after CIS 552 is over, we will be removing all repositories from github. If you want to make your project public, you should move it to your own github account.


Comments:

You did a great job designing, testing, and implementing your QuadTree datatype
and its algorithms. We were impressed by the evolution of the code over the
course of the project. You clearly put a considerable amount effort into it and
demonstrated your knowledge of typed functional programming. You also did a good
job of finding the right outside libraries and integrating them into your
development.

# Grade 100/100

## Proposal          5/5
## CP #1             10/10
## CP #2             10/10

## Correctness       25/25

The project functions correctly and is an appropriate level of difficulty.

## Design            30/30 
* Modularity
* Types
* FP
* Functors, Monads, etc.
* Abstraction

The project's modules and types are well-designed. It uses functional programming
concepts like folds and higher order functions well. Haskell's type system is
taken advantage of as demonstrated by the use of type classes like Monoid and
Foldable.

## Testing           15/15
The QuickCheck testing is extensive, and includes non-trivial generators.


## Style             5/5

Overall, the style of the project is good. Some finer points are:

- Many top-level functions are missing comments.
- DList should be defined as its own type synonym or newtype.
