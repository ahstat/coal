# Coal: Composition of Real Linear Functions

Begin with an example. Let *f: x ↦ x+1* and *g: x ↦ 2x*.
Let a word formed with *"f"* and *"g"* letters, for example *w="gfg"*.
By applying composition rules on *w*, we obtain for all *x*: 
*w(x)=gfg(x)=gf(2x)=g(2x+1)=4x+2*.
Slope is *4*, intercept is *2* and fixed point is *x=-2/3*.

This code automatizes composition of such linear functions.
When *f: x ↦ ax+b,* coefficients *a* and *b* can be formal letters, rational values or real values.
For rational values, gmp package is used to keep exact results.

## Motivating examples

### A functional way to count positive integers using two symbols
Let *f: x ↦ x+1* and *g x ↦ 10x*.
Any positive integer *n* can be written as *w(1)* for some word *w* (unicity is not guaranteed).
For example: *3402 = ffggffffgff(1)*,

If we take instead *g: x ↦ 2x*,
we get something related to the binary representation of integers.

### Functions involved in Collatz conjecture
Let *f: x ↦ (1/3)x-1/3* and *g x ↦ 2x*.
The [Collatz conjecture](https://en.wikipedia.org/wiki/Collatz_conjecture) considers composition of those linear functions 
(here when beginning with *1* and going up on the tree).

Important note: This repository does not provide any advance in that conjecture.
Before wasting time with it, make sure you can understand its underlying complexity, for example by reading carefully [this post](https://terrytao.wordpress.com/2011/08/25/the-collatz-conjecture-littlewood-offord-theory-and-powers-of-2-and-3). 

## Code examples

In main.R, a complete example of what it is possible to do.



Current features...

- composition of letters (words, f, g, m, n, ...)
- composition of coefficients (a, b, c, d, ...)
- composition of values (x, ...)
 
 
 
 Important functions:
 - word tree class,
 - pushn method,
 - add_signature method,
 - add_coeffs method,
 - random_word and random_words functions,
 - get_parents functions?
 
 
 
 
 