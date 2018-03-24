# Coal: Composition of Real Linear Functions

Begin with an example. Let *f: x ↦ x+1 *and g* x ↦ 2x.*
Let a word formed with "f" and "g" letters, for example w="gfg".
By applying composition rules on w, we obtain for all *x*: 
*w(x)=gfg(x)=gf(2x)=g(2x+1)=4x+2*.
Slope is *4*, intercept is *2* and fixed point is *x=-2/3*.

This code automatizes composition of such linear functions.
When *f: x ↦ ax+b,* coefficients *a* and *b* can be formal letters, rational values or real values.
For rational values, gmp package is used to keep exact results.


