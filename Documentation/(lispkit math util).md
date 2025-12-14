# LispKit Math Util

Library `(lispkit math util)` implements mathematical utility functions.


**(sgn _x_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Implements the sign/signum function. Returns -1 if _x_ is negative, 0 (or a signed zero, when inexact) if _x_ is zero, and 1 if _x_ is a positive number. `sgn` fails if _x_ is not a real number.

**(numbers _lo hi_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(numbers _lo hi f_)**  
**(numbers _lo hi guard f_)**  

Returns a list of numbers by iterating from integer _lo_ to integer _hi_ (both inclusive) and applying function _f_ to each integer in the range for which _guard_ returns true. The default guard always returns true. The default for _f_ is `identity`.

**(sum _xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the sum of all numbers of list _xs_. This procedure fails if there is an element in _xs_ which is not a number.

**(product _xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the product of all numbers of list _xs_. This procedure fails if there is an element in _xs_ which is not a number.

**(minimum _xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the minimum of all numbers of list _xs_. This procedure fails if there is an element in _xs_ which is not a number.

**(maximum _xs_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the maximum of all numbers of list _xs_. This procedure fails if there is an element in _xs_ which is not a number.

**(conjugate _x_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Conjugates number _x_. For real numbers _x_, `conjugate` returns _x_, otherwise _x_ is being returned with the opposite sign for the imaginary part.

**(degrees-\>radians _x_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Converts degrees into radians.

**(radians-\>degrees _x_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Converts radians into degrees.

**(prime? _n_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if integer _n_ is a prime number, `#f` otherwise.

**(make-nan _neg quiet payload_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a NaN whose sign bit is equal to _neg_ (`#t` for negative, `#f` for positive), whose quiet bit is equal to _quiet_ (`#t` for quiet, `#f` for signaling), and whose payload is the positive exact integer _payload_. It is an error if _payload_ is larger than a NaN can hold.

**(nan-negative? _x_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the sign bit of _x_ is 1 and `#f` otherwise.

**(nan-quiet? _x_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _x_ is a quiet NaN.

**(nan-payload _x_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the payload bits of floating-point number _x_ as a positive exact integer.

**(nan=? _x y_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _x_ and _y_ have the same sign, quiet bit, and payload; and `#f` otherwise.
