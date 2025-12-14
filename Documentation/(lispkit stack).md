# LispKit Stack

Library `(lispkit stack)` provides an implementation for mutable stacks, i.e. mutable LIFO buffers.

**stack-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `stack` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all stack objects.

**(make-stack)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a new empty stack.

**(stack _x ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a new stack with _x_ on its top position followed by the remaining parameters.

```scheme
(stack-top (stack 1 2 3))  ⇒ 1 
```

**(stack? _obj_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if _obj_ is a stack; otherwise `#f` is returned.

**(stack-empty? _s_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if stack _s_ is empty.

**(stack-size _s_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns the size of stack _s_, i.e. the number of elements buffered in _s_.

**(stack=? _s1 s2_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if stack _s1_ has the exact same elements in the same order like stack _s2_; otherwise, `#f` is returned.

**(stack-push! _s x_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Pushes element _x_ onto stack _s_.

**(stack-top _s_)** <span style="float:right;text-align:rigth;">[procedure]</span>   
  
Returns the top element of stack _s_. If the stack is empty, an error is raised.

**(stack-pop! _s_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Removes the top element from stack _s_ and returns its value.

```scheme
(define s (make-stack))
(stack-push! s 1)
(stack-push! s 2)
(stack-pop! s)  ⇒ 2
(stack-size s)  ⇒ 1
```

**(stack-clear! _s_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Removes all elements from stack _s_.

**(stack-copy _s_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a copy of stack _s_.

**(stack-\>list _s_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a list consisting of all elements on stack _s_ in the order they appear, i.e. starting with the top element.

```scheme
(stack->list (stack 1 2 3))
```

**(list-\>stack _l_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a new stack consisting of the elements of list _l_. The first element in _l_ will become the top element of the stack that is returned.

**(list-\>stack! _s l_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Pushes the elements of list _l_ onto stack _s_ in reverse order.

```scheme
(define s (list->stack '(3 2 1)))
(list->stack! s '(6 5 4))
(stack->list s)  ⇒ (6 5 4 3 2 1)
```
