# LispKit Heap

Library `(lispkit heap)` provides an implementation of a _priority queue_ in form of a _binary max heap_. A _max heap_ is a tree-based data structure in which for any given node _C_, if _P_ is a parent node of _C_, then the value of _P_ is greater than or equal to the value of _C_. Heaps as implemented by `(lispkit heap)` are mutable objects.

**heap-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `heap` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all heap objects.

**(make-heap _pred\<?_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a new empty binary max heap with _pred\<?_ being the associated ordering function.

**(heap-empty? _hp_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns `#t` if the heap _hp_ is empty, otherwise `#f` is returned.

**(heap-max _hp_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns the largest item in heap _hp_, i.e. the item which is larger than all others according to the comparison function of _hp_. Note, `heap-max` does not remove the largest item as opposed to `heap-delete-max!`. If there are no items on the heap, an error is signaled.

**(heap-add! _hp e1 ..._)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Inserts an item into the heap. The same item can be inserted multiple times.

**(heap-delete-max! _hp_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns the largest item in heap _hp_, i.e. the item which is larger than all others according to the comparison function of _hp_, and removes the item from the heap. If there are no items on the heap, an error is signaled.

**(heap-clear! _hp_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Removes all items from _hp_. After this procedure has been executed, the heap is empty.

**(heap-copy _hp_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a copy of heap _hp_.

**(heap-\>vector _hp_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a new vector containing all items of the heap _hp_ in descending order. This procedure does not mutate _hp_.

**(heap-\>list _hp_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a list containing all items of the heap _hp_ in descending order.

**(heap-\>reversed-list _hp_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Returns a list containing all items of the heap _hp_ in ascending order.

**(list-\>heap! _hp items_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Inserts all the items from list _items_ into heap _hp_.

**(list-\>heap _items pred\<?_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Creates a new heap for the given ordering predicate _pred\<?_ and inserts all the items from list _items_ into it. `list-\>heap` returns the new heap.

**(vector-\>heap _vec pred\<?_)** <span style="float:right;text-align:rigth;">[procedure]</span>   

Creates and returns a new heap for the given ordering predicate _pred\<?_ and inserts all the items from vector _vec_ into it.
