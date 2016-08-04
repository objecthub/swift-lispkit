//
//  Instruction.swift
//  LispKit
//
//  Created by Matthias Zenger on 12/04/2016.
//  Copyright © 2016 ObjectHub. All rights reserved.
//
//  Licensed under the Apache License, Version 2.0 (the "License");
//  you may not use this file except in compliance with the License.
//  You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.
//

import Darwin
import NumberKit

///
/// Enumeration `Instruction` defines the bytecode instruction set supported by the LispKit
/// compiler and virtual machine.
///
public enum Instruction: CustomStringConvertible {
  
  // Stack ------------------------------------------------------------------------------------
  
  /// **`pop`**: Drops the top element from stack.
  case Pop
  
  /// **`dup`**: Duplicates the top element on the stack.
  case Dup
  
  /// **`swap`**: Swaps the top two elements on the stack.
  case Swap
  
  /// **`alloc` _n_**: Pushes `n` undefined values onto the stack.
  case Alloc(Int)
  
  /// **`reset` _o_,_n_**: Replaces `n` values on the stack with the undefined value starting
  /// from frame pointer offset `o`.
  case Reset(Int, Int)
  
  
  // Constants --------------------------------------------------------------------------------
  
  /// **`push_undef`**: Pushes the _undefined value_ onto the stack.
  case PushUndef
  
  /// **`push_void`**: Pushes the _void value_ onto the stack.
  case PushVoid
  
  /// **`push_eof`**: Pushes the _EOF value_ onto the stack.
  case PushEof
  
  /// **`push_null`**: Pushes value _null_ (empty list) onto the stack.
  case PushNull
  
  /// **`push_true`**: Pushes value `#true` onto the stack.
  case PushTrue
  
  /// **`push_false`**: Pushes value `#false` onto the stack.
  case PushFalse
  
  /// **`push_fixnum` _n_**: Pushes the fixnum _n_ onto the stack.
  case PushFixnum(Int64)
  
  /// **`push_bignum` _bn_**: Pushes the bignum _bn_ onto the stack.
  case PushBignum(BigInt)
  
  /// **`push_rat` _r_**: Pushes the rational number _r_ onto the stack.
  case PushRat(Rational<Int64>)
  
  /// **`push_bigrat` _br_**: Pushes the given bigrat number _br_ onto the stack.
  case PushBigrat(Rational<BigInt>)
  
  /// **`push_flonum` _x_**: Pushes the flonum _x_ onto the stack.
  case PushFlonum(Double)
  
  /// **`push_complex` _cx_**: Pushes the complex number _cx_ onto the stack.
  case PushComplex(Complex<Double>)
  
  /// **`push_char` _ch_**: Pushes the character _ch_ onto the stack.
  case PushChar(UniChar)
  
  /// **`push_constant` _c_**: Pushes the constant from the constant pool at index _c_ onto
  /// the stack.
  case PushConstant(Int)
  
  
  // Functions --------------------------------------------------------------------------------
  
  /// **`make_closure` _i_,_n_,_f_**: Creates a new closure from a name, capture list and a code
  /// fragment. The capture list is created from the top _n_ elements on the stack. _f_ is
  /// an index into the list of code fragments of the currently executed code. _i_ is a
  /// reference into the constant pool referring to the name of the closure (-1 indicates that
  /// the closure is anonymous)
  case MakeClosure(Int, Int, Int)
  
  /// **`make_frame`**: Pushes a new stack frame onto the stack.
  case MakeFrame
  
  /// **`call` _n_**: Calls a procedure with _n_ arguments.
  case Call(Int)
  
  /// **`tail_call` _n_**: Calls a procedure with _n_ arguments. This instruction is used
  /// for tail calls and does not require a new frame to be pushed.
  case TailCall(Int)
  
  /// **`apply` _n_**: This instruction expects on the stack a function, _n - 1_ individual
  /// arguments and an additional list of arguments. apply pushes the elements of the list
  /// onto the stack as well and then applies the function to all arguments on the stack.
  /// The instruction puts the result of the function application onto the stack.
  case Apply(Int)
  
  /// **`return`**: Returns from the currently executed procedure.
  case Return
  
  /// **`assert_arg_count` _n_**: Checks that there are exactly _n_ arguments on the stack.
  case AssertArgCount(Int)
  
  /// **`assert_min_arg_count` _n_**: Checks that there are at least _n_ arguments on the
  /// stack.
  case AssertMinArgCount(Int)
  
  /// **`collect_rest` _n_**: Collects the arguments exceeding _n_ into a list.
  case CollectRest(Int)
  
  /// **`compile`**: Compiles the expression on top of the stack creating a thunk (a
  /// procedure without arguments) which is left on top of the stack.
  case Compile
  
  
  // Macros -----------------------------------------------------------------------------------
  
  /// **`make_syntax`**: Pops a syntax transformer function off the stack, creates a special
  /// form from it and pushes it onto the stack.
  case MakeSyntax
  
  
  // Promises ---------------------------------------------------------------------------------
  
  /// **`make_promise`**: Creates a new promise on the stack whose value will be computed
  /// by executing the closure on top of the stack.
  case MakePromise
  
  /// **`force`**: Forces the value of the promise on top of the stack. If the promise has
  /// been evaluated already, push the value onto the stack and skip the next instruction
  /// (which is typically a `store_in_promise` instruction).
  case Force
  
  /// **`store_in_promise`**: Stores the value on top of the stack in the promise to which
  /// the second top-most entry on the stack The promise gets removed from the stack.
  case StoreInPromise
  
  // Variables --------------------------------------------------------------------------------
  
  /// **`make_local_variable` _o_**: Creates a new variable, pops an expression from the
  /// stack and assignes the variable this expression as its initial value. The variable
  /// is then stored at the location specified by the frame pointer offset _o_.
  case MakeLocalVariable(Int)
  
  /// **`make_variable_argument` _o_**: Creates a new variable and assignes the variable the
  /// expression at the location specified by the frame pointer offset _o_. The variable is
  /// then stored at the location specified by the frame pointer offset _o_, i.e. this
  /// instruction swaps a value on the stack with a variable with the same value.
  case MakeVariableArgument(Int)
  
  
  // Bindings ---------------------------------------------------------------------------------
  
  /// **`push_global` _c_**: _c_ is an index into the constant pool referring to a symbol.
  /// `push_global` pushes the value to which this symbol is bound in the global environment
  /// onto the stack.
  case PushGlobal(Int)
  
  /// **`set_global` _c_**: _c_ is an index into the constant pool referring to a symbol.
  /// `set_global` binds the symbol in the global environment to the value on top of the
  /// stack. `set_global` fails if the symbol has not previously been bound in the global
  /// environment.
  case SetGlobal(Int)
  
  /// **`define_global` _c_**: _c_ is an index into the constant pool referring to a symbol.
  /// `define_global` binds the symbol in the global environment to the value on top of the
  /// stack. As opposed to `set_global`, the symbol does not have to be bound previously.
  case DefineGlobal(Int)
  
  /// **`push_captured` _d_**: _d_ is an index into the capture list. `push_captured` pushes
  /// the value _d_ refers to onto the stack.
  case PushCaptured(Int)
  
  /// **`push_captured_value` _d_**: _d_ is an index into the capture list referring to
  /// a variable. `push_captured_value` pushes the value of the variable to which _d_
  /// refers to onto the stack.
  case PushCapturedValue(Int)
  
  /// **`set_captured_value` _d_**: _d_ is an index into the capture list referring to
  /// a variable. `set_captured_value` stores the value on top of the stack in the variable
  /// to which _d_ refers to.
  case SetCapturedValue(Int)
  
  /// **`push_local` _o_**: is an offset relative to the frame pointer. `push_local` pushes
  /// the value in this location onto the stack.
  case PushLocal(Int)
  
  /// **`push_local_value` _o_**: _o_ is an offset relative to the frame pointer referring
  /// to a variable. `push_local_value` pushes the value of this variable onto the stack.
  case PushLocalValue(Int)
  
  /// **`set_local` _o_**: _o_ is an offset relative to the frame pointer. `set_local`
  /// stores the value on top of the stack in this stack location overwriting the previous
  /// value.
  case SetLocal(Int)
  
  /// **`set_local_value` _o_**: _o_ is an offset relative to the frame pointer referring
  /// to a variable. `set_local_value` stores the value on top of the stack in this variable.
  case SetLocalValue(Int)
  
  
  // Control flow -----------------------------------------------------------------------------
  
  /// **`branch` _i_**: _i_ is an offset relative to the current instruction pointer.
  /// `branch` jumps to the instruction to which _i_ refers to.
  case Branch(Int)
  
  /// **`branch_if` _i_**: _i_ is an offset relative to the current instruction pointer.
  /// `branch_if` jumps to the instruction to which _i_ refers to if the value on the stack
  /// is not `#false`.
  case BranchIf(Int)
  
  /// **`branch_if_not` _i_**: _i_ is an offset relative to the current instruction
  /// pointer. `branch_if` jumps to the instruction to which _i_ refers to if the value
  /// on the stack is `#false`.
  case BranchIfNot(Int)
  
  /// **`or` _i_**: _i_ is an offset relative to the current instruction pointer. `or`
  /// jumps to the instruction to which _i_ refers to if the value on the stack is not
  /// `#false`. As opposed to `branch_if`, the value on top of the stack will remain there.
  /// Only if the value on top of the stack is `#false`, `or` will pop this value off the
  /// stack.
  case Or(Int)
  
  /// **`and` _i_**: _i_ is an offset relative to the current instruction pointer. `or`
  /// jumps to the instruction to which _i_ refers to if the value on the stack is `#false`.
  /// As opposed to `branch_if_not`, the value on top of the stack will remain there. Only
  /// if the value on top of the stack is not `#false`, `and` will pop this value off the
  /// stack.
  case And(Int)
  
  
  // Equivalences -----------------------------------------------------------------------------
  
  /// **`eq`**: Compares the top two values on the stack via `eq?` and pushes the result
  /// onto the stack.
  case Eq
  
  /// **`eqv`**: Compares the top two values on the stack via `eqv?` and pushes the result
  /// onto the stack.
  case Eqv
  
  /// **`equal`**: Compares the top two values on the stack via `equal?` and pushes the
  /// result onto the stack.
  case Equal
  
  
  // Containers -------------------------------------------------------------------------------

  /// **`is_pair`**: Pushes `#false` onto the stack if the current value on top of
  /// the stack is not a pair.
  case IsPair
  
  /// **`is_null`**: Pushes `#false` onto the stack if the current value on top of
  /// the stack is not null.
  case IsNull
  
  /// **`list` _n_**: Pops the top _n_ values off the stack and constructs a list out of
  /// them on top of the stack.
  case List(Int)
  
  /// **`cons`**: Pops the head and the tail of a new pair off the stack and pushes a
  /// new pair with this head and tail onto the stack.
  case Cons
  
  /// **`car`**: Pops a pair off the stack and pushes its head onto the stack.
  case Car
  
  /// **`cdr`**: Pops a pair off the stack and pushes its tail onto the stack.
  case Cdr
  
  /// **`vector` _n_**: Pops the top _n_ values off the stack and constructs a vector
  /// out of them on top of the stack.
  case Vector(Int)
  
  /// **`list_to_vector`**: Converts a list to a vector.
  case ListToVector
  
  /// **`vector_append` _n_**: Pops the top _n_ vectors off the stack and constructs a
  /// new vector by concatenating the individual vectors.
  case VectorAppend(Int)
  
  /// **`is_vector`**: Pushes `#false` onto the stack if the current value on top of
  /// the stack is not a vector.
  case IsVector
  
  
  // Math -------------------------------------------------------------------------------------
  
  /// **`fx_plus`**: Computes the sum of two fixnum values on the stack.
  case FxPlus
  
  /// **`fx_minus`**: Computes the difference of two fixnum values on the stack.
  case FxMinus
  
  /// **`fx_mult`**: Multiplies two fixnum values on the stack.
  case FxMult
  
  /// **`fx_div`**: Divides a fixnum value by another fixnum value on the stack.
  case FxDiv
  
  /// **`fl_plus`**: Computes the sum of two flonum values on the stack.
  case FlPlus
  
  /// **`fl_minus`**: Computes the difference of two flonum values on the stack.
  case FlMinus
  
  /// **`fl_mult`**: Multiplies two flonum values on the stack.
  case FlMult
  
  /// **`fl_div`**: Divides a flonum value by another flonum value on the stack.
  case FlDiv
  
  
  // Miscellaneous ----------------------------------------------------------------------------

  /// **`push_current_time`**: Pushes the current time as a flonum onto the stack. The time
  /// is expressed as seconds since January 1, 1970 at 00:00.
  case PushCurrentTime
  
  /// **`display`**: Displays the value on top of the stack on the console.
  case Display
  
  /// **`newline`**: Displays a newline character on the console.
  case Newline
  
  /// **`noop`**: Empty instruction; proceeds to the next instruction.
  case NoOp
  
  
  // ------------------------------------------------------------------------------------------
  
  
  /// Provides supplemental information about this instruction in a given code context.
  public func commentFor(code: Code, _ ip: Int) -> String? {
    switch self {
      case PushGlobal(let index):
        return code.constants[index].description
      case SetGlobal(let index):
        return code.constants[index].description
      case PushCaptured(_):
        return nil
      case PushCapturedValue(_):
        return nil
      case SetCapturedValue(_):
        return nil
      case PushLocal(_):
        return nil
      case PushLocalValue(_):
        return nil
      case SetLocal(_):
        return nil
      case SetLocalValue(_):
        return nil
      case PushConstant(let index):
        return code.constants[index].description
      case MakeClosure(let i, _, _):
        return i >= 0 ? code.constants[i].description : nil
      case MakeVariableArgument(_):
        return nil
      case PushChar(let char):
        var res = "'"
        res.append(Character(UnicodeScalar(char)))
        res.append(Character("'"))
        return res
      case Call(_):
        return nil
      case TailCall(_):
        return nil
      case Branch(let offset):
        return "jump to \(ip + offset - 1)"
      case BranchIf(let offset):
        return "jump to \(ip + offset - 1)"
      case BranchIfNot(let offset):
        return "jump to \(ip + offset - 1)"
      case And(let offset):
        return "pop or jump to \(ip + offset - 1) if false"
      case Or(let offset):
        return "pop or jump to \(ip + offset - 1) if true"
      default:
        return nil
    }
  }
  
  /// Returns a textual description of this instruction.
  public var description: String {
    switch self {
      case NoOp:
        return "noop"
      case Pop:
        return "pop"
      case Dup:
        return "dup"
      case PushGlobal(let index):
        return "push_global \(index)"
      case SetGlobal(let index):
        return "set_global \(index)"
      case DefineGlobal(let index):
        return "define_global \(index)"
      case PushCaptured(let index):
        return "push_captured \(index)"
      case PushCapturedValue(let index):
        return "push_captured_value \(index)"
      case SetCapturedValue(let index):
        return "set_captured_value \(index)"
      case PushLocal(let index):
        return "push_local \(index)"
      case MakeLocalVariable(let index):
        return "make_local_variable \(index)"
      case PushLocalValue(let index):
        return "push_local_value \(index)"
      case SetLocal(let index):
        return "set_local \(index)"
      case SetLocalValue(let index):
        return "set_local_value \(index)"
      case PushConstant(let index):
        return "push_constant \(index)"
      case MakeVariableArgument(let index):
        return "make_variable_argument \(index)"
      case PushUndef:
        return "push_undef"
      case PushVoid:
        return "push_void"
      case PushEof:
        return "push_eof"
      case PushNull:
        return "push_null"
      case PushTrue:
        return "push_true"
      case PushFalse:
        return "push_false"
      case PushFixnum(let num):
        return "push_fixnum \(num)"
      case PushBignum(let num):
        return "push_bignum \(num)"
      case PushRat(let num):
        return "push_rat \(num)"
      case PushBigrat(let num):
        return "push_bigrat \(num)"
      case PushFlonum(let num):
        return "push_flonum \(num)"
      case PushComplex(let num):
        return "push_complex \(num)"
      case PushChar(let char):
        return "push_char \(char)"
      case MakeClosure(let i, let n, let index):
        return "make_closure \(i),\(n),\(index)"
      case MakePromise:
        return "make_promise"
      case MakeSyntax:
        return "make_syntax"
      case Compile:
        return "compile"
      case Apply(let n):
        return "apply \(n)"
      case MakeFrame:
        return "make_frame"
      case Call(let n):
        return "call \(n)"
      case TailCall(let n):
        return "tail_call \(n)"
      case AssertArgCount(let n):
        return "assert_arg_count \(n)"
      case AssertMinArgCount(let n):
        return "assert_min_arg_count \(n)"
      case CollectRest(let n):
        return "collect_rest \(n)"
      case Alloc(let n):
        return "alloc \(n)"
      case Reset(let index, let n):
        return "reset \(index), \(n)"
      case Return:
        return "return"
      case Branch(let offset):
        return "branch \(offset)"
      case BranchIf(let offset):
        return "branch_if \(offset)"
      case BranchIfNot(let offset):
        return "branch_if_not \(offset)"
      case And(let offset):
        return "and \(offset)"
      case Or(let offset):
        return "or \(offset)"
      case Force:
        return "force"
      case StoreInPromise:
        return "store_in_promise"
      case Swap:
        return "swap"
      case PushCurrentTime:
        return "push_current_time"
      case Display:
        return "display"
      case Newline:
        return "newline"
      case Eq:
        return "eq"
      case Eqv:
        return "eqv"
      case Equal:
        return "equal"
      case IsPair:
        return "is_pair"
      case IsNull:
        return "is_null"
      case Cons:
        return "cons"
      case Car:
        return "car"
      case Cdr:
        return "cdr"
      case List(let n):
        return "list \(n)"
      case Vector(let n):
        return "vector \(n)"
      case ListToVector:
        return "list_to_vector"
      case VectorAppend(let n):
        return "vector_append \(n)"
      case IsVector:
        return "is_vector"
      case FxPlus:
        return "fx_plus"
      case FxMinus:
        return "fx_minus"
      case FxMult:
        return "fx_mult"
      case FxDiv:
        return "fx_div"
      case FlPlus:
        return "fl_plus"
      case FlMinus:
        return "fl_minus"
      case FlMult:
        return "fl_mult"
      case FlDiv:
        return "fl_div"
    }
  }
}
