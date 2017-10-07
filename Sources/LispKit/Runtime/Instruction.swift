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
  case pop
  
  /// **`dup`**: Duplicates the top element on the stack.
  case dup
  
  /// **`swap`**: Swaps the top two elements on the stack.
  case swap
  
  /// **`alloc` _n_**: Pushes `n` undefined values onto the stack.
  case alloc(Int)
  
  /// **`reset` _o_,_n_**: Replaces `n` values on the stack with the undefined value starting
  /// from frame pointer offset `o`.
  case reset(Int, Int)
  
  
  // Constants --------------------------------------------------------------------------------
  
  /// **`push_undef`**: Pushes the _undefined value_ onto the stack.
  case pushUndef
  
  /// **`push_void`**: Pushes the _void value_ onto the stack.
  case pushVoid
  
  /// **`push_eof`**: Pushes the _EOF value_ onto the stack.
  case pushEof
  
  /// **`push_null`**: Pushes value _null_ (empty list) onto the stack.
  case pushNull
  
  /// **`push_true`**: Pushes value `#true` onto the stack.
  case pushTrue
  
  /// **`push_false`**: Pushes value `#false` onto the stack.
  case pushFalse
  
  /// **`push_fixnum` _n_**: Pushes the fixnum _n_ onto the stack.
  case pushFixnum(Int64)
  
  /// **`push_bignum` _bn_**: Pushes the bignum _bn_ onto the stack.
  case pushBignum(BigInt)
  
  /// **`push_rat` _r_**: Pushes the rational number _r_ onto the stack.
  case pushRat(Rational<Int64>)
  
  /// **`push_bigrat` _br_**: Pushes the given bigrat number _br_ onto the stack.
  case pushBigrat(Rational<BigInt>)
  
  /// **`push_flonum` _x_**: Pushes the flonum _x_ onto the stack.
  case pushFlonum(Double)
  
  /// **`push_complex` _cx_**: Pushes the complex number _cx_ onto the stack.
  case pushComplex(Complex<Double>)
  
  /// **`push_char` _ch_**: Pushes the character _ch_ onto the stack.
  case pushChar(UniChar)
  
  /// **`push_constant` _c_**: Pushes the constant from the constant pool at index _c_ onto
  /// the stack.
  case pushConstant(Int)
  
  
  // Multi values
  
  /// **`pack` _n_**: Pops _n_ values from the stack and packages them up in a multi-value
  /// expression.
  case pack(Int)
  
  /// **`unpack` _n_**: Retrieves _n_ values from a multi-value expression and stores them on
  /// the stack. If the boolean flag is true, all values beyond _n_ are pushed onto the stack
  /// as a list.
  case unpack(Int, Bool)
  
  
  // Functions --------------------------------------------------------------------------------
  
  /// **`make_closure` _i_,_n_,_f_**: Creates a new closure from a name, capture list and a code
  /// fragment. The capture list is created from the top _n_ elements on the stack. _f_ is
  /// an index into the list of code fragments of the currently executed code. _i_ is a
  /// reference into the constant pool referring to the name of the closure (-1 indicates that
  /// the closure is anonymous)
  case makeClosure(Int, Int, Int)
  
  /// **`make_frame`**: Pushes a new stack frame onto the stack.
  case makeFrame
  
  /// **`inject_frame` _i_**: Creates a new stack frame on the stack below the top value.
  case injectFrame
  
  /// **`call` _n_**: Calls a procedure with _n_ arguments.
  case call(Int)
  
  /// **`tail_call` _n_**: Calls a procedure with _n_ arguments. This instruction is used
  /// for tail calls and does not require a new frame to be pushed.
  case tailCall(Int)
  
  /// **`apply` _n_**: This instruction expects on the stack a function, _n - 1_ individual
  /// arguments and an additional list of arguments. apply pushes the elements of the list
  /// onto the stack as well and then applies the function to all arguments on the stack.
  /// The instruction puts the result of the function application onto the stack.
  case apply(Int)
  
  /// **`return`**: Returns from the currently executed procedure.
  case `return`
  
  /// **`assert_arg_count` _n_**: Checks that there are exactly _n_ arguments on the stack.
  case assertArgCount(Int)
  
  /// **`assert_min_arg_count` _n_**: Checks that there are at least _n_ arguments on the
  /// stack.
  case assertMinArgCount(Int)
  
  /// **`no_matching_arg_count`**: Fails with an error signaling the lack of a matching case
  /// (in a `case-lambda`).
  case noMatchingArgCount
  
  /// **`collect_rest` _n_**: Collects the arguments exceeding _n_ into a list.
  case collectRest(Int)
  
  /// **`compile`**: Compiles the expression on top of the stack creating a thunk (a
  /// procedure without arguments) which is left on top of the stack.
  case compile
  
  
  // Macros -----------------------------------------------------------------------------------
  
  /// **`make_syntax`**: Pops a syntax transformer function off the stack, creates a special
  /// form from it and pushes it onto the stack.
  case makeSyntax
  
  
  // Promises and streams ---------------------------------------------------------------------
  
  /// **`make_promise`**: Creates a new promise on the stack whose value will be computed
  /// by executing the closure on top of the stack.
  case makePromise
  
  /// **`make_stream`**: Creates a new stream on the stack whose value will be computed
  /// by executing the closure on top of the stack.
  case makeStream
  
  /// **`force`**: Forces the value of the promise or stream on top of the stack. If the
  /// promise or stream has been evaluated already, push the value onto the stack and skip
  /// the next instruction (which is typically a `store_in_promise` instruction).
  case force
  
  /// **`store_in_promise`**: Stores the value on top of the stack in the promise or stream
  /// to which the second top-most entry on the stack The promise gets removed from the stack.
  case storeInPromise
  
  
  // Variables --------------------------------------------------------------------------------
  
  /// **`make_local_variable` _o_**: Creates a new variable, pops an expression from the
  /// stack and assignes the variable this expression as its initial value. The variable
  /// is then stored at the location specified by the frame pointer offset _o_.
  case makeLocalVariable(Int)
  
  /// **`make_variable_argument` _o_**: Creates a new variable and assignes the variable the
  /// expression at the location specified by the frame pointer offset _o_. The variable is
  /// then stored at the location specified by the frame pointer offset _o_, i.e. this
  /// instruction swaps a value on the stack with a variable with the same value.
  case makeVariableArgument(Int)
  
  
  // Bindings ---------------------------------------------------------------------------------
  
  /// **`push_global` _c_**: _c_ is an index into the constant pool referring to a symbol.
  /// `push_global` pushes the value to which this symbol is bound in the global environment
  /// onto the stack.
  case pushGlobal(Int)
  
  /// **`set_global` _c_**: _c_ is an index into the constant pool referring to a symbol.
  /// `set_global` binds the symbol in the global environment to the value on top of the
  /// stack. `set_global` fails if the symbol has not previously been bound in the global
  /// environment.
  case setGlobal(Int)
  
  /// **`define_global` _c_**: _c_ is an index into the constant pool referring to a symbol.
  /// `define_global` binds the symbol in the global environment to the value on top of the
  /// stack. As opposed to `set_global`, the symbol does not have to be bound previously.
  case defineGlobal(Int)
  
  /// **`push_captured` _d_**: _d_ is an index into the capture list. `push_captured` pushes
  /// the value _d_ refers to onto the stack.
  case pushCaptured(Int)
  
  /// **`push_captured_value` _d_**: _d_ is an index into the capture list referring to
  /// a variable. `push_captured_value` pushes the value of the variable to which _d_
  /// refers to onto the stack.
  case pushCapturedValue(Int)
  
  /// **`set_captured_value` _d_**: _d_ is an index into the capture list referring to
  /// a variable. `set_captured_value` stores the value on top of the stack in the variable
  /// to which _d_ refers to.
  case setCapturedValue(Int)
  
  /// **`push_local` _o_**: is an offset relative to the frame pointer. `push_local` pushes
  /// the value in this location onto the stack.
  case pushLocal(Int)
  
  /// **`push_local_value` _o_**: _o_ is an offset relative to the frame pointer referring
  /// to a variable. `push_local_value` pushes the value of this variable onto the stack.
  case pushLocalValue(Int)
  
  /// **`set_local` _o_**: _o_ is an offset relative to the frame pointer. `set_local`
  /// stores the value on top of the stack in this stack location overwriting the previous
  /// value.
  case setLocal(Int)
  
  /// **`set_local_value` _o_**: _o_ is an offset relative to the frame pointer referring
  /// to a variable. `set_local_value` stores the value on top of the stack in this variable.
  case setLocalValue(Int)
  
  
  // Control flow -----------------------------------------------------------------------------
  
  /// **`branch` _i_**: _i_ is an offset relative to the current instruction pointer.
  /// `branch` jumps to the instruction to which _i_ refers to.
  case branch(Int)
  
  /// **`branch_if` _i_**: _i_ is an offset relative to the current instruction pointer.
  /// `branch_if` jumps to the instruction to which _i_ refers to if the value on the stack
  /// is not `#false`.
  case branchIf(Int)
  
  /// **`branch_if_not` _i_**: _i_ is an offset relative to the current instruction
  /// pointer. `branch_if_not` jumps to the instruction to which _i_ refers to if the value
  /// on the stack is `#false`.
  case branchIfNot(Int)
  
  /// **`keep_or_branch_if_not` _i_**: _i_ is an offset relative to the current instruction
  /// pointer. `keep_or_branch_if_not` jumps to the instruction to which _i_ refers to if
  /// the value on the stack is `#false`. If the value is not `#false`, the value will remain
  /// on the stack.
  case keepOrBranchIfNot(Int)
  
  /// **`branch_if_arg_mismatch` _n_,_i_**: _i_ is an offset relative to the current instruction
  /// pointer. `branch_if_arg_mismatch` jumps to the instruction to which _i_ refers to if
  /// there are not exactly _n_ arguments on the stack.
  case branchIfArgMismatch(Int, Int)
  
  /// **`branch_if_min_arg_mismatch` _n_,_i_**: _i_ is an offset relative to the current
  /// instruction pointer. `branch_if_min_arg_mismatch` jumps to the instruction to which _i_
  /// refers to if there are not at least _n_ arguments on the stack.
  case branchIfMinArgMismatch(Int, Int)
  
  /// **`or` _i_**: _i_ is an offset relative to the current instruction pointer. `or`
  /// jumps to the instruction to which _i_ refers to if the value on the stack is not
  /// `#false`. As opposed to `branch_if`, the value on top of the stack will remain there.
  /// Only if the value on top of the stack is `#false`, `or` will pop this value off the
  /// stack.
  case or(Int)
  
  /// **`and` _i_**: _i_ is an offset relative to the current instruction pointer. `or`
  /// jumps to the instruction to which _i_ refers to if the value on the stack is `#false`.
  /// As opposed to `branch_if_not`, the value on top of the stack will remain there. Only
  /// if the value on top of the stack is not `#false`, `and` will pop this value off the
  /// stack.
  case and(Int)
  
  
  // Equivalences -----------------------------------------------------------------------------
  
  /// **`eq`**: Compares the top two values on the stack via `eq?` and pushes the result
  /// onto the stack.
  case eq
  
  /// **`eqv`**: Compares the top two values on the stack via `eqv?` and pushes the result
  /// onto the stack.
  case eqv
  
  /// **`equal`**: Compares the top two values on the stack via `equal?` and pushes the
  /// result onto the stack.
  case equal
  
  
  // Frequently used primitives --------------------------------------------------------------

  /// **`is_pair`**: Pushes `#false` onto the stack if the current value on top of
  /// the stack is not a pair.
  case isPair
  
  /// **`is_null`**: Pushes `#false` onto the stack if the current value on top of
  /// the stack is not null.
  case isNull
  
  /// **`list` _n_**: Pops the top _n_ values off the stack and constructs a list out of
  /// them on top of the stack.
  case list(Int)
  
  /// **`cons`**: Pops the head and the tail of a new pair off the stack and pushes a
  /// new pair with this head and tail onto the stack.
  case cons
  
  /// **`decons`**: Pops a pair off the stack and pushes first its tail and then its head
  /// onto the stack.
  case decons
  
  /// **`car`**: Pops a pair off the stack and pushes its head onto the stack.
  case car
  
  /// **`cdr`**: Pops a pair off the stack and pushes its tail onto the stack.
  case cdr
  
  /// **`vector` _n_**: Pops the top _n_ values off the stack and constructs a vector
  /// out of them on top of the stack.
  case vector(Int)
  
  /// **`list_to_vector`**: Converts a list to a vector.
  case listToVector
  
  /// **`vector_append` _n_**: Pops the top _n_ vectors off the stack and constructs a
  /// new vector by concatenating the individual vectors.
  case vectorAppend(Int)
  
  /// **`is_vector`**: Pushes `#false` onto the stack if the current value on top of
  /// the stack is not a vector.
  case isVector
  
  /// **`not`**: Logical negation of the value on top of the stack.
  case not
  
  
  // Math -------------------------------------------------------------------------------------
  
  /// **`fx_plus`**: Computes the sum of two fixnum values on the stack.
  case fxPlus
  
  /// **`fx_minus`**: Computes the difference of two fixnum values on the stack.
  case fxMinus
  
  /// **`fx_mult`**: Multiplies two fixnum values on the stack.
  case fxMult
  
  /// **`fx_div`**: Divides a fixnum value by another fixnum value on the stack.
  case fxDiv
  
  /// **`fx_inc`**: Adds one to another fixnum value on the stack.
  case fxInc
  
  /// **`fx_dec`**: Substracts one from another fixnum value on the stack.
  case fxDec
  
  /// **`fx_is_zero`**: Puts true on the stack if the fixnum value on the stack is zero.
  case fxIsZero
  
  /// **`fx_eq`**: Compares a fixnum value with another fixnum value on the stack for equality.
  case fxEq
  
  /// **`fx_lt`**: Determines whether a fixnum value is less than another fixnum value.
  case fxLt
  
  /// **`fx_gt`**: Determines whether a fixnum value is greater than another fixnum value.
  case fxGt
  
  /// **`fx_lt_eq`**: Determines whether a fixnum value is less than or equal another fixnum
  /// value.
  case fxLtEq
  
  /// **`fx_gt_eq`**: Determines whether a fixnum value is greater than or equal another fixnum
  /// value.
  case fxGtEq
  
  /// **`fl_plus`**: Computes the sum of two flonum values on the stack.
  case flPlus
  
  /// **`fl_minus`**: Computes the difference of two flonum values on the stack.
  case flMinus
  
  /// **`fl_mult`**: Multiplies two flonum values on the stack.
  case flMult
  
  /// **`fl_div`**: Divides a flonum value by another flonum value on the stack.
  case flDiv
  
  /// **`fl_eq`**: Compares a flonum value with another flonum value on the stack for equality.
  case flEq
  
  /// **`fl_lt`**: Determines whether a flonum value is less than another flonum value.
  case flLt
  
  /// **`fl_gt`**: Determines whether a flonum value is greater than another flonum value.
  case flGt
  
  /// **`fl_lt_eq`**: Determines whether a flonum value is less than or equal another flonum
  /// value.
  case flLtEq
  
  /// **`fl_gt_eq`**: Determines whether a flonum value is greater than or equal another flonum
  /// value.
  case flGtEq
  
  // Miscellaneous ----------------------------------------------------------------------------

  /// **`push_current_time`**: Pushes the current time as a flonum onto the stack. The time
  /// is expressed as seconds since January 1, 1970 at 00:00.
  case pushCurrentTime
  
  /// **`display`**: Displays the value on top of the stack on the console.
  case display
  
  /// **`newline`**: Displays a newline character on the console.
  case newline
  
  /// **`noop`**: Empty instruction; proceeds to the next instruction.
  case noOp
  
  
  // ------------------------------------------------------------------------------------------
  
  
  /// Provides supplemental information about this instruction in a given code context.
  public func comment(for code: Code, at ip: Int) -> String? {
    switch self {
      case .pushGlobal(_):
        return nil
      case .setGlobal(_):
        return nil
      case .defineGlobal(_):
        return nil
      case .pushCaptured(_):
        return nil
      case .pushCapturedValue(_):
        return nil
      case .setCapturedValue(_):
        return nil
      case .pushLocal(_):
        return nil
      case .pushLocalValue(_):
        return nil
      case .setLocal(_):
        return nil
      case .setLocalValue(_):
        return nil
      case .pushConstant(let index):
        return code.constants[index].description
      case .makeClosure(let i, _, _):
        return i >= 0 ? code.constants[i].description : nil
      case .makeVariableArgument(_):
        return nil
      case .pushChar(let char):
        var res = "'"
        res.append(Character(UnicodeScalar(char)!))
        res.append(Character("'"))
        return res
      case .call(_):
        return nil
      case .tailCall(_):
        return nil
      case .branch(let offset):
        return "jump to \(ip + offset - 1)"
      case .branchIf(let offset):
        return "jump to \(ip + offset - 1)"
      case .branchIfNot(let offset):
        return "jump to \(ip + offset - 1)"
      case .keepOrBranchIfNot(let offset):
        return "jump to \(ip + offset - 1)"
      case .branchIfArgMismatch(_, let offset):
        return "jump to \(ip + offset - 1)"
      case .branchIfMinArgMismatch(_, let offset):
        return "jump to \(ip + offset - 1)"
      case .and(let offset):
        return "pop or jump to \(ip + offset - 1) if false"
      case .or(let offset):
        return "pop or jump to \(ip + offset - 1) if true"
      default:
        return nil
    }
  }
  
  /// Returns a textual description of this instruction.
  public var description: String {
    switch self {
      case .noOp:
        return "noop"
      case .pop:
        return "pop"
      case .dup:
        return "dup"
      case .pushGlobal(let loc):
        return "push_global \(loc)"
      case .setGlobal(let loc):
        return "set_global \(loc)"
      case .defineGlobal(let loc):
        return "define_global \(loc)"
      case .pushCaptured(let index):
        return "push_captured \(index)"
      case .pushCapturedValue(let index):
        return "push_captured_value \(index)"
      case .setCapturedValue(let index):
        return "set_captured_value \(index)"
      case .pushLocal(let index):
        return "push_local \(index)"
      case .makeLocalVariable(let index):
        return "make_local_variable \(index)"
      case .pushLocalValue(let index):
        return "push_local_value \(index)"
      case .setLocal(let index):
        return "set_local \(index)"
      case .setLocalValue(let index):
        return "set_local_value \(index)"
      case .pushConstant(let index):
        return "push_constant \(index)"
      case .makeVariableArgument(let index):
        return "make_variable_argument \(index)"
      case .pushUndef:
        return "push_undef"
      case .pushVoid:
        return "push_void"
      case .pushEof:
        return "push_eof"
      case .pushNull:
        return "push_null"
      case .pushTrue:
        return "push_true"
      case .pushFalse:
        return "push_false"
      case .pushFixnum(let num):
        return "push_fixnum \(num)"
      case .pushBignum(let num):
        return "push_bignum \(num)"
      case .pushRat(let num):
        return "push_rat \(num)"
      case .pushBigrat(let num):
        return "push_bigrat \(num)"
      case .pushFlonum(let num):
        return "push_flonum \(num)"
      case .pushComplex(let num):
        return "push_complex \(num)"
      case .pushChar(let char):
        return "push_char \(char)"
      case .pack(let n):
        return "pack \(n)"
      case .unpack(let n):
        return "unpack \(n)"
      case .makeClosure(let i, let n, let index):
        return "make_closure \(i),\(n),\(index)"
      case .makePromise:
        return "make_promise"
      case .makeStream:
        return "make_stream"
      case .makeSyntax:
        return "make_syntax"
      case .compile:
        return "compile"
      case .apply(let n):
        return "apply \(n)"
      case .makeFrame:
        return "make_frame"
      case .injectFrame:
        return "inject_frame"
      case .call(let n):
        return "call \(n)"
      case .tailCall(let n):
        return "tail_call \(n)"
      case .assertArgCount(let n):
        return "assert_arg_count \(n)"
      case .assertMinArgCount(let n):
        return "assert_min_arg_count \(n)"
      case .noMatchingArgCount:
        return "no_matching_arg_count"
      case .collectRest(let n):
        return "collect_rest \(n)"
      case .alloc(let n):
        return "alloc \(n)"
      case .reset(let index, let n):
        return "reset \(index), \(n)"
      case .`return`:
        return "return"
      case .branch(let offset):
        return "branch \(offset)"
      case .branchIf(let offset):
        return "branch_if \(offset)"
      case .branchIfNot(let offset):
        return "branch_if_not \(offset)"
      case .keepOrBranchIfNot(let offset):
        return "keep_or_branch_if_not \(offset)"
      case .branchIfArgMismatch(let n, let offset):
        return "branch_if_arg_mismatch \(n), \(offset)"
      case .branchIfMinArgMismatch(let n, let offset):
        return "branch_if_min_arg_mismatch \(n), \(offset)"
      case .and(let offset):
        return "and \(offset)"
      case .or(let offset):
        return "or \(offset)"
      case .force:
        return "force"
      case .storeInPromise:
        return "store_in_promise"
      case .swap:
        return "swap"
      case .pushCurrentTime:
        return "push_current_time"
      case .display:
        return "display"
      case .newline:
        return "newline"
      case .eq:
        return "eq"
      case .eqv:
        return "eqv"
      case .equal:
        return "equal"
      case .isPair:
        return "is_pair"
      case .isNull:
        return "is_null"
      case .cons:
        return "cons"
      case .decons:
        return "decons"
      case .car:
        return "car"
      case .cdr:
        return "cdr"
      case .list(let n):
        return "list \(n)"
      case .vector(let n):
        return "vector \(n)"
      case .listToVector:
        return "list_to_vector"
      case .vectorAppend(let n):
        return "vector_append \(n)"
      case .isVector:
        return "is_vector"
      case .not:
        return "not"
      case .fxPlus:
        return "fx_plus"
      case .fxMinus:
        return "fx_minus"
      case .fxMult:
        return "fx_mult"
      case .fxDiv:
        return "fx_div"
      case .fxInc:
        return "fx_inc"
      case .fxDec:
        return "fx_dec"
      case .fxIsZero:
        return "fx_is_zero"
      case .fxEq:
        return "fx_eq"
      case .fxLt:
        return "fx_lt"
      case .fxGt:
        return "fx_gt"
      case .fxLtEq:
        return "fx_lt_eq"
      case .fxGtEq:
        return "fx_gt_eq"
      case .flPlus:
        return "fl_plus"
      case .flMinus:
        return "fl_minus"
      case .flMult:
        return "fl_mult"
      case .flDiv:
        return "fl_div"
      case .flEq:
        return "fl_eq"
      case .flLt:
        return "fl_lt"
      case .flGt:
        return "fl_gt"
      case .flLtEq:
        return "fl_lt_eq"
      case .flGtEq:
        return "fl_gt_eq"
    }
  }
}
