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
/// virtual machine.
///
public enum Instruction: CustomStringConvertible {
  
  /// Empty instruction.
  case NoOp
  
  // Stack modifications ----------------------------------------------------------------------
  
  /// Drop top element on stack.
  case Pop
  
  /// Duplicate top element on stack.
  case Dup
  
  /// Swap the top two entries on the stack.
  case Swap
  
  // Pushing constant values onto the stack ---------------------------------------------------
  
  case PushUndef
  
  /// Push void onto the stack.
  case PushVoid
  
  /// Push Eof onto the stack.
  case PushEof
  
  /// Push the empty list onto the stack.
  case PushNull
  
  /// Push true onto the stack.
  case PushTrue
  
  /// Push false onto the stack.
  case PushFalse
  
  /// Push the given fixnum onto the stack.
  case PushFixnum(Int64)
  
  /// Push the given bignum onto the stack.
  case PushBignum(BigInt)
  
  /// Push the given rational number onto the stack.
  case PushRat(Rational<Int64>)
  
  /// Push the given rational number onto the stack.
  case PushBigrat(Rational<BigInt>)
  
  /// Push the given flonum onto the stack.
  case PushFlonum(Double)
  
  /// Push the given complex number onto the stack.
  case PushComplex(Complex<Double>)
  
  /// Push the given character literal onto the stack.
  case PushChar(UniChar)
  
  /// PushConstant(n): Pushes the constant at index `n` of the constant pool onto the stack
  case PushConstant(Int)
  
  // Handle bindings: global, local, captured -------------------------------------------------
  
  /// PushGlobal(constref): constref refers to a symbol in the constant table. PushGlobal
  /// pushes the value to which this symbol is bound on the top-level onto the stack
  case PushGlobal(Int)
  
  /// SetGlobal(constref): constref refers to a symbol in the constant table. SetGlobal sets
  /// the value of this symbol globally to the value on top of the stack, assuming the binding
  /// has been created previously already. SetGlobal pops the value off the stack.
  case SetGlobal(Int)
  
  /// DefineGlobal(constref): constref refers to a symbol in the constant table. SetGlobal
  /// sets the value of this symbol globally to the value on top of the stack.
  case DefineGlobal(Int)
  
  /// PushCaptured(captref): pushes the captured value to which captref refers to onto the
  /// stack
  case PushCaptured(Int)
  
  /// PushCapturedValue(captref): pushes the value of the captured variable to which captref
  /// refers to onto the stack.
  case PushCapturedValue(Int)
  
  /// SetCapturedValue(captref): sets the captured variable to which captref refers to the
  /// value on top of the stack. SetCapturedValue pops the value off the stack.
  case SetCapturedValue(Int)
  
  /// PushLocal(frameref): frameref is an offset relative to the frame pointer. The expression
  /// in this location is pushed onto the stack.
  case PushLocal(Int)
  
  /// PushLocal(frameref): frameref is an offset relative to the frame pointer. The value of
  /// the variable in this location is pushed onto the stack.
  case PushLocalValue(Int)
  
  /// SetLocalValue(frameref): frameref is an offset relative to the frame pointer.
  /// SetLocalValue sets the variable in this location to the value on top of the stack.
  case SetLocalValue(Int)
  
  // Create variables -------------------------------------------------------------------------
  
  /// MakeLocalVariable(frameref): frameref is an offset relative to the frame pointer.
  /// MakeLocalVariable creates a new variable, pops an expression from the stack and assignes
  /// the variable this expression as its initial value. The variable is then stored at the
  /// location specified by frameref.
  case MakeLocalVariable(Int)
  
  /// MakeVariableArgument(frameref): frameref is an offset relative to the frame pointer.
  /// MakeVariableArgument creates a new variable and assignes the variable the expression at
  /// the location specified by frameref. The variable is then stored at the location specified
  /// by frameref. This instruction swaps a value with a variable with the same value on the
  /// stack.
  case MakeVariableArgument(Int)
  
  /// AllocLocals(n): Pushes `n` undefined values onto the stack.
  case AllocLocals(Int)
  
  /// ResetLocals(frameref, n): frameref is an offset relative to the frame pointer.
  /// ResetLocals clears the next `n` values on the stack starting from this location.
  case ResetLocals(Int, Int)
  
  // Create containers ------------------------------------------------------------------------
  
  /// MakeClosure(n, code): Creates a closure on the stack consisting of `n` captured
  /// variables (on the stack), and the code at index `code`
  case MakeClosure(Int, Int)
  
  /// MakePromise(n, code): Creates a promise on the stack whose value can be computed by
  /// executing a closure that consists of `n` captured variables (on the stack), and the
  /// code at index `code`.
  case MakePromise(Int, Int)
  
  /// MakeSyntax: Pops a syntax transformer off the stack, creates a macro special form
  /// from it and pushes this onto the stack.
  case MakeSyntax
  
  // Calling instructions ---------------------------------------------------------------------
  
  /// Pushes a new stack frame onto the stack
  case MakeFrame
  
  /// Call(n): Calls a procedure with `n` arguments.
  case Call(Int)
  
  /// TailCall(n): Calls a procedure with `n` arguments. This instruction is used for tail calls
  /// and does not require a new frame to be pushed.
  case TailCall(Int)
  
  /// Apply(n): Apply expects on the stack a function, `n - 1` individual arguments and an
  /// additional list of arguments. Apply pushes the elements of the list onto the stack as
  /// well and then applies the function to all arguments on the stack. The instruction puts
  /// the result of the function application onto the stack.
  case Apply(Int)
  
  // Procedure prologue instructions ----------------------------------------------------------
  
  /// AssertArgCount(n): Checks that there are exactly `n` arguments on the stack.
  case AssertArgCount(Int)
  
  /// AssertMinArgs(n): Checks that there are at least `n` arguments on the stack.
  case AssertMinArgCount(Int)
  
  /// CollectRest(n): Collects the arguments exceeding `n` into a list
  case CollectRest(Int)
  
  // Branching and return instructions --------------------------------------------------------
  
  /// Returns from the current function
  case Return
  
  /// Jump to offset
  case Branch(Int)
  
  /// Jump to offset if the value on the stack is not false.
  case BranchIf(Int)
  
  /// Jump to offset if the value on the stack is false.
  case BranchIfNot(Int)
  
  case And(Int)
  
  case Or(Int)
  
  case Force
  
  case StoreInPromise
  
  case PushCurrentTime
  
  case Display
  
  case Newline

  /// Compile
  case Compile

  // Basic operations -------------------------------------------------------------------------
  
  case Eq
  case Eqv
  case Equal
  case Cons
  case Car
  case Cdr
  case List(Int)
  case Vector(Int)
  case ListToVector
  case VectorAppend(Int)
  case IsVector
  
  case FxPlus
  case FxMinus
  case FxMult
  case FxDiv
  case FlPlus
  case FlMinus
  case FlMult
  case FlDiv
  
  // ------------------------------------------------------------------------------------------
  
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
    case SetLocalValue(_):
      return nil
    case PushConstant(let index):
      return code.constants[index].description
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
    case MakeClosure(let n, let index):
      return "make_closure \(n),\(index)"
    case MakePromise(let n, let index):
      return "make_promise \(n),\(index)"
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
    case AllocLocals(let n):
      return "alloc_locals \(n)"
    case ResetLocals(let index, let n):
      return "reset_locals \(index), \(n)"
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
