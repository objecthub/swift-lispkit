//
//  BoxLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 18/07/2016.
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


public final class BoxLibrary: Library {
  
  public override func export() {
    // Boxes
    define(Procedure("box?", isBox))
    define(Procedure("box", box))
    define(Procedure("unbox", unbox))
    define(Procedure("set-box!", setBox))
    define("update-box!", compile: "(lambda (box proc) (set-box! box (proc (unbox box))))")
    
    // Mutable pairs
    define(Procedure("mpair?", isMpair))
    define(Procedure("mcons", mcons))
    define(Procedure("mcar", mcar))
    define(Procedure("mcdr", mcdr))
    define(Procedure("set-mcar!", setMcar))
    define(Procedure("set-mcdr!", setMcdr))
  }
  
  //-------- MARK: - Boxes
  
  func box(expr: Expr?) -> Expr {
    return .Box(Cell(expr ?? .Undef))
  }
  
  func unbox(expr: Expr) throws -> Expr {
    guard case .Box(let cell) = expr else {
      throw EvalError.TypeError(expr, [.BoxType])
    }
    return cell.value
  }
  
  func setBox(expr: Expr, value: Expr) throws -> Expr {
    guard case .Box(let cell) = expr else {
      throw EvalError.TypeError(expr, [.BoxType])
    }
    cell.value = value
    return .Void
  }
  
  func isBox(expr: Expr) -> Expr {
    guard case .Box(_) = expr else {
      return .False
    }
    return .True
  }
  
  //-------- MARK: - Mutable pairs
  
  func mcons(car: Expr, cdr: Expr) throws -> Expr {
    return .MutablePair(Tuple(car, cdr))
  }
  
  func mcar(expr: Expr) throws -> Expr {
    guard case .MutablePair(let tuple) = expr else {
      throw EvalError.TypeError(expr, [.MPairType])
    }
    return tuple.fst
  }
  
  func mcdr(expr: Expr) throws -> Expr {
    guard case .MutablePair(let tuple) = expr else {
      throw EvalError.TypeError(expr, [.MPairType])
    }
    return tuple.snd
  }
  
  func setMcar(expr: Expr, value: Expr) throws -> Expr {
    guard case .MutablePair(let tuple) = expr else {
      throw EvalError.TypeError(expr, [.MPairType])
    }
    tuple.fst = value
    return .Void
  }
  
  func setMcdr(expr: Expr, value: Expr) throws -> Expr {
    guard case .MutablePair(let tuple) = expr else {
      throw EvalError.TypeError(expr, [.MPairType])
    }
    tuple.snd = value
    return .Void
  }
  
  func isMpair(expr: Expr) -> Expr {
    guard case .MutablePair(_) = expr else {
      return .False
    }
    return .True
  }
}
