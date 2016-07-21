//
//  Hash.swift
//  LispKit
//
//  Created by Matthias Zenger on 16/07/2016.
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

import Foundation


public func equalHash(expr: Expr) -> Int {
  var visited = Set<Reference>()
  
  func alreadyVisited(ref: Reference) -> Bool {
    if visited.contains(ref) {
      return true
    }
    visited.insert(ref)
    return false
  }
  
  func hash(expr: Expr) -> Int {
    switch expr.normalized {
      case .Undef:
        return 0
      case .Void:
        return 1
      case .Eof:
        return 2
      case .Null:
        return 3
      case .True:
        return 4
      case .False:
        return 5
      case .Sym(let sym):
        return sym.hashValue &* 31 &+ 6
      case .Fixnum(let num):
        return num.hashValue &* 31 &+ 7
      case .Bignum(let num):
        return num.hashValue &* 31 &+ 8
      case .Rat(let num):
        return num.hashValue &* 31 &+ 9
      case .Bigrat(let num):
        return num.hashValue &* 31 &+ 10
      case .Flonum(let num):
        return num.hashValue &* 31 &+ 11
      case .Complexnum(let num):
        return num.hashValue &* 31 &+ 12
      case .Char(let char):
        return char.hashValue &* 31 &+ 13
      case .Str(let str):
        return str.hashValue &* 31 &+ 14
      case .Bytes(let bvector):
        var res = 0
        for byte in bvector.value {
          res = res &* 31 &+ byte.hashValue
        }
        return res &* 31 &+ 15
      case .Pair(let car, let cdr):
        return ((hash(car) &* 31) &+ hash(cdr)) &* 31 + 16
      case .Box(let cell):
        if alreadyVisited(cell) {
          return 0
        }
        let res = hash(cell.value)
        visited.remove(cell)
        return res &* 31 + 17
      case .MPair(let tuple):
        if alreadyVisited(tuple) {
          return 0
        }
        let res = hash(tuple.fst) &* 31 &+ hash(tuple.snd)
        visited.remove(tuple)
        return res &* 31 + 18
      case .Vec(let vector):
        if alreadyVisited(vector) {
          return 0
        }
        var res = 0
        for expr in vector.exprs {
          res = res &* 31 &+ hash(expr)
        }
        visited.remove(vector)
        return res &* 31 + 19
      case .Map(let map):
        if alreadyVisited(map) {
          return 0
        }
        var res = 0
        for (key, value) in map.mappings {
          res += (hash(key) &* 31) + hash(value)
        }
        visited.remove(map)
        return res &* 31 + 20
      case .Promise(let promise):
        return promise.hashValue &* 31 + 21
      case .Proc(let proc):
        return proc.hashValue &* 31 + 22
      case .Special(let special):
        return special.hashValue &* 31 + 23
      case .Prt(let port):
        return port.hashValue &* 31 + 24
      case .Error(let err):
        return err.hashValue &* 31 + 25
    }
  }
  
  return hash(expr)
}

public func eqvHash(expr: Expr) -> Int {
  switch expr.normalized {
    case .Undef:
      return 0
    case .Void:
      return 1
    case .Eof:
      return 2
    case .Null:
      return 3
    case .True:
      return 4
    case .False:
      return 5
    case .Sym(let sym):
      return sym.hashValue &* 31 &+ 6
    case .Fixnum(let num):
      return num.hashValue &* 31 &+ 7
    case .Bignum(let num):
      return num.hashValue &* 31 &+ 8
    case .Rat(let num):
      return num.hashValue &* 31 &+ 9
    case .Bigrat(let num):
      return num.hashValue &* 31 &+ 10
    case .Flonum(let num):
      return num.hashValue &* 31 &+ 11
    case .Complexnum(let num):
      return num.hashValue &* 31 &+ 12
    case .Char(let char):
      return char.hashValue &* 31 &+ 13
    case .Str(let str):
      return str.hashValue &* 31 &+ 14
    case .Bytes(let bvector):
      return bvector.hashValue &* 31 &+ 15
    case .Pair(let car, let cdr):
      return ((eqvHash(car) &* 31) &+ eqvHash(cdr)) &* 31 + 16
    case .Box(let cell):
      return cell.hashValue &* 31 &+ 17
    case .MPair(let tuple):
      return tuple.hashValue &* 31 &+ 18
    case .Vec(let vector):
      return vector.hashValue &* 31 &+ 19
    case .Map(let map):
      return map.hashValue &* 31 &+ 20
    case .Promise(let promise):
      return promise.hashValue &* 31 + 21
    case .Proc(let proc):
      return proc.hashValue &* 31 + 22
    case .Special(let special):
      return special.hashValue &* 31 + 23
    case .Prt(let port):
      return port.hashValue &* 31 + 24
    case .Error(let err):
      return err.hashValue &* 31 + 25
  }
}

public func eqHash(expr: Expr) -> Int {
  switch expr {
    case .Undef:
      return 0
    case .Void:
      return 1
    case .Eof:
      return 2
    case .Null:
      return 3
    case .True:
      return 4
    case .False:
      return 5
    case .Sym(let sym):
      return sym.hashValue &* 31 &+ 6
    case .Fixnum(let num):
      return num.hashValue &* 31 &+ 7
    case .Bignum(let num):
      return num.hashValue &* 31 &+ 8
    case .Rat(let num):
      return num.hashValue &* 31 &+ 9
    case .Bigrat(let num):
      return num.hashValue &* 31 &+ 10
    case .Flonum(let num):
      return num.hashValue &* 31 &+ 11
    case .Complexnum(let num):
      return num.hashValue &* 31 &+ 12
    case .Char(let char):
      return char.hashValue &* 31 &+ 13
    case .Str(let str):
      return str.hashValue &* 31 &+ 14
    case .Bytes(let bvector):
      return bvector.hashValue &* 31 &+ 15
    case .Pair(let car, let cdr):
      return ((eqHash(car) &* 31) &+ eqHash(cdr)) &* 31 + 16
    case .Box(let cell):
      return cell.hashValue &* 31 &+ 17
    case .MPair(let tuple):
      return tuple.hashValue &* 31 &+ 18
    case .Vec(let vector):
      return vector.hashValue &* 31 &+ 19
    case .Map(let map):
      return map.hashValue &* 31 &+ 20
    case .Promise(let promise):
      return promise.hashValue &* 31 + 21
    case .Proc(let proc):
      return proc.hashValue &* 31 + 22
    case .Special(let special):
      return special.hashValue &* 31 + 23
    case .Prt(let port):
      return port.hashValue &* 31 + 24
    case .Error(let err):
      return err.hashValue &* 31 + 25
  }
}
