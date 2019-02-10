//
//  Hash.swift
//  LispKit
//
//  Created by Matthias Zenger on 16/07/2016.
//  Copyright © 2016-2019 ObjectHub. All rights reserved.
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

public func equalHash(_ expr: Expr) -> Int {
  var hasher = Hasher()
  equalHash(expr, into: &hasher)
  return hasher.finalize()
}

public func equalHash(_ expr: Expr, into hasher: inout Hasher) {
  var visited = Set<Reference>()

  func alreadyVisited(_ ref: Reference) -> Bool {
    if visited.contains(ref) {
      return true
    }
    visited.insert(ref)
    return false
  }

  func hash(_ expr: Expr) {
    switch expr.normalized {
      case .undef,
           .uninit(_):
        hasher.combine(0)
      case .void:
        hasher.combine(1)
      case .eof:
        hasher.combine(2)
      case .null:
        hasher.combine(3)
      case .true:
        hasher.combine(4)
      case .false:
        hasher.combine(5)
      case .symbol(let sym):
        hasher.combine(6)
        hasher.combine(sym)
      case .fixnum(let num):
        hasher.combine(7)
        hasher.combine(num)
      case .bignum(let num):
        hasher.combine(8)
        hasher.combine(num)
      case .rational(let n, let d):
        hasher.combine(9)
        hash(n)
        hash(d)
      case .flonum(let num):
        hasher.combine(10)
        hasher.combine(num)
      case .complex(let num):
        hasher.combine(11)
        hasher.combine(num)
      case .char(let char):
        hasher.combine(12)
        hasher.combine(char)
      case .string(let str):
        hasher.combine(13)
        hasher.combine(str)
      case .bytes(let bvector):
        hasher.combine(14)
        for byte in bvector.value {
          hasher.combine(byte)
        }
      case .pair(let car, let cdr):
        hasher.combine(15)
        hash(car)
        hash(cdr)
      case .box(let cell):
        hasher.combine(16)
        guard alreadyVisited(cell) else {
          hash(cell.value)
          visited.remove(cell)
          break
        }
      case .mpair(let tuple):
        hasher.combine(17)
        guard alreadyVisited(tuple) else {
          hash(tuple.fst)
          hash(tuple.snd)
          visited.remove(tuple)
          break
        }
      case .array(let array):
        hasher.combine(18)
        guard alreadyVisited(array) else {
          for expr in array.exprs {
            hash(expr)
          }
          visited.remove(array)
          break
        }
      case .vector(let vector):
        hasher.combine(19)
        guard alreadyVisited(vector) else {
          for expr in vector.exprs {
            hash(expr)
          }
          visited.remove(vector)
          break
        }
      case .record(let record):
        hasher.combine(20)
        guard alreadyVisited(record) else {
          switch record.kind {
            case .recordType:
              hasher.combine(1)
            case .record(let type):
              hasher.combine(type)
            default:
              break
          }
          for expr in record.exprs {
            hash(expr)
          }
          visited.remove(record)
          break
        }
      case .table(let map):
        hasher.combine(21)
        guard alreadyVisited(map) else {
          for (key, value) in map.mappings {
            hash(key)
            hash(value)
          }
          visited.remove(map)
          break
        }
      case .promise(let promise):
        hasher.combine(22)
        hasher.combine(promise)
      case .values(let expr):
        hasher.combine(23)
        hash(expr)
      case .procedure(let proc):
        hasher.combine(24)
        hasher.combine(proc)
      case .special(let special):
        hasher.combine(25)
        hasher.combine(special)
      case .env(let environment):
        hasher.combine(26)
        hasher.combine(environment)
      case .port(let port):
        hasher.combine(27)
        hasher.combine(port)
      case .object(let obj):
        hasher.combine(28)
        hasher.combine(obj)
      case .tagged(let tag, let expr):
        hasher.combine(29)
        eqvHash(tag, into: &hasher)
        hash(expr)
      case .error(let err):
        hasher.combine(30)
        hasher.combine(err)
      case .syntax(let pos, let expr):
        hasher.combine(31)
        hasher.combine(pos)
        hash(expr)
    }
  }

  return hash(expr)
}

public func eqvHash(_ expr: Expr) -> Int {
  var hasher = Hasher()
  eqvHash(expr, into: &hasher)
  return hasher.finalize()
}

public func eqvHash(_ expr: Expr, into hasher: inout Hasher) {
  switch expr.normalized {
    case .undef,
         .uninit(_):
      hasher.combine(0)
    case .void:
      hasher.combine(1)
    case .eof:
      hasher.combine(2)
    case .null:
      hasher.combine(3)
    case .true:
      hasher.combine(4)
    case .false:
      hasher.combine(5)
    case .symbol(let sym):
      hasher.combine(6)
      hasher.combine(sym)
    case .fixnum(let num):
      hasher.combine(7)
      hasher.combine(num)
    case .bignum(let num):
      hasher.combine(8)
      hasher.combine(num)
    case .rational(let n, let d):
      hasher.combine(9)
      eqvHash(n, into: &hasher)
      eqvHash(d, into: &hasher)
    case .flonum(let num):
      hasher.combine(10)
      hasher.combine(num)
    case .complex(let num):
      hasher.combine(11)
      hasher.combine(num)
    case .char(let char):
      hasher.combine(12)
      hasher.combine(char)
    case .string(let str):
      hasher.combine(13)
      hasher.combine(ObjectIdentifier(str))
    case .bytes(let bvector):
      hasher.combine(14)
      hasher.combine(bvector)
    case .pair(let car, let cdr):
      hasher.combine(15)
      eqvHash(car, into: &hasher)
      eqvHash(cdr, into: &hasher)
    case .box(let cell):
      hasher.combine(16)
      hasher.combine(cell)
    case .mpair(let tuple):
      hasher.combine(17)
      hasher.combine(tuple)
    case .array(let array):
      hasher.combine(18)
      hasher.combine(array)
    case .vector(let vector):
      hasher.combine(19)
      hasher.combine(vector)
    case .record(let record):
      hasher.combine(20)
      hasher.combine(record)
    case .table(let map):
      hasher.combine(21)
      hasher.combine(map)
    case .promise(let promise):
      hasher.combine(22)
      hasher.combine(promise)
    case .values(let expr):
      hasher.combine(23)
      eqvHash(expr, into: &hasher)
    case .procedure(let proc):
      hasher.combine(24)
      hasher.combine(proc)
    case .special(let special):
      hasher.combine(25)
      hasher.combine(special)
    case .env(let environment):
      hasher.combine(26)
      hasher.combine(environment)
    case .port(let port):
      hasher.combine(27)
      hasher.combine(port)
    case .object(let obj):
      hasher.combine(28)
      hasher.combine(obj)
    case .tagged(let tag, let expr):
      hasher.combine(29)
      eqvHash(tag, into: &hasher)
      eqvHash(expr, into: &hasher)
    case .error(let err):
      hasher.combine(30)
      hasher.combine(err)
    case .syntax(let pos, let expr):
      hasher.combine(31)
      hasher.combine(pos)
      eqvHash(expr, into: &hasher)
  }
}

public func eqHash(_ expr: Expr) -> Int {
  var hasher = Hasher()
  eqHash(expr, into: &hasher)
  return hasher.finalize()
}

public func eqHash(_ expr: Expr, into hasher: inout Hasher) {
  switch expr.normalized {
    case .undef,
         .uninit(_):
      hasher.combine(0)
    case .void:
      hasher.combine(1)
    case .eof:
      hasher.combine(2)
    case .null:
      hasher.combine(3)
    case .true:
      hasher.combine(4)
    case .false:
      hasher.combine(5)
    case .symbol(let sym):
      hasher.combine(6)
      hasher.combine(sym)
    case .fixnum(let num):
      hasher.combine(7)
      hasher.combine(num)
    case .bignum(let num):
      hasher.combine(8)
      hasher.combine(num)
    case .rational(let n, let d):
      hasher.combine(9)
      eqHash(n, into: &hasher)
      eqHash(d, into: &hasher)
    case .flonum(let num):
      hasher.combine(10)
      hasher.combine(num)
    case .complex(let num):
      hasher.combine(11)
      hasher.combine(num)
    case .char(let char):
      hasher.combine(12)
      hasher.combine(char)
    case .string(let str):
      hasher.combine(13)
      hasher.combine(ObjectIdentifier(str))
    case .bytes(let bvector):
      hasher.combine(14)
      hasher.combine(bvector)
    case .pair(let car, let cdr):
      hasher.combine(15)
      eqHash(car, into: &hasher)
      eqHash(cdr, into: &hasher)
    case .box(let cell):
      hasher.combine(16)
      hasher.combine(cell)
    case .mpair(let tuple):
      hasher.combine(17)
      hasher.combine(tuple)
    case .array(let array):
      hasher.combine(18)
      hasher.combine(array)
    case .vector(let vector):
      hasher.combine(19)
      hasher.combine(vector)
    case .record(let record):
      hasher.combine(20)
      hasher.combine(record)
    case .table(let map):
      hasher.combine(21)
      hasher.combine(map)
    case .promise(let promise):
      hasher.combine(22)
      hasher.combine(promise)
    case .values(let expr):
      hasher.combine(23)
      eqHash(expr, into: &hasher)
    case .procedure(let proc):
      hasher.combine(24)
      hasher.combine(proc)
    case .special(let special):
      hasher.combine(25)
      hasher.combine(special)
    case .env(let environment):
      hasher.combine(26)
      hasher.combine(environment)
    case .port(let port):
      hasher.combine(27)
      hasher.combine(port)
    case .object(let obj):
      hasher.combine(28)
      hasher.combine(obj)
    case .tagged(let tag, let expr):
      hasher.combine(29)
      eqvHash(tag, into: &hasher)
      eqHash(expr, into: &hasher)
    case .error(let err):
      hasher.combine(30)
      hasher.combine(err)
    case .syntax(let pos, let expr):
      hasher.combine(31)
      hasher.combine(pos)
      eqHash(expr, into: &hasher)
  }
}
