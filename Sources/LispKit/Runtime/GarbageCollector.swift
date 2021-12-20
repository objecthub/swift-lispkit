//
//  GarbageCollector.swift
//  LispKit
//
//  Created by Matthias Zenger on 27/09/2019.
//  Copyright © 2019 ObjectHub. All rights reserved.
//

import Foundation

public final class GarbageCollector: ObjectMarker {

  /// The last collection tag
  public var tag: UInt8 = 0

  /// Number of collection cycles
  public var cycles: UInt64 = 0

  /// The collection backlog
  public var backlog: Exprs

  public init() {
    self.backlog = Exprs()
  }

  public var backlogCapacity: Int {
    return self.backlog.capacity
  }
  
  /// Mark all objects in the provided root set.
  public func mark(_ rootSet: ObjectPool<TrackedObject>) {
    // Increment cycle counter
    self.cycles += 1
    // Compute next tag
    self.tag &+= 1
    // Mark all root objects
    for root in rootSet {
      root.mark(in: self)
      while !self.backlog.isEmpty {
        self.mark(self.backlog.removeLast())
      }
    }
  }

  /// Mark given expression.
  public func mark(_ expr: Expr) {
    var expr = expr
    while true {
      switch expr {
        case .pair(let car, .null):
          expr = car
        case .pair(let car, let cdr):
          self.markLater(car)
          expr = cdr
        case .box(let cell):
          if cell.tag == self.tag {
            return
          } else {
            cell.tag = self.tag
            expr = cell.value
          }
        case .mpair(let tuple):
          if tuple.tag == self.tag {
            return
          } else {
            tuple.tag = self.tag
            self.markLater(tuple.fst)
            expr = tuple.snd
          }
        case .array(let array):
          self.mark(array)
          return
        case .vector(let vector):
          self.mark(vector)
          return
        case .record(let record):
          self.mark(record)
          return
        case .table(let map):
          self.mark(map)
          return
        case .promise(let future):
          if future.tag == self.tag {
            return
          } else {
            future.tag = self.tag
            switch future.state {
              case .lazy(let proc):
                self.mark(proc)
                return
              case .shared(let future):
                expr = .promise(future)
              case .value(let e):
                expr = e
            }
          }
        case .values(let list):
          expr = list
        case .procedure(let proc):
          self.mark(proc)
          return
        case .special(let special):
          self.mark(special)
          return
        case .tagged(let etag, let data):
          self.markLater(etag)
          expr = data
        case .error(let err):
          self.mark(err)
          return
        case .syntax(_, let datum):
          expr = datum
        default:
          return
      }
    }
  }

  /// Mark given expression later by putting it into the backlog.
  public func markLater(_ expr: Expr) {
    switch expr {
      case .pair(_, _), .box(_), .mpair(_), .array(_), .vector(_), .record(_), .table(_),
           .promise(_), .values(_), .procedure(_), .special(_), .tagged(_, _), .error(_),
           .syntax(_, _):
        self.backlog.append(expr)
      default:
        break
    }
  }

  /// Mark procedure object.
  public func mark(_ proc: Procedure) {
    if proc.tag != self.tag {
      proc.tag = self.tag
      switch proc.kind {
        case .closure(_, let tag, let captures, let code):
          self.markLater(tag)
          for capture in captures {
            self.markLater(capture)
          }
          self.mark(code)
        case .parameter(let tuple):
          self.mark(tuple)
        case .rawContinuation(let state):
          self.mark(state)
        default:
          break
      }
    }
  }

  /// Mark special form.
  public func mark(_ special: SpecialForm) {
    if case .macro(let proc) = special.kind {
      self.mark(proc)
    }
  }

  /// Mark promise object.
  public func mark(_ promise: Promise) {
    if promise.tag != self.tag {
      promise.tag = self.tag
      switch promise.state {
        case .lazy(let proc):
          self.mark(proc)
        case .shared(let future):
          self.mark(future)
        case .value(let expr):
          self.markLater(expr)
      }
    }
  }

  /// Mark code object.
  public func mark(_ code: Code) {
    for constant in code.constants {
      self.markLater(constant)
    }
    for fragment in code.fragments {
      self.mark(fragment)
    }
  }

  /// Mark cell object.
  public func mark(_ cell: Cell) {
    if cell.tag != self.tag {
      cell.tag = self.tag
      self.markLater(cell.value)
    }
  }

  /// Mark tuple object.
  public func mark(_ tuple: Tuple) {
    if tuple.tag != self.tag {
      tuple.tag = self.tag
      self.markLater(tuple.fst)
      self.markLater(tuple.snd)
    }
  }

  /// Mark collection object.
  public func mark(_ collection: Collection) {
    if collection.tag != self.tag {
      collection.tag = self.tag
      if case .record(let type) = collection.kind {
        self.mark(type)
      }
      for expr in collection.exprs {
        self.markLater(expr)
      }
    }
  }

  /// Mark hash table content.
  public func mark(_ hashTable: HashTable) {
    if hashTable.tag != self.tag {
      hashTable.tag = self.tag
      for bucket in hashTable.buckets {
        var current = bucket
        while case .pair(.pair(let key, let cell), let next) = current {
          self.markLater(key)
          self.markLater(cell)
          current = next
        }
      }
      if case .custom(let procs) = hashTable.equiv {
        self.mark(procs.eql)
        self.mark(procs.hsh)
        self.mark(procs.get)
        self.mark(procs.add)
        self.mark(procs.del)
      }
    }
  }

  /// Mark runtime error object.
  public func mark(_ error: RuntimeError) {
    for irritant in error.irritants {
      self.markLater(irritant)
    }
    if let stackTrace = error.stackTrace {
      for proc in stackTrace {
        self.mark(proc)
      }
    }
  }

  /// Mark virtual machine state object.
  public func mark(_ state: VirtualMachineState) {
    for i in 0..<state.sp {
      self.markLater(state.stack[i])
    }
    for expr in state.registers.captured {
      self.markLater(expr)
    }
    self.mark(state.registers.code)
  }
}
