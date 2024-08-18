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

///
/// Box library: based on Racket spec.
///
public final class BoxLibrary: NativeLibrary {
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "box"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
    self.`import`(from: ["lispkit", "core"], "define", "apply-with-values")
  }
  
  /// Declarations of the library.
  public override func declarations() {
    // Boxes
    self.define(Procedure("box?", self.isBox))
    self.define(Procedure("box", self.box))
    self.define(Procedure("unbox", self.unbox))
    self.define(Procedure("set-box!", self.setBox))
    self.define("update-box!", via:
      "(define (update-box! box proc) (set-box! box (apply-with-values proc (unbox box))))")
    
    // Mutable pairs
    self.define(Procedure("mpair?", self.isMpair))
    self.define(Procedure("mcons", self.mcons))
    self.define(Procedure("mcar", self.mcar))
    self.define(Procedure("mcdr", self.mcdr))
    self.define(Procedure("set-mcar!", self.setMcar))
    self.define(Procedure("set-mcdr!", self.setMcdr))
    
    // Atomic boxes
    self.define("atomic-box-type-tag", as: AtomicBox.type.objectTypeTag())
    self.define(Procedure("atomic-box?", self.isAtomicBox))
    self.define(Procedure("make-atomic-box", self.makeAtomicBox))
    self.define(Procedure("atomic-box-ref", self.atomicBoxRef))
    self.define(Procedure("atomic-box-set!", self.atomicBoxSet))
    self.define(Procedure("atomic-box-swap!", self.atomicBoxSwap))
    self.define(Procedure("atomic-box-compare-and-set!", self.atomicBoxCompareAndSet))
    self.define(Procedure("atomic-box-compare-and-swap!", self.atomicBoxCompareAndSwap))
    self.define(Procedure("atomic-box-inc+mul!", self.atomicBoxIncMul))
    self.define(Procedure("_atomic-box-ref-lock", self.atomicBoxRefLock))
    self.define(Procedure("_atomic-box-set-unlock!", self.atomicBoxSetUnlock))
    self.define("atomic-box-update!", via:
      "(define (atomic-box-update! box proc)",
      "  (_atomic-box-set-unlock! box (apply-with-values proc (_atomic-box-ref-lock box))))")
  }
  
  //-------- MARK: - Boxes
  
  private func box(_ args: Arguments) -> Expr {
    return .box(Cell(args.values))
  }
  
  private func unbox(_ expr: Expr) throws -> Expr {
    guard case .box(let cell) = expr else {
      throw RuntimeError.type(expr, expected: [.boxType])
    }
    return cell.value
  }
  
  private func setBox(_ expr: Expr, args: Arguments) throws -> Expr {
    guard case .box(let cell) = expr else {
      throw RuntimeError.type(expr, expected: [.boxType])
    }
    // Set cell value. Guarantee that cells for which `set-box!` is called are managed
    // by a managed object pool.
    let value =  args.values
    (value.isAtom ? cell : self.context.objects.manage(cell)).value = value
    return .void
  }
  
  private func isBox(_ expr: Expr) -> Expr {
    guard case .box(_) = expr else {
      return .false
    }
    return .true
  }
  
  //-------- MARK: - Mutable pairs
  
  private func isMpair(_ expr: Expr) -> Expr {
    guard case .mpair(_) = expr else {
      return .false
    }
    return .true
  }

  private func mcons(_ car: Expr, cdr: Expr) throws -> Expr {
    return .mpair(Tuple(car, cdr))
  }
  
  private func mcar(_ expr: Expr) throws -> Expr {
    guard case .mpair(let tuple) = expr else {
      throw RuntimeError.type(expr, expected: [.mpairType])
    }
    return tuple.fst
  }
  
  private func mcdr(_ expr: Expr) throws -> Expr {
    guard case .mpair(let tuple) = expr else {
      throw RuntimeError.type(expr, expected: [.mpairType])
    }
    return tuple.snd
  }
  
  private func setMcar(_ expr: Expr, value: Expr) throws -> Expr {
    guard case .mpair(let tuple) = expr else {
      throw RuntimeError.type(expr, expected: [.mpairType])
    }
    // Set car of tuple. Guarantee that tuples for which `set-mcar!` is called are managed
    // by a managed object pool.
    (value.isAtom ? tuple : self.context.objects.manage(tuple)).fst = value
    return .void
  }
  
  private func setMcdr(_ expr: Expr, value: Expr) throws -> Expr {
    guard case .mpair(let tuple) = expr else {
      throw RuntimeError.type(expr, expected: [.mpairType])
    }
    // Set cdr of tuple. Guarantee that tuples for which `set-mcdr!` is called are managed
    // by a managed object pool.
    (value.isAtom ? tuple : self.context.objects.manage(tuple)).snd = value
    return .void
  }
  
  private func atomicBox(from expr: Expr) throws -> AtomicBox {
    guard case .object(let obj) = expr, let abox = obj as? AtomicBox else {
      throw RuntimeError.type(expr, expected: [AtomicBox.type])
    }
    return abox
  }
  
  private func isAtomicBox(_ expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is AtomicBox else {
      return .false
    }
    return .true
  }
  
  private func makeAtomicBox(_ args: Arguments) -> Expr {
    return .object(AtomicBox(args.values))
  }
  
  private func atomicBoxRef(_ expr: Expr) throws -> Expr {
    return try self.atomicBox(from: expr).atomicGet
  }
  
  private func atomicBoxSet(_ expr: Expr, _ args: Arguments) throws -> Expr {
    let abox = try self.atomicBox(from: expr)
    // Guarantee that AtomicBox is included in the managed object pool.
    let value =  args.values
    _ = (value.isAtom ? abox : self.context.objects.manage(abox)).atomicSwap(value)
    return .void
  }
  
  private func atomicBoxSwap(_ expr: Expr, _ args: Arguments) throws -> Expr {
    let abox = try self.atomicBox(from: expr)
    // Guarantee that AtomicBox is included in the managed object pool.
    let value =  args.values
    return (value.isAtom ? abox : self.context.objects.manage(abox)).atomicSwap(value)
  }
  
  private func atomicBoxCompareAndSet(_ expr: Expr, _ current: Expr, _ args: Arguments) throws -> Expr {
    let abox = try self.atomicBox(from: expr)
    // Guarantee that AtomicBox is included in the managed object pool.
    let value =  args.values
    return .makeBoolean((value.isAtom ? abox : self.context.objects.manage(abox))
                          .atomicCompareSet(current, value))
  }
  
  private func atomicBoxCompareAndSwap(_ expr: Expr, _ current: Expr, _ args: Arguments) throws -> Expr {
    let abox = try self.atomicBox(from: expr)
    // Guarantee that AtomicBox is included in the managed object pool.
    let value =  args.values
    return (value.isAtom ? abox : self.context.objects.manage(abox)).atomicCompareSwap(current, value)
  }
  
  private func atomicBoxIncMul(_ expr: Expr, _ i0: Expr, _ m: Expr?, _ i1: Expr?) throws -> Expr {
    let abox = try self.atomicBox(from: expr)
    abox.lock.lock()
    defer {
      abox.lock.unlock()
    }
    switch abox.value {
      case .true, .false:
        var res = abox.value.isTrue || i0.isTrue
        if let m {
          res = res && m.isTrue
        }
        if let i1 {
          res = res || i1.isTrue
        }
        abox.value = .makeBoolean(res)
      case .fixnum(let num):
        var res = try num &+ i0.asInt64()
        if let m {
          res = try res &* m.asInt64()
        }
        if let i1 {
          res = try res &+ i1.asInt64()
        }
        abox.value = .fixnum(res)
      case .flonum(let num):
        var res = try num + i0.asDouble(coerce: true)
        if let m {
          res = try res * m.asDouble(coerce: true)
        }
        if let i1 {
          res = try res + i1.asDouble(coerce: true)
        }
        abox.value = .flonum(res)
      default:
        throw RuntimeError.eval(.illegalArithForAtomicBox, expr)
    }
    return abox.value
  }
  
  private func atomicBoxRefLock(_ expr: Expr) throws -> Expr {
    let abox = try self.atomicBox(from: expr)
    abox.lock.lock()
    return abox.value
  }
  
  private func atomicBoxSetUnlock(_ expr: Expr, _ args: Arguments) throws -> Expr {
    let abox = try self.atomicBox(from: expr)
    defer {
      abox.lock.unlock()
    }
    let value = args.values
    (value.isAtom ? abox : self.context.objects.manage(abox)).value = value
    return value
  }
}

public final class AtomicBox: Cell, CustomExpr {
  
  /// Type representing atomic boxes
  public static let type = Type.objectType(Symbol(uninterned: "atomic-box"))
  
  /// The lock used internally
  fileprivate let lock = EmbeddedUnfairLock()
  
  deinit {
    self.lock.release()
  }
  
  public var atomicGet: Expr {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    return self.value
  }
  
  public func atomicSwap(_ newValue: Expr) -> Expr {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    let oldValue = self.value
    self.value = newValue
    return oldValue
  }
  
  public func atomicCompareSwap(_ currentValue: Expr, _ newValue: Expr) -> Expr {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    let oldValue = self.value
    if self.value == currentValue {
      self.value = newValue
    }
    return oldValue
  }
  
  public func atomicCompareSet(_ currentValue: Expr, _ newValue: Expr) -> Bool {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    if self.value == currentValue {
      self.value = newValue
      return true
    } else {
      return false
    }
  }
  
  public var type: Type {
    return Self.type
  }
  
  public var string: String {
    return "#<\(self.tagString)>"
  }
  
  public var tagString: String {
    return "\(Self.type) \(self.value)"
  }
  
  public var isAtom: Bool {
    return false
  }
  
  public var hash: Int {
    return self.hashValue
  }
  
  public func equals(to expr: Expr) -> Bool {
    guard case .object(let obj) = expr, let other = obj as? AtomicBox else {
      return false
    }
    return self === other
  }
  
  public func mark(in gc: GarbageCollector) {
    gc.markLater(.box(self))
  }
  
  public func unpack(in context: Context) -> Exprs {
    return [.makeString(self.identityString), self.value]
  }
}
