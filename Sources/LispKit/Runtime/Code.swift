//
//  Code.swift
//  LispKit
//
//  Created by Matthias Zenger on 07/02/2016.
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
/// Struct `Code` represents an executable that bundles a list of instructions, a constant
/// pool and code referenced from the instructions. `Code` objects are used to construct
/// closures, which combine a `Code` object with a list of captured variables.
///
public struct Code: CustomStringConvertible {
  public let instructions: Instructions
  public let constants: Exprs
  public let fragments: Fragments
  
  public init(_ instructions: Instructions, _ constants: Exprs, _ fragments: Fragments) {
    self.instructions = instructions
    self.constants = constants
    self.fragments = fragments
  }
  
  public var arity: (Int, Int?) {
    var i = 0
    var min = Int.max
    var max: Int? = 0
    loop: while i < self.instructions.count {
      switch self.instructions[i] {
        case .assertArgCount(let n):
          return (n, n)
        case .assertMinArgCount(let n):
          return (n, nil)
        case .noMatchingArgCount:
          return (0, nil)
        case .branchIfArgMismatch(let n, let offset):
          min = n
          max = n
          i += offset
          break loop
        case .branchIfMinArgMismatch(let n, let offset):
          min = n
          max = nil
          i += offset
          break loop
        default:
          i += 1
      }
    }
    guard i < self.instructions.count else {
      return (0, nil)
    }
    while i < self.instructions.count {
      switch self.instructions[i] {
        case .branchIfArgMismatch(let n, let offset):
          if n < min {
            min = n
          }
          if let m = max, n > m {
            max = n
          }
          i += offset
        case .branchIfMinArgMismatch(let n, let offset):
          if n < min {
            min = n
          }
          max = nil
          i += offset
        default:
          return (min, max)
      }
    }
    return (min, max)
  }
  
  /// Returns `true` if the given arity is accepted by the procedure.
  public func arityAccepted(_ m: Int) -> Bool {
    var i = 0
    loop: while i < self.instructions.count {
      switch self.instructions[i] {
        case .assertArgCount(let n):
          return n == m
        case .assertMinArgCount(let n):
          return m >= n
        case .noMatchingArgCount:
          return true
        case .branchIfArgMismatch(let n, let offset):
          if n == m {
            return true
          }
          i += offset
          break loop
        case .branchIfMinArgMismatch(let n, let offset):
          if m >= n {
            return true
          }
          i += offset
          break loop
        default:
          i += 1
      }
    }
    guard i < self.instructions.count else {
      return true
    }
    while i < self.instructions.count {
      switch self.instructions[i] {
        case .branchIfArgMismatch(let n, let offset):
          if n == m {
            return true
          }
          i += offset
        case .branchIfMinArgMismatch(let n, let offset):
          if m >= n {
            return true
          }
          i += offset
        default:
          return false
      }
    }
    return false
  }
  
  public var description: String {
    var builder = StringBuilder(prefix: "CONSTANTS:")
    var n = 0
    for constant in self.constants {
      builder.appendNewline()
      builder.append(n, width: 5, alignRight: true)
      builder.append(": ")
      builder.append(constant.description)
      n += 1
    }
    builder.appendNewline()
    builder.append("INSTRUCTIONS:")
    n = 0
    for instr in self.instructions {
      builder.appendNewline()
      builder.append(n, width: 5, alignRight: true)
      builder.append(": ")
      n += 1
      if let comment = instr.comment(for: self, at: n) {
        builder.append(instr.description, width: 32)
        builder.append(";; ")
        builder.append(comment)
      } else {
        builder.append(instr.description)
      }
    }
    if self.fragments.count > 0 {
      n = 0
      for fragment in self.fragments {
        builder.appendNewline()
        builder.append("------ Code \(n) ------")
        builder.appendNewline()
        builder.append(fragment.description)
        n += 1
      }
    }
    builder.appendNewline()
    return builder.description
  }
}

/// A set of code fragments, represented as an array.
public typealias Fragments = ContiguousArray<Code>
