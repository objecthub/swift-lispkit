//
//  VirtualMachineState.swift
//  LispKit
//
//  Created by Matthias Zenger on 02/07/2016.
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
/// `VirtualMachineState` encapsulates per-continuation state of a virtual machine, such as:
///    - the stack
///    - the stack pointer
///    - the registers
///    - the winders stack
///
public final class VirtualMachineState: TrackedObject, CustomStringConvertible {

  /// The stack
  internal let stack: Exprs

  /// The stack pointer
  internal let sp: Int

  /// The set of registers
  internal let registers: VirtualMachine.Registers

  /// The stack of winders
  internal let winders: VirtualMachine.Winder?

  /// Creates a new virtual machine state
  internal init(stack: Exprs,
                sp: Int,
                spDelta: Int,
                ipDelta: Int,
                registers: VirtualMachine.Registers,
                winders: VirtualMachine.Winder?) {
    var adjustedStack = stack
    self.sp = sp + spDelta
    for i in self.sp..<sp {
      adjustedStack[i] = .undef
    }
    self.stack = adjustedStack
    var adjustedRegisters = registers
    adjustedRegisters.ip += ipDelta
    self.registers = adjustedRegisters
    self.winders = winders
  }

  public var description: String {
    var builder = StringBuilder(prefix: "vmstate {")
    if self.sp == 0 {
      builder.append("stack = []")
    } else if self.sp == 1 {
      builder.append("stack = [\(self.stack[0])]")
    } else if self.sp == 2 {
      builder.append("stack = [\(self.stack[0]), \(self.stack[1])]")
    } else {
      builder.append("stack = [..., \(self.stack[self.sp - 3]), \(self.stack[self.sp - 2]), ")
      builder.append("\(self.stack[self.sp - 1])]")
    }
    builder.append(", sp = \(self.sp)")
    builder.append(", ip = \(self.registers.ip)")
    builder.append(", fp = \(self.registers.fp)")
    builder.append(", initialFp = \(self.registers.initialFp)")
    builder.append(", numWinders = \(self.winders?.count ?? 0)")
    builder.append("}")
    return builder.description
  }

  public override func mark(_ tag: UInt8) {
    super.mark(tag)
    for i in 0..<self.sp {
      self.stack[i].mark(tag)
    }
    for expr in self.registers.captured {
      expr.mark(tag)
    }
    self.registers.code.mark(tag)
  }
}
