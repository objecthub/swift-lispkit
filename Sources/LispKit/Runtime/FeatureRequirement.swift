//
//  FeatureRequirement.swift
//  LispKit
//  
//  Created by Matthias Zenger on 15/10/2017.
//  Copyright © 2017 ObjectHub. All rights reserved.
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

public indirect enum FeatureRequirement: CustomStringConvertible {
  case feature(String)
  case library(Expr)
  case and(FeatureRequirement, FeatureRequirement)
  case or(FeatureRequirement, FeatureRequirement)
  case not(FeatureRequirement)
  
  /// Constructs a feature requirement from an expression for a given context.
  public init?(_ featureRequirement: Expr, in context: Context) {
    switch featureRequirement {
      case .pair(.symbol(context.symbols.not), .pair(let req, .null)):
        guard let first = FeatureRequirement(req, in: context) else {
          return nil
        }
        self = .not(first)
      case .pair(.symbol(context.symbols.and), .pair(let req, let others)):
        guard let first = FeatureRequirement(req, in: context) else {
          return nil
        }
        var res = first
        var reqs = others
        while case .pair(let r, let next) = reqs {
          guard let other = FeatureRequirement(r, in: context) else {
            return nil
          }
          res = .and(res, other)
          reqs = next
        }
        self = res
      case .pair(.symbol(context.symbols.or), .pair(let req, let others)):
        guard let first = FeatureRequirement(req, in: context) else {
          return nil
        }
        var res = first
        var reqs = others
        while case .pair(let r, let next) = reqs {
          guard let other = FeatureRequirement(r, in: context) else {
            return nil
          }
          res = .or(res, other)
          reqs = next
        }
        self = res
      case .pair(.symbol(context.symbols.library), .pair(let name, .null)):
        var libraryName = name
        while case .pair(let component, let next) = libraryName {
          switch component {
          case .symbol(_), .fixnum(_), .flonum(_):
            break
          default:
            return nil
          }
          libraryName = next
        }
        guard libraryName.isNull else {
          return nil
        }
        self = .library(name)
      case .symbol(let sym):
        self = .feature(sym.rawIdentifier)
      default:
        return nil
    }
  }
  
  /// `expand` returns, for this import set, a reference to the library from which definitions
  /// are imported. In addition, a mapping is returned that maps renamed definitions to the
  /// definitions as exported by the library.
  public func valid(in context: Context) -> Bool {
    switch self {
      case .feature(let name):
        return context.features.contains(name)
      case .library(let expr):
        return context.libraries.lookup(expr) != nil
      case .and(let left, let right):
        return left.valid(in: context) && right.valid(in: context)
      case .or(let left, let right):
        return left.valid(in: context) || right.valid(in: context)
      case .not(let req):
        return !req.valid(in: context)
    }
  }
  
  public var description: String {
    switch self {
      case .feature(let name):
        return name
      case .library(let expr):
        return "(library \(expr.description))"
      case .and(let left, let right):
        return "(and \(left.description) \(right.description))"
      case .or(let left, let right):
        return "(or \(left.description) \(right.description))"
      case .not(let req):
        return "(not \(req.description))"
    }
  }
}
