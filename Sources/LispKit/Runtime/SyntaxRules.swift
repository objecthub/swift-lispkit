//
//  SyntaxRules.swift
//  LispKit
//
//  Created by Matthias Zenger on 28/11/2015.
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
/// A `Rules` object defines syntax rules for a symbol. Class `Rules` is used to implement
/// hygienic Scheme macros based on the "syntax-rules" standard.
///
public final class SyntaxRules {
  private unowned let context: Context
  internal let name: Symbol?
  private let ellipsis: Symbol
  private let reserved: Set<Symbol>
  private let literals: Set<Symbol>
  private let patterns: Exprs
  private let templates: Exprs
  private let lexicalEnv: Env
  
  public init(context: Context,
              name: Symbol?,
              literals: Set<Symbol>,
              ellipsis: Symbol? = nil,
              patterns: Exprs,
              templates: Exprs,
              in env: Env) {
    self.context = context
    self.name = name
    self.ellipsis = ellipsis ?? context.symbols.ellipsis
    self.reserved = [context.symbols.wildcard, self.ellipsis]
    self.literals = literals
    self.patterns = patterns
    self.templates = templates
    self.lexicalEnv = env
  }
  
  internal func expand(_ input: Expr) throws -> Expr {
    // Swift.print("---- EXPAND: \(Expr.pair(.symbol(self.name ?? self.context.symbols.wildcard), input))") //DEBUG
    for index in self.patterns.indices {
      if let matches = self.match(self.patterns[index], with: input) {
        let res = try self.instantiate(template: self.templates[index], with: matches, at: 0)
        // Swift.print("---- RES: \(res)")
        return res
      }
    }
    throw RuntimeError.eval(.noExpansion, .pair(.symbol(self.name ?? self.context.symbols.wildcard),
                                                input))
  }
  
  private func match(_ pattern: Expr, with input: Expr) -> Matches? {
    // print("MATCH: \(pattern) WITH: \(input)") //DEBUG
    let matches = Matches(self.variables(in: pattern))
    return self.match(pattern, with: input, in: matches, at: 0) ? matches : nil
  }
  
  private func match(_ pattern: Expr,
                     with input: Expr,
                     in matches: Matches,
                     at depth: Int) -> Bool {
    // print(String(repeating: " ", count: depth * 2) +
    //       "MATCH: \(pattern) WITH: \(input) MATCHING: \(matches)") //DEBUG
    switch pattern {
      case .symbol(let sym):
        if self.literals.contains(sym.interned) {
          if case .symbol(let inputSym) = input {
            return sym.interned == inputSym.interned
          } else {
            return false
          }
        } else {
          matches.put(sym, input)
          return true
        }
      case .pair(_, _):
        switch input {
          case .null, .pair(_, _):
            break
          default:
            return false
        }
        var pat = pattern
        var inp = input
        while case .pair(let token, let rest) = pat {
          if case .symbol(let s) = token, s.interned == self.ellipsis {
            // ignore ellipsis
          } else {
            if case .pair(.symbol(let s), let tail) = rest, s.interned == self.ellipsis {
              // Register variable
              matches.register(self.variables(in: token), at: depth + 1)
              // Determine maximum number of matches
              var maxMatchCount = inp.length - tail.length
              // Match input
              while case .pair(let car, let cdr) = inp,
                    maxMatchCount > 0,
                    self.match(token, with: car, in: matches, at: depth + 1) {
                maxMatchCount -= 1
                inp = cdr
              }
            } else if case .pair(let car, let cdr) = inp,
                   self.match(token, with: car, in: matches, at: depth) {
              inp = cdr
            } else {
              return false
            }
          }
          pat = rest
        }
        return self.match(pat, with: inp, in: matches, at: depth)
      case .vector(let patVector):
        guard case .vector(let inpVector) = input else {
          return false
        }
        var inpIndex = 0
        for patIndex in patVector.exprs.indices {
          let token = patVector.exprs[patIndex]
          if case .symbol(let s) = token, s.interned == self.ellipsis {
            // ignore ellipsis
          } else {
            if patIndex < patVector.exprs.count - 1,
               case .symbol(let s) = patVector.exprs[patIndex + 1],
               s.interned == self.ellipsis {
              // Register variable
              matches.register(self.variables(in: token), at: depth + 1)
              // Determine maximum number of matches
              var maxMatchCount = (inpVector.exprs.count - inpIndex) -
                                  (patVector.exprs.count - patIndex - 2)
              // Match input
              while inpIndex < inpVector.exprs.count,
                    maxMatchCount > 0,
                    self.match(token, with: inpVector.exprs[inpIndex], in: matches, at: depth + 1) {
                maxMatchCount -= 1
                inpIndex += 1
              }
            } else if inpIndex < inpVector.exprs.count &&
                      self.match(token, with: inpVector.exprs[inpIndex], in: matches, at: depth) {
              inpIndex += 1
            } else {
              return false
            }
          }
        }
        return inpIndex == inpVector.exprs.count
      default:
        return pattern == input
    }
  }
  
  fileprivate func instantiate(template: Expr,
                               with matches: Matches,
                               at depth: Int) throws -> Expr {
    // print(String(repeating: " ", count: depth * 2) +
    //       "INSTANTIATE: \(template) USING: \(matches) DEPTH: \(depth)")//DEBUG
    switch template {
      case .symbol(let sym):
        return matches.get(sym, in: self.lexicalEnv)
      case .pair(.symbol(let s), let rest) where s.interned == self.ellipsis:
        guard case .pair(let car, _) = rest else {
          throw RuntimeError.type(rest, expected: [.pairType])
        }
        return self.instantiateRaw(template: car, with: matches)
      case .pair(_, _):
        var res = Exprs()
        var templ = template
        var repeater: Expr? = nil
        while case .pair(let token, let rest) = templ {
          if case .pair(.symbol(let s), _) = rest, s.interned == self.ellipsis {
            if case .symbol(let s) = token, s.interned == self.ellipsis {
              throw RuntimeError.eval(.macroMismatchedRepetitionPatterns, .symbol(self.ellipsis))
            }
            repeater = token
          } else if case .symbol(let s) = token, s.interned == self.ellipsis {
            guard let repeaterTemplate = repeater else {
              throw RuntimeError.eval(.macroMismatchedRepetitionPatterns, .symbol(self.ellipsis))
            }
            try matches.instantiate(template: repeaterTemplate,
                                    with: self,
                                    at: depth + 1,
                                    appendingTo: &res)
            repeater = nil
          } else {
            res.append(try self.instantiate(template: token, with: matches, at: depth))
          }
          templ = rest
        }
        return Expr.makeList(res, append: try self.instantiate(template: templ,
                                                               with: matches,
                                                               at: depth))
      case .vector(let vector):
        var res = Exprs()
        for i in vector.exprs.indices {
          if (i < vector.exprs.count - 1),
             case .symbol(let s) = vector.exprs[i + 1],
             s.interned == self.ellipsis {
            if case .symbol(let s) = vector.exprs[i], s.interned == self.ellipsis {
              throw RuntimeError.eval(.macroMismatchedRepetitionPatterns, .symbol(self.ellipsis))
            }
          } else if case .symbol(let s) = vector.exprs[i], s.interned == self.ellipsis {
            guard i > 0 else {
              throw RuntimeError.eval(.macroMismatchedRepetitionPatterns, .symbol(self.ellipsis))
            }
            try matches.instantiate(template: vector.exprs[i - 1],
                                    with: self,
                                    at: depth + 1,
                                    appendingTo: &res)
          } else {
            res.append(
              try self.instantiate(template: vector.exprs[i], with: matches, at: depth).datum)
          }
        }
        return Expr.vector(
          self.context.objects.manage(Collection(kind: .immutableVector, exprs: res)))
      default:
        return template
    }
  }
  
  fileprivate func instantiateRaw(template: Expr, with matches: Matches) -> Expr {
    switch template {
      case .symbol(let sym):
        return matches.get(sym, in: self.lexicalEnv)
      case .pair(_, _):
        var res = Exprs()
        var templ = template
        while case .pair(let token, let rest) = templ {
          res.append(self.instantiateRaw(template: token, with: matches))
          templ = rest
        }
        return Expr.makeList(res, append: self.instantiateRaw(template: templ, with: matches))
      case .vector(let vector):
        var res = Exprs()
        for i in vector.exprs.indices {
          res.append(self.instantiateRaw(template: vector.exprs[i], with: matches))
        }
        return .vector(self.context.objects.manage(Collection(kind: .immutableVector, exprs: res)))
      default:
        return template
    }
  }
  
  fileprivate func variables(in pattern: Expr) -> Set<Symbol> {
    var vars = Set<Symbol>()
    func traverse(_ pattern: Expr) {
      switch pattern {
        case .symbol(let sym):
          if !self.literals.contains(sym.interned) && !self.reserved.contains(sym.interned) {
            vars.insert(sym)
          }
        case .pair(let car, let cdr):
          traverse(car)
          traverse(cdr)
        case .vector(let vector):
          for expr in vector.exprs {
            traverse(expr)
          }
        default:
          break
      }
    }
    traverse(pattern)
    return vars
  }
}

///
/// Class `Matches` represents the result of matching an input expression with a template.
/// Objects of class `Matches` contain a mapping from symbols to values in the input expression
/// as well as symbols to generated symbols. Generated symbols are needed to guarantee the
/// hygiene property of Scheme's macros.
///
private final class Matches: CustomStringConvertible {
  private var generatedSym: [Symbol : Symbol]
  private var matchedVal: [Symbol : MatchTree]
  
  fileprivate init(_ syms: Set<Symbol>) {
    self.generatedSym = [Symbol : Symbol]()
    self.matchedVal = [Symbol : MatchTree]()
    for sym in syms {
      self.matchedVal[sym] = MatchTree()
    }
  }
  
  fileprivate func get(_ sym: Symbol, in lexicalEnv: Env) -> Expr {
    guard let value = self.matchedVal[sym]?.value else {
      if let gensym = self.generatedSym[sym] {
        return .symbol(gensym)
      } else {
        let gensym = Symbol(sym, lexicalEnv)
        self.generatedSym[sym] = gensym
        return .symbol(gensym)
      }
    }
    return value
  }
  
  fileprivate func put(_ sym: Symbol, _ expr: Expr) {
    self.matchedVal[sym]?.enter(expr)
  }
  
  fileprivate func register(_ syms: Set<Symbol>, at depth: Int) {
    for sym in syms {
      self.matchedVal[sym]?.descendAt(depth)
    }
  }
  
  fileprivate func instantiate(template: Expr,
                               with rules: SyntaxRules,
                               at depth: Int,
                               appendingTo exprs: inout Exprs) throws {
    let syms = rules.variables(in: template)
    for _ in 0..<(try self.numChildren(of: syms, at: depth)) {
      exprs.append(try rules.instantiate(template: template, with: self, at: depth))
      for sym in syms {
        self.matchedVal[sym]?.rotateAt(depth)
      }
    }
  }
  
  private func numChildren(of syms: Set<Symbol>, at depth: Int) throws -> Int {
    var res = 0
    for sym in syms {
      if let tree = self.matchedVal[sym] {
        let s = tree.numChildren(depth)
        if s > 0 {
          guard res == 0 || res == s else {
            throw RuntimeError.eval(.macroMismatchedRepetitionPatterns, .symbol(sym))
          }
          res = s
        }
      }
    }
    return res
  }
  
  fileprivate var description: String {
    var res = "{", sep = ""
    for (sym, tree) in self.matchedVal {
      res += sep + sym.description + " => " + tree.description
      sep = ", "
    }
    return res + "}"
  }
}

///
/// A match tree is a data structure that is used to construct the value matching a particular
/// pattern variable
///
private final class MatchTree: CustomStringConvertible {
  private var root: Node = Node()
  private var depth: Int = 0
  private var complete: Bool = false
  
  lazy var pos: [Int] = {
    [unowned self] in
      self.complete = true
      return [Int](repeating: 0, count: self.depth + 1)
  }()
  
  fileprivate enum Node: CustomStringConvertible {
    case leaf(Expr)
    case parent(MutableBox<[Node]>)
    
    fileprivate init() {
      self = .parent(MutableBox([Node]()))
    }
    
    fileprivate func appendChild(_ node: Node) {
      guard case .parent(let children) = self else {
        preconditionFailure("cannot append child to a leaf")
      }
      children.value.append(node)
    }
    
    fileprivate var lastChild: Node {
      guard case .parent(let children) = self else {
        preconditionFailure("a leaf does not have children")
      }
      return children.value.last!
    }
    
    fileprivate var numChildren: Int {
      switch self {
        case .leaf(_):
          return 0
        case .parent(let children):
          return children.value.count
      }
    }
    
    fileprivate var description: String {
      switch self {
        case .leaf(let expr):
          return expr.description
        case .parent(let children):
          var res = "["
          var sep = ""
          for child in children.value {
            res += sep
            res += child.description
            sep = ", "
          }
          return res + "]"
      }
    }
  }
  
  fileprivate var value: Expr? {
    guard case .some(.parent(let children)) = self.currentNode(self.depth) else {
      return nil
    }
    guard case .leaf(let expr) = children.value[self.pos[self.depth]] else {
      return nil
    }
    return expr
  }
  
  fileprivate func enter(_ expr: Expr) {
    self.tailNode(self.depth).appendChild(.leaf(expr))
  }

  fileprivate func tailNode(_ depth: Int) -> Node {
    var res = self.root
    for _ in 0..<depth {
      res = res.lastChild
    }
    return res
  }
  
  private func currentNode(_ depth: Int) -> Node? {
    var res = self.root
    for i in 0..<depth {
      guard case .parent(let children) = res else {
        return nil
      }
      res = children.value[self.pos[i]]
    }
    return res
  }
  
  fileprivate func numChildren(_ depth: Int) -> Int {
    guard depth <= self.depth else {
      return 0
    }
    return self.currentNode(depth)?.numChildren ?? 0
  }
  
  fileprivate func descendAt(_ depth: Int) {
    self.tailNode(depth - 1).appendChild(Node())
    self.depth = max(self.depth, depth)
  }
  
  fileprivate func rotateAt(_ depth: Int) {
    if depth <= self.depth {
      self.pos[depth] += 1
      if self.pos[depth] >= self.currentNode(depth)!.numChildren {
        self.pos[depth] = 0
      }
    }
  }
  
  fileprivate var description: String {
    var res = "(\(self.depth); \(self.root); "
    if self.complete {
      for idx in self.pos {
        res += " \(idx)"
      }
    }
    return res + ")"
  }
}

