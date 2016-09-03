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
open class SyntaxRules {
  fileprivate let context: Context
  fileprivate let ellipsis: Expr
  fileprivate let reserved: Set<Symbol>
  fileprivate let literals: Set<Symbol>
  fileprivate let patterns: Exprs
  fileprivate let templates: Exprs
  fileprivate let lexicalEnv: Env
  
  public init(_ context: Context, literals: Set<Symbol>, patterns: Exprs, templates: Exprs, in env: Env) {
    self.context = context
    self.ellipsis = .symbol(context.symbols.ELLIPSIS)
    self.reserved = [context.symbols.WILDCARD, context.symbols.ELLIPSIS]
    self.literals = literals
    self.patterns = patterns
    self.templates = templates
    self.lexicalEnv = env
  }
  
  open func expand(_ input: Expr) throws -> Expr {
    log("---- EXPAND: \(input)") //DEBUG
    for index in self.patterns.indices {
      if let matches = self.match(self.patterns[index], with: input) {
        return try self.instantiate(self.templates[index], with: matches, at: 0)
      }
    }
    return input // TODO: should we throw an error instead?
  }
  
  fileprivate func match(_ pattern: Expr, with input: Expr) -> Matches? {
    // print("MATCH: \(pattern) WITH: \(input)") //DEBUG
    let matches = Matches(self.variablesIn(pattern))
    return self.match(pattern, with: input, in: matches, at: 0) ? matches : nil
  }
  
  fileprivate func match(_ pattern: Expr,
                     with input: Expr,
                     in matches: Matches,
                     at depth: Int) -> Bool {
    // print("  MATCH: \(pattern) WITH: \(input) MATCHING: \(matches)") //DEBUG
    switch pattern {
      case .symbol(let sym):
        if self.literals.contains(sym) {
          return pattern == input
        } else {
          matches.put(sym, input)
          return true
        }
      case .pair(_, _):
        guard case .pair(_, _) = input else {
          return false
        }
        var pat = pattern
        var inp = input
        while case .pair(let token, let rest) = pat {
          if token != self.ellipsis {
            if case .pair(self.ellipsis, _) = rest {
              matches.register(self.variablesIn(token), at: depth + 1)
              while case .pair(let car, let cdr) = inp
                    , self.match(token, with: car, in: matches, at: depth + 1) {
                inp = cdr
              }
            } else if case .pair(let car, let cdr) = inp
                      , self.match(token, with: car, in: matches, at: depth) {
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
          if token != self.ellipsis {
            if patIndex < patVector.exprs.count - 1 &&
               patVector.exprs[patIndex + 1] == self.ellipsis {
              matches.register(self.variablesIn(token), at: depth + 1)
              while inpIndex < inpVector.exprs.count &&
                    self.match(token, with: inpVector.exprs[inpIndex], in: matches, at: depth + 1) {
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
  
  fileprivate func instantiate(_ template: Expr, with matches: Matches, at depth: Int) throws -> Expr {
    // print("INSTANTIATE: \(template) USING: \(matches) DEPTH: \(depth)")
    switch template {
      case .symbol(let sym):
        return matches.get(sym, in: self.lexicalEnv)
      case .pair(self.ellipsis, let rest):
        guard case .pair(let car, _) = rest else {
          throw EvalError.typeError(rest, [.pairType])
        }
        return self.instantiateRaw(car, with: matches)
      case .pair(_, _):
        var res = Exprs()
        var templ = template
        var repeater: Expr? = nil
        while case .pair(let token, let rest) = templ {
          if case .pair(self.ellipsis, _) = rest {
            guard token != self.ellipsis else {
              throw EvalError.macroMismatchedRepetitionPatterns(self.context.symbols.ELLIPSIS)
            }
            repeater = token
          } else if token == self.ellipsis {
            guard let repeaterTemplate = repeater else {
              throw EvalError.macroMismatchedRepetitionPatterns(self.context.symbols.ELLIPSIS)
            }
            try matches.instantiate(&res, self, repeaterTemplate, at: depth + 1)
            repeater = nil
          } else {
            res.append(try self.instantiate(token, with: matches, at: depth))
          }
          templ = rest
        }
        return Expr.List(res, append: try self.instantiate(templ, with: matches, at: depth))
      case .vector(let vector):
        var res = Exprs()
        for i in vector.exprs.indices {
          if (i < vector.exprs.count - 1) && vector.exprs[i + 1] == self.ellipsis {
            guard vector.exprs[i] != self.ellipsis else {
              throw EvalError.macroMismatchedRepetitionPatterns(self.context.symbols.ELLIPSIS)
            }
          } else if vector.exprs[i] == self.ellipsis {
            guard i > 0 else {
              throw EvalError.macroMismatchedRepetitionPatterns(self.context.symbols.ELLIPSIS)
            }
            try matches.instantiate(&res, self, vector.exprs[i - 1], at: depth + 1)
          } else {
            res.append(try self.instantiate(vector.exprs[i], with: matches, at: depth).datum)
          }
        }
        return .vector(self.context.objects.manage(Collection(kind: .immutableVector, exprs: res)))
      default:
        return template
    }
  }
  
  fileprivate func instantiateRaw(_ template: Expr, with matches: Matches) -> Expr {
    switch template {
      case .symbol(let sym):
        return matches.get(sym, in: self.lexicalEnv)
      case .pair(_, _):
        var res = Exprs()
        var templ = template
        while case .pair(let token, let rest) = templ {
          res.append(self.instantiateRaw(token, with: matches))
          templ = rest
        }
        return Expr.List(res, append: self.instantiateRaw(templ, with: matches))
      case .vector(let vector):
        var res = Exprs()
        for i in vector.exprs.indices {
          res.append(self.instantiateRaw(vector.exprs[i], with: matches))
        }
        return .vector(self.context.objects.manage(Collection(kind: .immutableVector, exprs: res)))
      default:
        return template
    }
  }
  
  fileprivate func variablesIn(_ pattern: Expr) -> Set<Symbol> {
    var vars = Set<Symbol>()
    func traverse(_ pattern: Expr) {
      switch pattern {
        case .symbol(let sym):
          if !self.literals.contains(sym) && !self.reserved.contains(sym) {
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
  fileprivate var generatedSym: [Symbol : Symbol]
  fileprivate var matchedVal: [Symbol : MatchTree]
  
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
  
  fileprivate func instantiate(_ exprs: inout Exprs,
                           _ rules: SyntaxRules,
                           _ template: Expr,
                           at depth: Int) throws {
    let syms = rules.variablesIn(template)
    for _ in 0..<(try self.numChildrenOf(syms, at: depth)) {
      exprs.append(try rules.instantiate(template, with: self, at: depth))
      for sym in syms {
        self.matchedVal[sym]?.rotateAt(depth)
      }
    }
  }
  
  fileprivate func numChildrenOf(_ syms: Set<Symbol>, at depth: Int) throws -> Int {
    var res = 0
    for sym in syms {
      if let tree = self.matchedVal[sym] {
        let s = tree.numChildren(depth)
        if s > 0 {
          guard res == 0 || res == s else {
            throw EvalError.macroMismatchedRepetitionPatterns(sym)
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
  var root: Node = Node()
  var depth: Int = 0
  var complete: Bool = false
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
    for _ in 0..<self.depth {
      res = res.lastChild
    }
    return res
  }
  
  fileprivate func currentNode(_ depth: Int) -> Node? {
    var res = self.root
    for i in 0..<self.depth {
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
