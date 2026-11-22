//
//  NaturalLanguageLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 22/11/2026.
//  Copyright © 2026 ObjectHub. All rights reserved.
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
import NaturalLanguage
import CoreML
#if canImport(CreateML)
import CreateML
#endif

///
/// Library `(lispkit natural-language)` provides access to Apple's NaturalLanguage
/// framework: language identification, tokenization, tagging (lexical classes, named
/// entities, lemmas, sentiment), word and sentence embeddings, contextual embeddings,
/// gazetteers, and custom text models, which can be trained via Create ML.
///
public final class NaturalLanguageLibrary: NativeLibrary {

  /// Tag schemes and the symbols representing them.
  private static let tagSchemes: [(String, NLTagScheme)] = [
    ("token-type", .tokenType),
    ("lexical-class", .lexicalClass),
    ("name-type", .nameType),
    ("name-type-or-lexical-class", .nameTypeOrLexicalClass),
    ("lemma", .lemma),
    ("language", .language),
    ("script", .script),
    ("sentiment-score", .sentimentScore)
  ]

  /// Tags defined by the NaturalLanguage framework and the symbols representing them.
  private static let tags: [(String, NLTag)] = [
    ("word", .word),
    ("punctuation", .punctuation),
    ("whitespace", .whitespace),
    ("other", .other),
    ("noun", .noun),
    ("verb", .verb),
    ("adjective", .adjective),
    ("adverb", .adverb),
    ("pronoun", .pronoun),
    ("determiner", .determiner),
    ("particle", .particle),
    ("preposition", .preposition),
    ("number", .number),
    ("conjunction", .conjunction),
    ("interjection", .interjection),
    ("classifier", .classifier),
    ("idiom", .idiom),
    ("other-word", .otherWord),
    ("sentence-terminator", .sentenceTerminator),
    ("open-quote", .openQuote),
    ("close-quote", .closeQuote),
    ("open-parenthesis", .openParenthesis),
    ("close-parenthesis", .closeParenthesis),
    ("word-joiner", .wordJoiner),
    ("dash", .dash),
    ("paragraph-break", .paragraphBreak),
    ("personal-name", .personalName),
    ("place-name", .placeName),
    ("organization-name", .organizationName)
  ]

  /// Tagger options and the symbols representing them.
  private static let taggerOptions: [(String, NLTagger.Options)] = [
    ("omit-words", .omitWords),
    ("omit-punctuation", .omitPunctuation),
    ("omit-whitespace", .omitWhitespace),
    ("omit-other", .omitOther),
    ("join-names", .joinNames),
    ("join-contractions", .joinContractions)
  ]

  /// Token units and the symbols representing them.
  private static let tokenUnits: [(String, NLTokenUnit)] = [
    ("word", .word),
    ("sentence", .sentence),
    ("paragraph", .paragraph),
    ("document", .document)
  ]

  /// Lookup tables
  private let schemeForName: [String : NLTagScheme]
  private let nameForScheme: [String : String]
  private let tagForName: [String : NLTag]
  private let nameForTag: [String : String]
  private let optionForName: [String : NLTagger.Options]
  private let unitForName: [String : NLTokenUnit]

  /// Initialize symbols.
  public required init(in context: Context) throws {
    self.schemeForName = Dictionary(uniqueKeysWithValues: Self.tagSchemes)
    self.nameForScheme = Dictionary(uniqueKeysWithValues: Self.tagSchemes.map { ($1.rawValue, $0) })
    self.tagForName = Dictionary(uniqueKeysWithValues: Self.tags)
    self.nameForTag = Dictionary(uniqueKeysWithValues: Self.tags.map { ($1.rawValue, $0) })
    self.optionForName = Dictionary(uniqueKeysWithValues: Self.taggerOptions)
    self.unitForName = Dictionary(uniqueKeysWithValues: Self.tokenUnits)
    try super.init(in: context)
  }

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "natural-language"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
  }

  /// Declarations of the library.
  public override func declarations() {
    // Languages
    self.define(Procedure("dominant-language", self.dominantLanguage))
    self.define(Procedure("language-hypotheses", self.languageHypotheses))
    // Tokenization
    self.define(Procedure("text-tokens", self.textTokens))
    self.define(Procedure("text-token-ranges", self.textTokenRanges))
    self.define(Procedure("text-token-range", self.textTokenRange))
    // Tagging
    self.define(Procedure("tag-scheme?", self.isTagScheme))
    self.define(Procedure("available-tag-schemes", self.availableTagSchemes))
    self.define(Procedure("request-tag-assets", self.requestTagAssets))
    self.define(Procedure("make-tagger", self.makeTagger))
    self.define(Procedure("tagger?", self.isTagger))
    self.define(Procedure("tagger-scheme", self.taggerScheme))
    self.define(Procedure("text-tags", self.textTags))
    self.define(Procedure("text-tag-ranges", self.textTagRanges))
    self.define(Procedure("text-tag", self.textTag))
    self.define(Procedure("text-tag-hypotheses", self.textTagHypotheses))
    self.define(Procedure("named-entities", self.namedEntities))
    self.define(Procedure("text-sentiment", self.textSentiment))
    // Embeddings
    self.define(Procedure("embedding?", self.isEmbedding))
    self.define(Procedure("word-embedding", self.wordEmbedding))
    self.define(Procedure("sentence-embedding", self.sentenceEmbedding))
    self.define(Procedure("word-embedding-revisions", self.wordEmbeddingRevisions))
    self.define(Procedure("sentence-embedding-revisions", self.sentenceEmbeddingRevisions))
    self.define(Procedure("load-embedding", self.loadEmbedding))
    self.define(Procedure("save-embedding", self.saveEmbedding))
    self.define(Procedure("embedding-type", self.embeddingType))
    self.define(Procedure("embedding-language", self.embeddingLanguage))
    self.define(Procedure("embedding-revision", self.embeddingRevision))
    self.define(Procedure("embedding-dimension", self.embeddingDimension))
    self.define(Procedure("embedding-vocabulary-size", self.embeddingVocabularySize))
    self.define(Procedure("embedding-contains?", self.embeddingContains))
    self.define(Procedure("embedding-vector", self.embeddingVector))
    self.define(Procedure("embedding-distance", self.embeddingDistance))
    self.define(Procedure("embedding-neighbors", self.embeddingNeighbors))
    self.define(Procedure("cosine-distance", self.cosineDistance))
    // Contextual embeddings
    self.define(Procedure("contextual-embedding?", self.isContextualEmbedding))
    self.define(Procedure("contextual-embedding", self.makeContextualEmbedding))
    self.define(Procedure("contextual-embedding-id", self.contextualEmbeddingId))
    self.define(Procedure("contextual-embedding-languages", self.contextualEmbeddingLanguages))
    self.define(Procedure("contextual-embedding-scripts", self.contextualEmbeddingScripts))
    self.define(Procedure("contextual-embedding-revision", self.contextualEmbeddingRevision))
    self.define(Procedure("contextual-embedding-dimension", self.contextualEmbeddingDimension))
    self.define(Procedure("contextual-embedding-max-length", self.contextualEmbeddingMaxLength))
    self.define(Procedure("contextual-embedding-assets?", self.contextualEmbeddingAssets))
    self.define(Procedure("request-contextual-embedding-assets",
                          self.requestContextualEmbeddingAssets))
    self.define(Procedure("contextual-embedding-vectors", self.contextualEmbeddingVectors))
    self.define(Procedure("contextual-embedding-vector", self.contextualEmbeddingVector))
    // Gazetteers
    self.define(Procedure("gazetteer?", self.isGazetteer))
    self.define(Procedure("make-gazetteer", self.makeGazetteer))
    self.define(Procedure("load-gazetteer", self.loadGazetteer))
    self.define(Procedure("save-gazetteer", self.saveGazetteer))
    self.define(Procedure("gazetteer-language", self.gazetteerLanguage))
    self.define(Procedure("gazetteer-label", self.gazetteerLabel))
    // Text models
    self.define(Procedure("text-model?", self.isTextModel))
    self.define(Procedure("load-text-model", self.loadTextModel))
    self.define(Procedure("text-model-type", self.textModelType))
    self.define(Procedure("text-model-language", self.textModelLanguage))
    self.define(Procedure("text-model-revision", self.textModelRevision))
    self.define(Procedure("text-model-label", self.textModelLabel))
    self.define(Procedure("text-model-label-hypotheses", self.textModelLabelHypotheses))
    self.define(Procedure("text-model-token-labels", self.textModelTokenLabels))
    #if canImport(CreateML)
    self.define(Procedure("train-text-classifier", self.trainTextClassifier))
    self.define(Procedure("text-model-metrics", self.textModelMetrics))
    self.define(Procedure("save-text-model", self.saveTextModel))
    #endif
  }

  // MARK: - Errors

  private func error(_ reason: String, _ expr: Expr) -> RuntimeError {
    return RuntimeError.custom("error", "\(reason): $0", [expr])
  }

  // MARK: - Conversions

  private func symbol(_ str: String) -> Expr {
    return .symbol(self.context.symbols.intern(str))
  }

  /// Returns the name of a symbol or the content of a string.
  private func name(_ expr: Expr) throws -> String {
    switch expr {
      case .symbol(let sym):
        return sym.identifier
      case .string(let str):
        return str as String
      default:
        throw RuntimeError.type(expr, expected: [.symbolType, .strType])
    }
  }

  /// Returns a list of the names in `expr`, which is either a single name or a list of names.
  private func names(_ expr: Expr) throws -> [String] {
    var res: [String] = []
    var list = expr
    switch list {
      case .symbol(_), .string(_):
        return [try self.name(list)]
      default:
        break
    }
    while case .pair(let name, let rest) = list {
      res.append(try self.name(name))
      list = rest
    }
    guard list.isNull else {
      throw RuntimeError.type(expr, expected: [.properListType])
    }
    return res
  }

  private func language(_ expr: Expr?) throws -> NLLanguage? {
    guard let expr, expr.isTrue else {
      return nil
    }
    return NLLanguage(rawValue: try self.name(expr))
  }

  private func languageExpr(_ language: NLLanguage?) -> Expr {
    guard let language, language != .undetermined else {
      return .false
    }
    return self.symbol(language.rawValue)
  }

  private func languages(_ expr: Expr?) throws -> [NLLanguage] {
    guard let expr, expr.isTrue else {
      return []
    }
    return try self.names(expr).map { NLLanguage(rawValue: $0) }
  }

  private func unit(_ expr: Expr?, _ def: NLTokenUnit = .word) throws -> NLTokenUnit {
    guard let expr, expr.isTrue else {
      return def
    }
    guard let unit = self.unitForName[try self.name(expr)] else {
      throw self.error("unknown token unit", expr)
    }
    return unit
  }

  private func scheme(_ expr: Expr) throws -> NLTagScheme {
    let name = try self.name(expr)
    return self.schemeForName[name] ?? NLTagScheme(rawValue: name)
  }

  private func schemeExpr(_ scheme: NLTagScheme) -> Expr {
    return self.symbol(self.nameForScheme[scheme.rawValue] ?? scheme.rawValue)
  }

  private func options(_ expr: Expr?) throws -> NLTagger.Options {
    guard let expr else {
      return [.omitWhitespace]
    }
    var res: NLTagger.Options = []
    guard expr.isTrue else {
      return res
    }
    for name in try self.names(expr) {
      guard let option = self.optionForName[name] else {
        throw self.error("unknown tagger option", self.symbol(name))
      }
      res.insert(option)
    }
    return res
  }

  /// Converts a label, i.e. a symbol or string, into a tag. Symbols denoting tags defined
  /// by the NaturalLanguage framework are mapped to the corresponding tag.
  private func label(_ expr: Expr) throws -> String {
    let name = try self.name(expr)
    return self.tagForName[name]?.rawValue ?? name
  }

  /// Converts a label into a symbol. Tags defined by the NaturalLanguage framework are
  /// mapped to their kebab-case names.
  private func labelExpr(_ label: String?) -> Expr {
    guard let label else {
      return .false
    }
    return self.symbol(self.nameForTag[label] ?? label)
  }

  /// Converts a tag of the given tag scheme into a Scheme value.
  private func tagExpr(_ tag: String?, scheme: NLTagScheme) -> Expr {
    guard let tag else {
      return .false
    }
    switch scheme {
      case .lemma:
        return .makeString(tag)
      case .sentimentScore:
        if let score = Double(tag) {
          return .makeNumber(score)
        }
        return .makeString(tag)
      case .language, .script:
        return tag == NLLanguage.undetermined.rawValue ? .false : self.symbol(tag)
      default:
        return self.labelExpr(tag)
    }
  }

  private func vector(_ vec: [Double]) -> Expr {
    return .vector(Collection(kind: .vector, exprs: Exprs(vec.map { .makeNumber($0) })))
  }

  private func doubles(_ expr: Expr) throws -> [Double] {
    var res: [Double] = []
    switch expr {
      case .vector(let col):
        for x in col.exprs {
          res.append(try x.asDouble(coerce: true))
        }
      case .null, .pair(_, _):
        var list = expr
        while case .pair(let x, let rest) = list {
          res.append(try x.asDouble(coerce: true))
          list = rest
        }
        guard list.isNull else {
          throw RuntimeError.type(expr, expected: [.properListType])
        }
      default:
        throw RuntimeError.type(expr, expected: [.vectorType, .properListType])
    }
    return res
  }

  private func strings(_ expr: Expr) throws -> [String] {
    var res: [String] = []
    var list = expr
    while case .pair(let str, let rest) = list {
      res.append(try str.asString())
      list = rest
    }
    guard list.isNull else {
      throw RuntimeError.type(expr, expected: [.properListType])
    }
    return res
  }

  /// Converts an association list mapping labels to lists of strings, i.e. a list of the
  /// form `((label str ...) ...)`, into a dictionary.
  private func labeledStrings(_ expr: Expr) throws -> [String : [String]] {
    var res: [String : [String]] = [:]
    var list = expr
    while case .pair(let entry, let rest) = list {
      guard case .pair(let label, let strs) = entry else {
        throw RuntimeError.type(entry, expected: [.pairType])
      }
      res[try self.label(label), default: []].append(contentsOf: try self.strings(strs))
      list = rest
    }
    guard list.isNull else {
      throw RuntimeError.type(expr, expected: [.properListType])
    }
    return res
  }

  private func rangeExpr(_ range: NSRange) -> Expr {
    return .pair(.makeNumber(range.location), .makeNumber(range.location + range.length))
  }

  private func path(_ expr: Expr) throws -> String {
    return self.context.fileHandler.path(try expr.asPath(),
                                         relativeTo: self.context.evaluator.currentDirectoryPath)
  }

  /// Returns `alist` sorted by descending values.
  private func hypotheses(_ dict: [String : Double], _ key: (String) -> Expr) -> Expr {
    var res = Exprs()
    for (k, v) in dict.sorted(by: { $0.value > $1.value || ($0.value == $1.value && $0.key < $1.key) }) {
      res.append(.pair(key(k), .makeNumber(v)))
    }
    return .makeList(res)
  }

  // MARK: - Text and ranges

  /// A string together with the part of it which should be processed.
  private struct Text {
    let string: String
    let range: Range<String.Index>
    let offset: Int
  }

  private func text(_ expr: Expr, _ start: Expr? = nil, _ end: Expr? = nil) throws -> Text {
    let str = try expr.asMutableStr()
    let e = (end == nil || end!.isFalse) ? str.length : try end!.asInt(below: str.length + 1)
    let s = (start == nil || start!.isFalse) ? 0 : try start!.asInt(below: e + 1)
    let string = str as String
    guard let range = Range(NSRange(location: s, length: e - s), in: string) else {
      throw self.error("invalid text range", .pair(.makeNumber(s), .makeNumber(e)))
    }
    return Text(string: string, range: range, offset: s)
  }

  private func index(_ text: Text, _ expr: Expr) throws -> String.Index {
    let i = try expr.asInt(below: text.string.utf16.count + 1)
    guard let range = Range(NSRange(location: i, length: 0), in: text.string) else {
      throw self.error("invalid text index", expr)
    }
    return range.lowerBound
  }

  // MARK: - Languages

  private func dominantLanguage(expr: Expr) throws -> Expr {
    return self.languageExpr(NLLanguageRecognizer.dominantLanguage(for: try expr.asString()))
  }

  private func languageHypotheses(expr: Expr, args: Arguments) throws -> Expr {
    guard let (max, constraints, hints) = args.optional(.fixnum(5), .false, .false) else {
      throw RuntimeError.argumentCount(of: "language-hypotheses",
                                       min: 1,
                                       max: 4,
                                       args: .pair(expr, .makeList(args)))
    }
    let recognizer = NLLanguageRecognizer()
    recognizer.languageConstraints = try self.languages(constraints)
    if hints.isTrue {
      var dict: [NLLanguage : Double] = [:]
      var list = hints
      while case .pair(.pair(let lang, let prob), let rest) = list {
        if let lang = try self.language(lang) {
          dict[lang] = try prob.asDouble(coerce: true)
        }
        list = rest
      }
      guard list.isNull else {
        throw RuntimeError.type(hints, expected: [.assocListType])
      }
      recognizer.languageHints = dict
    }
    recognizer.processString(try expr.asString())
    var dict: [String : Double] = [:]
    for (lang, prob) in recognizer.languageHypotheses(withMaximum: try max.asInt(above: 0)) {
      dict[lang.rawValue] = prob
    }
    return self.hypotheses(dict, self.symbol)
  }

  // MARK: - Tokenization

  private func tokenize(_ expr: Expr, _ args: Arguments, _ name: String) throws -> (Text, [Range<String.Index>]) {
    guard let (u, lang, start, end) = args.optional(.false, .false, .false, .false) else {
      throw RuntimeError.argumentCount(of: name, min: 1, max: 5, args: .pair(expr, .makeList(args)))
    }
    let text = try self.text(expr, start, end)
    let tokenizer = NLTokenizer(unit: try self.unit(u))
    tokenizer.string = text.string
    if let lang = try self.language(lang) {
      tokenizer.setLanguage(lang)
    }
    return (text, tokenizer.tokens(for: text.range))
  }

  private func textTokens(expr: Expr, args: Arguments) throws -> Expr {
    let (text, tokens) = try self.tokenize(expr, args, "text-tokens")
    return .makeList(Exprs(tokens.map { .makeString(String(text.string[$0])) }))
  }

  private func textTokenRanges(expr: Expr, args: Arguments) throws -> Expr {
    let (text, tokens) = try self.tokenize(expr, args, "text-token-ranges")
    return .makeList(Exprs(tokens.map { self.rangeExpr(NSRange($0, in: text.string)) }))
  }

  private func textTokenRange(expr: Expr, index: Expr, args: Arguments) throws -> Expr {
    guard let (u, lang) = args.optional(.false, .false) else {
      throw RuntimeError.argumentCount(of: "text-token-range",
                                       min: 2,
                                       max: 4,
                                       args: .pair(expr, .pair(index, .makeList(args))))
    }
    let text = try self.text(expr)
    let i = try self.index(text, index)
    let tokenizer = NLTokenizer(unit: try self.unit(u))
    tokenizer.string = text.string
    if let lang = try self.language(lang) {
      tokenizer.setLanguage(lang)
    }
    if i == text.string.endIndex {
      return .false
    }
    let range = tokenizer.tokenRange(at: i)
    return range.isEmpty ? .false : self.rangeExpr(NSRange(range, in: text.string))
  }

  // MARK: - Tagging

  /// Returns a tag scheme and a configured tagger for `expr`, which is either a tagger
  /// object or a tag scheme.
  private func tagger(_ expr: Expr) throws -> (NLTagScheme, NLTagger) {
    if case .object(let obj) = expr, let tagger = obj as? NativeTagger {
      return (tagger.scheme, tagger.makeTagger())
    }
    let scheme = try self.scheme(expr)
    return (scheme, NLTagger(tagSchemes: [scheme]))
  }

  private func nativeTagger(_ expr: Expr) throws -> NativeTagger {
    guard case .object(let obj) = expr, let tagger = obj as? NativeTagger else {
      throw RuntimeError.type(expr, expected: [NativeTagger.type])
    }
    return tagger
  }

  private func isTagScheme(expr: Expr) -> Expr {
    switch expr {
      case .symbol(let sym):
        return .makeBoolean(self.schemeForName[sym.identifier] != nil)
      default:
        return .false
    }
  }

  private func availableTagSchemes(lang: Expr, unit: Expr?) throws -> Expr {
    guard let language = try self.language(lang) else {
      throw RuntimeError.type(lang, expected: [.symbolType, .strType])
    }
    let schemes = NLTagger.availableTagSchemes(for: try self.unit(unit), language: language)
    return .makeList(Exprs(schemes.map(self.schemeExpr)))
  }

  private func requestTagAssets(lang: Expr, scheme: Expr) throws -> Expr {
    guard let language = try self.language(lang) else {
      throw RuntimeError.type(lang, expected: [.symbolType, .strType])
    }
    let result = Future(external: false)
    let context = self.context
    NLTagger.requestAssets(for: language, tagScheme: try self.scheme(scheme)) { res, error in
      if let error {
        _ = try? result.setResult(in: context, to: .error(RuntimeError.os(error)), raise: true)
      } else {
        _ = try? result.setResult(in: context, to: .makeBoolean(res == .available), raise: false)
      }
    }
    return .object(result)
  }

  private func makeTagger(scheme: Expr, models: Expr?, gazetteers: Expr?) throws -> Expr {
    var ms: [NLModel] = []
    var gs: [NLGazetteer] = []
    if let models, models.isTrue {
      var list = models
      if case .object(_) = list {
        list = .pair(list, .null)
      }
      while case .pair(let m, let rest) = list {
        ms.append(try self.textModel(m).model)
        list = rest
      }
      guard list.isNull else {
        throw RuntimeError.type(models, expected: [.properListType])
      }
    }
    if let gazetteers, gazetteers.isTrue {
      var list = gazetteers
      if case .object(_) = list {
        list = .pair(list, .null)
      }
      while case .pair(let g, let rest) = list {
        gs.append(try self.gazetteer(g).gazetteer)
        list = rest
      }
      guard list.isNull else {
        throw RuntimeError.type(gazetteers, expected: [.properListType])
      }
    }
    return .object(NativeTagger(scheme: try self.scheme(scheme), models: ms, gazetteers: gs))
  }

  private func isTagger(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is NativeTagger {
      return .true
    }
    return .false
  }

  private func taggerScheme(expr: Expr) throws -> Expr {
    return self.schemeExpr(try self.nativeTagger(expr).scheme)
  }

  private func tag(_ expr: Expr,
                   _ spec: Expr,
                   _ args: Arguments,
                   _ name: String,
                   _ f: (Text, NLTagScheme, NLTag?, Range<String.Index>) -> Expr) throws -> Expr {
    guard let (u, opts, lang, start, end) = args.optional(.false, .null, .false, .false, .false) else {
      throw RuntimeError.argumentCount(of: name,
                                       min: 2,
                                       max: 7,
                                       args: .pair(expr, .pair(spec, .makeList(args))))
    }
    let text = try self.text(expr, start, end)
    let unit = try self.unit(u)
    let options = try self.options(args.count >= 2 ? opts : nil)
    let (scheme, tagger) = try self.tagger(spec)
    tagger.string = text.string
    if let lang = try self.language(lang) {
      tagger.setLanguage(lang, range: text.string.startIndex..<text.string.endIndex)
    }
    var res = Exprs()
    // The tagger only omits whitespace tokens at the word level; whitespace-only sentences
    // and paragraphs are skipped here.
    let omitWhitespace = options.contains(.omitWhitespace)
    tagger.enumerateTags(in: text.range, unit: unit, scheme: scheme, options: options) { tag, range in
      if !omitWhitespace || text.string[range].contains(where: { !$0.isWhitespace }) {
        res.append(f(text, scheme, tag, range))
      }
      return true
    }
    return .makeList(res)
  }

  private func textTags(expr: Expr, spec: Expr, args: Arguments) throws -> Expr {
    return try self.tag(expr, spec, args, "text-tags") { text, scheme, tag, range in
      .pair(.makeString(String(text.string[range])), self.tagExpr(tag?.rawValue, scheme: scheme))
    }
  }

  private func textTagRanges(expr: Expr, spec: Expr, args: Arguments) throws -> Expr {
    return try self.tag(expr, spec, args, "text-tag-ranges") { text, scheme, tag, range in
      .pair(self.rangeExpr(NSRange(range, in: text.string)),
            self.tagExpr(tag?.rawValue, scheme: scheme))
    }
  }

  private func textTag(expr: Expr, index: Expr, spec: Expr, args: Arguments) throws -> Expr {
    guard let (u, lang) = args.optional(.false, .false) else {
      throw RuntimeError.argumentCount(of: "text-tag",
                                       min: 3,
                                       max: 5,
                                       args: .pair(expr, .pair(index, .pair(spec, .makeList(args)))))
    }
    let text = try self.text(expr)
    let i = try self.index(text, index)
    let (scheme, tagger) = try self.tagger(spec)
    tagger.string = text.string
    if let lang = try self.language(lang) {
      tagger.setLanguage(lang, range: text.string.startIndex..<text.string.endIndex)
    }
    if i == text.string.endIndex {
      return .false
    }
    let (tag, _) = tagger.tag(at: i, unit: try self.unit(u), scheme: scheme)
    return self.tagExpr(tag?.rawValue, scheme: scheme)
  }

  private func textTagHypotheses(expr: Expr, index: Expr, spec: Expr, args: Arguments) throws -> Expr {
    guard let (u, max, lang) = args.optional(.false, .fixnum(5), .false) else {
      throw RuntimeError.argumentCount(of: "text-tag-hypotheses",
                                       min: 3,
                                       max: 6,
                                       args: .pair(expr, .pair(index, .pair(spec, .makeList(args)))))
    }
    let text = try self.text(expr)
    let i = try self.index(text, index)
    let (scheme, tagger) = try self.tagger(spec)
    tagger.string = text.string
    if let lang = try self.language(lang) {
      tagger.setLanguage(lang, range: text.string.startIndex..<text.string.endIndex)
    }
    if i == text.string.endIndex {
      return .null
    }
    let (hyps, _) = tagger.tagHypotheses(at: i,
                                         unit: try self.unit(u),
                                         scheme: scheme,
                                         maximumCount: try max.asInt(above: 0))
    return self.hypotheses(hyps) { self.tagExpr($0, scheme: scheme) }
  }

  private func namedEntities(expr: Expr, lang: Expr?) throws -> Expr {
    let text = try self.text(expr)
    let tagger = NLTagger(tagSchemes: [.nameType])
    tagger.string = text.string
    if let lang = try self.language(lang) {
      tagger.setLanguage(lang, range: text.string.startIndex..<text.string.endIndex)
    }
    var res = Exprs()
    tagger.enumerateTags(in: text.range,
                         unit: .word,
                         scheme: .nameType,
                         options: [.omitWhitespace, .omitPunctuation, .joinNames]) { tag, range in
      if let tag, tag == .personalName || tag == .placeName || tag == .organizationName {
        res.append(.pair(.makeString(String(text.string[range])), self.labelExpr(tag.rawValue)))
      }
      return true
    }
    return .makeList(res)
  }

  private func textSentiment(expr: Expr, lang: Expr?) throws -> Expr {
    let text = try self.text(expr)
    let tagger = NLTagger(tagSchemes: [.sentimentScore])
    tagger.string = text.string
    if let lang = try self.language(lang) {
      tagger.setLanguage(lang, range: text.string.startIndex..<text.string.endIndex)
    }
    var total = 0.0
    var length = 0
    tagger.enumerateTags(in: text.range,
                         unit: .paragraph,
                         scheme: .sentimentScore,
                         options: [.omitWhitespace]) { tag, range in
      let paragraph = text.string[range]
      if let tag, let score = Double(tag.rawValue),
         paragraph.contains(where: { !$0.isWhitespace }) {
        let n = paragraph.utf16.count
        total += score * Double(n)
        length += n
      }
      return true
    }
    return length == 0 ? .false : .makeNumber(total / Double(length))
  }

  // MARK: - Embeddings

  private func embedding(_ expr: Expr) throws -> NativeEmbedding {
    guard case .object(let obj) = expr, let embedding = obj as? NativeEmbedding else {
      throw RuntimeError.type(expr, expected: [NativeEmbedding.type])
    }
    return embedding
  }

  private func isEmbedding(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is NativeEmbedding {
      return .true
    }
    return .false
  }

  private func wordEmbedding(lang: Expr, revision: Expr?) throws -> Expr {
    guard let language = try self.language(lang) else {
      throw RuntimeError.type(lang, expected: [.symbolType, .strType])
    }
    let embedding: NLEmbedding?
    if let revision, revision.isTrue {
      embedding = NLEmbedding.wordEmbedding(for: language, revision: try revision.asInt(above: 0))
    } else {
      embedding = NLEmbedding.wordEmbedding(for: language)
    }
    guard let embedding else {
      return .false
    }
    return .object(NativeEmbedding(embedding, kind: .word))
  }

  private func sentenceEmbedding(lang: Expr, revision: Expr?) throws -> Expr {
    guard let language = try self.language(lang) else {
      throw RuntimeError.type(lang, expected: [.symbolType, .strType])
    }
    let embedding: NLEmbedding?
    if let revision, revision.isTrue {
      embedding = NLEmbedding.sentenceEmbedding(for: language, revision: try revision.asInt(above: 0))
    } else {
      embedding = NLEmbedding.sentenceEmbedding(for: language)
    }
    guard let embedding else {
      return .false
    }
    return .object(NativeEmbedding(embedding, kind: .sentence))
  }

  private func revisions(_ set: IndexSet) -> Expr {
    return .makeList(Exprs(set.map { .makeNumber($0) }))
  }

  private func wordEmbeddingRevisions(lang: Expr) throws -> Expr {
    guard let language = try self.language(lang) else {
      throw RuntimeError.type(lang, expected: [.symbolType, .strType])
    }
    return self.revisions(NLEmbedding.supportedRevisions(for: language))
  }

  private func sentenceEmbeddingRevisions(lang: Expr) throws -> Expr {
    guard let language = try self.language(lang) else {
      throw RuntimeError.type(lang, expected: [.symbolType, .strType])
    }
    return self.revisions(NLEmbedding.supportedSentenceEmbeddingRevisions(for: language))
  }

  private func loadEmbedding(path: Expr) throws -> Expr {
    do {
      let embedding = try NLEmbedding(contentsOf: URL(fileURLWithPath: try self.path(path)))
      return .object(NativeEmbedding(embedding, kind: .custom))
    } catch let error as RuntimeError {
      throw error
    } catch {
      throw RuntimeError.os(error)
    }
  }

  private func saveEmbedding(path: Expr, entries: Expr, args: Arguments) throws -> Expr {
    guard let (lang, revision) = args.optional(.false, .fixnum(1)) else {
      throw RuntimeError.argumentCount(of: "save-embedding",
                                       min: 2,
                                       max: 4,
                                       args: .pair(path, .pair(entries, .makeList(args))))
    }
    var dict: [String : [Double]] = [:]
    var dimension: Int? = nil
    var list = entries
    while case .pair(let entry, let rest) = list {
      guard case .pair(let key, let vec) = entry else {
        throw RuntimeError.type(entry, expected: [.pairType])
      }
      let vector = try self.doubles(vec)
      if let dim = dimension, dim != vector.count {
        throw self.error("embedding vectors differ in dimension", entry)
      }
      dimension = vector.count
      dict[try key.asString()] = vector
      list = rest
    }
    guard list.isNull else {
      throw RuntimeError.type(entries, expected: [.assocListType])
    }
    do {
      try NLEmbedding.write(dict,
                            language: try self.language(lang),
                            revision: try revision.asInt(above: 0),
                            to: URL(fileURLWithPath: try self.path(path)))
    } catch let error as RuntimeError {
      throw error
    } catch {
      throw RuntimeError.os(error)
    }
    return .void
  }

  private func embeddingType(expr: Expr) throws -> Expr {
    return self.symbol(try self.embedding(expr).kind.rawValue)
  }

  private func embeddingLanguage(expr: Expr) throws -> Expr {
    return self.languageExpr(try self.embedding(expr).embedding.language)
  }

  private func embeddingRevision(expr: Expr) throws -> Expr {
    return .makeNumber(try self.embedding(expr).embedding.revision)
  }

  private func embeddingDimension(expr: Expr) throws -> Expr {
    return .makeNumber(try self.embedding(expr).embedding.dimension)
  }

  private func embeddingVocabularySize(expr: Expr) throws -> Expr {
    return .makeNumber(try self.embedding(expr).embedding.vocabularySize)
  }

  private func embeddingContains(expr: Expr, str: Expr) throws -> Expr {
    return .makeBoolean(try self.embedding(expr).embedding.contains(try str.asString()))
  }

  private func embeddingVector(expr: Expr, str: Expr) throws -> Expr {
    guard let vec = try self.embedding(expr).embedding.vector(for: try str.asString()) else {
      return .false
    }
    return self.vector(vec)
  }

  /// The NaturalLanguage framework reports the Euclidean distance between the normalized
  /// vectors for distance type `.cosine`, i.e. `sqrt(2 * (1 - cos))`, contrary to its
  /// documentation. This function converts such a distance into the cosine distance
  /// `1 - cos`, which is used consistently by this library.
  private func cosine(fromNL distance: NLDistance) -> Double {
    return distance * distance / 2.0
  }
  
  private func cosine(_ x: [Double], _ y: [Double]) -> Double? {
    guard x.count == y.count else {
      return nil
    }
    var dot = 0.0
    var nx = 0.0
    var ny = 0.0
    for i in x.indices {
      dot += x[i] * y[i]
      nx += x[i] * x[i]
      ny += y[i] * y[i]
    }
    guard nx > 0.0 && ny > 0.0 else {
      return nil
    }
    return 1.0 - dot / (nx.squareRoot() * ny.squareRoot())
  }

  private func embeddingDistance(expr: Expr, x: Expr, y: Expr) throws -> Expr {
    let embedding = try self.embedding(expr).embedding
    if case .string(let xs) = x, case .string(let ys) = y {
      let xstr = xs as String
      let ystr = ys as String
      guard embedding.vector(for: xstr) != nil, embedding.vector(for: ystr) != nil else {
        return .false
      }
      return .makeNumber(self.cosine(fromNL: embedding.distance(between: xstr,
                                                                and: ystr,
                                                                distanceType: .cosine)))
    }
    let xv: [Double]
    if case .string(let xs) = x {
      guard let v = embedding.vector(for: xs as String) else {
        return .false
      }
      xv = v
    } else {
      xv = try self.doubles(x)
    }
    let yv: [Double]
    if case .string(let ys) = y {
      guard let v = embedding.vector(for: ys as String) else {
        return .false
      }
      yv = v
    } else {
      yv = try self.doubles(y)
    }
    guard let dist = self.cosine(xv, yv) else {
      return .false
    }
    return .makeNumber(dist)
  }

  private func embeddingNeighbors(expr: Expr, x: Expr, args: Arguments) throws -> Expr {
    guard let (max, maxdist) = args.optional(.fixnum(10), .false) else {
      throw RuntimeError.argumentCount(of: "embedding-neighbors",
                                       min: 2,
                                       max: 4,
                                       args: .pair(expr, .pair(x, .makeList(args))))
    }
    let embedding = try self.embedding(expr).embedding
    let count = try max.asInt(above: 0)
    let maxDistance = maxdist.isFalse ? 2.0 : try maxdist.asDouble(coerce: true)
    let neighbors: [(String, NLDistance)]
    if case .string(let str) = x {
      neighbors = embedding.neighbors(for: str as String,
                                      maximumCount: count,
                                      distanceType: .cosine)
    } else {
      let vector = try self.doubles(x)
      guard vector.count == embedding.dimension else {
        throw self.error("vector does not match embedding dimension", x)
      }
      neighbors = embedding.neighbors(for: vector,
                                      maximumCount: count,
                                      distanceType: .cosine)
    }
    var res = Exprs()
    for (neighbor, distance) in neighbors {
      let dist = self.cosine(fromNL: distance)
      if dist <= maxDistance {
        res.append(.pair(.makeString(neighbor), .makeNumber(dist)))
      }
    }
    return .makeList(res)
  }

  private func cosineDistance(x: Expr, y: Expr) throws -> Expr {
    let xv = try self.doubles(x)
    let yv = try self.doubles(y)
    guard xv.count == yv.count else {
      throw self.error("vectors differ in dimension", .pair(x, y))
    }
    guard let dist = self.cosine(xv, yv) else {
      return .false
    }
    return .makeNumber(dist)
  }

  // MARK: - Contextual embeddings

  private func contextualEmbedding(_ expr: Expr) throws -> NativeContextualEmbedding {
    guard case .object(let obj) = expr, let embedding = obj as? NativeContextualEmbedding else {
      throw RuntimeError.type(expr, expected: [NativeContextualEmbedding.type])
    }
    return embedding
  }

  private func isContextualEmbedding(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is NativeContextualEmbedding {
      return .true
    }
    return .false
  }

  private func makeContextualEmbedding(lang: Expr, script: Expr?) throws -> Expr {
    let language = try self.language(lang)
    let embedding: NLContextualEmbedding?
    if let script, script.isTrue {
      var values: [NLContextualEmbeddingKey : Any] = [.scripts: [try self.name(script)]]
      if let language {
        values[.languages] = [language.rawValue]
      }
      embedding = NLContextualEmbedding.contextualEmbeddings(forValues: values).first
    } else if let language {
      embedding = NLContextualEmbedding(language: language)
    } else {
      throw RuntimeError.type(lang, expected: [.symbolType, .strType])
    }
    guard let embedding else {
      return .false
    }
    return .object(NativeContextualEmbedding(embedding))
  }

  private func contextualEmbeddingId(expr: Expr) throws -> Expr {
    return .makeString(try self.contextualEmbedding(expr).embedding.modelIdentifier)
  }

  private func contextualEmbeddingLanguages(expr: Expr) throws -> Expr {
    let langs = try self.contextualEmbedding(expr).embedding.languages
    return .makeList(Exprs(langs.map { self.symbol($0.rawValue) }))
  }

  private func contextualEmbeddingScripts(expr: Expr) throws -> Expr {
    let scripts = try self.contextualEmbedding(expr).embedding.scripts
    return .makeList(Exprs(scripts.map { self.symbol($0.rawValue) }))
  }

  private func contextualEmbeddingRevision(expr: Expr) throws -> Expr {
    return .makeNumber(try self.contextualEmbedding(expr).embedding.revision)
  }

  private func contextualEmbeddingDimension(expr: Expr) throws -> Expr {
    return .makeNumber(try self.contextualEmbedding(expr).embedding.dimension)
  }

  private func contextualEmbeddingMaxLength(expr: Expr) throws -> Expr {
    return .makeNumber(try self.contextualEmbedding(expr).embedding.maximumSequenceLength)
  }

  private func contextualEmbeddingAssets(expr: Expr) throws -> Expr {
    return .makeBoolean(try self.contextualEmbedding(expr).embedding.hasAvailableAssets)
  }

  private func requestContextualEmbeddingAssets(expr: Expr) throws -> Expr {
    let embedding = try self.contextualEmbedding(expr).embedding
    let result = Future(external: false)
    let context = self.context
    embedding.requestAssets { res, error in
      if let error {
        _ = try? result.setResult(in: context, to: .error(RuntimeError.os(error)), raise: true)
      } else {
        _ = try? result.setResult(in: context, to: .makeBoolean(res == .available), raise: false)
      }
    }
    return .object(result)
  }

  /// Returns the token vectors of `str` (skipping tokens with empty ranges).
  private func tokenVectors(_ expr: Expr,
                            _ str: Expr,
                            _ lang: Expr?) throws -> (String, [(Range<String.Index>, [Double])]) {
    let embedding = try self.contextualEmbedding(expr)
    let result = try embedding.result(for: try str.asString(), language: try self.language(lang))
    var res: [(Range<String.Index>, [Double])] = []
    result.enumerateTokenVectors(in: result.string.startIndex..<result.string.endIndex) { vec, range in
      if !range.isEmpty {
        res.append((range, vec))
      }
      return true
    }
    return (result.string, res)
  }

  private func contextualEmbeddingVectors(expr: Expr, str: Expr, lang: Expr?) throws -> Expr {
    let (string, vectors) = try self.tokenVectors(expr, str, lang)
    return .makeList(Exprs(vectors.map {
      .pair(self.rangeExpr(NSRange($0.0, in: string)), self.vector($0.1))
    }))
  }

  private func contextualEmbeddingVector(expr: Expr, str: Expr, lang: Expr?) throws -> Expr {
    let (_, vectors) = try self.tokenVectors(expr, str, lang)
    guard let first = vectors.first else {
      return .false
    }
    var mean = [Double](repeating: 0.0, count: first.1.count)
    for (_, vec) in vectors {
      for i in mean.indices where i < vec.count {
        mean[i] += vec[i]
      }
    }
    let n = Double(vectors.count)
    return self.vector(mean.map { $0 / n })
  }

  // MARK: - Gazetteers

  private func gazetteer(_ expr: Expr) throws -> NativeGazetteer {
    guard case .object(let obj) = expr, let gazetteer = obj as? NativeGazetteer else {
      throw RuntimeError.type(expr, expected: [NativeGazetteer.type])
    }
    return gazetteer
  }

  private func isGazetteer(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is NativeGazetteer {
      return .true
    }
    return .false
  }

  private func makeGazetteer(entries: Expr, lang: Expr?) throws -> Expr {
    let dict = try self.labeledStrings(entries)
    do {
      return .object(NativeGazetteer(try NLGazetteer(dictionary: dict,
                                                     language: try self.language(lang))))
    } catch let error as RuntimeError {
      throw error
    } catch {
      throw RuntimeError.os(error)
    }
  }

  private func loadGazetteer(path: Expr) throws -> Expr {
    do {
      return .object(NativeGazetteer(try NLGazetteer(contentsOf:
                                                      URL(fileURLWithPath: try self.path(path)))))
    } catch let error as RuntimeError {
      throw error
    } catch {
      throw RuntimeError.os(error)
    }
  }

  private func saveGazetteer(path: Expr, expr: Expr) throws -> Expr {
    let gazetteer = try self.gazetteer(expr).gazetteer
    do {
      try gazetteer.data.write(to: URL(fileURLWithPath: try self.path(path)), options: .atomic)
    } catch let error as RuntimeError {
      throw error
    } catch {
      throw RuntimeError.os(error)
    }
    return .void
  }

  private func gazetteerLanguage(expr: Expr) throws -> Expr {
    return self.languageExpr(try self.gazetteer(expr).gazetteer.language)
  }

  private func gazetteerLabel(expr: Expr, str: Expr) throws -> Expr {
    return self.labelExpr(try self.gazetteer(expr).gazetteer.label(for: try str.asString()))
  }

  // MARK: - Text models

  private func textModel(_ expr: Expr) throws -> NativeTextModel {
    guard case .object(let obj) = expr, let model = obj as? NativeTextModel else {
      throw RuntimeError.type(expr, expected: [NativeTextModel.type])
    }
    return model
  }

  private func isTextModel(expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is NativeTextModel {
      return .true
    }
    return .false
  }

  private func loadTextModel(path: Expr) throws -> Expr {
    var url = URL(fileURLWithPath: try self.path(path))
    var compiled: URL? = nil
    defer {
      if let compiled {
        try? FileManager.default.removeItem(at: compiled)
      }
    }
    if url.pathExtension == "mlmodel" {
      // Compile the model on a background queue and wait for the result
      let semaphore = DispatchSemaphore(value: 0)
      var result: Result<URL, Error>? = nil
      MLModel.compileModel(at: url) { res in
        result = res
        semaphore.signal()
      }
      semaphore.wait()
      switch result! {
        case .success(let u):
          compiled = u
          url = u
        case .failure(let error):
          throw RuntimeError.os(error)
      }
    }
    do {
      return .object(NativeTextModel(try NLModel(contentsOf: url)))
    } catch let error as RuntimeError {
      throw error
    } catch {
      throw RuntimeError.os(error)
    }
  }

  private func textModelType(expr: Expr) throws -> Expr {
    switch try self.textModel(expr).model.configuration.type {
      case .classifier:
        return self.symbol("classifier")
      case .sequence:
        return self.symbol("sequence")
      @unknown default:
        return .false
    }
  }

  private func textModelLanguage(expr: Expr) throws -> Expr {
    return self.languageExpr(try self.textModel(expr).model.configuration.language)
  }

  private func textModelRevision(expr: Expr) throws -> Expr {
    return .makeNumber(try self.textModel(expr).model.configuration.revision)
  }

  private func textModelLabel(expr: Expr, str: Expr) throws -> Expr {
    return self.labelExpr(try self.textModel(expr).model.predictedLabel(for: try str.asString()))
  }

  private func textModelLabelHypotheses(expr: Expr, str: Expr, max: Expr?) throws -> Expr {
    let model = try self.textModel(expr).model
    let count = try max?.asInt(above: 0) ?? 5
    return self.hypotheses(model.predictedLabelHypotheses(for: try str.asString(),
                                                          maximumCount: count),
                           self.labelExpr)
  }

  private func textModelTokenLabels(expr: Expr, tokens: Expr) throws -> Expr {
    let model = try self.textModel(expr).model
    let labels = model.predictedLabels(forTokens: try self.strings(tokens))
    return .makeList(Exprs(labels.map(self.labelExpr)))
  }

  #if canImport(CreateML)

  private func algorithm(_ expr: Expr?) throws -> MLTextClassifier.ModelAlgorithmType {
    guard let expr, expr.isTrue else {
      return .maxEnt(revision: nil)
    }
    if case .string(_) = expr {
      return .transferLearning(.customEmbedding(URL(fileURLWithPath: try self.path(expr))),
                               revision: nil)
    }
    switch try self.name(expr) {
      case "max-ent":
        return .maxEnt(revision: nil)
      case "crf":
        return .crf(revision: nil)
      case "static-embedding":
        return .transferLearning(.staticEmbedding, revision: nil)
      case "elmo-embedding":
        return .transferLearning(.elmoEmbedding, revision: nil)
      case "bert-embedding":
        return .transferLearning(.bertEmbedding, revision: nil)
      default:
        throw self.error("unknown text classifier algorithm", expr)
    }
  }

  private func trainTextClassifier(data: Expr, algorithm: Expr?, lang: Expr?) throws -> Expr {
    let trainingData = try self.labeledStrings(data)
    let parameters = MLTextClassifier.ModelParameters(validation: .split(strategy: .automatic),
                                                      algorithm: try self.algorithm(algorithm),
                                                      language: try self.language(lang))
    let result = Future(external: false)
    let context = self.context
    DispatchQueue.global(qos: .userInitiated).async {
      do {
        let classifier = try MLTextClassifier(trainingData: trainingData, parameters: parameters)
        let model = try NLModel(mlModel: classifier.model)
        _ = try result.setResult(in: context,
                                 to: .object(NativeTextModel(model, classifier: classifier)),
                                 raise: false)
      } catch let error as RuntimeError {
        _ = try? result.setResult(in: context, to: .error(error), raise: true)
      } catch {
        _ = try? result.setResult(in: context, to: .error(RuntimeError.os(error)), raise: true)
      }
    }
    return .object(result)
  }

  private func textModelMetrics(expr: Expr) throws -> Expr {
    guard let classifier = try self.textModel(expr).classifier as? MLTextClassifier else {
      return .false
    }
    var res = Exprs()
    let training = classifier.trainingMetrics
    if training.isValid {
      res.append(.pair(self.symbol("training-accuracy"),
                       .makeNumber(1.0 - training.classificationError)))
    }
    let validation = classifier.validationMetrics
    if validation.isValid {
      res.append(.pair(self.symbol("validation-accuracy"),
                       .makeNumber(1.0 - validation.classificationError)))
    }
    return .makeList(res)
  }

  private func saveTextModel(path: Expr, expr: Expr, args: Arguments) throws -> Expr {
    guard let (author, description, version) = args.optional(.false, .false, .false) else {
      throw RuntimeError.argumentCount(of: "save-text-model",
                                       min: 2,
                                       max: 5,
                                       args: .pair(path, .pair(expr, .makeList(args))))
    }
    guard let classifier = try self.textModel(expr).classifier as? MLTextClassifier else {
      throw self.error("text model was not trained and cannot be saved", expr)
    }
    var metadata = MLModelMetadata()
    if author.isTrue {
      metadata.author = try author.asString()
    }
    if description.isTrue {
      metadata.shortDescription = try description.asString()
    }
    if version.isTrue {
      metadata.version = try version.asString()
    }
    do {
      try classifier.write(to: URL(fileURLWithPath: try self.path(path)), metadata: metadata)
    } catch let error as RuntimeError {
      throw error
    } catch {
      throw RuntimeError.os(error)
    }
    return .void
  }

  #endif
}

// MARK: - Native objects

///
/// A tagger: a tag scheme together with custom models and gazetteers for this scheme.
///
public final class NativeTagger: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "tagger"))

  public let scheme: NLTagScheme
  public let models: [NLModel]
  public let gazetteers: [NLGazetteer]

  public init(scheme: NLTagScheme, models: [NLModel], gazetteers: [NLGazetteer]) {
    self.scheme = scheme
    self.models = models
    self.gazetteers = gazetteers
  }

  /// Returns a new tagger for this configuration. `NLTagger` objects are not thread-safe,
  /// therefore every operation uses its own tagger.
  public func makeTagger() -> NLTagger {
    let tagger = NLTagger(tagSchemes: [self.scheme])
    if !self.models.isEmpty {
      tagger.setModels(self.models, forTagScheme: self.scheme)
    }
    if !self.gazetteers.isEmpty {
      tagger.setGazetteers(self.gazetteers, for: self.scheme)
    }
    return tagger
  }

  public override var type: Type {
    return Self.type
  }

  public override var tagString: String {
    return "\(Self.type) \(self.identityString): \(self.scheme.rawValue)"
  }

  public override var string: String {
    return "#<\(self.tagString)>"
  }
}

///
/// A word or sentence embedding.
///
public final class NativeEmbedding: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "embedding"))

  public enum Kind: String {
    case word
    case sentence
    case custom
  }

  public let embedding: NLEmbedding
  public let kind: Kind

  public init(_ embedding: NLEmbedding, kind: Kind) {
    self.embedding = embedding
    self.kind = kind
  }

  public override var type: Type {
    return Self.type
  }

  public override var tagString: String {
    var res = "\(Self.type) \(self.identityString): \(self.kind.rawValue)"
    if let lang = self.embedding.language {
      res += " \(lang.rawValue)"
    }
    return res + " dimension=\(self.embedding.dimension)"
  }

  public override var string: String {
    return "#<\(self.tagString)>"
  }
}

///
/// A contextual embedding. The model is loaded on first use.
///
public final class NativeContextualEmbedding: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "contextual-embedding"))

  public let embedding: NLContextualEmbedding
  private let lock = NSLock()
  private var loaded = false

  public init(_ embedding: NLContextualEmbedding) {
    self.embedding = embedding
  }

  /// Computes the embedding result for `string`, loading the model if needed.
  public func result(for string: String, language: NLLanguage?) throws -> NLContextualEmbeddingResult {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    do {
      if !self.loaded {
        try self.embedding.load()
        self.loaded = true
      }
      return try self.embedding.embeddingResult(for: string, language: language)
    } catch {
      throw RuntimeError.os(error)
    }
  }

  public override var type: Type {
    return Self.type
  }

  public override var tagString: String {
    return "\(Self.type) \(self.identityString): " +
           "\(self.embedding.scripts.map(\.rawValue).joined(separator: ",")) " +
           "dimension=\(self.embedding.dimension)"
  }

  public override var string: String {
    return "#<\(self.tagString)>"
  }
}

///
/// A gazetteer, mapping strings to labels.
///
public final class NativeGazetteer: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "gazetteer"))

  public let gazetteer: NLGazetteer

  public init(_ gazetteer: NLGazetteer) {
    self.gazetteer = gazetteer
  }

  public override var type: Type {
    return Self.type
  }

  public override var tagString: String {
    if let lang = self.gazetteer.language {
      return "\(Self.type) \(self.identityString): \(lang.rawValue)"
    }
    return "\(Self.type) \(self.identityString)"
  }

  public override var string: String {
    return "#<\(self.tagString)>"
  }
}

///
/// A text model, i.e. a text classifier or a word tagger. Text models trained via
/// Create ML refer to the classifier from which they were created.
///
public final class NativeTextModel: NativeObject {
  public static let type = Type.objectType(Symbol(uninterned: "text-model"))

  public let model: NLModel
  public let classifier: Any?

  public init(_ model: NLModel, classifier: Any? = nil) {
    self.model = model
    self.classifier = classifier
  }

  public override var type: Type {
    return Self.type
  }

  public override var tagString: String {
    let kind: String
    switch self.model.configuration.type {
      case .classifier:
        kind = "classifier"
      case .sequence:
        kind = "sequence"
      @unknown default:
        kind = "unknown"
    }
    if let lang = self.model.configuration.language {
      return "\(Self.type) \(self.identityString): \(kind) \(lang.rawValue)"
    }
    return "\(Self.type) \(self.identityString): \(kind)"
  }

  public override var string: String {
    return "#<\(self.tagString)>"
  }
}
