//
//  EvalError.swift
//  LispKit
//
//  Created by Matthias Zenger on 01/03/2018.
//  Copyright © 2018 ObjectHub. All rights reserved.
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

///
/// Enumeration `EvalError` represents errors occuring during the evaluation or compilation
/// of LispKit expressions.
///
public enum EvalError: Int, Hashable {
  case illegalKeywordUsage
  case illegalFormalParameter
  case illegalFormalRestParameter
  case divisionByZero
  case unboundVariable
  case variableUndefined
  case variableNotYetInitialized
  case malformedCaseLambda
  case malformedArgumentList
  case malformedDefinition
  case malformedTransformer
  case malformedSyntaxRule
  case malformedPatternInSyntaxRule
  case malformedSyntaxRulePattern
  case malformedSyntaxRuleLiterals
  case noExpansion
  case invalidContextInQuasiquote
  case macroMismatchedRepetitionPatterns
  case malformedBinding
  case malformedBindings
  case malformedTest
  case malformedCondClause
  case malformedCondExpandClause
  case malformedCaseClause
  case duplicateBinding
  case notBoundInEnvironment
  case bindingImmutable
  case nonApplicativeValue
  case illegalRadix
  case argumentError
  case noMatchingCase
  case multiValueCountError
  case outOfScope
  case defineInLocalEnv
  case importInLocalEnv
  case importInLibrary
  case malformedImportSet
  case erroneousRedefinition
  case cannotExpandImportSet
  case defineSyntaxInLocalEnv
  case cannotModifyLibraryEnv
  case cannotEnterProtectedBlockTwice
  case targetBytevectorTooSmall
  case cannotOpenFile
  case cannotOpenAsset
  case cannotOpenUrl
  case invalidUrl
  case cannotWriteToPort
  case invalidDefaultPort
  case portClosed
  case illegalContinuationApplication
  case attemptToModifyImmutableData
  case vectorEmpty
  case unknownFieldOfRecordType
  case expectedRecordToAccessField
  case fieldCountError
  case malformedLibraryDefinition
  case malformedLibraryName
  case uninitializedExports
  case inconsistentImports
  case cyclicImportExport
  case redefinitionOfImport
  case unknownFile
  case unknownDirectory
  case unknownSystemDirectory
  case cannotDecodeBytevector
  case cannotEncodeBytevector
  case invalidDateTime
  case invalidTimeZone
  case invalidDateStyle
  case invalidDefaultDrawing
  case invalidDefaultShape
  case invalidPoint
  case invalidSize
  case invalidRect
  case cannotLoadImage
  case cannotLoadImageAsset
  case cannotCreateBitmap
  case invalidImageFileType
  case invalidCompositionOperation
  case invalidFlipOrientation
  case unknownInterpolateAlgorithm
  case unsupportedGradientColorSpec
  case invalidRegexpMatchingOption
  case unableToBindValue
  case unknownKeyword
  case expectedKeywordArg
  case assertion
  case cannotInsertExif
  case cannotCreateInMemoryZipArchive
  case cannotMakeInMemoryZipArchiveFromData
  case cannotCreateZipArchive
  case cannotOpenZipArchive
  case cannotMutateZipArchive
  case zipArchiveEntryDoesNotExist
  case unknownFileOrDirectory
  
  public var message: String {
    switch self {
      case .illegalKeywordUsage:
        return "illegal usage of syntactic keyword as expression: $0"
      case .illegalFormalParameter:
        return "illegal formal parameter: $0"
      case .illegalFormalRestParameter:
        return "illegal formal rest parameter: $0"
      case .divisionByZero:
        return "division by zero"
      case .unboundVariable:
        return "unbound variable: $0"
      case .variableUndefined:
        return "variable undefined: $0"
      case .variableNotYetInitialized:
        return "variable not yet initialized: $0"
      case .malformedCaseLambda:
        return "malformed lambda case list: $0"
      case .malformedArgumentList:
        return "malformed argument list: $0"
      case .malformedDefinition:
        return "malformed definition: $0"
      case .malformedTransformer:
        return "malformed transformer: $0"
      case .malformedSyntaxRule:
        return "not a valid syntax rule: $0"
      case .malformedPatternInSyntaxRule:
        return "illegal pattern $0 in syntax rule pattern: $1"
      case .malformedSyntaxRulePattern:
        return "malformed syntax rule pattern: $0"
      case .malformedSyntaxRuleLiterals:
        return "malformed list of syntax rule literals: $0"
      case .noExpansion:
        return "no expansion for: $0"
      case .invalidContextInQuasiquote:
        return "usage of $0 in invalid context within quasiquote: $1"
      case .macroMismatchedRepetitionPatterns:
        return "macro could not be expanded; mismatched repetition patterns: $0"
      case .malformedBinding:
        return "malformed binding $0 in $1"
      case .malformedBindings:
        return "malformed list of bindings: $0"
      case .malformedTest:
        return "malformed test expression: $0"
      case .malformedCondClause:
        return "malformed clause in cond form: $0"
      case .malformedCondExpandClause:
        return "malformed clause in cond-expand form: $0"
      case .malformedCaseClause:
        return "malformed clause in case form: $0"
      case .duplicateBinding:
        return "symbol $0 bound multiple times in $1"
      case .notBoundInEnvironment:
        return "symbol $0 not bound in environment"
      case .bindingImmutable:
        return "unable to assign value to immutable binding for symbol $0"
      case .nonApplicativeValue:
        return "cannot apply arguments to $0"
      case .illegalRadix:
        return "illegal radix: $0"
      case .argumentError:
        return "wrong number of arguments for $0: $1"
      case .noMatchingCase:
        return "arguments $0 not matching any case of $1"
      case .multiValueCountError:
        return "expected $0 values to be returned, but received instead: $1"
      case .outOfScope:
        return "out of scope evaluation of $0"
      case .defineInLocalEnv:
        return "definition of $0 in local environment"
      case .importInLocalEnv:
        return "import of $0 in local environment"
      case .importInLibrary:
        return "illegal import in library $,0"
      case .malformedImportSet:
        return "malformed import set: $0"
      case .erroneousRedefinition:
        return "attempted to redefine $0 with definition from $,1"
      case .cannotExpandImportSet:
        return "cannot expand import set $,0"
      case .defineSyntaxInLocalEnv:
        return "syntax definition of $0 in local environment"
      case .cannotModifyLibraryEnv:
        return "cannot modify environment of library $,0 dynamically"
      case .cannotEnterProtectedBlockTwice:
        return "attempt to invoke expression protected by unwind-protect twice"
      case .targetBytevectorTooSmall:
        return "target bytevector too small: $0"
      case .cannotOpenFile:
        return "cannot open file: $,0"
      case .cannotOpenAsset:
        return "cannot open asset $0 of type $1"
      case .cannotOpenUrl:
        return "cannot open URL: $,0"
      case .invalidUrl:
        return "invalid URL: $,0"
      case .cannotWriteToPort:
        return "cannot write to port $0"
      case .invalidDefaultPort:
        return "invalid default port: $0"
      case .portClosed:
        return "port is closed: $0"
      case .illegalContinuationApplication:
        return "continuation application in wrong context ($0 in $1)"
      case .attemptToModifyImmutableData:
        return "attempt to modify immutable data structure: $0"
      case .vectorEmpty:
        return "vector empty: $0"
      case .unknownFieldOfRecordType:
        return "unknown field $1 of record type $0"
      case .expectedRecordToAccessField:
        return "expected record of type $0 to access field: $1"
      case .fieldCountError:
        return "expected values for $0 fields, received instead: $1"
      case .malformedLibraryDefinition:
        return "malformed library definition: $0"
      case .malformedLibraryName:
        return "malformed library name: $,0"
      case .uninitializedExports:
        return "library $1 does not initialize the exported definitions $,0"
      case .inconsistentImports:
        return "inconsistent import of $0 from library $1 and $2 in library $3"
      case .cyclicImportExport:
        return "exported definition $0 imported in a cyclic fashion in library $1"
      case .redefinitionOfImport:
        return "redefinition of imported definition $0 in library $1"
      case .unknownFile:
        return "file '$,0' unknown or a directory"
      case .unknownDirectory:
        return "directory '$,0' unknown or a file"
      case .unknownSystemDirectory:
        return "system directory type '$,0' unknown"
      case .cannotDecodeBytevector:
        return "unable to decode $0 into bytevector"
      case .cannotEncodeBytevector:
        return "unable to encode bytevector $0"
      case .invalidDateTime:
        return "invalid/incomplete date-time components: $0"
      case .invalidTimeZone:
        return "invalid time zone: $0"
      case .invalidDateStyle:
        return "invalid date-time format style: $0"
      case .invalidDefaultDrawing:
        return "invalid default drawing: $0"
      case .invalidDefaultShape:
        return "invalid default shape: $0"
      case .invalidPoint:
        return "invalid point: $0"
      case .invalidSize:
        return "invalid size: $0"
      case .invalidRect:
        return "invalid rect: $0"
      case .cannotLoadImage:
        return "cannot load image from file $0"
      case .cannotLoadImageAsset:
        return "cannot load image asset $0 of type $1 from directory $2"
      case .cannotCreateBitmap:
        return "cannot create bitmap; error in arguments: $0"
      case .invalidImageFileType:
        return "invalid image file type: $0"
      case .invalidCompositionOperation:
        return "invalid image composition operation: $0"
      case .invalidFlipOrientation:
        return "invalid flip orientation: $0"
      case .unknownInterpolateAlgorithm:
        return "unknown interpolation algorithm: $0"
      case .unsupportedGradientColorSpec:
        return "unsupported gradient color specification: $0"
      case .invalidRegexpMatchingOption:
        return "invalid regexp matching option: $0"
      case .unableToBindValue:
        return "unable to bind $0 to parameter at index $1"
      case .unknownKeyword:
        return "unknown keyword $1; cannot assign value $0"
      case .expectedKeywordArg:
        return "expected keyword argument: $0"
      case .assertion:
        return "assertion failure in $,0: $1"
      case .cannotInsertExif:
        return "cannot insert exif into $0"
      case .cannotCreateInMemoryZipArchive:
        return "cannot create in-memory zip archive"
      case .cannotMakeInMemoryZipArchiveFromData:
        return "cannot make in-memory zip archive from bytevector $0"
      case .cannotCreateZipArchive:
        return "cannot create new zip archive at $,0"
      case .cannotOpenZipArchive:
        return "cannot open zip archive at $,0"
      case .cannotMutateZipArchive:
        return "cannot mutate zip archive $0 with read-only access mode"
      case .zipArchiveEntryDoesNotExist:
        return "no entry for path $,1 in zip archive $0"
      case .unknownFileOrDirectory:
        return "unknown file or directory $,0"
    }
  }
  
  public static func assert(_ args: Arguments, count: Int) throws {
    guard args.count == count else {
      throw RuntimeError.argumentCount(min: count, max: count, args: .makeList(args))
    }
  }
  
  public static func ==(_ lhs: EvalError, _ rhs: EvalError) -> Bool {
    return lhs.rawValue == rhs.rawValue
  }
}
