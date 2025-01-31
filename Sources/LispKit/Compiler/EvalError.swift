//
//  EvalError.swift
//  LispKit
//
//  Created by Matthias Zenger on 01/03/2018.
//  Copyright © 2018-2022 ObjectHub. All rights reserved.
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
public enum EvalError: Int, Hashable, Codable {
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
  case listTooLong
  case duplicateBinding
  case notBoundInEnvironment
  case bindingImmutable
  case nonApplicativeValue
  case executeEmptyList
  case illegalRadix
  case argumentError
  case noMatchingCase
  case procedureWithoutTag
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
  case enumTypeEmpty
  case invalidEnumValue
  case unknownEnumValue
  case invalidEnumSpecifier
  case invalidEnumSet
  case incompatibleEnumSetTypes
  case enumNotMatchingEnumSetType
  case enumNotMatchingEnumSet
  case malformedLibraryDefinition
  case malformedLibraryName
  case uninitializedExports
  case inconsistentImports
  case cyclicImportExport
  case corruptLibrary
  case redefinitionOfImport
  case stackOverflow
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
  case cannotCreateImage
  case cannotCreateBitmap
  case imageIsNotABitmap
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
  case zipEntryTooLarge
  case tarArchiveEntryDoesNotExist
  case cannotWriteTarArchive
  case unknownFileOrDirectory
  case abandonedMutex
  case mutexUseInInvalidContext
  case attemptedToStartNonNewThread
  case joinTimedOut
  case joinWithMainThread
  case joinWithItself
  case threadJoinInInvalidContext
  case expectedUncaughtException
  case tooManyThreads
  case threadTerminated
  case firstArgOfProcViolation
  case secondArgOfProcViolation
  case unknownTextAlignment
  case unknownTextFitMode
  case unknownWritingDirection
  case unknownParagraphStyleAttribute
  case expectedParagraphStyleAttributeKeyword
  case unknownTextStyleAttribute
  case expectedTextStyleAttributeKeyword
  case invalidShadowSpec
  case cannotMakeStyledText
  case textStyleNotApplicableToImage
  case unsupportedDocType
  case expectedTextBlockStyleAttribute
  case unknownTextBlockStyleAttribute
  case unsupportedCryptoSystem
  case unsupportedCryptoAlgorithm
  case keyDoesNotSupportAlgorithm
  case controlStringMissing
  case invalidDefaultFormatConfig
  case invalidControlSpec
  case cannotConvertToJSON
  case jsonReferenceExpected
  case invalidJSONPatchOp
  case unableToCreateJSONPatch
  case unsupportedJSONSchemaDialect
  case undefinedDefaultSchemaRegistry
  case settingFutureValueTwice
  case unknownCachePolicy
  case undefinedDefaultHttpSession
  case invalidHttpHeaderSpec
  case unableToReturnResultViaFuture
  case serverError
  case visionError
  case unsupportedHttpMethod
  case insertIntoClosedQueue
  case insertIntoMaxQueue
  case queueIsEmpty
  case illegalArithForAtomicBox
  case cannotSerialize
  case expectedAccessibilitySpecifier
  case illegalPolicySpecifier
  case unsupportedBarcodeSetting
  case oauthError
  case urlFormatConstraintError
  case urlAuthorityError
  case urlPrototypeInvalid
  case cannotCreatePdf
  case invalidPDFAccessIdentifier
  case invalidPDFDocGenerationOption
  case unknownPDFAnnotationType
  case invalidPDFBorder
  case invalidPDFPadding
  case invalidPDFCalloutPoints
  case cannotMapAttributeValue
  case invalidSymbolicEnumValue
  case invalidFilterParameterValue
  case unknownFilterParameter
  case invalidAutoAdjustmentOption
  
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
      case .listTooLong:
        return "argument list too long; overhead: $0"
      case .duplicateBinding:
        return "symbol $0 bound multiple times in $1"
      case .notBoundInEnvironment:
        return "symbol $0 not bound in environment"
      case .bindingImmutable:
        return "unable to assign value to immutable binding for symbol $0"
      case .nonApplicativeValue:
        return "cannot apply arguments to $0"
      case .executeEmptyList:
        return "cannot execute empty application"
      case .illegalRadix:
        return "illegal radix: $0"
      case .argumentError:
        return "wrong number of arguments for $0: $1"
      case .noMatchingCase:
        return "arguments $0 not matching any case of $1"
      case .procedureWithoutTag:
        return "procedure does not have a tag"
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
        return "expected record of type $0 to access field but received $1"
      case .fieldCountError:
        return "expected values for $0 fields, received instead: $1"
      case .enumTypeEmpty:
        return "enum-types must have at least one enum value"
      case .invalidEnumValue:
        return "not a valid enum: $0"
      case .unknownEnumValue:
        return "$1 not referring to an enum of type $0"
      case .invalidEnumSpecifier:
        return "not a valid enum specifier: $0"
      case .invalidEnumSet:
        return "not a valid enum set: $0"
      case .incompatibleEnumSetTypes:
        return "enum set $0 incompatible to enum set $1"
      case .enumNotMatchingEnumSetType:
        return "enum $0 not matching type $1 of enum set"
      case .enumNotMatchingEnumSet:
        return "enum $0 not matching enum set $1"
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
      case .corruptLibrary:
        return "internal state of library $0 is corrupt ($,1)"
      case .redefinitionOfImport:
        return "redefinition of imported definition $0 in library $1"
      case .stackOverflow:
        return "stack overflow"
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
      case .cannotCreateImage:
        return "cannot create image from bytevector $0"
      case .cannotCreateBitmap:
        return "cannot create bitmap; error in arguments: $0"
      case .imageIsNotABitmap:
        return "expected a bitmap; provided image is not a bitmap: $0"
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
      case .zipEntryTooLarge:
        return "zip entry too large: $0"
      case .tarArchiveEntryDoesNotExist:
        return "no entry for path $,1 in tar archive $0"
      case .cannotWriteTarArchive:
        return "cannot write tar archive $0; missing file path"
      case .unknownFileOrDirectory:
        return "unknown file or directory $,0"
      case .abandonedMutex:
        return "abandoned mutex was used in a lock-mutex! operation: $0"
      case .mutexUseInInvalidContext:
        return "mutex use in invalid context: $0"
      case .attemptedToStartNonNewThread:
        return "attempted to start a thread that is not new: $0"
      case .joinTimedOut:
        return "thread-join! timed out, waiting for thread $0 to terminate"
      case .joinWithMainThread:
        return "unsupported join with main thread"
      case .joinWithItself:
        return "attempted to join thread with itself: $0"
      case .threadJoinInInvalidContext:
        return "thread join in invalid context with $0"
      case .expectedUncaughtException:
        return "expected an uncaught exception instead of $0"
      case .tooManyThreads:
        return "too many threads; reached limit of \(EvalThread.maxThreads) runnable threads"
      case .threadTerminated:
        return "terminated thread"
      case .firstArgOfProcViolation:
        return "first argument $0 of $,1 violates requirement: $0 $,2"
      case .secondArgOfProcViolation:
        return "second argument $0 of $,1 violates requirement: $0 $,2"
      case .unknownTextAlignment:
        return "unknown text alignment: $0"
      case .unknownTextFitMode:
        return "unknown text fit mode: $0"
      case .unknownWritingDirection:
        return "unknown writing direction: $0"
      case .unknownParagraphStyleAttribute:
        return "unknown paragraph style attribute: $0"
      case .expectedParagraphStyleAttributeKeyword:
        return "expected a paragraph style attribute keyword, but received: $0"
      case .unknownTextStyleAttribute:
        return "unknown text style attribute: $0"
      case .expectedTextStyleAttributeKeyword:
        return "expected a text style attribute keyword, but received: $0"
      case .invalidShadowSpec:
        return "invalid shadow specification: $0"
      case .cannotMakeStyledText:
        return "cannot create a styled text from $0"
      case .textStyleNotApplicableToImage:
        return "text styles are not applicable to image $0 and must be dropped"
      case .unsupportedDocType:
        return "unsupported document type: $0"
      case .expectedTextBlockStyleAttribute:
        return "expected a text block style attribute keyword, but received: $0"
      case .unknownTextBlockStyleAttribute:
        return "unknown text block style attribute: $0"
      case .unsupportedCryptoSystem:
        return "unsupported crypto system: $0"
      case .unsupportedCryptoAlgorithm:
        return "unsupported crypto algorithm: $0"
      case .keyDoesNotSupportAlgorithm:
        return "secure key $0 does not support $,2 via algorithm $1"
      case .controlStringMissing:
        return "control string missing in format arguments"
      case .invalidDefaultFormatConfig:
        return "invalid default format configuration: $0"
      case .invalidControlSpec:
        return "invalid formatting control specifier: $0"
      case .cannotConvertToJSON:
        return "cannot convert $0 to JSON"
      case .jsonReferenceExpected:
        return "expected JSON reference but found $0"
      case .invalidJSONPatchOp:
        return "invalid JSON patch operation: $0"
      case .unableToCreateJSONPatch:
        return "unable to create JSON patch from $0"
      case .unsupportedJSONSchemaDialect:
        return "unsupported JSON schema dialect: $0"
      case .undefinedDefaultSchemaRegistry:
        return "undefined default JSON schema registry"
      case .settingFutureValueTwice:
        return "trying to set value of future $0 twice; attempted to assign $1"
      case .unknownCachePolicy:
        return "unknown HTTP request cache policy: $0"
      case .undefinedDefaultHttpSession:
        return "undefined default http session"
      case .invalidHttpHeaderSpec:
        return "invalid HTTP header specification starting $1 in $0"
      case .unableToReturnResultViaFuture:
        return "unable to return result via future $0: $1"
      case .serverError:
        return "server error: did not receive data"
      case .visionError:
        return "vision handling error: did not receive a result for $0"
      case .unsupportedHttpMethod:
        return "unsupported HTTP method: $0"
      case .insertIntoClosedQueue:
        return "trying to insert elements from $0 into closed queue $1"
      case .insertIntoMaxQueue:
        return "trying to insert elements from $0 into queue $1 of maximum length"
      case .queueIsEmpty:
        return "cannot complete operation since queue $0 is empty"
      case .illegalArithForAtomicBox:
        return "cannot apply arithmetic updates to atomic box $0"
      case .cannotSerialize:
        return "unable to serialize value: $0"
      case .expectedAccessibilitySpecifier:
        return "expected accessibility specifier but received $0"
      case .illegalPolicySpecifier:
        return "illegal authentication policy specifier: $0"
      case .unsupportedBarcodeSetting:
        return "unsupported barcode setting key: $0"
      case .oauthError:
        return "oauth2 error $0: $,1"
      case .urlFormatConstraintError:
        return "not a valid URL format constraint: $0"
      case .urlAuthorityError:
        return "invalid URL authority specifier: $0"
      case .urlPrototypeInvalid:
        return "$0 is not a valid URL prototype for procedure $,1"
      case .cannotCreatePdf:
        return "cannot create pdf document from bytevector $0"
      case .invalidPDFAccessIdentifier:
        return "invalid PDF access permission identifier: $0"
      case .invalidPDFDocGenerationOption:
        return "invalid PDF document generation option: $0"
      case .unknownPDFAnnotationType:
        return "unknown PDF annotation type: $0"
      case .invalidPDFBorder:
        return "invalid PDF border specifier: $0"
      case .invalidPDFPadding:
        return "invalid specification of PDF annotation padding: $0"
      case .invalidPDFCalloutPoints:
        return "a PDF callout is specified with a list of 2 or 3 points; instead $0 was provided"
      case .cannotMapAttributeValue:
        return "unable to interpret attribute value: $0"
      case .invalidSymbolicEnumValue:
        return "invalid ,$0: $1"
      case .invalidFilterParameterValue:
        return "invalid value for filter argument $0: $1"
      case .unknownFilterParameter:
        return "trying to set unknown image filter argument $0 to $1"
      case .invalidAutoAdjustmentOption:
        return "invalid auto adjustment option key: $0"
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
