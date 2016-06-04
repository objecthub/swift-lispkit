//
//  Type.swift
//  LispKit
//
//  Created by Matthias Zenger on 30/12/2015.
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
/// Represents a type of an expression/value
///
public enum Type: Int, CustomStringConvertible {
  case UndefinedType
  case ErrorType
  case VoidType
  case EofType
  case NullType
  case SymbolType
  case BooleanType
  case ByteType
  case IntegerType
  case RationalType
  case FloatType
  case RealType
  case ComplexType
  case CharType
  case StrType
  case PairType
  case VectorType
  case ByteVectorType
  case PromiseType
  case SpecialType
  case ProcedureType
  case ThunkType
  case SyntaxType
  case NumberType
  case ExactNumberType
  case ListType
  case ProperListType
  case AssocListType
  
  public var description: String {
    switch self {
      case UndefinedType:
        return "undefined"
      case ErrorType:
        return "error"
      case VoidType:
        return "void"
      case EofType:
        return "eof"
      case NullType:
        return "null"
      case SymbolType:
        return "symbol"
      case BooleanType:
        return "boolean"
      case ByteType:
        return "byte"
      case IntegerType:
        return "integer"
      case RationalType:
        return "rational"
      case FloatType:
        return "float"
      case ComplexType:
        return "complex"
      case CharType:
        return "character"
      case StrType:
        return "string"
      case PairType:
        return "pair"
      case VectorType:
        return "vector"
      case ByteVectorType:
        return "bytevector"
      case PromiseType:
        return "promise"
      case SpecialType:
        return "special"
      case ProcedureType:
        return "procedure"
      case ThunkType:
        return "thunk"
      case SyntaxType:
        return "syntax"
      case NumberType:
        return "number"
      case ExactNumberType:
        return "exact number"
      case RealType:
        return "real"
      case ListType:
        return "list"
      case ProperListType:
        return "proper list"
      case AssocListType:
        return "association list"
    }
  }
  
  public var included: Set<Type> {
    switch self {
      case NumberType:
        return NUMBER_SUBTYPES
      case ExactNumberType:
        return EXACT_NUMBER_SUBTYPES
      case RealType:
        return REAL_SUBTYPES
      case ListType:
        return LIST_SUBTYPES
      case ProperListType:
        return PROPERLIST_SUBTYPES
      case AssocListType:
        return ASSOCLIST_SUBTYPES
      default:
        return [self]
    }
  }
  
  public func includes(type: Type) -> Bool {
    switch self {
      case NumberType:
        return NUMBER_SUBTYPES.contains(type)
      case ExactNumberType:
        return EXACT_NUMBER_SUBTYPES.contains(type)
      case RealType:
        return REAL_SUBTYPES.contains(type)
      case ListType:
        return LIST_SUBTYPES.contains(type)
      case ProperListType:
        return PROPERLIST_SUBTYPES.contains(type)
      case AssocListType:
        return ASSOCLIST_SUBTYPES.contains(type)
      default:
        return self == type
    }
  }
}

private let NUMBER_SUBTYPES: Set<Type> = [
  .ByteType, .IntegerType, .RationalType, .FloatType, .ComplexType
]
private let EXACT_NUMBER_SUBTYPES: Set<Type> = [.ByteType, .IntegerType, .RationalType]
private let REAL_SUBTYPES: Set<Type> = [.IntegerType, .RationalType, .FloatType]
private let LIST_SUBTYPES: Set<Type> = [.PairType, .NullType]
private let PROPERLIST_SUBTYPES = LIST_SUBTYPES
private let ASSOCLIST_SUBTYPES = LIST_SUBTYPES

