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
/// `Type` represents a type of an expression/value
///
public enum Type: Int, CustomStringConvertible {
  case undefinedType
  case errorType
  case voidType
  case eofType
  case nullType
  case symbolType
  case booleanType
  case byteType
  case integerType
  case exactIntegerType
  case rationalType
  case floatType
  case realType
  case complexType
  case charType
  case strType
  case byteVectorType
  case pairType
  case boxType
  case mpairType
  case vectorType
  case tableType
  case recordType
  case promiseType
  case streamType
  case valuesType
  case procedureType
  case parameterType
  case specialType
  case envType
  case portType
  case objectType
  case numberType
  case exactNumberType
  case listType
  case properListType
  case assocListType
  case inputPortType
  case outputPortType
  case textInputPortType
  case textOutputPortType
  case binaryInputPortType
  case binaryOutputPortType
  case taggedType
  case syntaxType
  case colorType
  
  public var description: String {
    switch self {
      case .undefinedType:
        return "undefined"
      case .errorType:
        return "error"
      case .voidType:
        return "void"
      case .eofType:
        return "eof"
      case .nullType:
        return "null"
      case .symbolType:
        return "symbol"
      case .booleanType:
        return "boolean"
      case .byteType:
        return "byte"
      case .integerType:
        return "integer"
      case .exactIntegerType:
        return "exact integer"
      case .rationalType:
        return "rational"
      case .floatType:
        return "float"
      case .complexType:
        return "complex"
      case .charType:
        return "character"
      case .strType:
        return "string"
      case .pairType:
        return "pair"
      case .vectorType:
        return "vector"
      case .byteVectorType:
        return "bytevector"
      case .tableType:
        return "map"
      case .recordType:
        return "record"
      case .promiseType:
        return "promise"
      case .streamType:
        return "stream"
      case .valuesType:
        return "values"
      case .specialType:
        return "special"
      case .procedureType:
        return "procedure"
      case .parameterType:
        return "parameter"
      case .envType:
        return "environment"
      case .portType:
        return "port"
      case .objectType:
        return "object"
      case .numberType:
        return "number"
      case .exactNumberType:
        return "exact number"
      case .realType:
        return "real"
      case .listType:
        return "list"
      case .properListType:
        return "proper list"
      case .assocListType:
        return "association list"
      case .textInputPortType:
        return "textual input port"
      case .textOutputPortType:
        return "textual output port"
      case .binaryInputPortType:
        return "binary input port"
      case .binaryOutputPortType:
        return "binary output port"
      case .inputPortType:
        return "input port"
      case .outputPortType:
        return "output port"
      case .boxType:
        return "box"
      case .mpairType:
        return "mpair"
      case .taggedType:
        return "tagged"
      case .syntaxType:
        return "syntax"
      case .colorType:
        return "color"
    }
  }
  
  public var included: Set<Type> {
    switch self {
      case .procedureType:
        return procedureSubtypes
      case .numberType:
        return numberSubtypes
      case .exactNumberType:
        return exactNumberSubtypes
      case .realType:
        return realSubtypes
      case .integerType:
        return integerSubtypes
      case .listType:
        return listSubtypes
      case .properListType:
        return properListSubtypes
      case .assocListType:
        return assocListSubtypes
      case .portType:
        return portSubtypes
      case .inputPortType:
        return inputPortSubtypes
      case .outputPortType:
        return outputPortSubtypes
      case .objectType:
        return objectSubtypes
      default:
        return [self]
    }
  }
  
  public func includes(_ type: Type) -> Bool {
    return self.included.contains(type)
  }
}

private let procedureSubtypes  : Set<Type> = [.procedureType,
                                              .parameterType]
private let numberSubtypes     : Set<Type> = [.numberType,
                                              .byteType,
                                              .integerType,
                                              .exactIntegerType,
                                              .rationalType,
                                              .floatType,
                                              .complexType,
                                              .exactNumberType,
                                              .realType]
private let exactNumberSubtypes: Set<Type> = [.exactNumberType,
                                              .byteType,
                                              .exactIntegerType,
                                              .rationalType]
private let realSubtypes       : Set<Type> = [.realType,
                                              .integerType,
                                              .rationalType,
                                              .floatType]
private let integerSubtypes    : Set<Type> = [.integerType,
                                              .exactIntegerType]
private let listSubtypes       : Set<Type> = [.listType,
                                              .pairType,
                                              .nullType,
                                              .properListType,
                                              .assocListType]
private let properListSubtypes : Set<Type> = [.properListType,
                                              .pairType,
                                              .nullType]
private let assocListSubtypes  : Set<Type> = [.assocListType,
                                              .pairType,
                                              .nullType]
private let portSubtypes       : Set<Type> = [.portType,
                                              .inputPortType,
                                              .outputPortType,
                                              .textInputPortType,
                                              .textOutputPortType,
                                              .binaryInputPortType,
                                              .binaryOutputPortType]
private let inputPortSubtypes  : Set<Type> = [.inputPortType,
                                              .textInputPortType,
                                              .binaryInputPortType]
private let outputPortSubtypes : Set<Type> = [.outputPortType,
                                              .textOutputPortType,
                                              .binaryOutputPortType]
private let objectSubtypes: Set<Type> = [.objectType,
                                         .colorType]
