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
  case procedureType
  case parameterType
  case specialType
  case portType
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
      case .specialType:
        return "special"
      case .procedureType:
        return "procedure"
      case .parameterType:
        return "parameter"
      case .portType:
        return "port"
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
    }
  }
  
  public var included: Set<Type> {
    switch self {
      case .procedureType:
        return PROCEDURE_SUBTYPES
      case .numberType:
        return NUMBER_SUBTYPES
      case .exactNumberType:
        return EXACT_NUMBER_SUBTYPES
      case .realType:
        return REAL_SUBTYPES
      case .listType:
        return LIST_SUBTYPES
      case .properListType:
        return PROPERLIST_SUBTYPES
      case .assocListType:
        return ASSOCLIST_SUBTYPES
      case .portType:
        return PORT_SUBTYPES
      case .inputPortType:
        return INPUT_PORT_SUBTYPES
      case .outputPortType:
        return OUTPUT_PORT_SUBTYPES
      default:
        return [self]
    }
  }
  
  public func includes(_ type: Type) -> Bool {
    return self.included.contains(type)
  }
}

private let PROCEDURE_SUBTYPES: Set<Type> = [.procedureType, .parameterType]
private let NUMBER_SUBTYPES: Set<Type> = [.numberType, .byteType, .integerType, .rationalType,
                                          .floatType, .complexType, .exactNumberType, .realType]
private let EXACT_NUMBER_SUBTYPES: Set<Type> = [.exactNumberType, .byteType,
                                                .integerType, .rationalType]
private let REAL_SUBTYPES: Set<Type> = [.realType, .integerType, .rationalType, .floatType]
private let LIST_SUBTYPES: Set<Type> = [.listType, .pairType, .nullType, .properListType,
                                        .assocListType]
private let PROPERLIST_SUBTYPES: Set<Type> = [.properListType, .pairType, .nullType]
private let ASSOCLIST_SUBTYPES: Set<Type> = [.assocListType, .pairType, .nullType]
private let PORT_SUBTYPES: Set<Type> = [.portType, .inputPortType, .outputPortType,
                                        .textInputPortType, .textOutputPortType,
                                        .binaryInputPortType, .binaryOutputPortType]
private let INPUT_PORT_SUBTYPES: Set<Type> = [.inputPortType, .textInputPortType,
                                              .binaryInputPortType]
private let OUTPUT_PORT_SUBTYPES: Set<Type> = [.outputPortType, .textOutputPortType,
                                               .binaryOutputPortType]
