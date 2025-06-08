//
//  LibraryRegistry.swift
//  LispKit
//
//  Created by Matthias Zenger on 24/10/2016.
//  Copyright © 2016-2022 ObjectHub. All rights reserved.
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
/// Registry of all native libraries. Libraries defined outside of the LispKit
/// framework need to be registered with the function `register`. This needs to happen
/// before the first `Context` object is created.
///
public struct LibraryRegistry {
  
  #if os(iOS) || os(watchOS) || os(tvOS)
  public private(set) static var nativeLibraries: [NativeLibrary.Type] = [
    ControlFlowLibrary.self,
    CoreLibrary.self,
    SystemLibrary.self,
    KeychainLibrary.self,
    PasteboardLibrary.self,
    DebugLibrary.self,
    DateTimeLibrary.self,
    BoxLibrary.self,
    HashTableLibrary.self,
    DynamicControlLibrary.self,
    MathLibrary.self,
    ListLibrary.self,
    TypeLibrary.self,
    VectorLibrary.self,
    GrowableVectorLibrary.self,
    BitsetLibrary.self,
    EnumLibrary.self,
    RecordLibrary.self,
    BytevectorLibrary.self,
    SerializeLibrary.self,
    CharLibrary.self,
    CharSetLibrary.self,
    StringLibrary.self,
    FormatLibrary.self,
    RegexpLibrary.self,
    PortLibrary.self,
    BaseLibrary.self,
    DrawingLibrary.self,
    DrawBarcodeLibrary.self,
    DrawMapLibrary.self,
    LocationLibrary.self,
    ImageLibrary.self,
    VisionLibrary.self,
    StyledTextLibrary.self,
    InternalLibrary.self,
    MarkdownLibrary.self,
    SQLiteLibrary.self,
    ZipArchiveLibrary.self,
    TarArchiveLibrary.self,
    CryptoLibrary.self,
    ThreadLibrary.self,
    ThreadFutureLibrary.self,
    SharedQueueLibrary.self,
    URLLibrary.self,
    JSONLibrary.self,
    JSONSchemaLibrary.self,
    HTTPLibrary.self,
    HTTPOAuthLibrary.self,
    HTTPServerLibrary.self,
    PDFLibrary.self
  ]
  #elseif os(macOS)
  public private(set) static var nativeLibraries: [NativeLibrary.Type] = [
    ControlFlowLibrary.self,
    CoreLibrary.self,
    SystemLibrary.self,
    KeychainLibrary.self,
    PasteboardLibrary.self,
    DebugLibrary.self,
    DateTimeLibrary.self,
    BoxLibrary.self,
    HashTableLibrary.self,
    DynamicControlLibrary.self,
    MathLibrary.self,
    ListLibrary.self,
    TypeLibrary.self,
    VectorLibrary.self,
    GrowableVectorLibrary.self,
    BitsetLibrary.self,
    EnumLibrary.self,
    RecordLibrary.self,
    BytevectorLibrary.self,
    SerializeLibrary.self,
    CharLibrary.self,
    CharSetLibrary.self,
    StringLibrary.self,
    FormatLibrary.self,
    RegexpLibrary.self,
    PortLibrary.self,
    BaseLibrary.self,
    SystemCallLibrary.self,
    DrawingLibrary.self,
    DrawBarcodeLibrary.self,
    DrawMapLibrary.self,
    LocationLibrary.self,
    ImageLibrary.self,
    VisionLibrary.self,
    StyledTextLibrary.self,
    InternalLibrary.self,
    MarkdownLibrary.self,
    SQLiteLibrary.self,
    ZipArchiveLibrary.self,
    TarArchiveLibrary.self,
    CryptoLibrary.self,
    ThreadLibrary.self,
    ThreadFutureLibrary.self,
    SharedQueueLibrary.self,
    URLLibrary.self,
    JSONLibrary.self,
    JSONSchemaLibrary.self,
    HTTPLibrary.self,
    HTTPOAuthLibrary.self,
    HTTPServerLibrary.self,
    PDFLibrary.self
  ]
  #endif
  
  public static func register(_ nativeLibrary: NativeLibrary.Type) {
    LibraryRegistry.nativeLibraries.append(nativeLibrary)
  }
  
  public static func register<T: Sequence>(_ nativeLibraries: T)
                                            where T.Element == NativeLibrary.Type {
    for library in nativeLibraries {
      LibraryRegistry.register(library)
    }
  }
}
