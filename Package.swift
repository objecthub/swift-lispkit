// swift-tools-version:5.2
//
//  Package.swift
//  LispKit
//
//  Build targets by calling the Swift Package Manager in the following way for debug purposes:
//  swift build -Xswiftc "-D" -Xswiftc "SPM"
//
//  Run REPL:
//  swift run -Xswiftc "-D" -Xswiftc "SPM"
//
//  A release can be built with these options:
//  swift build -c release -Xswiftc "-D" -Xswiftc "SPM"
//
//  This creates a release binary in .build/release/. Assumung that a LispKit directory is
//  located in ~/Documents/LispKit, the binary can be invoked like this:
//  .build/release/LispKitRepl -d LispKit
//
//
//  Created by Matthias Zenger on 16/10/2017.
//  Copyright © 2017-2021 ObjectHub. All rights reserved.
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

import PackageDescription

let package = Package(
  name: "LispKit",
  platforms: [
    .macOS(.v10_13)
  ],
  products: [
    .library(name: "LispKit", targets: ["LispKit"]),
    .library(name: "LispKitTools", targets: ["LispKitTools"]),
    .executable(name: "LispKitRepl", targets: ["LispKitRepl"])
  ],
  dependencies: [
    .package(name: "NumberKit",
             url: "https://github.com/objecthub/swift-numberkit.git",
             .upToNextMajor(from: "2.3.9")),
    .package(name: "MarkdownKit",
             url: "https://github.com/objecthub/swift-markdownkit.git",
             .upToNextMajor(from: "1.0.4")),
    .package(name: "CommandLineKit",
             url: "https://github.com/objecthub/swift-commandlinekit.git",
             .upToNextMajor(from: "0.3.3")),
    .package(name: "SQLiteExpress",
             url: "https://github.com/objecthub/swift-sqliteexpress.git",
             .upToNextMajor(from: "1.0.3")),
    .package(name: "ZIPFoundation",
             url: "https://github.com/weichsel/ZIPFoundation.git",
             .upToNextMajor(from: "0.9.12"))
  ],
  targets: [
    .target(name: "LispKit",
            dependencies: ["NumberKit", "MarkdownKit", "SQLiteExpress", "ZIPFoundation"],
            exclude: ["Graphics/Drawing_iOS.swift",
                      "Graphics/Transformation_iOS.swift"
                      "Primitives/DrawingLibrary_iOS.swift"]),
    .target(name: "LispKitTools",
            dependencies: ["LispKit", "CommandLineKit"]),
    .target(name: "LispKitRepl",
            dependencies: ["LispKit", "LispKitTools"],
            exclude: ["BuildMetadata.m", "BuildMetadata.h"]),
    .testTarget(name: "LispKitTests",
                dependencies: ["LispKit"])
  ],
  swiftLanguageVersions: [.v5]
)
