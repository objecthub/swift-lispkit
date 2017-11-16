// swift-tools-version:4.0
//
//  Package.swift
//  LispKit
//
//  Build targets by calling the Swift Package Manager in the following way for debug purposes:
//  swift build -Xswiftc "-target" -Xswiftc "x86_64-apple-macosx10.11" -Xswiftc "-D" -Xswiftc "SPM"
//
//  A release can be built with these options:
//  swift build -c release -Xswiftc -static-stdlib -Xswiftc "-target" -Xswiftc "x86_64-apple-macosx10.11" -Xswiftc "-D" -Xswiftc "SPM"
//
//  Created by Matthias Zenger on 16/10/2017.
//  Copyright © 2017 ObjectHub. All rights reserved.
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
  products: [
    .library(name: "LispKit", targets: ["LispKit"]),
    .executable(name: "LispKitRepl", targets: ["LispKitRepl"])
  ],
  dependencies: [
    .package(url: "https://github.com/objecthub/swift-numberkit.git", from: "2.0.4")
  ],
  targets: [
    .target(name: "LispKit",
            dependencies: ["NumberKit"]),
    .target(name: "LispKitRepl",
            dependencies: ["LispKit"],
            exclude: ["BuildMetadata.m", "BuildMetadata.h"]),
    .testTarget(name: "LispKitTests",
                dependencies: ["LispKit"])
  ]
)
