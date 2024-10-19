//
//  PasteboardLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 18/10/2024.
//  Copyright © 2024 ObjectHub. All rights reserved.
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
import AppKit

public final class PasteboardLibrary: NativeLibrary {
  
  /// Initialize symbols
  public required init(in context: Context) throws {
    try super.init(in: context)
  }

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "system", "pasteboard"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
  }

  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("pasteboard-change-count", self.pasteboardChangeCount))
    self.define(Procedure("pasteboard-empty?", self.pasteboardEmpty))
    self.define(Procedure("pasteboard-contains?", self.pasteboardContains))
    self.define(Procedure("pasteboard-types", self.pasteboardTypes))
    self.define(Procedure("pasteboard-ref", self.pasteboardRef))
    self.define(Procedure("pasteboard-ref-string", self.pasteboardRefString))
    self.define(Procedure("pasteboard-ref-data", self.pasteboardRefData))
    self.define(Procedure("pasteboard-set!", self.pasteboardSet))
    self.define(Procedure("pasteboard-clear!", self.pasteboardClear))
  }
  
  private func pasteboardChangeCount() throws -> Expr {
    return .makeNumber(NSPasteboard.general.changeCount)
  }
  
  private func pasteboardEmpty() -> Expr {
    return .makeBoolean((NSPasteboard.general.pasteboardItems?.count ?? 0) == 0)
  }
  
  private func pasteboardTypes() -> Expr {
    if let items = NSPasteboard.general.pasteboardItems {
      var types: Set<String> = []
      for item in items {
        for type in item.types {
          let uti = type.rawValue
          if !uti.hasPrefix("dyn.") {
            types.insert(type.rawValue)
          }
        }
      }
      var res = Expr.null
      for type in types {
        res = .pair(.makeString(type), res)
      }
      return res
    } else {
      return .null
    }
  }
  
  private func pasteboardContains(expr: Expr) throws -> Expr {
    var types: [String] = []
    if case .string(let str) = expr {
      types.append(str as String)
    } else {
      var list = expr
      while case .pair(let type, let rest) = list {
        types.append(try type.asString())
        list = rest
      }
      guard case .null = list else {
        throw RuntimeError.type(expr, expected: [.properListType])
      }
    }
    return .makeBoolean(NSPasteboard.general.canReadItem(withDataConformingToTypes: types))
  }
  
  private func write(image: NSImage, as tpe: String) -> Bool {
    var pasteboardType: NSPasteboard.PasteboardType
    var type: NSBitmapImageRep.FileType
    switch tpe {
      case "public.tiff":
        pasteboardType = .tiff
        type = .tiff
      case "public.png":
        pasteboardType = .png
        type = .png
      case "public.jpeg":
        pasteboardType = NSPasteboard.PasteboardType(tpe)
        type = .jpeg
      case "public.jpeg-2000":
        pasteboardType = NSPasteboard.PasteboardType(tpe)
        type = .jpeg2000
      case "public.gif":
        pasteboardType = NSPasteboard.PasteboardType(tpe)
        type = .gif
      case "public.bmp":
        pasteboardType = NSPasteboard.PasteboardType(tpe)
        type = .bmp
      default:
        return false
    }
    for repr in image.representations {
      if let bitmapRepr = repr as? NSBitmapImageRep {
        if NSPasteboard.general.setData(bitmapRepr.representation(using: type, properties: [:]),
                                        forType: pasteboardType) {
          return true
        }
      }
    }
    return false
  }
  
  private func pasteboardSet(expr: Expr, args: Arguments) throws -> Expr {
    guard let (tpe, local, expiry) = args.optional(.false, .false, .false) else {
      throw RuntimeError.argumentCount(of: "pasteboard-set!",
                                       min: 1,
                                       max: 4,
                                       args: .pair(expr, .makeList(args)))
    }
    let type: String? = tpe.isTrue ? try tpe.asString() : nil
    if expiry.isFalse {
      // nothing to do
    } else if case .object(let obj) = expr, obj is NativeDateTime {
      // nothing to do
    } else {
      throw RuntimeError.type(expiry, expected: [NativeDateTime.type])
    }
    if local.isTrue {
      NSPasteboard.general.prepareForNewContents(with: .currentHostOnly)
    } else {
      NSPasteboard.general.prepareForNewContents()
    }
    switch expr {
      case .string(let str):
        var values: [any NSPasteboardWriting] = [NSString(string: str)]
        switch type {
          case .none, "public.utf8-plain-text", "public.plain-text":
            NSPasteboard.general.writeObjects(values)
            NSPasteboard.general.setString(str as String,
                                           forType: NSPasteboard.PasteboardType("public.plain-text"))
          case "public.url":
            if let url = URL(string: str as String) {
              values.append(url as NSURL)
            } else {
              return .false
            }
            NSPasteboard.general.writeObjects(values)
            NSPasteboard.general.setString(str as String,
                                           forType: NSPasteboard.PasteboardType("public.plain-text"))
          case "public.file-url":
            if let url = str.hasPrefix("file:") ? URL(string: str as String)
                                                : URL(filePath: str as String) {
              values.append(url as NSURL)
            } else {
              return .false
            }
            NSPasteboard.general.writeObjects(values)
            NSPasteboard.general.setString(str as String,
                                           forType: NSPasteboard.PasteboardType("public.plain-text"))
            NSPasteboard.general.setString(str as String,
                                           forType: NSPasteboard.PasteboardType("public.url"))
          case .some(let tpe):
            NSPasteboard.general.writeObjects(values)
            NSPasteboard.general.setString(str as String,
                                           forType: NSPasteboard.PasteboardType("public.plain-text"))
            NSPasteboard.general.setString(str as String,
                                           forType: NSPasteboard.PasteboardType(tpe))
        }
      case .bytes(let bvec):
        let data = Data(bvec.value)
        NSPasteboard.general.setData(data, forType: NSPasteboard.PasteboardType("public.data"))
        switch type {
          case .none, "public.data":
            break
          case .some(let tpe):
            NSPasteboard.general.setData(data, forType: NSPasteboard.PasteboardType(tpe))
        }
      case .object(let obj):
        if let imageBox = obj as? NativeImage {
          NSPasteboard.general.writeObjects([imageBox.value])
          switch type {
            case .none:
              break
            case .some("public.tiff"):
              NSPasteboard.general.setData(imageBox.value.tiffRepresentation, forType: .tiff)
            case .some(let tpe):
              _ = self.write(image: imageBox.value, as: tpe)
          }
        } else if let color = obj as? LispKit.Color {
          NSPasteboard.general.writeObjects([color.nsColor])
        } else if let styledText = obj as? StyledText {
          NSPasteboard.general.writeObjects([styledText.value])
          switch type {
            case .none:
              break
            case .some(let tpe):
              let rtf = try styledText.value.data(
                from: NSMakeRange(0, styledText.value.length),
                documentAttributes: [.documentType : NSAttributedString.DocumentType.rtf])
              NSPasteboard.general.setData(rtf, forType: NSPasteboard.PasteboardType(tpe))
          }
        }
      default:
        throw RuntimeError.custom("error", "cannot copy $0 to pasteboard; unsupported type", [expr])
    }
    return .void
  }
  
  private func pasteboardRefData(expr: Expr?) throws -> Expr {
    if let data = NSPasteboard.general.data(forType:
                    NSPasteboard.PasteboardType(rawValue: (try expr?.asString()) ?? "public.data")) {
      let count = data.count
      var res = [UInt8](repeating: 0, count: count)
      data.copyBytes(to: &res, count: count)
      return .bytes(MutableBox(res))
    } else {
      return .false
    }
  }
  
  private func pasteboardRefString(expr: Expr?) throws -> Expr {
    if let str = NSPasteboard.general.string(forType:
                   NSPasteboard.PasteboardType(rawValue: (try expr?.asString()) ?? "public.plain-text")) {
      return .makeString(str)
    } else {
      return .false
    }
  }
  
  private func pasteboardRef() throws -> Expr {
    if let objects = NSPasteboard.general.readObjects(forClasses: [NSImage.self,
                                                                   NSColor.self,
                                                                   NSURL.self,
                                                                   NSAttributedString.self]) {
      for object in objects {
        if let image = object as? NSImage {
          return .object(NativeImage(image))
        } else if let color = object as? NSColor {
          return .object(Color(color))
        } else if let url = object as? NSURL, let str = url.absoluteString {
          return .makeString(str)
        } else if let str = object as? NSAttributedString,
                  let mstr = str.mutableCopy() as? NSMutableAttributedString {
          return .object(StyledText(mstr))
        }
      }
    }
    if let obj = NSPasteboard.general.readObjects(forClasses: [NSString.self])?.first,
       let str = obj as? NSString {
      return .makeString(str as String)
    }
    return .false
  }
  
  private func pasteboardClear() throws -> Expr {
    NSPasteboard.general.clearContents()
    return .void
  }
}
