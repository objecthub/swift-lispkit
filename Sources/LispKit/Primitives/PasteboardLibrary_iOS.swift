//
//  PasteboardLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 19/10/2024.
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
import UIKit

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
    return .makeNumber(UIPasteboard.general.changeCount)
  }
  
  private func pasteboardEmpty() throws -> Expr {
    return .makeBoolean(UIPasteboard.general.numberOfItems == 0)
  }
  
  private func pasteboardTypes() throws -> Expr {
    var res = Expr.null
    for type in UIPasteboard.general.types {
      res = .pair(.makeString(type), res)
    }
    return res
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
    return .makeBoolean(UIPasteboard.general.contains(pasteboardTypes: types))
  }
  
  private func pasteboardSet(expr: Expr, args: Arguments) throws -> Expr {
    guard let (tpe, local, expiry) = args.optional(.false, .false, .false) else {
      throw RuntimeError.argumentCount(of: "pasteboard-copy!",
                                       min: 1,
                                       max: 4,
                                       args: .pair(expr, .makeList(args)))
    }
    let type: String? = tpe.isTrue ? try tpe.asString() : nil
    let expires: Date?
    if expiry.isFalse {
      expires = nil
    } else if case .object(let obj) = expr, let dateBox = obj as? NativeDateTime {
      expires = dateBox.value.date
    } else {
      throw RuntimeError.type(expiry, expected: [NativeDateTime.type])
    }
    var values: [String : Any] = [:]
    var options: [UIPasteboard.OptionsKey : Any] = [
      .localOnly : NSNumber(booleanLiteral: local.isTrue)
    ]
    if let expires {
      options[.expirationDate] = expires as NSDate
    }
    switch expr {
      case .string(let str):
        values["public.utf8-plain-text"] = str as NSString
        values["public.plain-text"] = str as NSString
        switch type {
          case .none, "public.plain-text", "public.utf8-plain-text":
            break
          case "public.url":
            values["public.url"] = NSURL(string: str as String)
          case "public.file-url":
            if let url = str.hasPrefix("file:") ? URL(string: str as String)
                                                : URL(filePath: str as String) {
              values["public.url"] = url as NSURL
              values["public.file-url"] = url as NSURL
            } else {
              return .false
            }
          case .some(let tpe):
            values[tpe] = str as NSString
        }
      case .bytes(let bvec):
        let data = Data(bvec.value)
        values["public.data"] = data
        switch type {
          case .none, "public.data":
            break
          case .some(let tpe):
            values[tpe] = data
        }
        break
      case .object(let obj):
        if let imageBox = obj as? NativeImage {
          values["public.png"] = imageBox.value
          values["public.jpeg"] = imageBox.value
          values["com.apple.uikit.image"] = imageBox.value
          switch type {
            case .none, "public.png", "public.jpeg", "com.apple.uikit.image":
              break
            case .some(let tpe):
              values[tpe] = imageBox.value
          }
          break
        } else if let color = obj as? LispKit.Color {
          values["com.apple.uikit.color"] = color.nsColor
          switch type {
            case .none, "com.apple.uikit.color":
              break
            case .some(let tpe):
              values[tpe] = color.nsColor
          }
          break
        } else if let styledText = obj as? StyledText {
          let rtf = try styledText.value.data(from: NSMakeRange(0, styledText.value.length),
                                              documentAttributes: [.documentType : NSAttributedString.DocumentType.rtf])
          values["public.rtf"] = rtf
          values["public.rtfd"] = rtf
          values["public.utf8-plain-text"] = obj.string
          switch type {
            case .none, "public.rtf", "public.rtfd", "public.utf8-plain-text":
              break
            case "public.plain-text":
              values[type!] = obj.string
            case .some(let tpe):
              values[tpe] = rtf
          }
          break
        }
        fallthrough
      default:
        throw RuntimeError.custom("error", "cannot copy $0 to pasteboard; unsupported type", [expr])
    }
    UIPasteboard.general.setItems([values], options: options)
    return .void
  }
  
  private func pasteboardRefData(expr: Expr?) throws -> Expr {
    if let data = UIPasteboard.general.data(forPasteboardType:
                                              (try expr?.asString()) ?? "public.data") {
      let count = data.count
      var res = [UInt8](repeating: 0, count: count)
      data.copyBytes(to: &res, count: count)
      return .bytes(MutableBox(res))
    } else {
      return .false
    }
  }
  
  private func pasteboardRefString(expr: Expr?) throws -> Expr {
    if let data = UIPasteboard.general.data(forPasteboardType:
                                              (try expr?.asString()) ?? "public.plain-text"),
       let str = String(data: data, encoding: .utf8) ?? String(data: data, encoding: .utf16) {
      return .makeString(str)
    } else {
      return .false
    }
  }
  
  private func pasteboardRef(expr: Expr?) throws -> Expr {
    if let image = UIPasteboard.general.image {
      return .object(NativeImage(image))
    } else if let color = UIPasteboard.general.color {
      return .object(Color(color))
    } else if let url = UIPasteboard.general.url {
      return .makeString(url.absoluteString)
    } else if let str = UIPasteboard.general.string {
      return .makeString(str)
    } else if let data = UIPasteboard.general.data(forPasteboardType: "public.data") {
      let count = data.count
      var res = [UInt8](repeating: 0, count: count)
      data.copyBytes(to: &res, count: count)
      return .bytes(MutableBox(res))
    } else {
      return .false
    }
  }
  
  private func pasteboardClear() throws -> Expr {
    UIPasteboard.general.items = [[:]]
    return .void
  }
}
