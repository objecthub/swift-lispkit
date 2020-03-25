//
//  DrawingDocument.swift
//  LispKit
//
//  Created by Matthias Zenger on 01/07/2018.
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
import Cocoa
import CoreGraphics

///
/// A `DrawingDocument` object represents a document whose pages consist of drawings.
/// For now, the main purpose of creating a `DrawingDocument` object is to save it into
/// a PDF document.
///
public final class DrawingDocument {
  
  /// The title of the collection.
  public var title: String?
  
  /// The author of the collection.
  public var author: String?
  
  /// The creator of the collection.
  public var creator: String?
  
  /// The subject of the collection.
  public var subject: String?
  
  /// Keywords describing the collection
  public var keywords: String?
  
  /// The owner's password
  public var ownerPassword: String?
  
  /// A user password
  public var userPassword: String?
  
  /// Can this collection be printed?
  public var allowsPrinting: Bool = true
  
  /// Can this collection be copied (copy/pasted)?
  public var allowsCopying: Bool = true
  
  /// Internal representation of the various pages of the drawing collection.
  public private(set) var pages: [Page]
  
  /// Initializer
  public init (title: String? = nil,
               author: String? = nil,
               creator: String? = nil,
               subject: String? = nil,
               keywords: String? = nil) {
    self.pages = []
    self.title = title
    self.author = author
    self.creator = creator
    self.subject = subject
    self.keywords = keywords
  }
  
  /// Append a new drawing to the collection.
  public func append(_ drawing: Drawing,
                     flipped: Bool = false,
                     width: Int,
                     height: Int) {
    self.pages.append(Page(drawing: drawing, flipped: flipped, width: width, height: height))
  }
  
  /// Save the collection as a PDF file to URL `url`.
  public func saveAsPDF(url: URL) -> Bool {
    // First check if we can write to the URL
    var dir: ObjCBool = false
    let parent = url.deletingLastPathComponent().path
    guard FileManager.default.fileExists(atPath: parent, isDirectory: &dir) && dir.boolValue else {
      return false
    }
    guard FileManager.default.isWritableFile(atPath: parent) else {
      return false
    }
    // Define PDF document information
    let pdfInfo: NSMutableDictionary = [
      kCGPDFContextAllowsPrinting: (self.allowsPrinting ? kCFBooleanTrue : kCFBooleanFalse) as Any,
      kCGPDFContextAllowsCopying : (self.allowsCopying ? kCFBooleanTrue : kCFBooleanFalse) as Any
    ]
    if let title = self.title {
      pdfInfo[kCGPDFContextTitle] = title
    }
    if let author = self.author {
      pdfInfo[kCGPDFContextAuthor] = author
    }
    if let creator = self.creator {
      pdfInfo[kCGPDFContextCreator] = creator
    }
    if let subject = self.subject {
      pdfInfo[kCGPDFContextSubject] = subject
    }
    if let keywords = self.keywords {
      pdfInfo[kCGPDFContextKeywords] = keywords
    }
    if let password = self.ownerPassword {
      pdfInfo[kCGPDFContextOwnerPassword] = password
    }
    if let password = self.userPassword {
      pdfInfo[kCGPDFContextUserPassword] = password
    }
    // Default media box (will be overwritten on a page by page basis)
    var mediaBox = NSRect(x: 0, y: 0, width: Double(200), height: Double(200))
    // Create a core graphics context suitable for creating PDF files
    guard let cgc = CGContext(url as CFURL, mediaBox: &mediaBox, pdfInfo as CFDictionary) else {
      return false
    }
    let previous = NSGraphicsContext.current
    defer {
      NSGraphicsContext.current = previous
    }
    for page in self.pages {
      page.createPDFPage(in: cgc)
    }
    cgc.closePDF()
    return true
  }
  
  /// Representation of a page.
  public struct Page {
    public let drawing: Drawing
    public let flipped: Bool
    public let width: Int
    public let height: Int
    
    fileprivate func createPDFPage(in cgc: CGContext) {
      var mediaBox = NSRect(x: 0, y: 0, width: Double(self.width), height: Double(self.height))
      let pageInfo: NSDictionary = [
        kCGPDFContextMediaBox : NSData(bytes: &mediaBox,
                                       length: MemoryLayout.size(ofValue: mediaBox))
      ]
      // Create a graphics context for drawing into the PDF page
      NSGraphicsContext.current = NSGraphicsContext(cgContext: cgc, flipped: self.flipped)
      // Create a new PDF page
      cgc.beginPDFPage(pageInfo as CFDictionary)
      cgc.saveGState()
      // Flip graphics if required
      if self.flipped {
        cgc.translateBy(x: 0.0, y: CGFloat(self.height))
        cgc.scaleBy(x: 1.0, y: -1.0)
      }
      // Draw the image
      self.drawing.draw()
      cgc.restoreGState()
      // Close PDF page and document
      cgc.endPDFPage()
    }
  }
}
