//
//  PDFLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 04/12/2024.
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
import CoreGraphics
import PDFKit

///
/// Box library: based on Racket spec.
///
public final class PDFLibrary: NativeLibrary {
  
  // Action types
  public let goto: Symbol
  public let gotoRemote: Symbol
  public let gotoURL: Symbol
  public let perform: Symbol
  public let resetFields: Symbol
  public let resetFieldsExcept: Symbol
  public let none: Symbol
  public let find: Symbol
  public let goBack: Symbol
  public let goForward: Symbol
  public let gotoPage: Symbol
  public let firstPage: Symbol
  public let lastPage: Symbol
  public let nextPage: Symbol
  public let previousPage: Symbol
  public let print: Symbol
  public let zoomIn: Symbol
  public let zoomOut: Symbol
  
  // Annotation types
  public let circle: Symbol
  public let freeText: Symbol
  public let highlight: Symbol
  public let ink: Symbol
  public let link: Symbol
  public let popup: Symbol
  public let square: Symbol
  public let stamp: Symbol
  public let strikeOut: Symbol
  public let text: Symbol
  public let underline: Symbol
  public let widget: Symbol
  
  // Display boxes
  private let displayBox: SymbolTable.Enumeration<PDFDisplayBox>
  
  // Line styles
  private let lineStyle: SymbolTable.Enumeration<PDFLineStyle>
  
  // Text alignment
  private let textAlignment: SymbolTable.Enumeration<NSTextAlignment>
  
  // Text annotation icon type
  private let iconType: SymbolTable.Enumeration<PDFTextAnnotationIconType>
  
  // Markup type
  private let markupType: SymbolTable.Enumeration<PDFMarkupType>
  
  // Text intent
  private let callout: Symbol
  private let typeWriter: Symbol
  
  public let pdfDocumentDelegate = LispKitPDFDocumentDelegate()
  
  /// Initialize symbols
  public required init(in context: Context) throws {
    self.goto = context.symbols.intern("goto")
    self.gotoRemote = context.symbols.intern("goto-remote")
    self.gotoURL = context.symbols.intern("goto-url")
    self.perform = context.symbols.intern("perform")
    self.resetFields = context.symbols.intern("reset-fields")
    self.resetFieldsExcept = context.symbols.intern("reset-fields-except")
    self.none = context.symbols.intern("none")
    self.find = context.symbols.intern("find")
    self.goBack = context.symbols.intern("go-back")
    self.goForward = context.symbols.intern("go-forward")
    self.gotoPage = context.symbols.intern("goto-page")
    self.firstPage = context.symbols.intern("first-page")
    self.lastPage = context.symbols.intern("last-page")
    self.nextPage = context.symbols.intern("next-page")
    self.previousPage = context.symbols.intern("previous-page")
    self.print = context.symbols.intern("print")
    self.zoomIn = context.symbols.intern("zoom-in")
    self.zoomOut = context.symbols.intern("zoom-out")
    self.circle = context.symbols.intern("circle")
    self.freeText = context.symbols.intern("free-text")
    self.highlight = context.symbols.intern("highlight")
    self.ink = context.symbols.intern("shape")
    self.link = context.symbols.intern("link")
    self.popup = context.symbols.intern("popup")
    self.square = context.symbols.intern("square")
    self.stamp = context.symbols.intern("stamp")
    self.strikeOut = context.symbols.intern("strike-out")
    self.text = context.symbols.intern("text")
    self.underline = context.symbols.intern("underline")
    self.widget = context.symbols.intern("widget")
    self.callout = context.symbols.intern("callout")
    self.typeWriter = context.symbols.intern("type-writer")
    self.displayBox = context.symbols.Enum("PDF display box", .mediaBox) {
      ("media-box", .mediaBox)
      ("crop-box", .cropBox)
      ("bleed-box", .bleedBox)
      ("trim-box", .trimBox)
      ("art-box", .artBox)
    }
    self.lineStyle = context.symbols.Enum("PDF line style", PDFLineStyle.none) {
      ("none", .none)
      ("square", .square)
      ("circle", .circle)
      ("diamond", .diamond)
      ("open-arrow", .openArrow)
      ("closed-arrow", .closedArrow)
    }
    self.textAlignment = context.symbols.Enum("text alignment", .natural) {
      ("left", .left)
      ("right", .right)
      ("center", .center)
      ("justified", .justified)
      ("natural", .natural)
    }
    self.iconType = context.symbols.Enum("PDF text annotation icon typ", .comment) {
      ("comment", .comment)
      ("key", .key)
      ("note", .note)
      ("help", .help)
      ("new-paragraph", .newParagraph)
      ("paragraph", .paragraph)
      ("insert", .insert)
    }
    self.markupType = context.symbols.Enum("PDF text markup type", .highlight) {
      ("highlight", .highlight)
      ("strike-out", .strikeOut)
      ("underline", .underline)
      ("redact", .redact)
    }
    try super.init(in: context)
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "pdf"]
  }
  
  /// Dependencies of the library.
  public override func dependencies() {
  }
  
  /// Declarations of the library.
  public override func declarations() {
    
    // Paper sizes
    self.define("letter-size", as: .pair(.flonum(612.0), .flonum(792.0)))
    self.define("legal-size", as: .pair(.flonum(612.0), .flonum(1008.0)))
    self.define("executive-size", as: .pair(.flonum(521.86), .flonum(756.0)))
    self.define("a2-size", as: .pair(.flonum(1190.55), .flonum(1683.78)))
    self.define("a3-size", as: .pair(.flonum(841.89), .flonum(1190.55)))
    self.define("a4-size", as: .pair(.flonum(595.28), .flonum(841.89)))
    self.define("a5-size", as: .pair(.flonum(419.53), .flonum(595.28)))
    self.define("a6-size", as: .pair(.flonum(297.64), .flonum(419.53)))
    self.define("b3-size", as: .pair(.flonum(1000.63), .flonum(1417.32)))
    self.define("b4-size", as: .pair(.flonum(708.66), .flonum(1000.63)))
    self.define("b5-size", as: .pair(.flonum(498.90), .flonum(708.66)))
    self.define("b6-size", as: .pair(.flonum(354.33), .flonum(498.90)))
    self.define("c3-size", as: .pair(.flonum(918.43), .flonum(1298.27)))
    self.define("c4-size", as: .pair(.flonum(649.13), .flonum(918.43)))
    self.define("c5-size", as: .pair(.flonum(459.21), .flonum(649.13)))
    self.define("c6-size", as: .pair(.flonum(323.15), .flonum(459.21)))
    
    // PDF documents
    self.define(Procedure("pdf?", self.isPdf))
    self.define(Procedure("pdf-encrypted?", self.isPdfEncrypted))
    self.define(Procedure("pdf-locked?", self.isPdfLocked))
    self.define(Procedure("make-pdf", self.makePdf))
    self.define(Procedure("bytevector->pdf", self.bytevectorToPdf))
    self.define(Procedure("load-pdf", self.loadPdf))
    self.define(Procedure("save-pdf", self.savePdf))
    self.define(Procedure("pdf-unlock", self.pdfUnlock))
    self.define(Procedure("pdf->string", self.pdfToString))
    self.define(Procedure("pdf->bytevector", self.pdfToBytevector))
    self.define(Procedure("pdf-path", self.pdfPath))
    self.define(Procedure("pdf-version", self.pdfVersion))
    self.define(Procedure("pdf-attributes", self.pdfAttributes))
    self.define(Procedure("pdf-attribute-ref", self.pdfAttributeRef))
    self.define(Procedure("pdf-attribute-set!", self.pdfAttributeSet))
    self.define(Procedure("pdf-attribute-remove!", self.pdfAttributeRemove))
    self.define(Procedure("pdf-access-permissions", self.pdfPermissions))
    
    self.define(Procedure("pdf-page-count", self.pdfPageCount))
    self.define(Procedure("pdf-pages", self.pdfPages))
    self.define(Procedure("pdf-page", self.pdfPage))
    self.define(Procedure("pdf-insert-page!", self.pdfInsertPage))
    self.define(Procedure("pdf-remove-page!", self.pdfRemovePage))
    self.define(Procedure("pdf-swap-page!", self.pdfSwapPage))
    self.define(Procedure("pdf-page-index", self.pdfPageIndex))
    
    self.define(Procedure("pdf-outline", self.pdfOutline))
    
    // Predicates
    self.define(Procedure("pdf-display-box?", self.isDisplayBox))
    self.define(Procedure("pdf-line-style?", self.isLineStyle))
    self.define(Procedure("pdf-text-alignment?", self.isTextAlignment))
    self.define(Procedure("pdf-icon-type?", self.isIconType))
    self.define(Procedure("pdf-markup-type?", self.isMarkupType))
    
    // PDF pages
    self.define(Procedure("pdf-page?", self.isPdfPage))
    self.define(Procedure("make-pdf-page", self.makePdfPage))
    self.define(Procedure("pdf-page-copy", self.pdfPageCopy))
    self.define(Procedure("pdf-page-document", self.pdfPageDocument))
    self.define(Procedure("pdf-page-number", self.pdfPageNumber))
    self.define(Procedure("pdf-page-label", self.pdfPageLabel))
    self.define(Procedure("pdf-page-bounds", self.pdfPageBounds))
    self.define(Procedure("pdf-page-bounds-set!", self.pdfPageBoundsSet))
    self.define(Procedure("pdf-page-rotation", self.pdfPageRotation))
    self.define(Procedure("pdf-page-rotation-set!", self.pdfPageRotationSet))
    self.define(Procedure("pdf-page-annotations-display", self.pdfPageAnnotationsDisplay))
    self.define(Procedure("pdf-page-annotations-display-set!", self.pdfPageAnnotationsDisplaySet))
    self.define(Procedure("pdf-page-annotations", self.pdfPageAnnotations))
    self.define(Procedure("pdf-page-annotation-ref", self.pdfPageAnnotationRef))
    self.define(Procedure("pdf-page-annotation-add!", self.pdfPageAnnotationAdd))
    self.define(Procedure("pdf-page-annotation-remove!", self.pdfPageAnnotationRemove))
    self.define(Procedure("pdf-page-images", self.pdfPageImages))
    self.define(Procedure("pdf-page-thumbnail", self.pdfPageThumbnail))
    self.define(Procedure("pdf-page->bitmap", self.pdfPageToBitmap))
    self.define(Procedure("pdf-page->string", self.pdfPageToString))
    self.define(Procedure("pdf-page->styled-text", self.pdfPageToStyledText))
    self.define(Procedure("pdf-page->bytevector", self.pdfPageToBytevector))
    self.define(Procedure("draw-pdf-page", self.drawPdfPage))
    self.define(Procedure("pdf-page-underlay", self.pdfPageUnderlay))
    self.define(Procedure("pdf-page-underlay-set!", self.pdfPageUnderlaySet))
    self.define(Procedure("pdf-page-overlay", self.pdfPageOverlay))
    self.define(Procedure("pdf-page-overlay-set!", self.pdfPageOverlaySet))
    
    // PDF outlines
    self.define(Procedure("pdf-outline?", self.isPdfOutline))
    self.define(Procedure("make-pdf-outline", self.makePdfOutline))
    self.define(Procedure("pdf-outline-document", self.pdfOutlineDocument))
    self.define(Procedure("pdf-outline-parent", self.pdfOutlineParent))
    self.define(Procedure("pdf-outline-index", self.pdfOutlineIndex))
    self.define(Procedure("pdf-outline-label", self.pdfOutlineLabel))
    self.define(Procedure("pdf-outline-label-set!", self.pdfOutlineLabelSet))
    self.define(Procedure("pdf-outline-destination", self.pdfOutlineDestination))
    self.define(Procedure("pdf-outline-destination-set!", self.pdfOutlineDestinationSet))
    self.define(Procedure("pdf-outline-action", self.pdfOutlineAction))
    self.define(Procedure("pdf-outline-action-set!", self.pdfOutlineActionSet))
    self.define(Procedure("pdf-outline-open?", self.pdfOutlineOpen))
    self.define(Procedure("pdf-outline-open-set!", self.pdfOutlineOpenSet))
    self.define(Procedure("pdf-outline-child-count", self.pdfOutlineChildCount))
    self.define(Procedure("pdf-outline-child-ref", self.pdfOutlineChildRef))
    self.define(Procedure("pdf-outline-child-insert!", self.pdfOutlineChildInsert))
    self.define(Procedure("pdf-outline-child-remove!", self.pdfOutlineChildRemove))
    
    // PDF annotations
    self.define(Procedure("pdf-annotation?", self.isPdfAnnotation))
    self.define(Procedure("make-pdf-annotation", self.makePdfAnnotation))
    self.define(Procedure("pdf-annotation-page", self.pdfAnnotationPage))
    self.define(Procedure("pdf-annotation-type", self.pdfAnnotationType))
    self.define(Procedure("pdf-annotation-name", self.pdfAnnotationName))
    self.define(Procedure("pdf-annotation-name-set!", self.pdfAnnotationNameSet))
    self.define(Procedure("pdf-annotation-bounds", self.pdfAnnotationBounds))
    self.define(Procedure("pdf-annotation-bounds-set!", self.pdfAnnotationBoundsSet))
    self.define(Procedure("pdf-annotation-padding", self.pdfAnnotationPadding))
    self.define(Procedure("pdf-annotation-padding-set!", self.pdfAnnotationPaddingSet))
    self.define(Procedure("pdf-annotation-border", self.pdfAnnotationBorder))
    self.define(Procedure("pdf-annotation-border-set!", self.pdfAnnotationBorderSet))
    self.define(Procedure("pdf-annotation-contents", self.pdfAnnotationContents))
    self.define(Procedure("pdf-annotation-contents-set!", self.pdfAnnotationContentsSet))
    self.define(Procedure("pdf-annotation-alignment", self.pdfAnnotationAlignment))
    self.define(Procedure("pdf-annotation-alignment-set!", self.pdfAnnotationAlignmentSet))
    self.define(Procedure("pdf-annotation-text-intent", self.pdfAnnotationTextIntent))
    self.define(Procedure("pdf-annotation-text-intent-set!", self.pdfAnnotationTextIntentSet))
    self.define(Procedure("pdf-annotation-font", self.pdfAnnotationFont))
    self.define(Procedure("pdf-annotation-font-set!", self.pdfAnnotationFontSet))
    self.define(Procedure("pdf-annotation-color", self.pdfAnnotationColor))
    self.define(Procedure("pdf-annotation-color-set!", self.pdfAnnotationColorSet))
    self.define(Procedure("pdf-annotation-interior-color", self.pdfAnnotationInteriorColor))
    self.define(Procedure("pdf-annotation-interior-color-set!", self.pdfAnnotationInteriorColorSet))
    self.define(Procedure("pdf-annotation-background-color", self.pdfAnnotationBackgroundColor))
    self.define(Procedure("pdf-annotation-background-color-set!", self.pdfAnnotationBackgroundColorSet))
    self.define(Procedure("pdf-annotation-font-color", self.pdfAnnotationFontColor))
    self.define(Procedure("pdf-annotation-font-color-set!", self.pdfAnnotationFontColorSet))
    self.define(Procedure("pdf-annotation-start-point", self.pdfAnnotationStartPoint))
    self.define(Procedure("pdf-annotation-start-point-set!", self.pdfAnnotationStartPointSet))
    self.define(Procedure("pdf-annotation-end-point", self.pdfAnnotationEndPoint))
    self.define(Procedure("pdf-annotation-end-point-set!", self.pdfAnnotationEndPointSet))
    self.define(Procedure("pdf-annotation-icon", self.pdfAnnotationIcon))
    self.define(Procedure("pdf-annotation-icon-set!", self.pdfAnnotationIconSet))
    self.define(Procedure("pdf-annotation-stamp", self.pdfAnnotationStamp))
    self.define(Procedure("pdf-annotation-stamp-set!", self.pdfAnnotationStampSet))
    self.define(Procedure("pdf-annotation-popup", self.pdfAnnotationPopup))
    self.define(Procedure("pdf-annotation-popup-set!", self.pdfAnnotationPopupSet))
    self.define(Procedure("pdf-annotation-markup-type", self.pdfAnnotationMarkupType))
    self.define(Procedure("pdf-annotation-markup-type-set!", self.pdfAnnotationMarkupTypeSet))
    self.define(Procedure("pdf-annotation-markup-points", self.pdfAnnotationMarkupPoints))
    self.define(Procedure("pdf-annotation-markup-points-set!", self.pdfAnnotationMarkupPointsSet))
    self.define(Procedure("pdf-annotation-callout-points", self.pdfAnnotationCalloutPoints))
    self.define(Procedure("pdf-annotation-callout-points-set!", self.pdfAnnotationCalloutPointsSet))
    self.define(Procedure("pdf-annotation-shapes", self.pdfAnnotationShapes))
    self.define(Procedure("pdf-annotation-shape-add!", self.pdfAnnotationShapeAdd))
    self.define(Procedure("pdf-annotation-shapes-clear!", self.pdfAnnotationShapesClear))
    self.define(Procedure("pdf-annotation-modification-date", self.pdfAnnotationModificationDate))
    self.define(Procedure("pdf-annotation-modification-date-set!", self.pdfAnnotationModificationDateSet))
    self.define(Procedure("pdf-annotation-username", self.pdfAnnotationUsername))
    self.define(Procedure("pdf-annotation-username-set!", self.pdfAnnotationUsernameSet))
    self.define(Procedure("pdf-annotation-url", self.pdfAnnotationUrl))
    self.define(Procedure("pdf-annotation-url-set!", self.pdfAnnotationUrlSet))
    self.define(Procedure("pdf-annotation-destination", self.pdfAnnotationDestination))
    self.define(Procedure("pdf-annotation-destination-set!", self.pdfAnnotationDestinationSet))
    self.define(Procedure("pdf-annotation-action", self.pdfAnnotationAction))
    self.define(Procedure("pdf-annotation-action-set!", self.pdfAnnotationActionSet))
    self.define(Procedure("pdf-annotation-display", self.pdfAnnotationDisplay))
    self.define(Procedure("pdf-annotation-display-set!", self.pdfAnnotationDisplaySet))
    self.define(Procedure("pdf-annotation-print", self.pdfAnnotationPrint))
    self.define(Procedure("pdf-annotation-print-set!", self.pdfAnnotationPrintSet))
    self.define(Procedure("pdf-annotation-highlighted", self.pdfAnnotationHighlighted))
    self.define(Procedure("pdf-annotation-highlighted-set!", self.pdfAnnotationHighlightedSet))
    self.define(Procedure("pdf-annotation-attributes", self.pdfAnnotationAttributes))
    self.define(Procedure("pdf-annotation-attributes-ref", self.pdfAnnotationAttributesRef))
    self.define(Procedure("pdf-annotation-attributes-set!", self.pdfAnnotationAttributesSet))
    self.define(Procedure("pdf-annotation-attributes-remove!", self.pdfAnnotationAttributesRemove))
    self.define(Procedure("draw-pdf-annotation", self.drawPdfAnnotation))
  }
  
  private func pdf(from expr: Expr) throws -> PDFDocument {
    guard case .object(let obj) = expr,
          let document = obj as? NativePDFDocument else {
      throw RuntimeError.type(expr, expected: [NativePDFDocument.type])
    }
    return document.document
  }
  
  private func page(from expr: Expr) throws -> PDFPage {
    guard case .object(let obj) = expr,
          let page = obj as? NativePDFPage else {
      throw RuntimeError.type(expr, expected: [NativePDFPage.type])
    }
    return page.page
  }
  
  private func annotation(from expr: Expr) throws -> PDFAnnotation {
    guard case .object(let obj) = expr,
          let annotation = obj as? NativePDFAnnotation else {
      throw RuntimeError.type(expr, expected: [NativePDFAnnotation.type])
    }
    return annotation.annotation
  }
  
  private func outline(from expr: Expr) throws -> PDFOutline {
    guard case .object(let obj) = expr,
          let outline = obj as? NativePDFOutline else {
      throw RuntimeError.type(expr, expected: [NativePDFOutline.type])
    }
    return outline.outline
  }
  
  private func expr(for border: PDFBorder?) -> Expr {
    guard let border else {
      return .false
    }
    let type: String
    switch border.style {
      case .solid:
        type = "solid"
      case .dashed:
        type = "dashed"
      case .beveled:
        type = "beveled"
      case .inset:
        type = "inset"
      case .underline:
        type = "underline"
      default:
        return .false
    }
    if let dashes = border.dashPattern, dashes.count > 0 {
      var dashPattern = Expr.null
      for dash in dashes.reversed() {
        if let length = dash as? Double {
          dashPattern = .pair(.flonum(length), dashPattern)
        }
      }
      return .pair(.symbol(self.context.symbols.intern(type)),
                   .pair(.makeNumber(border.lineWidth),
                         .pair(dashPattern, .null)))
    } else {
      return .pair(.symbol(self.context.symbols.intern(type)),
                   .pair(.makeNumber(border.lineWidth), .null))
    }
  }
  
  private func pdfBorder(from: Expr) throws -> PDFBorder {
    guard case .pair(.symbol(let sym), let tail) = from else {
      throw RuntimeError.eval(.invalidPDFBorder, from)
    }
    let style: PDFBorderStyle
    switch sym.identifier {
      case "solid":
        style = .solid
      case "dashed":
        style = .dashed
      case "beveled":
        style = .beveled
      case "inset":
        style = .inset
      case "underline":
        style = .underline
      default:
        throw RuntimeError.eval(.invalidPDFBorder, from)
    }
    let border = PDFBorder()
    border.style = style
    switch tail {
      case .null:
        border.lineWidth = 1.0
        return border
      case .pair(let width, .null):
        border.lineWidth = try width.asDouble(coerce: true)
        return border
      case .pair(let width, .pair(let dashes, .null)):
        border.lineWidth = try width.asDouble(coerce: true)
        var dashLengths: [Any] = []
        var list = dashes
        while case .pair(let length, let next) = list {
          dashLengths.append(try length.asDouble(coerce: true))
          list = next
        }
        border.dashPattern = dashLengths
        return border
      default:
        throw RuntimeError.eval(.invalidPDFBorder, from)
    }
  }

  private func accessPermissions(from: Expr, permissions: PDFAccessPermissions? = nil)
                                                              throws -> PDFAccessPermissions? {
    switch from {
      case .false:
        return nil
      case .true:
        return permissions
      default:
        var permissions: UInt = 0
        var list = from
        while case .pair(let token, let rest) = list {
          switch try token.asSymbol().identifier {
            case "commenting":
              permissions += PDFAccessPermissions.allowsCommenting.rawValue
            case "content-accessibility":
              permissions += PDFAccessPermissions.allowsContentAccessibility.rawValue
            case "content-copying":
              permissions += PDFAccessPermissions.allowsContentCopying.rawValue
            case "document-assembly":
              permissions += PDFAccessPermissions.allowsDocumentAssembly.rawValue
            case "document-changes":
              permissions += PDFAccessPermissions.allowsDocumentChanges.rawValue
            case "form-field-entry":
              permissions += PDFAccessPermissions.allowsFormFieldEntry.rawValue
            case "high-quality-printing":
              permissions += PDFAccessPermissions.allowsHighQualityPrinting.rawValue
            case "low-quality-printing":
              permissions += PDFAccessPermissions.allowsLowQualityPrinting.rawValue
            default:
              throw RuntimeError.eval(.invalidPDFAccessIdentifier, token)
          }
          list = rest
        }
        return PDFAccessPermissions(rawValue: permissions)
    }
  }
  
  private func writeOptions(from: Expr?, permissions: PDFAccessPermissions? = nil)
                                                        throws -> [PDFDocumentWriteOption : Any] {
    guard from?.isTrue ?? false else {
      return [:]
    }
    var res: [PDFDocumentWriteOption : Any] = [:]
    var list = from!
    while case .pair(.pair(let k, let value), let rest) = list {
      let key = try k.asSymbol().identifier
      if !value.isNull {
        switch key {
          case "user-password":
            if value.isTrue {
              res[.userPasswordOption] = try value.asString()
            }
          case "owner-password":
            if value.isTrue {
              res[.ownerPasswordOption] = try value.asString()
            }
          case "access-permissions":
            if value.isTrue,
               let permissions = try self.accessPermissions(from: value, permissions: permissions) {
              res[.accessPermissionsOption] = permissions
            }
          case "burn-in-annotations":
            res[.burnInAnnotationsOption] = value.isTrue
          case "optimize-for-screen":
            res[.optimizeImagesForScreenOption] = value.isTrue
          case "save-images-as-jpeg":
            res[.saveImagesAsJPEGOption] = value.isTrue
          case "save-text-from-ocr":
            res[.saveTextFromOCROption] = value.isTrue
          default:
            throw RuntimeError.eval(.invalidPDFDocGenerationOption, k)
        }
      }
      list = rest
    }
    return res
  }
  
  private func annotationType(from: Expr) throws -> PDFAnnotationSubtype {
    guard case .symbol(let sym) = from else {
      throw RuntimeError.eval(.unknownPDFAnnotationType, from)
    }
    switch sym {
      case self.circle:
        return .circle
      case self.freeText:
        return .freeText
      case self.highlight:
        return .highlight
      case self.ink:
        return .ink
      case self.link:
        return .link
      case self.popup:
        return .popup
      case self.square:
        return .square
      case self.stamp:
        return .stamp
      case self.strikeOut:
        return .strikeOut
      case self.text:
        return .text
      case self.underline:
        return .underline
      case self.widget:
        return .widget
      default:
        return .init(rawValue: sym.identifier)
    }
  }
  
  private func annotationTypeExpr(for str: String) -> Expr {
    switch str {
      case "Circle":
        return .symbol(self.circle)
      case "FreeText":
        return .symbol(self.freeText)
      case "Highlight":
        return .symbol(self.highlight)
      case "Ink":
        return .symbol(self.ink)
      case "Link":
        return .symbol(self.link)
      case "Popup":
        return .symbol(self.popup)
      case "Square":
        return .symbol(self.square)
      case "Stamp":
        return .symbol(self.stamp)
      case "StrikeOut":
        return .symbol(self.strikeOut)
      case "Text":
        return .symbol(self.text)
      case "Underline":
        return .symbol(self.underline)
      case "Widget":
        return .symbol(self.widget)
      default:
        return .symbol(self.context.symbols.intern(str))
    }
  }
  
  private func color(from: Expr) throws -> NativeColor {
    guard case .object(let obj) = from, let color = obj as? Color else {
      throw RuntimeError.type(from, expected: [Color.type])
    }
    return color.nsColor
  }
  
  private func optColor(from: Expr) throws -> NativeColor? {
    guard from.isTrue else {
      return nil
    }
    guard case .object(let obj) = from, let color = obj as? Color else {
      throw RuntimeError.type(from, expected: [Color.type])
    }
    return color.nsColor
  }
  
  private func expr(from style: PDFLineStyle) -> Expr {
    let res = self.lineStyle.expr(for: style)
    return res.isNull ? .makeString(PDFAnnotation.name(for: style)) : res
  }
  
  private func lineStyle(from expr: Expr) throws -> PDFLineStyle {
    switch expr {
      case .string(let str):
        return PDFAnnotation.lineStyle(fromName: str as String)
      default:
        return try self.lineStyle.value(for: expr)
    }
  }
  
  private func isPdf(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, obj is NativePDFDocument else {
      return .false
    }
    return .true
  }
  
  private func isPdfEncrypted(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let document = obj as? NativePDFDocument else {
      return .false
    }
    return .makeBoolean(document.document.isEncrypted)
  }
  
  private func isPdfLocked(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, let document = obj as? NativePDFDocument else {
      return .false
    }
    return .makeBoolean(document.document.isLocked)
  }
  
  private func makePdf() -> Expr {
    return .object(NativePDFDocument(document:
                                      LispKitPDFDocument(delegate: self.pdfDocumentDelegate)))
  }
  
  private func bytevectorToPdf(expr: Expr, args: Arguments) throws -> Expr {
    let subvec = try BytevectorLibrary.subVector("bytevector->pdf", expr, args)
    guard let document = LispKitPDFDocument(data: Data(subvec),
                                            delegate: self.pdfDocumentDelegate) else {
      throw RuntimeError.eval(.cannotCreatePdf, expr)
    }
    return .object(NativePDFDocument(document: document))
  }
  
  private func loadPdf(filename: Expr) throws -> Expr {
    let path = self.context.fileHandler.path(try filename.asPath(),
                                             relativeTo: self.context.evaluator.currentDirectoryPath)
    guard let document = LispKitPDFDocument(url: URL(fileURLWithPath: path),
                                            delegate: self.pdfDocumentDelegate) else {
      return .false
    }
    return .object(NativePDFDocument(document: document))
  }
  
  private func savePdf(filename: Expr, expr: Expr, opt: Expr?) throws -> Expr {
    let url = URL(fileURLWithPath: self.context.fileHandler.path(try filename.asPath(),
                  relativeTo: self.context.evaluator.currentDirectoryPath))
    guard let document = try self.pdf(from: expr) as? LispKitPDFDocument else {
      return .false
    }
    let newDocument = document.persistDrawings() ?? document
    newDocument.write(to: url,
                      withOptions: try self.writeOptions(from: opt,
                                                         permissions: document.accessPermissions))
    return .void
  }
  
  private func pdfUnlock(expr: Expr, passwd: Expr) throws -> Expr {
    return .makeBoolean(try self.pdf(from: expr).unlock(withPassword: passwd.asString()))
  }
  
  private func pdfToString(expr: Expr) throws -> Expr {
    guard let res = try self.pdf(from: expr).string else {
      return .false
    }
    return .makeString(res)
  }
  
  private func pdfToBytevector(expr: Expr, opt: Expr?) throws -> Expr {
    guard let document = try self.pdf(from: expr) as? LispKitPDFDocument else {
      return .false
    }
    let options = try self.writeOptions(from: opt, permissions: document.accessPermissions)
    let newDocument = document.persistDrawings() ?? document
    guard let data = newDocument.dataRepresentation(options: options) else {
      return .false
    }
    let count = data.count
    var res = [UInt8](repeating: 0, count: count)
    data.copyBytes(to: &res, count: count)
    return .bytes(MutableBox(res))
  }
  
  private func pdfPageCount(expr: Expr) throws -> Expr {
    return .makeNumber(try self.pdf(from: expr).pageCount)
  }
  
  private func pdfPages(expr: Expr) throws -> Expr {
    let document = try self.pdf(from: expr)
    var res = Expr.null
    for i in 0..<document.pageCount {
      if let page = document.page(at: i) {
        res = .pair(.object(NativePDFPage(page: page)), res)
      }
    }
    return res
  }
  
  private func attributeExpr(from obj: Any) -> Expr? {
    if let str = obj as? String {
      return .makeString(str)
    } else if let uuid = obj as? UUID {
      return .symbol(self.context.symbols.intern(uuid.description))
    } else if let num = obj as? Int64 {
      return .makeNumber(num)
    } else if let num = obj as? Int {
      return .makeNumber(num)
    } else if let num = obj as? Double {
      return .makeNumber(num)
    } else if let bool = obj as? Bool {
      return .makeBoolean(bool)
    } else if let date = obj as? Date {
      return .object(NativeDateTime(DateTimeLibrary.calendar.dateComponents(in: TimeZone.current,
                                                                            from: date)))
    } else if let color = obj as? NativeColor {
      return .object(Color(color))
    } else if let border = obj as? PDFBorder {
      return self.expr(for: border)
    } else if let action = obj as? PDFAction {
      return self.expr(for: action)
    } else if let rect = obj as? CGRect {
      return .pair(.pair(.flonum(rect.origin.x), .flonum(rect.origin.y)),
                   .pair(.flonum(rect.width), .flonum(rect.height)))
    } else if let pnt = obj as? CGPoint {
      return .pair(.flonum(pnt.x), .flonum(pnt.y))
    } else if let array = obj as? NSArray {
      var exprs: Exprs = []
      for comp in array {
        if let expr = self.attributeExpr(from: comp) {
          exprs.append(expr)
        }
      }
      return .vector(Collection(kind: .vector, exprs: exprs))
    } else {
      return nil
    }
  }
  
  private func attributeValue(from: Expr) -> Any? {
    switch from {
      case .true:
        return true
      case .false:
        return false
      case .symbol(let sym):
        return NSUUID(uuidString: sym.identifier)
      case .string(let str):
        return NSString(string: str)
      case .fixnum(let num):
        return NSNumber(value: num)
      case .flonum(let num):
        return NSNumber(value: num)
      case .pair(.flonum(let x), .flonum(let y)):
        return CGPoint(x: x, y: y)
      case .pair(.pair(.flonum(let x), .flonum(let y)), .pair(.flonum(let w), .flonum(let h))):
        return CGRect(x: x, y: y, width: w, height: h)
      case .pair(.symbol(_), _):
        return (try? pdfBorder(from: from)) ?? (try? action(from: from))
      case .vector(let coll):
        let array = NSMutableArray()
        for comp in coll.exprs {
          if let elem = self.attributeValue(from: comp) {
            array.add(elem)
          }
        }
        return NSArray(array: array)
      case .object(let obj):
        if let nd = obj as? NativeDateTime, let date = nd.value.date {
          return date as NSDate
        } else if let nc = obj as? Color {
          return nc.nsColor
        } else {
          fallthrough
        }
      default:
        return nil
    }
  }
  
  private func pdfPath(expr: Expr) throws -> Expr {
    if let url = try self.pdf(from: expr).documentURL {
      return .makeString(url.path(percentEncoded: false))
    } else {
      return .false
    }
  }
  
  private func pdfVersion(expr: Expr) throws -> Expr {
    let document = try self.pdf(from: expr)
    if document.majorVersion == 0 && document.minorVersion == 0 {
      return .false
    } else {
      return .pair(.makeNumber(document.majorVersion), .makeNumber(document.minorVersion))
    }
  }
  
  private func pdfAttributes(expr: Expr) throws -> Expr {
    if let attribs = try self.pdf(from: expr).documentAttributes {
      var res = Expr.null
      for (key, value) in attribs {
        guard let str = key as? String else {
          continue
        }
        if let val = self.attributeExpr(from: value) {
          res = .pair(.pair(Expr.symbol(self.context.symbols.intern(str)), val), res)
        }
      }
      return res
    } else {
      return .false
    }
  }
  
  // Common attribute keys: Creator, Producer, Author, Title, Subject, CreationDate,
  //                        ModDate, Keywords
  private func pdfAttributeRef(expr: Expr, sym: Expr, default: Expr?) throws -> Expr {
    let key = NSString(string: try sym.asSymbol().identifier)
    if let value = try self.pdf(from: expr).documentAttributes?[key] {
      return self.attributeExpr(from: value) ?? .null
    } else {
      return `default` ?? .false
    }
  }
  
  private func pdfAttributeSet(expr: Expr, sym: Expr, value: Expr) throws -> Expr {
    let document = try self.pdf(from: expr)
    let key = NSString(string: try sym.asSymbol().identifier)
    if let val = self.attributeValue(from: value) {
      document.documentAttributes?[key] = val
    } else {
      throw RuntimeError.eval(.cannotMapAttributeValue, value)
    }
    return .void
  }
  
  private func pdfAttributeRemove(expr: Expr, sym: Expr) throws -> Expr {
    try self.pdf(from: expr).documentAttributes?.removeValue(forKey:
                                                   NSString(string: try sym.asSymbol().identifier))
    return .void
  }
  
  private func pdfPermissions(expr: Expr) throws -> Expr {
    let document = try self.pdf(from: expr)
    var res = Expr.null
    if document.allowsCommenting {
      res = .pair(.symbol(self.context.symbols.intern("commenting")), res)
    }
    if document.allowsContentAccessibility {
      res = .pair(.symbol(self.context.symbols.intern("content-accessibility")), res)
    }
    if document.allowsCopying {
      res = .pair(.symbol(self.context.symbols.intern("content-copying")), res)
    }
    if document.allowsDocumentAssembly {
      res = .pair(.symbol(self.context.symbols.intern("document-assembly")), res)
    }
    if document.allowsDocumentChanges {
      res = .pair(.symbol(self.context.symbols.intern("document-changes")), res)
    }
    if document.allowsFormFieldEntry {
      res = .pair(.symbol(self.context.symbols.intern("form-field-entry")), res)
    }
    if (document.accessPermissions.rawValue &
        PDFAccessPermissions.allowsHighQualityPrinting.rawValue) != 0 {
      res = .pair(.symbol(self.context.symbols.intern("high-quality-printing")), res)
    }
    if (document.accessPermissions.rawValue &
        PDFAccessPermissions.allowsLowQualityPrinting.rawValue) != 0 {
      res = .pair(.symbol(self.context.symbols.intern("low-quality-printing")), res)
    }
    return res
  }
  
  private func pdfPage(expr: Expr, index: Expr) throws -> Expr {
    let document = try self.pdf(from: expr)
    if let page = try document.page(at: index.asInt(above: 0, below: document.pageCount)) {
      return .object(NativePDFPage(page: page))
    } else {
      return .false
    }
  }
  
  private func pdfInsertPage(expr: Expr, index: Expr, page: Expr) throws -> Expr {
    let document = try self.pdf(from: expr)
    try document.insert(self.page(from: page),
                        at: index.asInt(above: 0, below: document.pageCount + 1))
    return .void
  }
  
  private func pdfRemovePage(expr: Expr, index: Expr) throws -> Expr {
    let document = try self.pdf(from: expr)
    try document.removePage(at: index.asInt(above: 0, below: document.pageCount))
    return .void
  }
  
  private func pdfSwapPage(expr: Expr, first: Expr, second: Expr) throws -> Expr {
    let document = try self.pdf(from: expr)
    try document.exchangePage(at: first.asInt(above: 0, below: document.pageCount),
                              withPageAt: second.asInt(above: 0, below: document.pageCount))
    return .void
  }
  
  private func pdfPageIndex(expr: Expr, page: Expr) throws -> Expr {
    let res = try self.pdf(from: expr).index(for: self.page(from: page))
    return res == NSNotFound ? .false : .makeNumber(res)
  }
  
  private func pdfOutline(expr: Expr) throws -> Expr {
    guard let outline = try self.pdf(from: expr).outlineRoot else {
      return .false
    }
    return .object(NativePDFOutline(outline: outline))
  }
  
  // Predicates
  
  private func isDisplayBox(obj: Expr) -> Expr {
    return .makeBoolean(self.displayBox.isValue(obj))
  }
  
  private func isLineStyle(obj: Expr) -> Expr {
    return .makeBoolean(self.lineStyle.isValue(obj))
  }
  
  private func isTextAlignment(obj: Expr) -> Expr {
    return .makeBoolean(self.textAlignment.isValue(obj))
  }
  
  private func isIconType(obj: Expr) -> Expr {
    return .makeBoolean(self.iconType.isValue(obj))
  }
  
  private func isMarkupType(obj: Expr) -> Expr {
    return .makeBoolean(self.markupType.isValue(obj))
  }
  
  // PDF Pages
  
  private func isPdfPage(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, obj is NativePDFPage else {
      return .false
    }
    return .true
  }
  
  private func makePdfPage(args: Arguments) throws -> Expr {
    guard let (image, compress, rotate, media, upscale) = args.optional(.false, .false, .false,
                                                                     .false, .null) else {
      throw RuntimeError.argumentCount(of: "make-pdf-page", min: 0, max: 5, args: .makeList(args))
    }
    var options: [PDFPage.ImageInitializationOption : Any] = [:]
    #if os(iOS) || os(watchOS) || os(tvOS)
    let img: UIImage?
    #elseif os(macOS)
    let img: NSImage?
    #endif
    switch image {
      case .false:
        img = nil
      case .pair(.flonum(let w), .flonum(let h)):
        guard compress.isFalse, rotate.isFalse, media.isFalse, upscale.isFalse else {
          throw RuntimeError.type(image, expected: [NativeImage.type])
        }
        options[.mediaBox] = CGRect(x: 0, y: 0, width: w, height: h)
        img = nil
      case .pair(.pair(.flonum(let x), .flonum(let y)),
                 .pair(.flonum(let w), .flonum(let h))):
        guard compress.isFalse, rotate.isFalse, media.isFalse, upscale.isFalse else {
          throw RuntimeError.type(image, expected: [NativeImage.type])
        }
        options[.mediaBox] = CGRect(x: x, y: y, width: w, height: h)
        img = nil
      case .object(let obj):
        if let imageBox = obj as? NativeImage {
          img = imageBox.value
        } else {
          fallthrough
        }
      default:
        throw RuntimeError.type(image, expected: [NativeImage.type])
    }
    if compress.isTrue {
      var compr = try compress.asDouble(coerce: true)
      if compr < 0.0 {
        compr = 0.0
      }
      if compr > 1.0 {
        compr = 1.0
      }
      options[.compressionQuality] = compr
    }
    if rotate.isTrue {
      options[.rotation] = try rotate.asInt(above: 0, below: 100000)
    }
    switch media {
      case .false:
        break
      case .pair(.flonum(let w), .flonum(let h)):
        options[.mediaBox] = CGRect(x: 0, y: 0, width: w, height: h)
      case .pair(.pair(.flonum(let x), .flonum(let y)),
                 .pair(.flonum(let w), .flonum(let h))):
        options[.mediaBox] = CGRect(x: x, y: y, width: w, height: h)
      default:
        throw RuntimeError.eval(.invalidRect, media)
    }
    if !upscale.isNull {
      options[.upscaleIfSmaller] = upscale.isTrue
    }
    if let img {
      if let page = PDFPage(image: img, options: options) {
        return .object(NativePDFPage(page: page))
      } else {
        return .false
      }
    } else {
      return .object(NativePDFPage(page: PDFPage()))
    }
  }
  
  private func pdfPageCopy(expr: Expr) throws -> Expr {
    guard let page = try self.page(from: expr).copy() as? PDFPage else {
      return .false
    }
    if let index = page.document?.index(for: page) {
      page.document?.removePage(at: index)
    }
    return .object(NativePDFPage(page: page))
  }
  
  private func pdfPageDocument(expr: Expr) throws -> Expr {
    let page = try self.page(from: expr)
    guard let document = page.document, document.index(for: page) < Int.max else {
      return .false
    }
    return .object(NativePDFDocument(document: document))
  }
  
  private func pdfPageNumber(expr: Expr) throws -> Expr {
    let page = try self.page(from: expr)
    guard let index = page.document?.index(for: page), index < Int.max else {
      return .false
    }
    return .makeNumber(index)
  }
  
  private func pdfPageLabel(expr: Expr) throws -> Expr {
    let page = try self.page(from: expr)
    guard let label = page.label,
          let index = page.document?.index(for: page), index < Int.max else {
      return .false
    }
    return .makeString(label)
  }
  
  private func pdfPageBounds(expr: Expr, box: Expr) throws -> Expr {
    let bounds = try self.page(from: expr).bounds(for: self.displayBox.value(for: box))
    return .pair(.pair(.flonum(bounds.origin.x), .flonum(bounds.origin.y)),
                 .pair(.flonum(bounds.width), .flonum(bounds.height)))
  }
  
  private func pdfPageBoundsSet(expr: Expr, box: Expr, bounds: Expr) throws -> Expr {
    let page = try self.page(from: expr)
    let displayBox = try self.displayBox.value(for: box)
    switch bounds {
      case .pair(.flonum(let w), .flonum(let h)):
        page.setBounds(CGRect(x: 0, y: 0, width: w, height: h), for: displayBox)
      case .pair(.pair(.flonum(let x), .flonum(let y)),
                 .pair(.flonum(let w), .flonum(let h))):
        page.setBounds(CGRect(x: x, y: y, width: w, height: h), for: displayBox)
      default:
        throw RuntimeError.eval(.invalidRect, bounds)
    }
    return .void
  }
  
  private func pdfPageRotation(expr: Expr) throws -> Expr {
    return .makeNumber(try self.page(from: expr).rotation)
  }
  
  private func pdfPageRotationSet(expr: Expr, rotation: Expr) throws -> Expr {
    try self.page(from: expr).rotation = try rotation.asInt(above: 0, below: 100000)
    return .void
  }
  
  private func pdfPageAnnotationsDisplay(expr: Expr) throws -> Expr {
    return .makeBoolean(try self.page(from: expr).displaysAnnotations)
  }
  
  private func pdfPageAnnotationsDisplaySet(expr: Expr, display: Expr) throws -> Expr {
    try self.page(from: expr).displaysAnnotations = display.isTrue
    return .void
  }
  
  private func pdfPageAnnotations(expr: Expr) throws -> Expr {
    var res = Expr.null
    for annotation in try self.page(from: expr).annotations.reversed() {
      res = .pair(.object(NativePDFAnnotation(annotation: annotation)), res)
    }
    return res
  }
  
  private func pdfPageAnnotationRef(expr: Expr, point: Expr) throws -> Expr {
    guard case .pair(.flonum(let x), .flonum(let y)) = point else {
      throw RuntimeError.eval(.invalidPoint, point)
    }
    guard let annot = try self.page(from: expr).annotation(at: CGPoint(x: x, y: y)) else {
      return .false
    }
    return .object(NativePDFAnnotation(annotation: annot))
  }
  
  private func pdfPageAnnotationAdd(expr: Expr, annot: Expr) throws -> Expr {
    try self.page(from: expr).addAnnotation(self.annotation(from: annot))
    return .void
  }
  
  private func pdfPageAnnotationRemove(expr: Expr, annot: Expr) throws -> Expr {
    try self.page(from: expr).removeAnnotation(self.annotation(from: annot))
    return .void
  }
  
  private func pdfPageImages(expr: Expr) throws -> Expr {
    guard let page = try self.page(from: expr) as? LispKitPDFPage else {
      return .false
    }
    let images = page.images
    var res = Expr.null
    for image in images {
      res = .pair(.object(NativeImage(image)), res)
    }
    return res
  }
  
  private func pdfPageThumbnail(expr: Expr, box: Expr, size: Expr) throws -> Expr {
    let page = try self.page(from: expr)
    let displayBox = try self.displayBox.value(for: box)
    guard case .pair(.flonum(let w), .flonum(let h)) = size else {
      throw RuntimeError.eval(.invalidSize, size)
    }
    let image = page.thumbnail(of: CGSize(width: w, height: h), for: displayBox)
    return .object(NativeImage(image))
  }
  
  #if os(iOS) || os(watchOS) || os(tvOS)
  
  private func pdfPageToBitmap(expr: Expr,
                               box: Expr,
                               size: Expr,
                               dpi: Expr?,
                               ipol: Expr?) throws -> Expr {
    let page = try self.page(from: expr)
    let displayBox = try self.displayBox.value(for: box)
    guard case .pair(.flonum(let w), .flonum(let h)) = size else {
      throw RuntimeError.eval(.invalidSize, size)
    }
    let scale = (try dpi?.asDouble(coerce: true) ?? 72.0)/72.0
    guard scale > 0.0 && scale <= 10.0 else {
      throw RuntimeError.range(parameter: 4,
                               of: "pdf-page->bitmap",
                               dpi ?? .fixnum(72),
                               min: 0,
                               max: 720,
                               at: SourcePosition.unknown)
    }
    // Create a bitmap suitable for storing the image in a PNG
    guard let context = CGContext(data: nil,
                                  width: Int(w * scale),
                                  height: Int(h * scale),
                                  bitsPerComponent: 8,
                                  bytesPerRow: 0,
                                  space: Color.colorSpaceName,
                                  bitmapInfo: CGBitmapInfo(rawValue:
                                                  CGImageAlphaInfo.premultipliedFirst.rawValue)
                                                .union(.byteOrder32Little).rawValue) else {
      throw RuntimeError.eval(.cannotCreateBitmap,
                              .pair(expr, .pair(size, .pair(dpi ?? .fixnum(72), .null))))
    }
    // Push the new graphics context
    UIGraphicsPushContext(context)
    defer {
      UIGraphicsPopContext()
    }
    // Set it up
    context.saveGState()
    if let ipol,
       let quality = CGInterpolationQuality(rawValue: Int32(try ipol.asInt(above: 0, below: 5))) {
      context.interpolationQuality = quality
    }
    page.transform(context, for: displayBox)
    // Fit the page into the display box
    let bounds = page.bounds(for: displayBox)
    context.scaleBy(x: scale * w / bounds.width, y: scale * h / bounds.height)
    // Draw into the graphics context
    page.draw(with: displayBox, to: context)
    context.restoreGState()
    // Create an image and add the bitmap as a representation
    guard let cgImage = context.makeImage() else {
      throw RuntimeError.eval(.cannotCreateBitmap,
                              .pair(expr, .pair(size, .pair(dpi ?? .fixnum(72), .null))))
    }
    // Create a UIImage which has metadata about the scale factor and orientation
    let uiImage = UIImage(cgImage: cgImage, scale: CGFloat(scale), orientation: .up)
    return .object(NativeImage(uiImage))
  }
  
  #elseif os(macOS)
  
  private func pdfPageToBitmap(expr: Expr,
                               box: Expr,
                               size: Expr,
                               dpi: Expr?,
                               ipol: Expr?) throws -> Expr {
    let page = try self.page(from: expr)
    let displayBox = try self.displayBox.value(for: box)
    guard case .pair(.flonum(let w), .flonum(let h)) = size else {
      throw RuntimeError.eval(.invalidSize, size)
    }
    let scale = (try dpi?.asDouble(coerce: true) ?? 72.0)/72.0
    guard scale > 0.0 && scale <= 10.0 else {
      throw RuntimeError.range(parameter: 4,
                               of: "pdf-page->bitmap",
                               dpi ?? .fixnum(72),
                               min: 0,
                               max: 720,
                               at: SourcePosition.unknown)
    }
    // Create a bitmap suitable for storing the image
    guard let bitmap = NSBitmapImageRep(bitmapDataPlanes: nil,
                                        pixelsWide: Int(w * scale),
                                        pixelsHigh: Int(h * scale),
                                        bitsPerSample: 8,
                                        samplesPerPixel: 4,
                                        hasAlpha: true,
                                        isPlanar: false,
                                        colorSpaceName: NSColorSpaceName.deviceRGB,
                                        bytesPerRow: 0,
                                        bitsPerPixel: 0) else {
      throw RuntimeError.eval(.cannotCreateBitmap,
                              .pair(expr, .pair(size, .pair(dpi ?? .fixnum(72), .null))))
    }
    // Set the intended size of the image (vs. size of the bitmap above)
    bitmap.size = NSSize(width: w, height: h)
    // Create a graphics context for drawing into the bitmap
    guard let context = NSGraphicsContext(bitmapImageRep: bitmap) else {
      throw RuntimeError.eval(.cannotCreateBitmap,
                              .pair(expr, .pair(size, .pair(dpi ?? .fixnum(72), .null))))
    }
    let previous = NSGraphicsContext.current
    // Create a flipped graphics context if required
    let nscontext = NSGraphicsContext(cgContext: context.cgContext, flipped: true)
    NSGraphicsContext.current = nscontext
    defer {
      NSGraphicsContext.current = previous
    }
    // Draw into the bitmap
    let cgcontext = nscontext.cgContext
    cgcontext.saveGState()
    if let ipol,
       let quality = CGInterpolationQuality(rawValue: Int32(try ipol.asInt(above: 0, below: 5))) {
      cgcontext.interpolationQuality = quality
    }
    page.transform(cgcontext, for: displayBox)
    let bounds = page.bounds(for: displayBox)
    cgcontext.scaleBy(x: w / bounds.width, y: h / bounds.height)
    page.draw(with: displayBox, to: cgcontext)
    cgcontext.restoreGState()
    // Create an image and add the bitmap as a representation
    let nsimage = NSImage(size: bitmap.size)
    nsimage.addRepresentation(bitmap)
    return .object(NativeImage(nsimage))
  }
  
  #endif
  
  private func pdfPageToString(expr: Expr) throws -> Expr {
    guard let str = try self.page(from: expr).string else {
      return .false
    }
    return .makeString(str)
  }
  
  private func pdfPageToStyledText(expr: Expr) throws -> Expr {
    guard let str = try self.page(from: expr).attributedString else {
      return .false
    }
    return .object(StyledText(str.mutableCopy() as! NSMutableAttributedString))
  }
  
  private func pdfPageToBytevector(expr: Expr) throws -> Expr {
    return .void
  }
  
  private func drawPdfPage(expr: Expr, box: Expr, rect: Expr, d: Expr) throws -> Expr {
    let page = try self.page(from: expr)
    let displayBox = try self.displayBox.value(for: box)
    guard case .object(let obj) = d, let drawing = obj as? Drawing else {
      throw RuntimeError.type(expr, expected: [Drawing.type])
    }
    switch rect {
      case .pair(.flonum(let w), .flonum(let h)):
        drawing.append(.page(page, displayBox, CGRect(x: 0, y: 0, width: w, height: h)))
      case .pair(.pair(.flonum(let x), .flonum(let y)),
                 .pair(.flonum(let w), .flonum(let h))):
        drawing.append(.page(page, displayBox, CGRect(x: x, y: y, width: w, height: h)))
      default:
        throw RuntimeError.eval(.invalidRect, rect)
    }
    return .void
  }
  
  private func pdfPageUnderlay(expr: Expr) throws -> Expr {
    guard let page = try self.page(from: expr) as? LispKitPDFPage else {
      return .false
    }
    if let drawing = page.underlay {
      return .object(drawing)
    } else {
      return .false
    }
  }
  
  private func pdfPageUnderlaySet(expr: Expr, d: Expr) throws -> Expr {
    guard let page = try self.page(from: expr) as? LispKitPDFPage else {
      return .false
    }
    guard d.isTrue else {
      page.underlay = nil
      return .void
    }
    guard case .object(let obj) = d, let drawing = obj as? Drawing else {
      throw RuntimeError.type(d, expected: [Drawing.type])
    }
    page.underlay = drawing
    return .void
  }
  
  private func pdfPageOverlay(expr: Expr) throws -> Expr {
    guard let page = try self.page(from: expr) as? LispKitPDFPage else {
      return .false
    }
    if let drawing = page.overlay {
      return .object(drawing)
    } else {
      return .false
    }
  }
  
  private func pdfPageOverlaySet(expr: Expr, d: Expr) throws -> Expr {
    guard let page = try self.page(from: expr) as? LispKitPDFPage else {
      return .false
    }
    guard d.isTrue else {
      page.overlay = nil
      return .void
    }
    guard case .object(let obj) = d, let drawing = obj as? Drawing else {
      throw RuntimeError.type(d, expected: [Drawing.type])
    }
    page.overlay = drawing
    return .void
  }
  
  // PDF Outlines
  
  private func isPdfOutline(expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr, obj is NativePDFOutline else {
      return .false
    }
    return .true
  }
  
  private func strings(from: Expr) throws -> [String] {
    var res: [String] = []
    var list = from
    while case .pair(let str, let next) = list {
      res.append(try str.asString())
      list = next
    }
    return res
  }
  
  private func expr(from: [String]?) -> Expr {
    guard let strings = from else {
      return .null
    }
    var res = Expr.null
    for str in strings {
      res = .pair(.makeString(str), res)
    }
    return res
  }
  
  private func destination(for page: Expr, point: Expr?, zoom: Expr?) throws -> PDFDestination {
    let page = try self.page(from: page)
    var p = CGPoint(x: kPDFDestinationUnspecifiedValue, y: kPDFDestinationUnspecifiedValue)
    if let point {
      switch point {
        case .false:
          break
        case .pair(let x, let y):
          p = CGPoint(x: try x.asDouble(coerce: true), y: try y.asDouble(coerce: true))
        default:
          throw RuntimeError.type(point, expected: [.pairType])
      }
    }
    let destination = PDFDestination(page: page, at: p)
    if let zoom {
      destination.zoom = try zoom.asDouble(coerce: true)
    }
    return destination
  }
  
  private func action(from: Expr) throws -> PDFAction? {
    switch from {
      case .pair(.symbol(self.goto), .pair(let page, .pair(.pair(let x, let y), .null))):
        return PDFActionGoTo(destination:
                              PDFDestination(page: try self.page(from: page),
                                             at: CGPoint(
                                                   x: try x.asDouble(coerce: true),
                                                   y: try y.asDouble(coerce: true))))
      case .pair(.symbol(self.gotoRemote), .pair(let url, .pair(let index,
                                                              .pair(.pair(let x, let y), .null)))):
        return PDFActionRemoteGoTo(pageIndex: try index.asInt(above: 0, below: 100000),
                                   at: CGPoint(x: try x.asInt(above: 0, below: 100000),
                                               y: try y.asInt(above: 0, below: 100000)),
                                   fileURL: try url.asURL())
      case .pair(.symbol(self.gotoURL), .pair(let url, .null)):
        return PDFActionURL(url: try url.asURL())
      case .pair(.symbol(self.perform), .pair(.symbol(let action), .null)):
        switch action {
          case self.none:
            return PDFActionNamed(name: .none)
          case self.find:
            return PDFActionNamed(name: .find)
          case self.goBack:
            return PDFActionNamed(name: .goBack)
          case self.goForward:
            return PDFActionNamed(name: .goForward)
          case self.gotoPage:
            return PDFActionNamed(name: .goToPage)
          case self.firstPage:
            return PDFActionNamed(name: .firstPage)
          case self.lastPage:
            return PDFActionNamed(name: .lastPage)
          case self.nextPage:
            return PDFActionNamed(name: .nextPage)
          case self.previousPage:
            return PDFActionNamed(name: .previousPage)
          case self.print:
            return PDFActionNamed(name: .print)
          case self.zoomIn:
            return PDFActionNamed(name: .zoomIn)
          case self.zoomOut:
            return PDFActionNamed(name: .zoomOut)
          default:
            return nil
        }
      case .pair(.symbol(self.resetFields), let fields):
        let action = PDFActionResetForm()
        action.fields = try self.strings(from: fields)
        action.fieldsIncludedAreCleared = false
        return action
      case .pair(.symbol(self.resetFieldsExcept), let fields):
        let action = PDFActionResetForm()
        action.fields = try self.strings(from: fields)
        action.fieldsIncludedAreCleared = true
        return action
      default:
        return nil
    }
  }
  
  private func expr(for destination: PDFDestination) -> Expr {
    let p: Expr = destination.page == nil ? .false : .object(NativePDFPage(page: destination.page!))
    let pt: Expr = destination.point.x == kPDFDestinationUnspecifiedValue
                 ? .false
                 : .pair(.flonum(destination.point.x), .flonum(destination.point.y))
    return .pair(p, .pair(pt, .pair(.flonum(destination.zoom), .null)))
  }
  
  private func expr(for a: PDFAction) -> Expr {
    if let action = a as? PDFActionGoTo {
      let page: Expr
      if let p = action.destination.page {
        page = .object(NativePDFPage(page: p))
      } else {
        page = .false
      }
      return .pair(.symbol(self.goto),
                   .pair(page,
                         .pair(.pair(.flonum(action.destination.point.x),
                                     .flonum(action.destination.point.y)), .null)))
    } else if let action = a as? PDFActionRemoteGoTo {
      return .pair(.symbol(self.gotoRemote),
                   .pair(.makeString(action.url.absoluteString),
                         .pair(.makeNumber(action.pageIndex),
                               .pair(.pair(.flonum(action.point.x),
                                           .flonum(action.point.y)), .null))))
    } else if let action = a as? PDFActionURL {
      return .pair(.symbol(self.gotoURL),
                   .pair(action.url == nil ? .false : .makeString(action.url!.absoluteString),
                         .null))
    } else if let action = a as? PDFActionNamed {
      let symbol: Symbol
      switch action.name {
        case .none:
          symbol = self.none
        case .find:
          symbol = self.find
        case .goBack:
          symbol = self.goBack
        case .goForward:
          symbol = self.goForward
        case .goToPage:
          symbol = self.gotoPage
        case .firstPage:
          symbol = self.firstPage
        case .lastPage:
          symbol = self.lastPage
        case .nextPage:
          symbol = self.nextPage
        case .previousPage:
          symbol = self.previousPage
        case .print:
          symbol = self.print
        case .zoomIn:
          symbol = self.zoomIn
        case .zoomOut:
          symbol = self.zoomOut
        @unknown default:
          return .false
      }
      return .pair(.symbol(self.perform), .pair(.symbol(symbol), .null))
    } else if let action = a as? PDFActionResetForm {
      if action.fieldsIncludedAreCleared {
        return .pair(.symbol(self.resetFieldsExcept), self.expr(from: action.fields))
      } else {
        return .pair(.symbol(self.resetFields), self.expr(from: action.fields))
      }
    } else {
      return .false
    }
  }
  
  private func makePdfOutline(a: Arguments) throws -> Expr {
    guard let (label, page, point, zoom) = a.optional(.false, .false, .false, .false) else {
      throw RuntimeError.argumentCount(of: "make-pdf-outline", min: 0, max: 4, args: .makeList(a))
    }
    let outline = PDFOutline()
    if label.isTrue {
      outline.label = try label.asString()
    }
    if page.isTrue {
      let p: CGPoint
      switch point {
        case .false:
          p = CGPoint(x: kPDFDestinationUnspecifiedValue, y: kPDFDestinationUnspecifiedValue)
        case .pair(let x, let y):
          p = CGPoint(x: try x.asDouble(coerce: true), y: try y.asDouble(coerce: true))
        default:
          throw RuntimeError.type(point, expected: [.pairType])
      }
      outline.destination = PDFDestination(page: try self.page(from: page), at: p)
      if zoom.isTrue {
        outline.destination?.zoom = try zoom.asDouble(coerce: true)
      }
    }
    return .object(NativePDFOutline(outline: outline))
  }
  
  private func pdfOutlineDocument(expr: Expr) throws -> Expr {
    guard let document = try self.outline(from: expr).document else {
      return .false
    }
    return .object(NativePDFDocument(document: document))
  }
  
  private func pdfOutlineParent(expr: Expr) throws -> Expr {
    guard let outline = try self.outline(from: expr).parent else {
      return .false
    }
    return .object(NativePDFOutline(outline: outline))
  }
  
  private func pdfOutlineIndex(expr: Expr) throws -> Expr {
    return .makeNumber(try self.outline(from: expr).index)
  }
  
  private func pdfOutlineLabel(expr: Expr) throws -> Expr {
    guard let label = try self.outline(from: expr).label else {
      return .false
    }
    return .makeString(label)
  }
  
  private func pdfOutlineLabelSet(expr: Expr) throws -> Expr {
    try self.outline(from: expr).label = expr.asString()
    return .void
  }
  
  private func pdfOutlineDestination(expr: Expr) throws -> Expr {
    guard let destination = try self.outline(from: expr).destination else {
      return .false
    }
    return self.expr(for: destination)
  }
  
  private func pdfOutlineDestinationSet(expr: Expr,
                                        page: Expr,
                                        point: Expr?,
                                        zoom: Expr?) throws -> Expr {
    try self.outline(from: expr).destination = try self.destination(for: page,
                                                                    point: point,
                                                                    zoom: zoom)
    return .void
  }
  
  private func pdfOutlineAction(expr: Expr) throws -> Expr {
    guard let action = try self.outline(from: expr).action else {
      return .false
    }
    return self.expr(for: action)
  }
  
  private func pdfOutlineActionSet(expr: Expr, action: Expr) throws -> Expr {
    try self.outline(from: expr).action = self.action(from: action)
    return .void
  }
  
  private func pdfOutlineOpen(expr: Expr) throws -> Expr {
    return .makeBoolean(try self.outline(from: expr).isOpen)
  }
  
  private func pdfOutlineOpenSet(expr: Expr, open: Expr) throws -> Expr {
    try self.outline(from: expr).isOpen = open.isTrue
    return .void
  }
  
  private func pdfOutlineChildCount(expr: Expr) throws -> Expr {
    return .makeNumber(try self.outline(from: expr).numberOfChildren)
  }
  
  private func pdfOutlineChildRef(expr: Expr, index: Expr) throws -> Expr {
    if let child = try self.outline(from: expr).child(at: index.asInt(above: 0, below: 100000)) {
      return .object(NativePDFOutline(outline: child))
    }
    return .false
  }
  
  private func pdfOutlineChildInsert(expr: Expr, index: Expr, child: Expr) throws -> Expr {
    try self.outline(from: expr).insertChild(self.outline(from: child),
                                             at: index.asInt(above: 0, below: 100000))
    return .void
  }
  
  private func pdfOutlineChildRemove(expr: Expr, index: Expr) throws -> Expr {
    if let child = try self.outline(from: expr).child(at: index.asInt(above: 0, below: 100000)) {
      child.removeFromParent()
      return .void
    }
    return .false
  }
  
  private func isPdfAnnotation(expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is NativePDFAnnotation else {
      return .false
    }
    return .true
  }
  
  /// Shape annotations:
  ///   type = shape
  ///   border: determines stroke width and style
  ///   color: determines stroke color
  /// 
  /// 
  
  private func makePdfAnnotation(bounds: Expr, type: Expr) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = bounds else {
      throw RuntimeError.eval(.invalidRect, bounds)
    }
    return .object(NativePDFAnnotation(annotation:
      PDFAnnotation(bounds: CGRect(x: x, y: y, width: w, height: h),
                    forType: try self.annotationType(from: type),
                    withProperties: nil)))
  }
  
  private func pdfAnnotationPage(expr: Expr) throws -> Expr {
    guard let page = try self.annotation(from: expr).page else {
      return .false
    }
    return .object(NativePDFPage(page: page))
  }
  
  private func pdfAnnotationType(expr: Expr) throws -> Expr {
    guard let type = try self.annotation(from: expr).type else {
      return .false
    }
    return self.annotationTypeExpr(for: type)
  }
  
  private func pdfAnnotationName(expr: Expr) throws -> Expr {
    guard let value = try self.annotation(from: expr).value(forAnnotationKey: .name),
          let name = value as? String else {
      return .false
    }
    return .makeString(name)
  }
  
  private func pdfAnnotationNameSet(expr: Expr, value: Expr) throws -> Expr {
    try self.annotation(from: expr).setValue(try value.asString(), forAnnotationKey: .name)
    return .void
  }
  
  private func pdfAnnotationTextIntent(expr: Expr) throws -> Expr {
    guard let value = try self.annotation(from: expr).value(forAnnotationKey: .init(rawValue: "IT")),
          let intent = value as? String else {
      return .false
    }
    switch intent {
      case "/FreeTextCallout":
        return .symbol(self.callout)
      case "/FreeTextTypeWriter":
        return .symbol(self.typeWriter)
      default:
        return .makeString(intent)
    }
  }
  
  private func pdfAnnotationTextIntentSet(expr: Expr, value: Expr) throws -> Expr {
    let intent: String
    switch value {
      case .symbol(self.callout):
        intent = "/FreeTextCallout"
      case .symbol(self.typeWriter):
        intent = "/FreeTextTypeWriter"
      default:
        intent = try value.asString()
    }
    try self.annotation(from: expr).setValue(intent, forAnnotationKey: .init(rawValue: "IT"))
    return .void
  }
  
  private func pdfAnnotationBounds(expr: Expr) throws -> Expr {
    let bounds = try self.annotation(from: expr).bounds
    return .pair(.pair(.flonum(bounds.origin.x), .flonum(bounds.origin.y)),
                 .pair(.flonum(bounds.width), .flonum(bounds.height)))
  }
  
  private func pdfAnnotationBoundsSet(expr: Expr, bounds: Expr) throws -> Expr {
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = bounds else {
      throw RuntimeError.eval(.invalidRect, bounds)
    }
    try self.annotation(from: expr).bounds = CGRect(x: x, y: y, width: w, height: h)
    return .void
  }
  
  private func pdfAnnotationPadding(expr: Expr) throws -> Expr {
    guard let value = try self.annotation(from: expr).value(forAnnotationKey:
                            .init(rawValue: "RD")),
          let padding = value as? NSArray,
          padding.count == 4,
          let left = padding[0] as? Double,
          let top = padding[3] as? Double,
          let right = padding[2] as? Double,
          let bottom = padding[1] as? Double else {
      return .false
    }
    return .pair(.flonum(left),
                 .pair(.flonum(top),
                       .pair(.flonum(right),
                             .pair(.flonum(bottom), .null))))
    
  }
  
  private func pdfAnnotationPaddingSet(expr: Expr, v: Expr) throws -> Expr {
    guard case .pair(let left, .pair(let top, .pair(let right, .pair(let bottom, .null)))) = v else {
      throw RuntimeError.eval(.invalidPDFPadding, v)
    }
    let l = try left.asDouble(coerce: true)
    let t = try top.asDouble(coerce: true)
    let r = try right.asDouble(coerce: true)
    let b = try bottom.asDouble(coerce: true)
    guard l >= 0.0, t >= 0.0, r >= 0.0, b >= 0.0 else {
      throw RuntimeError.eval(.invalidPDFPadding, v)
    }
    try self.annotation(from: expr).setValue([l, b, r, t], forAnnotationKey: .init(rawValue: "RD"))
    return .void
  }
  
  private func pdfAnnotationBorder(expr: Expr) throws -> Expr {
    return self.expr(for: try self.annotation(from: expr).border)
  }
  
  private func pdfAnnotationBorderSet(expr: Expr, border: Expr) throws -> Expr {
    try self.annotation(from: expr).border = try self.pdfBorder(from: border)
    return .void
  }
  
  private func pdfAnnotationContents(expr: Expr) throws -> Expr {
    guard let contents = try self.annotation(from: expr).contents else {
      return .false
    }
    return .makeString(contents)
  }
  
  private func pdfAnnotationContentsSet(expr: Expr, contents: Expr) throws -> Expr {
    try self.annotation(from: expr).contents = try contents.asString()
    return .void
  }
  
  private func pdfAnnotationAlignment(expr: Expr) throws -> Expr {
    return self.textAlignment.expr(for: try self.annotation(from: expr).alignment)
  }
  
  private func pdfAnnotationAlignmentSet(expr: Expr, value: Expr) throws -> Expr {
    try self.annotation(from: expr).alignment = self.textAlignment.value(for: value)
    return .void
  }
  
  private func pdfAnnotationFont(expr: Expr) throws -> Expr {
    guard let font = try self.annotation(from: expr).font else {
      return .false
    }
    return .object(NativeFont(font))
  }
  
  private func pdfAnnotationFontSet(expr: Expr, font: Expr) throws -> Expr {
    guard case .object(let obj) = font, let nfont = obj as? NativeFont else {
      throw RuntimeError.type(font, expected: [NativeFont.type])
    }
    try self.annotation(from: expr).font = nfont.value
    return .void
  }
  
  private func pdfAnnotationColor(expr: Expr) throws -> Expr {
    return .object(Color(try self.annotation(from: expr).color))
  }
  
  private func pdfAnnotationColorSet(expr: Expr, value: Expr) throws -> Expr {
    try self.annotation(from: expr).color = self.color(from: value)
    return .void
  }
  
  private func pdfAnnotationBackgroundColor(expr: Expr) throws -> Expr {
    guard let color = try self.annotation(from: expr).backgroundColor else {
      return .false
    }
    return .object(Color(color))
  }
  
  private func pdfAnnotationBackgroundColorSet(expr: Expr, value: Expr) throws -> Expr {
    try self.annotation(from: expr).backgroundColor = self.optColor(from: value)
    return .void
  }
  
  private func pdfAnnotationInteriorColor(expr: Expr) throws -> Expr {
    guard let color = try self.annotation(from: expr).interiorColor else {
      return .false
    }
    return .object(Color(color))
  }
  
  private func pdfAnnotationInteriorColorSet(expr: Expr, value: Expr) throws -> Expr {
    try self.annotation(from: expr).interiorColor = self.optColor(from: value)
    return .void
  }
  
  private func pdfAnnotationFontColor(expr: Expr) throws -> Expr {
    guard let color = try self.annotation(from: expr).fontColor else {
      return .false
    }
    return .object(Color(color))
  }
  
  private func pdfAnnotationFontColorSet(expr: Expr, value: Expr) throws -> Expr {
    try self.annotation(from: expr).fontColor = self.optColor(from: value)
    return .void
  }
  
  private func pdfAnnotationStartPoint(expr: Expr) throws -> Expr {
    let annotation = try self.annotation(from: expr)
    let point = annotation.startPoint
    let style = annotation.startLineStyle
    return .pair(.pair(.flonum(point.x), .flonum(point.y)), self.expr(from: style))
  }
  
  private func pdfAnnotationStartPointSet(expr: Expr, value: Expr, style: Expr?) throws -> Expr {
    let annotation = try self.annotation(from: expr)
    if let style {
      guard case .pair(.flonum(let x), .flonum(let y)) = value else {
        throw RuntimeError.eval(.invalidPoint, value)
      }
      annotation.startPoint = CGPoint(x: x, y: y)
      annotation.startLineStyle = try self.lineStyle(from: style)
    } else if case .pair(let fst, let snd) = value {
      if case .pair(.flonum(let x), .flonum(let y)) = fst {
        annotation.startPoint = CGPoint(x: x, y: y)
        annotation.startLineStyle = try self.lineStyle(from: snd)
      } else {
        annotation.startPoint = CGPoint(x: try fst.asDouble(coerce: true),
                                        y: try snd.asDouble(coerce: true))
      }
    } else {
      annotation.startLineStyle = try self.lineStyle(from: value)
    }
    return .void
  }
  
  private func pdfAnnotationEndPoint(expr: Expr) throws -> Expr {
    let annotation = try self.annotation(from: expr)
    let point = annotation.endPoint
    let style = annotation.endLineStyle
    return .pair(.pair(.flonum(point.x), .flonum(point.y)), self.expr(from: style))
  }
  
  private func pdfAnnotationEndPointSet(expr: Expr, value: Expr, style: Expr?) throws -> Expr {
    let annotation = try self.annotation(from: expr)
    if let style {
      guard case .pair(.flonum(let x), .flonum(let y)) = value else {
        throw RuntimeError.eval(.invalidPoint, value)
      }
      annotation.endPoint = CGPoint(x: x, y: y)
      annotation.endLineStyle = try self.lineStyle(from: style)
    } else if case .pair(let fst, let snd) = value {
      if case .pair(.flonum(let x), .flonum(let y)) = fst {
        annotation.endPoint = CGPoint(x: x, y: y)
        annotation.endLineStyle = try self.lineStyle(from: snd)
      } else {
        annotation.endPoint = CGPoint(x: try fst.asDouble(coerce: true),
                                      y: try snd.asDouble(coerce: true))
      }
    } else {
      annotation.endLineStyle = try self.lineStyle(from: value)
    }
    return .void
  }
  
  private func pdfAnnotationIcon(expr: Expr) throws -> Expr {
    return self.iconType.expr(for: try self.annotation(from: expr).iconType)
  }
  
  private func pdfAnnotationIconSet(expr: Expr, value: Expr) throws -> Expr {
    try self.annotation(from: expr).iconType = try self.iconType.value(for: value)
    return .void
  }
  
  private func pdfAnnotationStamp(expr: Expr) throws -> Expr {
    guard let stamp = try self.annotation(from: expr).stampName else {
      return .false
    }
    return .makeString(stamp)
  }
  
  private func pdfAnnotationStampSet(expr: Expr, value: Expr) throws -> Expr {
    guard value.isTrue else {
      try self.annotation(from: expr).stampName = nil
      return .void
    }
    try self.annotation(from: expr).stampName = value.asString()
    return .void
  }
  
  private func pdfAnnotationPopup(expr: Expr) throws -> Expr {
    let annotation = try self.annotation(from: expr)
    guard let popup = annotation.popup else {
      return .false
    }
    return .pair(.object(NativePDFAnnotation(annotation: popup)), .makeBoolean(annotation.isOpen))
  }
  
  private func pdfAnnotationPopupSet(expr: Expr, fst: Expr, snd: Expr?) throws -> Expr {
    let annotation = try self.annotation(from: expr)
    if let snd {
      if !fst.isNull {
        annotation.popup = fst.isTrue ? try self.annotation(from: fst) : nil
      }
      annotation.isOpen = snd.isTrue
    } else if case .pair(let popup, let open) = fst {
      annotation.popup = try self.annotation(from: popup)
      annotation.isOpen = open.isTrue
    } else if fst.isFalse {
      annotation.popup = nil
    } else {
      annotation.popup = try self.annotation(from: fst)
    }
    return .void
  }
  
  private func pdfAnnotationMarkupType(expr: Expr) throws -> Expr {
    return self.markupType.expr(for: try self.annotation(from: expr).markupType)
  }
  
  private func pdfAnnotationMarkupTypeSet(expr: Expr, value: Expr) throws -> Expr {
    try self.annotation(from: expr).markupType = self.markupType.value(for: expr)
    return .void
  }
  
  private func pdfAnnotationMarkupPoints(expr: Expr) throws -> Expr {
    guard let points = try self.annotation(from: expr).quadrilateralPoints,
          !points.isEmpty else {
      return .false
    }
    var res = Expr.null
    for value in points.reversed() {
      #if os(iOS) || os(watchOS) || os(tvOS)
      let point = value.cgPointValue
      #elseif os(macOS)
      let point = value.pointValue
      #endif
      res = .pair(.pair(.flonum(point.x), .flonum(point.y)), res)
    }
    return res
  }
  
  private func pdfAnnotationMarkupPointsSet(expr: Expr, value: Expr) throws -> Expr {
    guard value.isTrue else {
      try self.annotation(from: expr).quadrilateralPoints = nil
      return .void
    }
    var points: [NSValue] = []
    var list = value
    while case .pair(let point, let rest) = list {
      guard case .pair(.flonum(let x), .flonum(let y)) = point else {
        throw RuntimeError.eval(.invalidPoint, point)
      }
      #if os(iOS) || os(watchOS) || os(tvOS)
      points.append(NSValue(cgPoint: CGPoint(x: x, y: y)))
      #elseif os(macOS)
      points.append(NSValue(point: CGPoint(x: x, y: y)))
      #endif
      list = rest
    }
    guard case .null = list else {
      throw RuntimeError.type(value, expected: [.properListType])
    }
    try self.annotation(from: expr).quadrilateralPoints = points
    return .void
  }
  
  private func pdfAnnotationCalloutPoints(expr: Expr) throws -> Expr {
    guard let value = try self.annotation(from: expr).value(forAnnotationKey:
                            .init(rawValue: "CL")),
          let points = value as? NSArray,
          points.count == 4 || points.count == 6,
          let x1 = points[0] as? Double,
          let y1 = points[1] as? Double,
          let x2 = points[2] as? Double,
          let y2 = points[3] as? Double else {
      return .false
    }
    if points.count == 6,
       let x3 = points[4] as? Double,
       let y3 = points[5] as? Double {
      return .pair(.pair(.flonum(x1), .flonum(y1)),
                   .pair(.pair(.flonum(x2), .flonum(y2)),
                         .pair(.pair(.flonum(x3), .flonum(y3)), .null)))
    } else {
      return .pair(.pair(.flonum(x1), .flonum(y1)),
                   .pair(.pair(.flonum(x2), .flonum(y2)), .null))
    }
  }
  
  private func pdfAnnotationCalloutPointsSet(expr: Expr, value: Expr) throws -> Expr {
    switch value {
      case .pair(.pair(let x1, let y1), .pair(.pair(let x2, let y2), .null)):
        try self.annotation(from: expr).setValue([x1.asDouble(coerce: true),
                                                  y1.asDouble(coerce: true),
                                                  x2.asDouble(coerce: true),
                                                  y2.asDouble(coerce: true)],
                                                 forAnnotationKey: .init(rawValue: "CL"))
      case .pair(.pair(let x1, let y1),
                 .pair(.pair(let x2, let y2),
                       .pair(.pair(let x3, let y3), .null))):
        try self.annotation(from: expr).setValue([x1.asDouble(coerce: true),
                                                  y1.asDouble(coerce: true),
                                                  x2.asDouble(coerce: true),
                                                  y2.asDouble(coerce: true),
                                                  x3.asDouble(coerce: true),
                                                  y3.asDouble(coerce: true)],
                                                 forAnnotationKey: .init(rawValue: "CL"))
      default:
        throw RuntimeError.eval(.invalidPDFCalloutPoints, value)
    }
    return .void
  }
  
  private func pdfAnnotationShapes(expr: Expr) throws -> Expr {
    guard let paths = try self.annotation(from: expr).paths else {
      return .false
    }
    var res = Expr.null
    for path in paths {
      res = .pair(.object(Shape(.path(path), closed: false, flipped: false)), res)
    }
    return res
  }
  
  private func pdfAnnotationShapeAdd(expr: Expr, shape: Expr) throws -> Expr {
    guard case .object(let obj) = shape, let shape = obj as? Shape else {
      throw RuntimeError.type(shape, expected: [Shape.type])
    }
    try self.annotation(from: expr).add(shape.compile())
    return .void
  }
  
  private func pdfAnnotationShapesClear(expr: Expr) throws -> Expr {
    let annotation = try self.annotation(from: expr)
    if let paths = annotation.paths {
      for path in paths {
        annotation.remove(path)
      }
    }
    return .void
  }
  
  private func pdfAnnotationModificationDate(expr: Expr) throws -> Expr {
    guard let date = try self.annotation(from: expr).modificationDate else {
      return .false
    }
    return .object(NativeDateTime(DateTimeLibrary.calendar.dateComponents(in: TimeZone.current,
                                                                          from: date)))
  }
  
  private func pdfAnnotationModificationDateSet(expr: Expr, value: Expr) throws -> Expr {
    guard value.isTrue else {
      try self.annotation(from: expr).modificationDate = nil
      return .void
    }
    guard case .object(let obj) = value,
          let ndate = obj as? NativeDateTime,
          let date = ndate.value.date else {
      throw RuntimeError.type(value, expected: [NativeDateTime.type])
    }
    try self.annotation(from: expr).modificationDate = date
    return .void
  }
  
  private func pdfAnnotationUsername(expr: Expr) throws -> Expr {
    guard let username = try self.annotation(from: expr).userName else {
      return .false
    }
    return .makeString(username)
  }
  
  private func pdfAnnotationUsernameSet(expr: Expr, value: Expr) throws -> Expr {
    try self.annotation(from: expr).userName = value.isTrue ? try value.asString() : nil
    return .void
  }
  
  private func pdfAnnotationUrl(expr: Expr) throws -> Expr {
    guard let url = try self.annotation(from: expr).url else {
      return .false
    }
    return .makeString(url.absoluteString)
  }
  
  private func pdfAnnotationUrlSet(expr: Expr, url: Expr) throws -> Expr {
    try self.annotation(from: expr).url = url.isTrue ? url.asURL() : nil
    return .void
  }
  
  private func pdfAnnotationDestination(expr: Expr) throws -> Expr {
    guard let destination = try self.annotation(from: expr).destination else {
      return .false
    }
    return self.expr(for: destination)
  }
  
  private func pdfAnnotationDestinationSet(expr: Expr,
                                           page: Expr,
                                           point: Expr?,
                                           zoom: Expr?) throws -> Expr {
    try self.annotation(from: expr).destination = try self.destination(for: page,
                                                                       point: point,
                                                                       zoom: zoom)
    return .void
  }
  
  private func pdfAnnotationAction(expr: Expr) throws -> Expr {
    guard let action = try self.annotation(from: expr).action else {
      return .false
    }
    return self.expr(for: action)
  }
  
  private func pdfAnnotationActionSet(expr: Expr, value: Expr) throws -> Expr {
    try self.annotation(from: expr).action = value.isTrue ? try self.action(from: value) : nil
    return .void
  }
  
  private func pdfAnnotationDisplay(expr: Expr) throws -> Expr {
    return .makeBoolean(try self.annotation(from: expr).shouldDisplay)
  }
  
  private func pdfAnnotationDisplaySet(expr: Expr, value: Expr) throws -> Expr {
    try self.annotation(from: expr).shouldDisplay = value.isTrue
    return .void
  }
  
  private func pdfAnnotationPrint(expr: Expr) throws -> Expr {
    return .makeBoolean(try self.annotation(from: expr).shouldPrint)
  }
  
  private func pdfAnnotationPrintSet(expr: Expr, value: Expr) throws -> Expr {
    try self.annotation(from: expr).shouldPrint = value.isTrue
    return .void
  }
  
  private func pdfAnnotationHighlighted(expr: Expr) throws -> Expr {
    return .makeBoolean(try self.annotation(from: expr).isHighlighted)
  }
  
  private func pdfAnnotationHighlightedSet(expr: Expr, value: Expr) throws -> Expr {
    try self.annotation(from: expr).isHighlighted = value.isTrue
    return .void
  }
  
  private func pdfAnnotationAttributes(expr: Expr) throws -> Expr {
    let dict = try self.annotation(from: expr).annotationKeyValues
    var res = Expr.null
    for (key, value) in dict {
      if let expr = self.attributeExpr(from: value) {
        res = .pair(.pair(.symbol(self.context.symbols.intern(key.description)), expr), res)
      } else {
        res = .pair(.pair(.symbol(self.context.symbols.intern(key.description)), .null), res)
      }
    }
    return res
  }
  
  private func pdfAnnotationAttributesRef(expr: Expr, key: Expr, default: Expr?) throws -> Expr {
    if let res = try self.annotation(from: expr).value(forAnnotationKey:
        .init(rawValue: try key.asSymbol().identifier)) {
      return self.attributeExpr(from: res) ?? .null
    } else {
      return `default` ?? .false
    }
  }
  
  private func pdfAnnotationAttributesSet(expr: Expr, key: Expr, value: Expr) throws -> Expr {
    guard let v = self.attributeValue(from: value) else {
      throw RuntimeError.eval(.cannotMapAttributeValue, value)
    }
    try self.annotation(from: expr).setValue(v, forAnnotationKey:
          .init(rawValue: try key.asSymbol().identifier))
    return .void
  }
  
  private func pdfAnnotationAttributesRemove(expr: Expr, key: Expr) throws -> Expr {
    try self.annotation(from: expr).removeValue(forAnnotationKey:
          .init(rawValue: key.asSymbol().identifier))
    return .void
  }
  
  private func drawPdfAnnotation(expr: Expr, box: Expr, rect: Expr, d: Expr) throws -> Expr {
    let annotation = try self.annotation(from: expr)
    let displayBox = try self.displayBox.value(for: box)
    guard case .pair(.pair(.flonum(let x), .flonum(let y)),
                     .pair(.flonum(let w), .flonum(let h))) = rect else {
      throw RuntimeError.eval(.invalidRect, rect)
    }
    guard case .object(let obj) = d, let drawing = obj as? Drawing else {
      throw RuntimeError.type(expr, expected: [Drawing.type])
    }
    drawing.append(.annotation(annotation, displayBox, CGRect(x: x, y: y, width: w, height: h)))
    return .void
  }
  
  
}

///
/// Proxy for PDF documents
/// 
public struct NativePDFDocument: CustomExpr {
  public static let type = Type.objectType(Symbol(uninterned: "pdf"))
  
  public let document: PDFDocument
  
  public var type: Type {
    return Self.type
  }
  
  public var tagString: String {
    if let url = self.document.documentURL {
      if url.isFileURL {
        return "pdf \(self.identityString): pages=\(self.document.pageCount), " +
               "path=\"\(url.path(percentEncoded: false))\""
      } else {
        return "pdf \(self.identityString): pages=\(self.document.pageCount), url=\"\(url)\""
      }
    } else {
      return "pdf \(self.identityString): pages=\(self.document.pageCount)"
    }
  }
  
  public var identity: UInt {
    return UInt(bitPattern: ObjectIdentifier(self.document))
  }
  
  public var identityString: String {
    return String(self.identity, radix: 16)
  }
  
  public var hash: Int {
    return self.document.hash
  }
  
  public func equals(to expr: Expr) -> Bool {
    guard case .object(let obj) = expr,
          let other = obj as? NativePDFDocument else {
      return false
    }
    return self.document === other.document
  }
  
  public func unpack(in context: Context) -> Exprs {
    var path: Expr
    if let docUrl = self.document.documentURL {
      path = .makeString(docUrl.path(percentEncoded: true))
    } else {
      path = .false
    }
    return [.makeString(self.identityString),
            .makeNumber(self.document.pageCount),
            path]
  }
}

///
/// Proxy for PDF pages
/// 
public struct NativePDFPage: CustomExpr {
  public static let type = Type.objectType(Symbol(uninterned: "pdf-page"))
  
  public let page: PDFPage
  
  public var type: Type {
    return Self.type
  }
  
  public var tagString: String {
    if let label = self.page.label {
      if let doc = self.page.document {
        let ndoc = NativePDFDocument(document: doc)
        return "pdf-page \(self.identityString): doc=\(ndoc.identityString), label=\"\(label)\""
      } else {
        return "pdf-page \(self.identityString): label=\"\(label)\""
      }
    } else if let doc = self.page.document {
      let ndoc = NativePDFDocument(document: doc)
      return "pdf-page \(self.identityString): doc=\(ndoc.identityString)"
    } else {
      return "pdf-page \(self.identityString)"
    }
  }
  
  public var identity: UInt {
    return UInt(bitPattern: ObjectIdentifier(self.page))
  }
  
  public var identityString: String {
    return String(self.identity, radix: 16)
  }
  
  public var hash: Int {
    return self.page.hash
  }
  
  public func equals(to expr: Expr) -> Bool {
    guard case .object(let obj) = expr,
          let other = obj as? NativePDFPage else {
      return false
    }
    return self.page === other.page
  }
  
  public func unpack(in context: Context) -> Exprs {
    let doc: Expr
    if let document = self.page.document {
      doc = .object(NativePDFDocument(document: document))
    } else {
      doc = .false
    }
    let label: Expr
    if let lbl = self.page.label {
      label = .makeString(lbl)
    } else {
      label = .false
    }
    return [.makeString(self.identityString), doc, label]
  }
}

///
/// Proxy for PDF annotations
/// 
public struct NativePDFAnnotation: CustomExpr {
  public static let type = Type.objectType(Symbol(uninterned: "pdf-annotation"))
  
  public let annotation: PDFAnnotation
  
  public var type: Type {
    return Self.type
  }
  
  public var tagString: String {
    let fmt = NumberFormatter()
    fmt.minimumFractionDigits = 2
    fmt.maximumFractionDigits = 2
    fmt.roundingMode = .halfEven
    fmt.numberStyle = .decimal
    fmt.usesGroupingSeparator = false
    let bounds = "(\(fmt.string(for: self.annotation.bounds.width) ?? "?") × " +
                 "\(fmt.string(for: self.annotation.bounds.height) ?? "?"))@" +
                 "(\(fmt.string(for: self.annotation.bounds.origin.x) ?? "?"), " +
                 "\(fmt.string(for: self.annotation.bounds.origin.y) ?? "?"))"
    let type: String?
    if let str = self.annotation.type {
      switch str {
        case "Circle", "Highlight", "Ink", "Link", "Popup", "Square", "Stamp", "Text",
             "Underline", "Widget":
          type = str.firstLowercased
        case "FreeText":
          type = "free-text"
        case "StrikeOut":
          type = "strike-out"
        default:
          type = str
      }
    } else {
      type = nil
    }
    let initial: String
    if let popup = self.annotation.popup {
      initial = "pdf-annotation \(self.identityString): popup=\(String(UInt(bitPattern: ObjectIdentifier(popup)), radix: 16)), "
    } else {
      initial = "pdf-annotation \(self.identityString): "
    }
    if let page = self.annotation.page {
      let npage = NativePDFPage(page: page)
      if let type {
        return "\(initial)page=\(npage.identityString), type=\(type), bounds=\(bounds)"
      } else {
        return "\(initial)page=\(npage.identityString), bounds=\(bounds)"
      }
    } else if let type {
      return "\(initial)type=\(type), bounds=\(bounds)"
    } else {
      return "\(initial)bounds=\(bounds)"
    }
  }
  
  public var identity: UInt {
    return UInt(bitPattern: ObjectIdentifier(self.annotation))
  }
  
  public var identityString: String {
    return String(self.identity, radix: 16)
  }
  
  public var hash: Int {
    return self.annotation.hash
  }
  
  public func equals(to expr: Expr) -> Bool {
    guard case .object(let obj) = expr,
          let other = obj as? NativePDFAnnotation else {
      return false
    }
    return self.annotation === other.annotation
  }
  
  public func unpack(in context: Context) -> Exprs {
    let p: Expr
    if let page = self.annotation.page {
      p = .object(NativePDFPage(page: page))
    } else {
      p = .false
    }
    let t: Expr
    if let type = self.annotation.type {
      t = .makeString(type)
    } else {
      t = .false
    }
    return [
      .makeString(self.identityString),
      p,
      t,
      .pair(.flonum(self.annotation.bounds.origin.x),
            .pair(.flonum(self.annotation.bounds.origin.y),
                  .pair(.flonum(self.annotation.bounds.width),
                        .pair(.flonum(self.annotation.bounds.height), .null))))
    ]
  }
}

///
/// Proxy for PDF outlines
/// 
public struct NativePDFOutline: CustomExpr {
  public static let type = Type.objectType(Symbol(uninterned: "pdf-outline"))
  
  public let outline: PDFOutline
  
  public var type: Type {
    return Self.type
  }
  
  public var tagString: String {
    let parent: NativePDFOutline?
    if let p = self.outline.parent {
      parent = NativePDFOutline(outline: p)
    } else {
      parent = nil
    }
    let index = self.outline.index
    let label = self.outline.label
    if let parent {
      if let label {
        return "pdf-outline \(self.identityString): parent=\(parent.identityString), " +
               "index=\(index), children=\(self.outline.numberOfChildren), label=\"\(label)\""
      } else {
        return "pdf-outline \(self.identityString): parent=\(parent.identityString), " +
               "index=\(index), children=\(self.outline.numberOfChildren)"
      }
    } else if let label {
      return "pdf-outline \(self.identityString): index=\(index), " +
             "children=\(self.outline.numberOfChildren), label=\"\(label)\""
    } else {
      return "pdf-outline \(self.identityString): index=\(index), " +
             "children=\(self.outline.numberOfChildren)"
    }
  }
  
  public var identity: UInt {
    return UInt(bitPattern: ObjectIdentifier(self.outline))
  }
  
  public var identityString: String {
    return String(self.identity, radix: 16)
  }
  
  public var hash: Int {
    return self.outline.hash
  }
  
  public func equals(to expr: Expr) -> Bool {
    guard case .object(let obj) = expr,
          let other = obj as? NativePDFOutline else {
      return false
    }
    return self.outline === other.outline
  }
  
  public func unpack(in context: Context) -> Exprs {
    let parent: NativePDFOutline?
    if let p = self.outline.parent {
      parent = NativePDFOutline(outline: p)
    } else {
      parent = nil
    }
    let index = self.outline.index
    let label = self.outline.label
    return [
      .makeString(self.identityString),
      parent == nil ? .false : .makeString(parent!.identityString),
      .makeNumber(index),
      label == nil ? .false : .makeString(label!),
      .makeNumber(self.outline.numberOfChildren)
    ]
  }
}

public final class LispKitPDFDocument: PDFDocument {
  
  public init(delegate: LispKitPDFDocumentDelegate) {
    super.init()
    self.delegate = delegate
  }
  
  public init?(data: Data, delegate: LispKitPDFDocumentDelegate) {
    super.init(data: data)
    self.delegate = delegate
  }
  
  public init?(url: URL, delegate: LispKitPDFDocumentDelegate) {
    super.init(url: url)
    self.delegate = delegate
  }
  
  public var hasDrawings: Bool {
    for y in stride(from: 0, to: self.pageCount, by: 1) {
      guard let page = self.page(at: y) as? LispKitPDFPage else {
        return true
      }
      if page.underlay != nil || page.overlay != nil {
        return true
      }
    }
    return false
  }
  
  public func options(userPassword: String? = nil,
                      ownerPassword: String? = nil,
                      accessPermissions: PDFAccessPermissions? = nil,
                      burnInAnnotations: Bool? = nil,
                      optimizeImagesForScreen: Bool? = nil,
                      saveImagesAsJPEG: Bool? = nil,
                      saveTextFromOCR: Bool? = nil) -> [PDFDocumentWriteOption : Any]? {
    var res: [PDFDocumentWriteOption : Any] = [.accessPermissionsOption :
                                                  accessPermissions ?? self.accessPermissions]
    if let userPassword {
      res[.userPasswordOption] = userPassword
    }
    if let ownerPassword {
      res[.ownerPasswordOption] = ownerPassword
    }
    if let burnInAnnotations {
      res[.burnInAnnotationsOption] = burnInAnnotations
    }
    if let optimizeImagesForScreen {
      res[.optimizeImagesForScreenOption] = optimizeImagesForScreen
    }
    if let saveTextFromOCR {
      res[.saveTextFromOCROption] = saveTextFromOCR
    }
    return res
  }
  
  public func persistDrawings() -> PDFDocument? {
    guard self.hasDrawings else {
      return self
    }
    let data = NSMutableData()
    guard let consumer = CGDataConsumer(data: data as CFMutableData),
          let context = CGContext(consumer: consumer, mediaBox: nil, nil) else {
      return nil
    }
    for y in stride(from: 0, to: self.pageCount, by: 1) {
      guard let page: PDFPage = self.page(at: y) else {
        return nil
      }
      var mediaBox = page.bounds(for: .mediaBox)
      var trimBox = page.bounds(for: .trimBox)
      var cropBox = page.bounds(for: .cropBox)
      var bleedBox = page.bounds(for: .bleedBox)
      var artBox = page.bounds(for: .bleedBox)
      let mediaData = NSData(bytes: &mediaBox, length: MemoryLayout.size(ofValue: mediaBox))
      let trimData = NSData(bytes: &trimBox, length: MemoryLayout.size(ofValue: trimBox))
      let cropData = NSData(bytes: &cropBox, length: MemoryLayout.size(ofValue: cropBox))
      let bleedData = NSData(bytes: &bleedBox, length: MemoryLayout.size(ofValue: bleedBox))
      let artData = NSData(bytes: &artBox, length: MemoryLayout.size(ofValue: artBox))
      #if os(macOS)
      let previousContext = NSGraphicsContext.current
      NSGraphicsContext.current = NSGraphicsContext(cgContext: context, flipped: true)
      defer {
        NSGraphicsContext.current = previousContext
      }
      #endif
      context.beginPDFPage([kCGPDFContextMediaBox as String: mediaData,
                            kCGPDFContextTrimBox as String: trimData,
                            kCGPDFContextCropBox as String: cropData,
                            kCGPDFContextBleedBox as String: bleedData,
                            kCGPDFContextArtBox as String: artData] as CFDictionary)
      let displaysAnnotations = page.displaysAnnotations
      page.displaysAnnotations = false
      page.draw(with: .mediaBox, to: context)
      page.displaysAnnotations = displaysAnnotations
      context.endPDFPage()
    }
    context.closePDF()
    guard let flattened = PDFDocument(data: data as Data) else {
      return nil
    }
    flattened.documentAttributes = self.documentAttributes
    flattened.outlineRoot = self.outlineRoot
    for i in 0..<self.pageCount {
      guard let oldPage = self.page(at: i), let newPage = flattened.page(at: i) else {
        continue
      }
      newPage.displaysAnnotations = oldPage.displaysAnnotations
      newPage.rotation = oldPage.rotation
      for annotation in oldPage.annotations {
        newPage.addAnnotation(annotation)
      }
    }
    return flattened
  }
}

public final class LispKitPDFPage: PDFPage {
  var underlay: Drawing? = nil
  var overlay: Drawing? = nil
  
  public override func copy(with zone: NSZone?) -> Any {
    let copy = super.copy(with: zone) as! LispKitPDFPage
    copy.underlay = self.underlay
    copy.overlay = self.overlay
    return copy
  }
  
  #if os(iOS) || os(watchOS) || os(tvOS)
  
  public override func draw(with box: PDFDisplayBox, to context: CGContext) {
    let pageBounds = self.bounds(for: box)
    if let underlay {
      context.saveGState()
      context.translateBy(x: 0.0, y: pageBounds.size.height)
      context.scaleBy(x: 1.0, y: -1.0)
      underlay.drawInline(in: context)
      context.restoreGState()
    }
    super.draw(with: box, to: context)
    if let overlay {
      context.saveGState()
      context.translateBy(x: 0.0, y: pageBounds.size.height)
      context.scaleBy(x: 1.0, y: -1.0)
      overlay.drawInline(in: context)
      context.restoreGState()
    }
  }
  
  public var images: [UIImage] {
    guard let page = self.pageRef,
          let dictionary = page.dictionary,
          let resources = dictionary[CGPDFDictionaryGetDictionary, "Resources"] else {
      return []
    }
    if let xObject = resources[CGPDFDictionaryGetDictionary, "XObject"] {
      var imageKeys: [String] = []
      CGPDFDictionaryApplyBlock(xObject, { key, object, _ in
        var stream: CGPDFStreamRef?
        guard CGPDFObjectGetValue(object, .stream, &stream),
              let objectStream = stream,
              let streamDictionary = CGPDFStreamGetDictionary(objectStream) else {
          return true
        }
        var subtype: UnsafePointer<Int8>?
        guard CGPDFDictionaryGetName(streamDictionary, "Subtype", &subtype),
              let subtypeName = subtype else {
          return true
        }
        if String(cString: subtypeName) == "Image" {
          imageKeys.append(String(cString: key))
        }
        return true
      }, nil)
      return imageKeys.compactMap { imageKey -> UIImage? in
        var stream: CGPDFStreamRef?
        guard CGPDFDictionaryGetStream(xObject, imageKey, &stream), let imageStream = stream else {
          return nil
        }
        guard let dictionary = CGPDFStreamGetDictionary(imageStream) else {
          return nil
        }
        var format: CGPDFDataFormat = .raw
        guard let data = CGPDFStreamCopyData(imageStream, &format) else {
          return nil
        }
        if format == .JPEG2000 || format == .jpegEncoded {
          if let cspace = try? dictionary[CGPDFDictionaryGetObject, "ColorSpace"]?.getColorSpace(),
             let provider = CGDataProvider(data: data),
             let embeddedImage = CGImage(jpegDataProviderSource: provider,
                                         decode: nil,
                                         shouldInterpolate: false,
                                         intent: .defaultIntent),
             let ci = embeddedImage.copy(colorSpace: cspace) {
            return UIImage(cgImage: ci)
          } else {
            return try? self.getNSImage(data: data as CFData, info: dictionary)
          }
        } else {
          return try? self.getNSImage(data: data as CFData, info: dictionary)
        }
      }
    } else {
      return []
    }
  }
  
  private func getNSImage(data: CFData, info: CGPDFDictionaryRef) throws -> UIImage {
    guard let colorSpace = try info[CGPDFDictionaryGetObject, "ColorSpace"]?.getColorSpace() else {
      throw RawDecodingError.noColorSpace(info.getNameArray(for: "Filter"))
    }
    guard let width = info[CGPDFDictionaryGetInteger, "Width"],
          let height = info[CGPDFDictionaryGetInteger, "Height"] else {
        throw RawDecodingError.cannotReadSize
    }
    guard let bitsPerComponent = info[CGPDFDictionaryGetInteger, "BitsPerComponent"] else {
      throw RawDecodingError.cannotReadBitsPerComponent
    }
    let decode: [CGFloat]?
    if let decodeRef = info[CGPDFDictionaryGetArray, "Decode"] {
      let count = CGPDFArrayGetCount(decodeRef)
      decode = (0..<count).map {
        decodeRef[CGPDFArrayGetNumber, $0]!
      }
    } else {
      decode = nil
    }
    guard let databuffer = CGDataProvider(data: data) else {
      throw RawDecodingError.cannotConstructImage
    }
    guard let image = CGImage(
      width: width,
      height: height,
      bitsPerComponent: bitsPerComponent,
      bitsPerPixel: bitsPerComponent * colorSpace.numberOfComponents,
      bytesPerRow: Int((Double(width * bitsPerComponent * colorSpace.numberOfComponents) /
                          8.0).rounded(.up)),
      space: colorSpace,
      bitmapInfo: CGBitmapInfo(),
      provider: databuffer,
      decode: decode,
      shouldInterpolate: false,
      intent: .defaultIntent
    ) else {
      throw RawDecodingError.cannotConstructImage
    }
    return UIImage(cgImage: image)
  }
  
  #elseif os(macOS)
  
  public override func draw(with box: PDFDisplayBox, to context: CGContext) {
    var nscontext: NSGraphicsContext? = nil
    let pageBounds = self.bounds(for: box)
    if let underlay {
      context.saveGState()
      context.translateBy(x: 0.0, y: pageBounds.size.height)
      context.scaleBy(x: 1.0, y: -1.0)
      if nscontext == nil {
        nscontext = NSGraphicsContext(cgContext: context, flipped: false)
      }
      underlay.drawInline(in: nscontext!)
      context.restoreGState()
    }
    super.draw(with: box, to: context)
    if let overlay {
      context.saveGState()
      context.translateBy(x: 0.0, y: pageBounds.size.height)
      context.scaleBy(x: 1.0, y: -1.0)
      if nscontext == nil {
        nscontext = NSGraphicsContext(cgContext: context, flipped: false)
      }
      overlay.drawInline(in: nscontext!)
      context.restoreGState()
    }
  }
  
  public var images: [NSImage] {
    guard let page = self.pageRef,
          let dictionary = page.dictionary,
          let resources = dictionary[CGPDFDictionaryGetDictionary, "Resources"] else {
      return []
    }
    if let xObject = resources[CGPDFDictionaryGetDictionary, "XObject"] {
      var imageKeys: [String] = []
      CGPDFDictionaryApplyBlock(xObject, { key, object, _ in
        var stream: CGPDFStreamRef?
        guard CGPDFObjectGetValue(object, .stream, &stream),
              let objectStream = stream,
              let streamDictionary = CGPDFStreamGetDictionary(objectStream) else {
          return true
        }
        var subtype: UnsafePointer<Int8>?
        guard CGPDFDictionaryGetName(streamDictionary, "Subtype", &subtype),
              let subtypeName = subtype else {
          return true
        }
        if String(cString: subtypeName) == "Image" {
          imageKeys.append(String(cString: key))
        }
        return true
      }, nil)
      return imageKeys.compactMap { imageKey -> NSImage? in
        var stream: CGPDFStreamRef?
        guard CGPDFDictionaryGetStream(xObject, imageKey, &stream), let imageStream = stream else {
          return nil
        }
        guard let dictionary = CGPDFStreamGetDictionary(imageStream) else {
          return nil
        }
        var format: CGPDFDataFormat = .raw
        guard let data = CGPDFStreamCopyData(imageStream, &format) else {
          return nil
        }
        if format == .JPEG2000 || format == .jpegEncoded {
          if let colorSpace = try? dictionary[CGPDFDictionaryGetObject, "ColorSpace"]?
                                     .getColorSpace(),
             let provider = CGDataProvider(data: data),
             let embeddedImage = CGImage(jpegDataProviderSource: provider,
                                         decode: nil,
                                         shouldInterpolate: false,
                                         intent: .defaultIntent),
             let ci = embeddedImage.copy(colorSpace: colorSpace) {
            return NSImage(cgImage: ci, size: NSSize(width: ci.width, height: ci.height))
          } else {
            return try? self.getNSImage(data: data as CFData, info: dictionary)
          }
        } else {
          return try? self.getNSImage(data: data as CFData, info: dictionary)
        }
      }
    } else {
      return []
    }
  }
  
  private func getNSImage(data: CFData, info: CGPDFDictionaryRef) throws -> NSImage {
    guard let colorSpace = try info[CGPDFDictionaryGetObject, "ColorSpace"]?.getColorSpace() else {
      throw RawDecodingError.noColorSpace(info.getNameArray(for: "Filter"))
    }
    guard let width = info[CGPDFDictionaryGetInteger, "Width"],
          let height = info[CGPDFDictionaryGetInteger, "Height"] else {
        throw RawDecodingError.cannotReadSize
    }
    guard let bitsPerComponent = info[CGPDFDictionaryGetInteger, "BitsPerComponent"] else {
      throw RawDecodingError.cannotReadBitsPerComponent
    }
    let decode: [CGFloat]?
    if let decodeRef = info[CGPDFDictionaryGetArray, "Decode"] {
      let count = CGPDFArrayGetCount(decodeRef)
      decode = (0..<count).map {
        decodeRef[CGPDFArrayGetNumber, $0]!
      }
    } else {
      decode = nil
    }
    guard let databuffer = CGDataProvider(data: data) else {
      throw RawDecodingError.cannotConstructImage
    }
    guard let image = CGImage(
      width: width,
      height: height,
      bitsPerComponent: bitsPerComponent,
      bitsPerPixel: bitsPerComponent * colorSpace.numberOfComponents,
      bytesPerRow: Int((Double(width * bitsPerComponent * colorSpace.numberOfComponents) /
                          8.0).rounded(.up)),
      space: colorSpace,
      bitmapInfo: CGBitmapInfo(),
      provider: databuffer,
      decode: decode,
      shouldInterpolate: false,
      intent: .defaultIntent
    ) else {
      throw RawDecodingError.cannotConstructImage
    }
    return NSImage(cgImage: image, size: NSSize(width: width, height: height))
  }
  #endif
}

public final class LispKitPDFDocumentDelegate: NSObject, PDFDocumentDelegate {
  public func classForPage() -> AnyClass {
      return LispKitPDFPage.self
  }
}

protocol DefaultInitializer {
  init()
}

extension Int: DefaultInitializer {}

extension CGFloat: DefaultInitializer {}

enum RawDecodingError: Error {
  case cannotConstructImage
  case cannotReadSize
  case cannotReadBitsPerComponent
  case noColorSpace([String]?)
  case unkownColorSpace(String)
  case corruptColorSpace
  case noLookupTable
}

extension CGPDFObjectRef {
  
  func getName<K>(_ key: K,
                  _ getter: (OpaquePointer, K, UnsafeMutablePointer<UnsafePointer<Int8>?>) -> Bool)
      -> String? {
    guard let pointer = self[getter, key] else {
      return nil
    }
    return String(cString: pointer)
  }

  func getName<K>(_ key: K,
                  _ getter: (OpaquePointer, K, UnsafeMutableRawPointer?)->Bool) -> String? {
    var result: UnsafePointer<UInt8>!
    guard getter(self, key, &result) else {
      return nil
    }
    return String(cString: result)
  }

  subscript<R, K>(_ getter: (OpaquePointer, K, UnsafeMutablePointer<R?>)->Bool, _ key: K) -> R? {
    var result: R!
    guard getter(self, key, &result) else {
      return nil
    }
    return result
  }

  subscript<R: DefaultInitializer, K>(_ getter: (OpaquePointer, K, UnsafeMutablePointer<R>) -> Bool,
                                      _ key: K) -> R? {
    var result = R()
    guard getter(self, key, &result) else {
      return nil
    }
    return result
  }
  
  func getNameArray(for key: String) -> [String]? {
    var object: CGPDFObjectRef!
    guard CGPDFDictionaryGetObject(self, key, &object) else {
      return nil
    }
    if let name = object.getName(.name, CGPDFObjectGetValue) {
      return [name]
    } else {
      var array: CGPDFArrayRef!
      guard CGPDFObjectGetValue(self, .array, &array) else {
        return nil
      }
      var names = [String]()
      for index in 0..<CGPDFArrayGetCount(array) {
        guard let name = array.getName(index, CGPDFArrayGetName) else {
          continue
        }
        names.append(name)
      }
      assert(names.count == CGPDFArrayGetCount(array))
      return names
    }
  }

  func getColorSpace() throws -> CGColorSpace {
    if let name = getName(.name, CGPDFObjectGetValue) {
      switch name {
        case "DeviceGray":
          return CGColorSpaceCreateDeviceGray()
        case "DeviceRGB":
          return CGColorSpaceCreateDeviceRGB()
        case "DeviceCMYK":
          return CGColorSpaceCreateDeviceCMYK()
        default:
          throw RawDecodingError.unkownColorSpace(name)
      }
    } else {
      var array: CGPDFArrayRef!
      guard CGPDFObjectGetValue(self, .array, &array) else {
        throw RawDecodingError.corruptColorSpace
      }
      guard let name = array.getName(0, CGPDFArrayGetName) else {
        throw RawDecodingError.corruptColorSpace
      }
      switch name {
        case "CalRGB":
          return CGColorSpaceCreateDeviceRGB()
        case "CalGray":
          return CGColorSpaceCreateDeviceGray()
        case "Indexed":
          guard let base = try array[CGPDFArrayGetObject, 1]?.getColorSpace(),
                let hival = array[CGPDFArrayGetInteger, 2],
                hival < 256 else {
            throw RawDecodingError.corruptColorSpace
          }
          let colorSpace: CGColorSpace?
          if let lookupTable = array[CGPDFArrayGetString, 3] {
            guard let pointer = CGPDFStringGetBytePtr(lookupTable) else {
              throw RawDecodingError.corruptColorSpace
            }
            colorSpace = CGColorSpace(indexedBaseSpace: base, last: hival, colorTable: pointer)
          } else if let lookupTable = array[CGPDFArrayGetStream, 3] {
            var format = CGPDFDataFormat.raw
            guard let data = CGPDFStreamCopyData(lookupTable, &format) else {
              throw RawDecodingError.corruptColorSpace
            }
            colorSpace = CGColorSpace(indexedBaseSpace: base,
                                      last: hival,
                                      colorTable: CFDataGetBytePtr(data))
          } else {
            throw RawDecodingError.noLookupTable
          }
          guard let result = colorSpace else {
            throw RawDecodingError.corruptColorSpace
          }
          return result
        case "ICCBased":
          var format = CGPDFDataFormat.raw
          guard let stream = array[CGPDFArrayGetStream, 1],
                let info = CGPDFStreamGetDictionary(stream),
                let componentCount = info[CGPDFDictionaryGetInteger, "N"],
                let profileData = CGPDFStreamCopyData(stream, &format),
                let profile = CGDataProvider(data: profileData) else {
            throw RawDecodingError.corruptColorSpace
          }
          let alternate = try info[CGPDFDictionaryGetObject, "Alternate"]?.getColorSpace()
          guard let colorSpace = CGColorSpace(iccBasedNComponents: componentCount,
                                              range: nil,
                                              profile: profile,
                                              alternate: alternate) else {
            throw RawDecodingError.corruptColorSpace
          }
          return colorSpace
        case "Lab":
          guard let info = array[CGPDFArrayGetDictionary, 1],
                let whitePointRef = info[CGPDFDictionaryGetArray, "WhitePoint"]?.asFloatArray() else {
            throw RawDecodingError.corruptColorSpace
          }
          guard let colorSpace =
              CGColorSpace(labWhitePoint: whitePointRef,
                           blackPoint: info[CGPDFDictionaryGetArray, "BlackPoint"]?.asFloatArray(),
                           range: info[CGPDFDictionaryGetArray, "Range"]?.asFloatArray()) else {
            throw RawDecodingError.corruptColorSpace
          }
          return colorSpace
        default:
          throw RawDecodingError.unkownColorSpace(name)
      }
    }
  }

  func asFloatArray() -> [CGFloat] {
    return (0..<CGPDFArrayGetCount(self)).map {
      self[CGPDFArrayGetNumber, $0]!
    }
  }
}
