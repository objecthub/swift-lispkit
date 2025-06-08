//
//  DrawMapLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 08/06/2025.
//  Copyright © 2025 ObjectHub. All rights reserved.
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
import MapKit

public final class DrawMapLibrary: NativeLibrary {
  
  /// Map types
  
  // standard: A street map that shows the position of all roads and some road names.
  private let standard: Symbol
  
  // satellite: Satellite imagery of the area.
  private let satellite: Symbol
  
  // satelliteFlyover: A satellite image of the area with flyover data where available.
  private let satelliteFlyover: Symbol
  
  // hybrid: A satellite image of the area with road and road name information layered on top.
  private let hybrid: Symbol
  
  // hybridFlyover: A hybrid satellite image with flyover data where available.
  private let hybridFlyover: Symbol
  
  // mutedStandard: A street map where your data is emphasized over the underlying map details.
  private let mutedStandard: Symbol
  
  /// Point of interest categories
  
  // airport: point of interest category for airports
  private let airport: Symbol

  // amusementPark: point of interest category for amusement parks
  private let amusementPark: Symbol

  // aquarium: point of interest category for aquariums
  private let aquarium: Symbol

  // atm: point of interest category for ATM machines
  private let atm: Symbol

  // bakery: point of interest category for bakeries
  private let bakery: Symbol

  // bank: point of interest category for banks
  private let bank: Symbol

  // beach: point of interest category for beaches
  private let beach: Symbol

  // brewery: point of interest category for breweries
  private let brewery: Symbol

  // cafe: point of interest category for cafes
  private let cafe: Symbol

  // campground: point of interest category for campgrounds
  private let campground: Symbol

  // carRental: point of interest category for car rentals
  private let carRental: Symbol

  // evCharger: point of interest category for EV chargers
  private let evCharger: Symbol

  // fireStation: point of interest category for fire stations
  private let fireStation: Symbol

  // fitnessCenter: point of interest category for fitness centers
  private let fitnessCenter: Symbol

  // supermarket: point of interest category for food markets, supermarkets, grocery stores,
  //              and convenience stores
  private let supermarket: Symbol

  // gasStation: point of interest category for gas stations
  private let gasStation: Symbol

  // hospital: point of interest category for hospitals
  private let hospital: Symbol

  // hotel: point of interest category for hotels
  private let hotel: Symbol

  // laundry: point of interest category for laundries
  private let laundry: Symbol

  // library: point of interest category for libraries
  private let library: Symbol

  // marina: point of interest category for marinas
  private let marina: Symbol

  // movieTheater: point of interest category for movie theaters
  private let movieTheater: Symbol

  // museum: point of interest category for museums
  private let museum: Symbol

  // nationalPark: point of interest category for national parks
  private let nationalPark: Symbol

  // nightlife: point of interest category for nightlife
  private let nightlife: Symbol

  // park: point of interest category for parks
  private let park: Symbol

  // parking: point of interest category for parking locations
  private let parking: Symbol

  // pharmacy: point of interest category for pharmacies
  private let pharmacy: Symbol

  // police: point of interest category for police
  private let police: Symbol

  // postOffice: point of interest category for post offices
  private let postOffice: Symbol

  // publicTransport: point of interest category for locations of public transportation
  private let publicTransport: Symbol

  // restaurant: point of interest category for restaurants
  private let restaurant: Symbol

  // restroom: point of interest category for restrooms
  private let restroom: Symbol

  // school: point of interest category for schools
  private let school: Symbol

  // stadium: point of interest category for stadiums
  private let stadium: Symbol

  // store: point of interest category for stores
  private let store: Symbol

  // theater: point of interest category for theaters
  private let theater: Symbol

  // university: point of interest category for universities
  private let university: Symbol

  // winery: point of interest category for wineries
  private let winery: Symbol

  // zoo: point of interest category for zoos
  private let zoo: Symbol
  
  /// Initialize symbols.
  public required init(in context: Context) throws {
    self.standard = context.symbols.intern("standard")
    self.satellite = context.symbols.intern("satellite")
    self.satelliteFlyover = context.symbols.intern("satellite-flyover")
    self.hybrid = context.symbols.intern("hybrid")
    self.hybridFlyover = context.symbols.intern("hybrid-flyover")
    self.mutedStandard = context.symbols.intern("standard-muted")
    self.airport = context.symbols.intern("airport")
    self.amusementPark = context.symbols.intern("amusement-park")
    self.aquarium = context.symbols.intern("aquarium")
    self.atm = context.symbols.intern("atm")
    self.bakery = context.symbols.intern("bakery")
    self.bank = context.symbols.intern("bank")
    self.beach = context.symbols.intern("beach")
    self.brewery = context.symbols.intern("brewery")
    self.cafe = context.symbols.intern("cafe")
    self.campground = context.symbols.intern("campground")
    self.carRental = context.symbols.intern("car-rental")
    self.evCharger = context.symbols.intern("ev-charger")
    self.fireStation = context.symbols.intern("fire-station")
    self.fitnessCenter = context.symbols.intern("fitness-center")
    self.supermarket = context.symbols.intern("supermarket")
    self.gasStation = context.symbols.intern("gas-station")
    self.hospital = context.symbols.intern("hospital")
    self.hotel = context.symbols.intern("hotel")
    self.laundry = context.symbols.intern("laundry")
    self.library = context.symbols.intern("library")
    self.marina = context.symbols.intern("marina")
    self.movieTheater = context.symbols.intern("movie-theater")
    self.museum = context.symbols.intern("museum")
    self.nationalPark = context.symbols.intern("national-park")
    self.nightlife = context.symbols.intern("nightlife")
    self.park = context.symbols.intern("park")
    self.parking = context.symbols.intern("parking")
    self.pharmacy = context.symbols.intern("pharmacy")
    self.police = context.symbols.intern("police")
    self.postOffice = context.symbols.intern("post-office")
    self.publicTransport = context.symbols.intern("public-transport")
    self.restaurant = context.symbols.intern("restaurant")
    self.restroom = context.symbols.intern("restroom")
    self.school = context.symbols.intern("school")
    self.stadium = context.symbols.intern("stadium")
    self.store = context.symbols.intern("store")
    self.theater = context.symbols.intern("theater")
    self.university = context.symbols.intern("university")
    self.winery = context.symbols.intern("winery")
    self.zoo = context.symbols.intern("zoo")
    try super.init(in: context)
  }
  
  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "draw", "map"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
  }

  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("map-snapshot?", self.isMapSnapshot))
    self.define(Procedure("make-map-snapshot", self.makeMapSnapshot))
    self.define(Procedure("map-snapshot-image", self.mapSnapshotImage))
    self.define(Procedure("map-snapshot-point", self.mapSnapshotPoint))
    self.define(Procedure("lat-long-span", self.latLongSpan))
  }
  
  /// Initializations of the library.
  public override func initializations() {
  }
  
  private func isMapSnapshot(_ expr: Expr) -> Expr {
    guard case .object(let obj) = expr, obj is MapSnapshot else {
      return .false
    }
    return .true
  }
  
  private func makeMapSnapshot(_ center: Expr,
                               _ dist: Expr,
                               _ size: Expr,
                               _ args: Arguments) throws -> Expr {
    guard let (mapType, pois, showBuildings) = args.optional(.symbol(self.standard),
                                                             .null,
                                                             .false) else {
      throw RuntimeError.argumentCount(of: "make-map-snapshot",
                                       min: 3,
                                       max: 6,
                                       args: .pair(center,
                                                   .pair(dist,
                                                         .pair(size, .makeList(args)))))
    }
    guard case .pair(.flonum(let lat), .pair(.flonum(let long), _)) = center else {
      throw RuntimeError.custom("error", "not a valid location", [center])
    }
    let snapshotOptions = MKMapSnapshotter.Options()
    if case .object(let obj) = dist, let span = obj as? LatLongSpan {
      snapshotOptions.region = MKCoordinateRegion(
        center: CLLocationCoordinate2D(latitude: lat, longitude: long),
        span: MKCoordinateSpan(latitudeDelta: span.latd, longitudeDelta: span.longd))
    } else {
      guard case .pair(.flonum(let latDist), .flonum(let longDist)) = dist else {
        throw RuntimeError.eval(.invalidSize, size)
      }
      snapshotOptions.region = MKCoordinateRegion(
        center: CLLocationCoordinate2D(latitude: lat, longitude: long),
        latitudinalMeters: latDist,
        longitudinalMeters: longDist)
    }
    guard case .pair(.flonum(let w), .flonum(let h)) = size else {
      throw RuntimeError.eval(.invalidSize, size)
    }
    snapshotOptions.size = CGSize(width: w, height: h)
    switch try mapType.asSymbol() {
      case self.standard:
        snapshotOptions.mapType = .standard
      case self.satellite:
        snapshotOptions.mapType = .satellite
      case self.satelliteFlyover:
        snapshotOptions.mapType = .satelliteFlyover
      case self.hybrid:
        snapshotOptions.mapType = .hybrid
      case self.hybridFlyover:
        snapshotOptions.mapType = .hybridFlyover
      case self.mutedStandard:
        snapshotOptions.mapType = .mutedStandard
      default:
        throw RuntimeError.custom("error", "not a valid map type", [mapType])
    }
    snapshotOptions.showsBuildings = showBuildings.isTrue
    let filter = MKPointOfInterestFilter(including: try self.mapPOICategories(pois))
    snapshotOptions.pointOfInterestFilter = .some(filter)
    let snapshotter = MKMapSnapshotter(options: snapshotOptions)
    let result = Future(external: false)
    let context = self.context
    snapshotter.start(with: .global()) { sh, error in
      do {
        if let error {
          _ = try result.setResult(in: context, to: .error(RuntimeError.os(error)), raise: true)
        } else if let sh {
          _ = try result.setResult(in: context, to: .object(MapSnapshot(sh)), raise: false)
        } else {
          _ = try result.setResult(in: context, to: .false, raise: false)
        }
      } catch let error {
        do {
          _ = try result.setResult(in: context,
                                   to: .error(RuntimeError.eval(.unableToReturnResultViaFuture,
                                                                .object(result),
                                                                .error(RuntimeError.os(error)))),
                                   raise: true)
        } catch {}
      }
    }
    return .object(result)
  }
  
  private func mapPOICategories(_ lst: Expr) throws -> [MKPointOfInterestCategory] {
    var res: [MKPointOfInterestCategory] = []
    var expr = lst
    while case .pair(let x, let rest) = expr {
      switch try x.asSymbol() {
        case self.airport:
          res.append(.airport)
        case self.amusementPark:
          res.append(.amusementPark)
        case self.aquarium:
          res.append(.aquarium)
        case self.atm:
          res.append(.atm)
        case self.bakery:
          res.append(.bakery)
        case self.bank:
          res.append(.bank)
        case self.beach:
          res.append(.beach)
        case self.brewery:
          res.append(.brewery)
        case self.cafe:
          res.append(.cafe)
        case self.campground:
          res.append(.campground)
        case self.carRental:
          res.append(.carRental)
        case self.evCharger:
          res.append(.evCharger)
        case self.fireStation:
          res.append(.fireStation)
        case self.fitnessCenter:
          res.append(.fitnessCenter)
        case self.supermarket:
          res.append(.foodMarket)
        case self.gasStation:
          res.append(.gasStation)
        case self.hospital:
          res.append(.hospital)
        case self.hotel:
          res.append(.hotel)
        case self.laundry:
          res.append(.laundry)
        case self.library:
          res.append(.library)
        case self.marina:
          res.append(.marina)
        case self.movieTheater:
          res.append(.movieTheater)
        case self.museum:
          res.append(.museum)
        case self.nationalPark:
          res.append(.nationalPark)
        case self.nightlife:
          res.append(.nightlife)
        case self.park:
          res.append(.park)
        case self.parking:
          res.append(.parking)
        case self.pharmacy:
          res.append(.pharmacy)
        case self.police:
          res.append(.police)
        case self.postOffice:
          res.append(.postOffice)
        case self.publicTransport:
          res.append(.publicTransport)
        case self.restaurant:
          res.append(.restaurant)
        case self.restroom:
          res.append(.restroom)
        case self.school:
          res.append(.school)
        case self.stadium:
          res.append(.stadium)
        case self.store:
          res.append(.store)
        case self.theater:
          res.append(.theater)
        case self.university:
          res.append(.university)
        case self.winery:
          res.append(.winery)
        case self.zoo:
          res.append(.zoo)
        default:
          throw RuntimeError.custom("error", "not a valid point of interest category", [x])
      }
      expr = rest
    }
    guard case .null = expr else {
      throw RuntimeError.type(lst, expected: [.properListType])
    }
    return res
  }
  
  private func mapSnapshotImage(_ expr: Expr) throws -> Expr {
    guard case .object(let obj) = expr,
          let snapshot = obj as? MapSnapshot else {
      throw RuntimeError.type(expr, expected: [MapSnapshot.type])
    }
    return .object(NativeImage(snapshot.image))
  }
  
  private func mapSnapshotPoint(_ expr: Expr, fst: Expr, _ snd: Expr?) throws -> Expr {
    guard case .object(let obj) = expr,
          let snapshot = obj as? MapSnapshot else {
      throw RuntimeError.type(expr, expected: [MapSnapshot.type])
    }
    let point: CGPoint
    if let snd {
      let lat = try fst.asDouble(coerce: true)
      let long = try snd.asDouble(coerce: true)
      point = snapshot.point(for: CLLocationCoordinate2D(latitude: lat, longitude: long))
    } else {
      guard case .pair(.flonum(let lat), .pair(.flonum(let long), _)) = fst else {
        throw RuntimeError.custom("error", "not a valid location", [fst])
      }
      point = snapshot.point(for: CLLocationCoordinate2D(latitude: lat, longitude: long))
    }
    #if os(iOS) || os(watchOS) || os(tvOS)
    return .pair(.flonum(point.x), .flonum(point.y))
    #elseif os(macOS)
    return .pair(.flonum(point.x), .flonum(snapshot.image.size.height - point.y))
    #endif
  }
  
  private func latLongSpan(_ fst: Expr, _ snd: Expr) throws -> Expr {
    return .object(LatLongSpan(try fst.asDouble(coerce: true), try snd.asDouble(coerce: true)))
  }
}

class MapSnapshot: NativeObject {
  
  /// Type representing fonts
  public static let type = Type.objectType(Symbol(uninterned: "map-snapshot"))
  
  let snapshot: MKMapSnapshotter.Snapshot
  
  init(_ snapshot: MKMapSnapshotter.Snapshot) {
    self.snapshot = snapshot
  }
  
  #if os(iOS) || os(watchOS) || os(tvOS)
  public var image: UIImage {
    return self.snapshot.image
  }
  #elseif os(macOS)
  public var image: NSImage {
    return self.snapshot.image
  }
  #endif
  
  public func point(for coords: CLLocationCoordinate2D) -> CGPoint {
    return self.snapshot.point(for: coords)
  }
  
  public override var type: Type {
    return Self.type
  }
}

class LatLongSpan: NativeObject {
  
  /// Type representing fonts
  public static let type = Type.objectType(Symbol(uninterned: "lat-long-span"))
  
  let latd: Double
  let longd: Double
  
  init(_ latd: Double, _ longd: Double) {
    self.latd = latd
    self.longd = longd
  }
  
  public override var type: Type {
    return Self.type
  }
}
