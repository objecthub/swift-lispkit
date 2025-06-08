//
//  LocationLibrary.swift
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
import CoreLocation
import Contacts

public final class LocationLibrary: NativeLibrary {
  
  private var locationManager: LocationManager? = nil
  
  /// Initialize symbols.
  public required init(in context: Context) throws {
    try super.init(in: context)
  }

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "location"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
  }

  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("location?", self.isLocation))
    self.define(Procedure("location", self.location))
    self.define(Procedure("current-location", self.currentLocation))
    self.define(Procedure("location-latitude", self.locationLatitude))
    self.define(Procedure("location-longitude", self.locationLongitude))
    self.define(Procedure("location-altitude", self.locationAltitude))
    self.define(Procedure("location-distance", self.locationDistance))
    self.define(Procedure("location->timezone", self.locationToTimezone))
    self.define(Procedure("place?", self.isPlace))
    self.define(Procedure("place", self.place))
    self.define(Procedure("place-country-code", self.placeCountryCode))
    self.define(Procedure("place-country", self.placeCountry))
    self.define(Procedure("place-region", self.placeRegion))
    self.define(Procedure("place-admin", self.placeAdmin))
    self.define(Procedure("place-postal-code", self.placePostalCode))
    self.define(Procedure("place-city", self.placeCity))
    self.define(Procedure("place-locality", self.placeLocality))
    self.define(Procedure("place-street", self.placeStreet))
    self.define(Procedure("place-street-number", self.placeStreetNumber))
    self.define(Procedure("place-timezone", self.placeTimezone))
    self.define(Procedure("geocode", self.geocode))
    self.define(Procedure("reverse-geocode", self.reverseGeocode))
    self.define(Procedure("address->place", self.addressToPlace))
    self.define(Procedure("place->address", self.placeToAddress))
  }
  
  /// Initializations of the library.
  public override func initializations() {
  }
  
  private func isLocation(_ expr: Expr) -> Expr {
    guard case .pair(.flonum(_), .pair(.flonum(_), let rest)) = expr else {
      return .false
    }
    switch rest {
      case .null, .pair(.flonum(_), .null):
        return .true
      default:
        return .false
    }
  }
  
  private func location(_ lat: Expr, _ lon: Expr, _ alt: Expr?) throws -> Expr {
    let rest: Expr = alt == nil ? .null : .pair(.flonum(try alt!.asDouble(coerce: true)), .null)
    return .pair(.flonum(try lat.asDouble(coerce: true)),
                 .pair(.flonum(try lon.asDouble(coerce: true)), rest))
  }
  
  private func currentLocation(raiseErrors: Expr?) throws -> Expr {
    let result = Future(external: false)
    if let locationManager {
      locationManager.writeCurrentLocation(to: result,
                                           in: self.context,
                                           raising: raiseErrors?.isTrue ?? false)
    } else {
      let locationManager = LocationManager(evaluator: self.context.evaluator)
      self.locationManager = locationManager
      locationManager.writeCurrentLocation(to: result,
                                           in: self.context,
                                           raising: raiseErrors?.isTrue ?? false)
    }
    return .object(result)
  }
  
  private func locationLatitude(_ expr: Expr) throws -> Expr {
    guard case .pair(.flonum(let num), .pair(.flonum(_), _)) = expr else {
      return .false
    }
    return .flonum(num)
  }
  
  private func locationLongitude(_ expr: Expr) throws -> Expr {
    guard case .pair(.flonum(_), .pair(.flonum(let num), _)) = expr else {
      return .false
    }
    return .flonum(num)
  }
  
  private func locationAltitude(_ expr: Expr) throws -> Expr {
    guard case .pair(.flonum(_), .pair(.flonum(_), .pair(.flonum(let num), .null))) = expr else {
      return .false
    }
    return .flonum(num)
  }
  
  private func locationDistance(_ l1: Expr, _ l2: Expr) throws -> Expr {
    guard case .pair(.flonum(let lat1), .pair(.flonum(let long1), _)) = l1 else {
      throw RuntimeError.custom("error", "not a valid location", [l1])
    }
    guard case .pair(.flonum(let lat2), .pair(.flonum(let long2), _)) = l2 else {
      throw RuntimeError.custom("error", "not a valid location", [l2])
    }
    let dist = CLLocation(latitude: lat1, longitude: long1).distance(
                   from: CLLocation(latitude: lat2, longitude: long2))
    return .makeNumber(dist)
  }
  
  private func locationToTimezone(_ expr: Expr) throws -> Expr {
    guard case .pair(.flonum(let lat), .pair(.flonum(let lon), _)) = expr,
          !lat.isNaN, !lon.isNaN,
          lat >= -90.0, lat <= 90.0, lon >= -180.0, lon <= 180.0 else {
      throw RuntimeError.custom("error", "not a valid location", [expr])
    }
    return .makeString(TimezoneLookup.tzLookup(lat: lat, lon: lon))
  }
  
  private func isPlace(_ expr: Expr) throws -> Expr {
    var n = 0
    var place = expr
    while case .pair(let arg, let rest) = place {
      switch arg {
        case .false, .string(_):
          n += 1
          guard n < 11 else {
            return .false
          }
          place = rest
        default:
          return .false
      }
    }
    return .makeBoolean(place.isNull && n > 0)
  }
  
  private func place(_ args: Arguments) throws -> Expr {
    var res = Expr.null
    var n = 0
    for expr in args.reversed() {
      switch expr {
        case .false, .string(_):
          break
        default:
          throw RuntimeError.type(expr, expected: [.strType])
      }
      n += 1
      guard n < 11 else {
        throw RuntimeError.argumentCount(of: "place", min: 1, max: 10, expr: .makeList(args))
      }
      res = .pair(expr, res)
    }
    guard !res.isNull else {
      throw RuntimeError.argumentCount(of: "place", min: 1, max: 10, expr: .makeList(args))
    }
    return res
  }
  
  private func placeCountryCode(_ expr: Expr) throws -> Expr {
    guard case .pair(.string(let res), _) = expr else {
      return .false
    }
    return .string(res)
  }
  
  private func placeCountry(_ expr: Expr) throws -> Expr {
    guard case .pair(_, .pair(.string(let res), _)) = expr else {
      return .false
    }
    return .string(res)
  }
  
  private func placeRegion(_ expr: Expr) throws -> Expr {
    guard case .pair(_, .pair(_, .pair(.string(let res), _))) = expr else {
      return .false
    }
    return .string(res)
  }
  
  private func placeAdmin(_ expr: Expr) throws -> Expr {
    guard case .pair(_, .pair(_, .pair(_, .pair(.string(let res), _)))) = expr else {
      return .false
    }
    return .string(res)
  }
  
  private func placePostalCode(_ expr: Expr) throws -> Expr {
    guard case .pair(_, .pair(_, .pair(_, .pair(_, .pair(.string(let res), _))))) = expr else {
      return .false
    }
    return .string(res)
  }
  
  private func placeCity(_ e: Expr) throws -> Expr {
    guard case .pair(_, .pair(_,
                              .pair(_, .pair(_, .pair(_, .pair(.string(let res), _)))))) = e else {
      return .false
    }
    return .string(res)
  }
  
  private func placeLocality(_ e: Expr) throws -> Expr {
    guard case .pair(_, .pair(_,
                 .pair(_, .pair(_, .pair(_, .pair(_, .pair(.string(let res), _))))))) = e else {
      return .false
    }
    return .string(res)
  }
  
  private func placeStreet(_ e: Expr) throws -> Expr {
    guard case .pair(_, .pair(_, .pair(_,
                    .pair(_, .pair(_, .pair(_, .pair(_, .pair(.string(let res), _)))))))) = e else {
      return .false
    }
    return .string(res)
  }
  
  private func placeStreetNumber(_ e: Expr) throws -> Expr {
    guard case .pair(_, .pair(_, .pair(_, .pair(_,
                  .pair(_, .pair(_, .pair(_, .pair(_, .pair(.string(let res), _))))))))) = e else {
      return .false
    }
    return .string(res)
  }
  
  private func placeTimezone(_ e: Expr) throws -> Expr {
    guard case .pair(_, .pair(_, .pair(_, .pair(_, .pair(_, .pair(_,
                          .pair(_, .pair(_, .pair(_, .pair(.string(let res), _)))))))))) = e else {
      return .false
    }
    return .string(res)
  }
  
  private func asLocale(_ expr: Expr?) throws -> Locale {
    guard let locale = expr else {
      return Locale.current
    }
    return Locale(identifier: try locale.asSymbol().identifier)
  }
  
  private func geocodeToLocation(_ op: (@escaping CLGeocodeCompletionHandler) -> Void) throws -> Expr {
    let result = Future(external: false)
    let context = self.context
    op { placemarks, error -> Void in
      do {
        guard error == nil else {
          if (error! as NSError).code == 8 {
            _ = try result.setResult(in: context, to: .null, raise: false)
          } else {
            _ = try result.setResult(in: context, to: .error(RuntimeError.os(error!)), raise: true)
          }
          return
        }
        guard placemarks != nil else {
          _ = try result.setResult(in: context, to: .null, raise: false)
          return
        }
        var res = Expr.null
        for pm in placemarks! {
          if let location = pm.location {
            res = .pair(.pair(.makeNumber(location.coordinate.latitude),
                              .pair(.makeNumber(location.coordinate.longitude), .null)), res)
          }
        }
        _ = try result.setResult(in: context, to: res, raise: false)
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
  
  private func geocodeToPlace(_ op: (@escaping CLGeocodeCompletionHandler) -> Void) -> Expr {
    let result = Future(external: false)
    let context = self.context
    op { placemarks, error -> Void in
      do {
        guard error == nil else {
          _ = try result.setResult(in: context, to: .false, raise: false)
          return
        }
        guard placemarks != nil else {
          _ = try result.setResult(in: context, to: .null, raise: false)
          return
        }
        var res = Expr.null
        for pm in placemarks! {
          var address = Expr.null
          if let tz = pm.timeZone {
            address = .pair(.makeString(tz.identifier), .null)
          }
          if let str = pm.subThoroughfare {
            address = .pair(.makeString(str), address)
          } else if address != .null {
            address = .pair(.false, address)
          }
          if let str = pm.thoroughfare {
            address = .pair(.makeString(str), address)
          } else if address != .null {
            address = .pair(.false, address)
          }
          if let str = pm.subLocality {
            address = .pair(.makeString(str), address)
          } else if address != .null {
            address = .pair(.false, address)
          }
          if let str = pm.locality {
            address = .pair(.makeString(str), address)
          } else if address != .null {
            address = .pair(.false, address)
          }
          if let str = pm.postalCode {
            address = .pair(.makeString(str), address)
          } else if address != .null {
            address = .pair(.false, address)
          }
          if let str = pm.subAdministrativeArea {
            address = .pair(.makeString(str), address)
          } else if address != .null {
            address = .pair(.false, address)
          }
          if let str = pm.administrativeArea {
            address = .pair(.makeString(str), address)
          } else if address != .null {
            address = .pair(.false, address)
          }
          if let str = pm.country {
            address = .pair(.makeString(str), address)
          } else if address != .null {
            address = .pair(.false, address)
          }
          if let str = pm.isoCountryCode {
            address = .pair(.makeString(str), address)
          } else if address != .null {
            address = .pair(.false, address)
          }
          res = .pair(address, res)
        }
        _ = try result.setResult(in: context, to: res, raise: false)
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
  
  private func postalAddress(from: Expr) throws -> CNPostalAddress {
    var place = from
    let pm = CNMutablePostalAddress()
    if case .pair(let iso, let rest) = place {
      if iso.isTrue {
        pm.isoCountryCode = try iso.asString()
      }
      place = rest
    } else if place.isNull {
      return pm
    }
    if case .pair(let country, let rest) = place {
      if country.isTrue {
        pm.country = try country.asString()
      }
      place = rest
    } else {
      return pm
    }
    if case .pair(let state, let rest) = place {
      if state.isTrue {
        pm.state = try state.asString()
      }
      place = rest
    } else {
      return pm
    }
    if case .pair(let admin, let rest) = place {
      if admin.isTrue {
        pm.subAdministrativeArea = try admin.asString()
      }
      place = rest
    } else {
      return pm
    }
    if case .pair(let zip, let rest) = place {
      if zip.isTrue {
        pm.postalCode = try zip.asString()
      }
      place = rest
    } else {
      return pm
    }
    if case .pair(let city, let rest) = place {
      if city.isTrue {
        pm.city = try city.asString()
      }
      place = rest
    } else {
      return pm
    }
    if case .pair(let loc, let rest) = place {
      if loc.isTrue {
        pm.subLocality = try loc.asString()
      }
      place = rest
    } else {
      return pm
    }
    if case .pair(let street, .pair(let num, let rest)) = place {
      if street.isTrue {
        if num.isTrue {
          pm.street = try street.asString() + " " + num.asString()
        } else {
          pm.street = try street.asString()
        }
      } else if num.isTrue {
        pm.street = try num.asString()
      }
      place = rest
    } else if case .pair(let street, let rest) = place {
      if street.isTrue {
        pm.street = try street.asString()
      }
      place = rest
    }
    return pm
  }
  
  private func geocode(_ expr: Expr, _ locale: Expr?) throws -> Expr {
    switch expr {
      case .pair(_, _):
        let postalAddr = try self.postalAddress(from: expr)
        let locale = try self.asLocale(locale)
        let geocoder = CLGeocoder()
        return try self.geocodeToLocation { handler in
          geocoder.geocodePostalAddress(postalAddr,
                                        preferredLocale: locale,
                                        completionHandler: handler)
        }
      default:
        let str = try expr.asString()
        let locale = try self.asLocale(locale)
        let geocoder = CLGeocoder()
        return try self.geocodeToLocation { handler in
          geocoder.geocodeAddressString(str,
                                        in: nil,
                                        preferredLocale: locale,
                                        completionHandler: handler)
        }
    }
  }
  
  private func reverseGeocode(_ expr: Expr, _ second: Expr?, _ locl: Expr?) throws -> Expr {
    let latitude: Double
    let longitude: Double
    let locale: Locale
    if let long = second {
      if locl == nil, case .symbol(_) = long {
        guard case .pair(let lat, .pair (let lon, _)) = expr else {
          return .false
        }
        latitude = try lat.asDouble(coerce: true)
        longitude = try lon.asDouble(coerce: true)
        locale = try self.asLocale(second)
      } else {
        latitude = try expr.asDouble(coerce: true)
        longitude = try long.asDouble(coerce: true)
        locale = try self.asLocale(locl)
      }
    } else {
      guard case .pair(let lat, .pair (let lon, _)) = expr else {
        return .false
      }
      latitude = try lat.asDouble(coerce: true)
      longitude = try lon.asDouble(coerce: true)
      locale = try self.asLocale(second)
    }
    let geocoder = CLGeocoder()
    let location = CLLocation(latitude: latitude, longitude: longitude)
    return self.geocodeToPlace { handler in
      geocoder.reverseGeocodeLocation(location, preferredLocale: locale, completionHandler: handler)
    }
  }
  
  private func placeToAddress(_ expr: Expr) throws -> Expr {
    let postalAddr = try self.postalAddress(from: expr)
    let formatter = CNPostalAddressFormatter()
    return .makeString(formatter.string(from: postalAddr))
  }
  
  private func addressToPlace(_ expr: Expr, _ locale: Expr?) throws -> Expr {
    let str = try expr.asString()
    let locale = try self.asLocale(locale)
    let geocoder = CLGeocoder()
    return self.geocodeToPlace { handler in
      geocoder.geocodeAddressString(str,
                                    in: nil,
                                    preferredLocale: locale,
                                    completionHandler: handler)
    }
  }
}

fileprivate final class LocationManager: NSObject, ObservableObject, CLLocationManagerDelegate {
  private let evaluator: Evaluator
  private let locationManager: CLLocationManager
  private let condition = NSCondition()
  var locationError: Error? = nil
  var locationStatus: CLAuthorizationStatus? = nil
  var lastLocation: CLLocation? = nil

  init(evaluator: Evaluator) {
    self.evaluator = evaluator
    self.locationManager = CLLocationManager()
    super.init()
    self.locationManager.delegate = self
    self.locationManager.desiredAccuracy = kCLLocationAccuracyBest
    self.locationManager.distanceFilter = kCLDistanceFilterNone
    self.locationManager.activityType = .other
    self.locationManager.requestWhenInUseAuthorization()

  }
  
  var accessDenied: Bool {
    guard let status = self.locationStatus else {
      return false
    }
    switch status {
      case .notDetermined, .authorizedWhenInUse, .authorizedAlways:
        return false
      case .restricted, .denied:
        return true
      default:
        return true
    }
  }
  
  func writeCurrentLocation(to result: Future,
                            in context: Context,
                            raising: Bool) {
    DispatchQueue.global(qos: .userInitiated).async {
      self.condition.lock()
      self.locationManager.requestLocation()
      self.lastLocation = self.locationManager.location
      self.locationStatus = self.locationManager.authorizationStatus
      self.locationError = nil
      var n = 0
      while self.lastLocation == nil &&
            self.locationError == nil &&
            !self.accessDenied &&
            !self.evaluator.isAbortionRequested() {
        n += 1
        if n >= 16 {
          break
        }
        self.condition.wait(until: Date().advanced(by: 0.5))
        if self.lastLocation == nil {
          self.lastLocation = self.locationManager.location
        }
      }
      do {
        if let location = self.lastLocation {
          self.condition.unlock()
          let res: Expr = .pair(.makeNumber(location.coordinate.latitude),
                                .pair(.makeNumber(location.coordinate.longitude),
                                      .pair(.makeNumber(location.altitude), .null)))
          _ = try result.setResult(in: context, to: res, raise: false)
        } else if let error = self.locationError {
          self.condition.unlock()
          if raising {
            _ = try result.setResult(in: context, to: .error(RuntimeError.os(error)), raise: true)
          } else {
            _ = try result.setResult(in: context, to: .false, raise: false)
          }
        } else {
          self.condition.unlock()
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
  }
  
  func locationManager(_ manager: CLLocationManager, didFailWithError: Error) {
    self.condition.lock()
    self.locationError = didFailWithError
    self.condition.signal()
    self.condition.unlock()
  }
  
  func locationManager(_ manager: CLLocationManager,
                       didChangeAuthorization status: CLAuthorizationStatus) {
    self.condition.lock()
    self.locationStatus = status
    self.condition.signal()
    self.condition.unlock()
  }
  
  func locationManager(_ manager: CLLocationManager, didUpdateLocations locations: [CLLocation]) {
    guard let location = locations.last else {
      return
    }
    self.condition.lock()
    self.lastLocation = location
    self.condition.signal()
    self.condition.unlock()
  }
}
