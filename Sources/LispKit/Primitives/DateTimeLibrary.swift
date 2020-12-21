//
//  DateTimeLibrary.swift
//  LispKit
//
//  Created by Matthias Zenger on 23/02/2019.
//  Copyright © 2019 ObjectHub. All rights reserved.
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

///
/// Date-time library: LispKit-specific library for handling dates and times in different
/// timezones and for different locales.
///
public final class DateTimeLibrary: NativeLibrary {

  // Time zone name styles
  private let timezoneStandard: Symbol
  private let timezoneStandardShort: Symbol
  private let timezoneDST: Symbol
  private let timezoneDSTShort: Symbol
  private let timezoneGeneric: Symbol
  private let timezoneGenericShort: Symbol

  // Date-time styles
  private let dateTimeNone: Symbol
  private let dateTimeShort: Symbol
  private let dateTimeMedium: Symbol
  private let dateTimeLong: Symbol
  private let dateTimeFull: Symbol

  // Calendar used by this library; this is hard-coded to the gregorian calendar for now.
  private let calendar = Calendar(identifier: Calendar.Identifier.gregorian)
  
  /// Initialize time zone name styles
  public required init(in context: Context) throws {
    self.timezoneStandard = context.symbols.intern("standard")
    self.timezoneStandardShort = context.symbols.intern("standard-short")
    self.timezoneDST = context.symbols.intern("dst")
    self.timezoneDSTShort = context.symbols.intern("dst-short")
    self.timezoneGeneric = context.symbols.intern("generic")
    self.timezoneGenericShort = context.symbols.intern("generic-short")
    self.dateTimeNone = context.symbols.intern("none")
    self.dateTimeShort = context.symbols.intern("short")
    self.dateTimeMedium = context.symbols.intern("medium")
    self.dateTimeLong = context.symbols.intern("long")
    self.dateTimeFull = context.symbols.intern("full")
    try super.init(in: context)
  }

  /// Name of the library.
  public override class var name: [String] {
    return ["lispkit", "date-time"]
  }

  /// Dependencies of the library.
  public override func dependencies() {
  }

  /// Declarations of the library.
  public override func declarations() {
    self.define(Procedure("current-seconds", self.currentSeconds))
    self.define(Procedure("timezones", self.timeZones))
    self.define(Procedure("timezone?", self.isTimeZone))
    self.define(Procedure("timezone", self.timeZone))
    self.define(Procedure("timezone-name", self.timeZoneName))
    self.define(Procedure("timezone-abbreviation", self.timeZoneAbbreviation))
    self.define(Procedure("timezone-gmt-offset", self.timeZoneGmtOffset))
    self.define(Procedure("date-time?", self.isDateTime))
    self.define(Procedure("date-time", self.dateTime))
    self.define(Procedure("week->date-time", self.weekToDateTime))
    self.define(Procedure("seconds->date-time", self.secondsToDateTime))
    self.define(Procedure("string->date-time", self.stringToDateTime))
    self.define(Procedure("date-time-add", self.dateTimeAdd))
    self.define(Procedure("date-time-add-seconds", self.dateTimeAddSeconds))
    self.define(Procedure("date-time-diff-seconds", self.dateTimeDiffSeconds))
    self.define(Procedure("date-time-in-timezone", self.dateTimeInTimezone))
    self.define(Procedure("date-time-same?", self.isDateTimeSame))
    self.define(Procedure("date-time=?", self.isDateTimeEquals))
    self.define(Procedure("date-time<?", self.isDateTimeLess))
    self.define(Procedure("date-time>?", self.isDateTimeGreater))
    self.define(Procedure("date-time<=?", self.isDateTimeLessEquals))
    self.define(Procedure("date-time>=?", self.isDateTimeGreaterEquals))
    self.define(Procedure("date-time->seconds", self.dateTimeToSeconds))
    self.define(Procedure("date-time->string", self.dateTimeToString))
    self.define(Procedure("date-time->iso8601-string", self.dateTimeToISO8601String))
    self.define(Procedure("date-time-timezone", self.dateTimeTimeZone))
    self.define(Procedure("date-time-year", self.dateTimeYear))
    self.define(Procedure("date-time-month", self.dateTimeMonth))
    self.define(Procedure("date-time-day", self.dateTimeDay))
    self.define(Procedure("date-time-hour", self.dateTimeHour))
    self.define(Procedure("date-time-minute", self.dateTimeMinute))
    self.define(Procedure("date-time-second", self.dateTimeSecond))
    self.define(Procedure("date-time-nano", self.dateTimeNano))
    self.define(Procedure("date-time-weekday", self.dateTimeWeekday))
    self.define(Procedure("date-time-week", self.dateTimeWeek))
    self.define(Procedure("date-time-dst-offset", self.dateTimeDstOffset))
    self.define(Procedure("date-time-has-dst?", self.dateTimeHasDst))
    self.define(Procedure("next-dst-transition", self.nextDstTransition))
    self.define(Procedure("date-time-hash", self.dateTimeHash))
  }

  private func currentSeconds() -> Expr {
    return .flonum(Double(Date().timeIntervalSince1970))
  }

  private func timeZones(_ selector: Expr?) -> Expr {
    switch selector ?? .true {
      case .false:
        var res = Expr.null
        for tzAbbrev in TimeZone.abbreviationDictionary.keys {
          res = .pair(.makeString(tzAbbrev), res)
        }
        return res
      case .string(let str):
        var res = Expr.null
        let filter = str as String
        for tzId in TimeZone.knownTimeZoneIdentifiers.reversed() {
          if tzId.contains(filter) {
            res = .pair(.makeString(tzId), res)
          }
        }
        return res
      default:
        var res = Expr.null
        for tzId in TimeZone.knownTimeZoneIdentifiers.reversed() {
          res = .pair(.makeString(tzId), res)
        }
        return res
    }
  }

  private func isTimeZone(_ expr: Expr) -> Expr {
    return .makeBoolean(self.getTimeZone(expr) != nil)
  }

  private func timeZone(_ expr: Expr?) throws -> Expr {
    return .makeString(try self.asTimeZone(expr).identifier)
  }

  private func timeZoneName(_ timeZone: Expr, _ locale: Expr?, _ format: Expr?) throws -> Expr {
    let tzone = try self.asTimeZone(timeZone)
    let locale = try self.asLocale(locale)
    guard case .symbol(let sym) = format ?? Expr.symbol(self.timezoneStandard) else {
      throw RuntimeError.eval(.invalidImageFileType, format!)
    }
    let nameStyle: NSTimeZone.NameStyle
    switch sym {
      case self.timezoneStandard:
        nameStyle = .standard
      case self.timezoneStandardShort:
        nameStyle = .shortStandard
      case self.timezoneDST:
        nameStyle = .daylightSaving
      case self.timezoneDSTShort:
        nameStyle = .shortDaylightSaving
      case self.timezoneGeneric:
        nameStyle = .generic
      case self.timezoneGenericShort:
        nameStyle = .shortGeneric
      default:
        throw RuntimeError.eval(.invalidImageFileType, format!)
    }
    guard let name = tzone.localizedName(for: nameStyle, locale: locale) else {
      return .false
    }
    return .makeString(name)
  }

  private func timeZoneAbbreviation(_ timeZone: Expr) throws -> Expr {
    let tzone = try self.asTimeZone(timeZone)
    guard let abbrev = tzone.abbreviation() else {
      return .false
    }
    return .makeString(abbrev)
  }

  private func timeZoneGmtOffset(_ timeZone: Expr) throws -> Expr {
    return .flonum(Double(try self.asTimeZone(timeZone).secondsFromGMT()))
  }

  private func isDateTime(_ expr: Expr) -> Expr {
    if case .object(let obj) = expr, obj is NativeDateTime {
      return .true
    }
    return .false
  }

  private func dateTime(args: Arguments) throws -> Expr {
    guard args.count > 1 else {
      return .object(
        NativeDateTime(self.calendar.dateComponents(in: try self.asTimeZone(args.first),
                                                    from: Date())))
    }
    var components = Expr.null
    for arg in args.reversed() {
      components = .pair(arg, components)
    }
    var dt = components
    var tz: TimeZone
    switch dt {
      case .pair(.symbol(let sym), let rest):
        tz = try self.asTimeZone(.symbol(sym))
        dt = rest
      case .pair(.string(let str), let rest):
        tz = try self.asTimeZone(.string(str))
        dt = rest
      default:
        tz = TimeZone.current
    }
    guard case .pair(.fixnum(let y), .pair(.fixnum(let m), .pair(.fixnum(let d), let time))) = dt,
          y >= 0 && y < Int.max, m >= 1 && m <= 12, d >= 1 && d <= 31 else {
      throw RuntimeError.eval(.invalidDateTime, components)
    }
    var hour: Int = 0
    var minute: Int = 0
    var second: Int = 0
    var nanosecond: Int = 0
    if case .pair(.fixnum(let hr), let rest) = time, hr >= 0 && hr <= 24 {
      hour = Int(hr)
      if case .pair(.fixnum(let min), let rest) = rest, min >= 0 && min <= 60 {
        minute = Int(min)
        if case .pair(.fixnum(let sec), let rest) = rest, sec >= 0 && sec <= 60 {
          second = Int(sec)
          if case .pair(.fixnum(let nano), _) = rest, nano >= 0 && nano <= Int.max {
            nanosecond = Int(nano)
          } else if !rest.isNull {
            throw RuntimeError.eval(.invalidDateTime, components)
          }
        } else if !rest.isNull {
          throw RuntimeError.eval(.invalidDateTime, components)
        }
      } else if !rest.isNull {
        throw RuntimeError.eval(.invalidDateTime, components)
      }
    } else if !time.isNull {
      throw RuntimeError.eval(.invalidDateTime, components)
    }
    let dc = DateComponents(calendar: self.calendar,
                            timeZone: tz,
                            year: Int(y),
                            month: Int(m),
                            day: Int(d),
                            hour: hour,
                            minute: minute,
                            second: second,
                            nanosecond: nanosecond)
    guard dc.isValidDate, let res = self.toDateTime(dc) else {
      throw RuntimeError.eval(.invalidDateTime, components)
    }
    return res
  }

  private func weekToDateTime(args: Arguments) throws -> Expr {
    var components = Expr.null
    for arg in args.reversed() {
      components = .pair(arg, components)
    }
    var dt = components
    var tz: TimeZone
    switch dt {
      case .pair(.symbol(let sym), let rest):
        tz = try self.asTimeZone(.symbol(sym))
        dt = rest
      case .pair(.string(let str), let rest):
        tz = try self.asTimeZone(.string(str))
        dt = rest
      default:
        tz = TimeZone.current
    }
    guard case .pair(.fixnum(let y), .pair(.fixnum(let week), let day)) = dt,
               y >= 0 && y < Int.max && week >= 0 else {
      throw RuntimeError.eval(.invalidDateTime, components)
    }
    var weekday: Int = 2
    var hour: Int = 0
    var minute: Int = 0
    var second: Int = 0
    var nanosecond: Int = 0
    if case .pair(.fixnum(let wd), let rest) = day, wd >= 1 && wd <= 7 {
      weekday = self.encodeWeekday(from: Int(wd))
      if case .pair(.fixnum(let hr), let rest) = rest, hr >= 0 && hr <= 24 {
        hour = Int(hr)
        if case .pair(.fixnum(let min), let rest) = rest, min >= 0 && min <= 60 {
          minute = Int(min)
          if case .pair(.fixnum(let sec), let rest) = rest, sec >= 0 && sec <= 60 {
            second = Int(sec)
            if case .pair(.fixnum(let nano), _) = rest, nano >= 0 && nano <= Int.max {
              nanosecond = Int(nano)
            } else if !rest.isNull {
              throw RuntimeError.eval(.invalidDateTime, components)
            }
          } else if !rest.isNull {
            throw RuntimeError.eval(.invalidDateTime, components)
          }
        } else if !rest.isNull {
          throw RuntimeError.eval(.invalidDateTime, components)
        }
      } else if !rest.isNull {
        throw RuntimeError.eval(.invalidDateTime, components)
      }
    } else if !day.isNull {
      throw RuntimeError.eval(.invalidDateTime, components)
    }
    let dc = DateComponents(calendar: self.calendar,
                            timeZone: tz,
                            hour: hour,
                            minute: minute,
                            second: second,
                            nanosecond: nanosecond,
                            weekday: weekday,
                            weekOfYear: Int(week),
                            yearForWeekOfYear: Int(y))
    guard dc.isValidDate, let res = self.toDateTime(dc) else {
      throw RuntimeError.eval(.invalidDateTime, components)
    }
    return res
  }

  private func secondsToDateTime(_ seconds: Expr, _ timeZone: Expr?) throws -> Expr {
    let date = Date(timeIntervalSince1970: try seconds.asDouble(coerce: true))
    return .object(NativeDateTime(self.calendar.dateComponents(in: try self.asTimeZone(timeZone),
                                                               from: date)))
  }

  private func stringToDateTime(_ str: Expr, _ args: Arguments) throws -> Expr {
    guard let (timeZone, locale, dateFormat) = args.optional(.false, .false, .false) else {
      throw RuntimeError.argumentCount(of: "string->date-time",
                                       min: 1,
                                       max: 4,
                                       args: .pair(str, .makeList(args)))
    }
    let formatter = DateFormatter()
    formatter.calendar = self.calendar
    formatter.locale = try self.asLocale(locale == .false ? nil : locale)
    formatter.timeZone = try self.asTimeZone(timeZone)
    if dateFormat == .false {
      formatter.dateStyle = .short
      formatter.timeStyle = .medium
    } else {
      switch dateFormat {
        case .pair(let dateStyle, .pair(let timeStyle, _)):
          formatter.dateStyle = try self.asDateStyle(dateStyle)
          formatter.timeStyle = try self.asDateStyle(timeStyle)
        case .symbol(_):
          let style = try self.asDateStyle(dateFormat)
          formatter.dateStyle = style
          formatter.timeStyle = style
        default:
          formatter.dateFormat = try dateFormat.asString()
      }
    }
    guard let date = formatter.date(from: try str.asString()) else {
      return .false
    }
    return .object(NativeDateTime(self.calendar.dateComponents(in: formatter.timeZone,
                                                               from: date)))
  }

  private func dateTimeAdd(_ expr: Expr, _ days: Expr, _ args: Arguments) throws -> Expr {
    guard let (hrs, min, sec, nano) = args.optional(.fixnum(0),
                                                    .fixnum(0),
                                                    .fixnum(0),
                                                    .fixnum(0)) else {
      throw RuntimeError.argumentCount(of: "date-time-increment",
                                       min: 2,
                                       max: 6,
                                       args: .pair(expr, .pair(days, .makeList(args))))
    }
    let dateComponents = try self.asDateComponents(expr)
    guard let date = dateComponents.date else {
      throw RuntimeError.type(expr, expected: [NativeDateTime.type])
    }
    let dc = DateComponents(calendar: self.calendar,
                            timeZone: dateComponents.timeZone,
                            year: 0,
                            month: 0,
                            day: try days.asInt(above: Int.min),
                            hour: try hrs.asInt(above: Int.min),
                            minute: try min.asInt(above: Int.min),
                            second: try sec.asInt(above: Int.min),
                            nanosecond: try nano.asInt(above: Int.min))
    guard let target = self.calendar.date(byAdding: dc, to: date, wrappingComponents: false),
      let timeZone = dateComponents.timeZone else {
      return .false
    }
    return .object(NativeDateTime(self.calendar.dateComponents(in: timeZone, from: target)))
  }
  
  private func dateTimeAddSeconds(_ expr: Expr, _ seconds: Expr) throws -> Expr {
    let dateComponents = try self.asDateComponents(expr)
    guard let date = dateComponents.date else {
      throw RuntimeError.type(expr, expected: [NativeDateTime.type])
    }
    let target = Date(timeIntervalSince1970: try date.timeIntervalSince1970 +
                                                 seconds.asDouble(coerce: true))
    guard let timeZone = dateComponents.timeZone else {
      return .false
    }
    return .object(NativeDateTime(self.calendar.dateComponents(in: timeZone, from: target)))
  }
  
  private func dateTimeDiffSeconds(_ expr: Expr, _ target: Expr) throws -> Expr {
    guard let start = try self.asDateComponents(expr).date else {
      throw RuntimeError.type(expr, expected: [NativeDateTime.type])
    }
    guard let end = try self.asDateComponents(target).date else {
      throw RuntimeError.type(target, expected: [NativeDateTime.type])
    }
    if end >= start {
      return .makeNumber(DateInterval(start: start, end: end).duration)
    } else {
      return .makeNumber(-DateInterval(start: end, end: start).duration)
    }
  }
  
  private func dateTimeInTimezone(_ expr: Expr, _ timeZone: Expr?) throws -> Expr {
    guard let date = try self.asDateComponents(expr).date else {
      throw RuntimeError.type(expr, expected: [NativeDateTime.type])
    }
    let tzone = try self.asTimeZone(timeZone)
    return .object(NativeDateTime(self.calendar.dateComponents(in: tzone, from: date)))
  }

  private func isDateTimeSame(_ lhs: Expr, _ rhs: Expr) throws -> Expr {
    let left = try self.asDateComponents(lhs)
    let right = try self.asDateComponents(rhs)
    return .makeBoolean(left == right)
  }

  private func isDateTimeEquals(_ lhs: Expr, _ rhs: Expr) throws -> Expr {
    guard let left = try self.asDateComponents(lhs).date else {
      throw RuntimeError.type(lhs, expected: [NativeDateTime.type])
    }
    guard let right = try self.asDateComponents(rhs).date else {
      throw RuntimeError.type(rhs, expected: [NativeDateTime.type])
    }
    return .makeBoolean(left == right)
  }
  
  private func isDateTimeLess(_ lhs: Expr, _ rhs: Expr) throws -> Expr {
    guard let left = try self.asDateComponents(lhs).date else {
      throw RuntimeError.type(lhs, expected: [NativeDateTime.type])
    }
    guard let right = try self.asDateComponents(rhs).date else {
      throw RuntimeError.type(rhs, expected: [NativeDateTime.type])
    }
    return .makeBoolean(left < right)
  }
  
  private func isDateTimeGreater(_ lhs: Expr, _ rhs: Expr) throws -> Expr {
    guard let left = try self.asDateComponents(lhs).date else {
      throw RuntimeError.type(lhs, expected: [NativeDateTime.type])
    }
    guard let right = try self.asDateComponents(rhs).date else {
      throw RuntimeError.type(rhs, expected: [NativeDateTime.type])
    }
    return .makeBoolean(left < right)
  }
  
  private func isDateTimeLessEquals(_ lhs: Expr, _ rhs: Expr) throws -> Expr {
    guard let left = try self.asDateComponents(lhs).date else {
      throw RuntimeError.type(lhs, expected: [NativeDateTime.type])
    }
    guard let right = try self.asDateComponents(rhs).date else {
      throw RuntimeError.type(rhs, expected: [NativeDateTime.type])
    }
    return .makeBoolean(left <= right)
  }
  
  private func isDateTimeGreaterEquals(_ lhs: Expr, _ rhs: Expr) throws -> Expr {
    guard let left = try self.asDateComponents(lhs).date else {
      throw RuntimeError.type(lhs, expected: [NativeDateTime.type])
    }
    guard let right = try self.asDateComponents(rhs).date else {
      throw RuntimeError.type(rhs, expected: [NativeDateTime.type])
    }
    return .makeBoolean(left >= right)
  }
  
  private func dateTimeToSeconds(_ expr: Expr) throws -> Expr {
    guard let date = try self.asDateComponents(expr).date else {
      throw RuntimeError.type(expr, expected: [NativeDateTime.type])
    }
    return .makeNumber(date.timeIntervalSince1970)
  }

  private func dateTimeToString(_ expr: Expr, _ locale: Expr?, _ dateFormat: Expr?) throws -> Expr {
    let dateComponents = try self.asDateComponents(expr)
    guard let date = dateComponents.date else {
      throw RuntimeError.type(expr, expected: [NativeDateTime.type])
    }
    let formatter = DateFormatter()
    formatter.calendar = dateComponents.calendar ?? self.calendar
    formatter.timeZone = dateComponents.timeZone ?? TimeZone.current
    formatter.locale = try self.asLocale(locale)
    if let format = dateFormat {
      switch format {
        case .pair(let dateStyle, .pair(let timeStyle, _)):
          formatter.dateStyle = try self.asDateStyle(dateStyle)
          formatter.timeStyle = try self.asDateStyle(timeStyle)
        case .symbol(_):
          let style = try self.asDateStyle(format)
          formatter.dateStyle = style
          formatter.timeStyle = style
        default:
          formatter.dateFormat = try format.asString()
      }
    } else {
      formatter.dateStyle = .short
      formatter.timeStyle = .medium
    }
    return .makeString(formatter.string(from: date))
  }

  private func dateTimeToISO8601String(_ expr: Expr) throws -> Expr {
    let dateComponents = try self.asDateComponents(expr)
    guard let date = dateComponents.date else {
      throw RuntimeError.type(expr, expected: [NativeDateTime.type])
    }
    let formatter = ISO8601DateFormatter()
    formatter.timeZone = dateComponents.timeZone ?? TimeZone.current
    return .makeString(formatter.string(from: date))
  }

  private func dateTimeTimeZone(_ expr: Expr) throws -> Expr {
    return .makeString(try self.asDateComponents(expr).timeZone!.identifier)
  }

  private func dateTimeYear(_ expr: Expr) throws -> Expr {
    return .fixnum(Int64(try self.asDateComponents(expr).year!))
  }

  private func dateTimeMonth(_ expr: Expr) throws -> Expr {
    return .fixnum(Int64(try self.asDateComponents(expr).month!))
  }

  private func dateTimeDay(_ expr: Expr) throws -> Expr {
    return .fixnum(Int64(try self.asDateComponents(expr).day!))
  }

  private func dateTimeHour(_ expr: Expr) throws -> Expr {
    return .fixnum(Int64(try self.asDateComponents(expr).hour!))
  }

  private func dateTimeMinute(_ expr: Expr) throws -> Expr {
    return .fixnum(Int64(try self.asDateComponents(expr).minute!))
  }

  private func dateTimeSecond(_ expr: Expr) throws -> Expr {
    return .fixnum(Int64(try self.asDateComponents(expr).second!))
  }

  private func dateTimeNano(_ expr: Expr) throws -> Expr {
    return .fixnum(Int64(try self.asDateComponents(expr).nanosecond!))
  }

  private func dateTimeWeekday(_ expr: Expr) throws -> Expr {
    return .fixnum(Int64(self.weekday(from: try self.asDateComponents(expr).weekday!)))
  }

  private func dateTimeWeek(_ expr: Expr) throws -> Expr {
    return .fixnum(Int64(try self.asDateComponents(expr).weekOfYear!))
  }

  private func dateTimeDstOffset(_ expr: Expr) throws -> Expr {
    let dateComponents = try self.asDateComponents(expr)
    guard let date = dateComponents.date else {
      throw RuntimeError.type(expr, expected: [NativeDateTime.type])
    }
    return .makeNumber(dateComponents.timeZone!.daylightSavingTimeOffset(for: date))
  }

  private func dateTimeHasDst(_ expr: Expr) throws -> Expr {
    let dateComponents = try self.asDateComponents(expr)
    guard let date = dateComponents.date else {
      throw RuntimeError.type(expr, expected: [NativeDateTime.type])
    }
    return .makeBoolean(dateComponents.timeZone!.isDaylightSavingTime(for: date))
  }

  private func nextDstTransition(_ expr: Expr) throws -> Expr {
    let dateComponents = try self.asDateComponents(expr)
    guard let date = dateComponents.date else {
      throw RuntimeError.type(expr, expected: [NativeDateTime.type])
    }
    guard let next = dateComponents.timeZone!.nextDaylightSavingTimeTransition(after: date) else {
      return .false
    }
    return .object(NativeDateTime(self.calendar.dateComponents(in: dateComponents.timeZone!,
                                                               from: next)))
  }
  
  private func dateTimeHash(_ expr: Expr, _ bound: Expr?) throws -> Expr {
    guard let date = try self.asDateComponents(expr).date else {
      throw RuntimeError.type(expr, expected: [NativeDateTime.type])
    }
    let bnd = try bound?.asInt64() ?? 0
    if bnd <= 0 {
      return .fixnum(Int64(date.hashValue))
    } else {
      return .fixnum(Int64(date.hashValue) %% bnd)
    }
  }
  
  private func weekday(from value: Int) -> Int {
    return value > 1 && value < 8 ? value - 1 : 7
  }

  private func encodeWeekday(from n: Int) -> Int {
    return n >= 1 && n < 7 ? n + 1 : 1
  }

  private func asLocale(_ expr: Expr?) throws -> Locale {
    guard let locale = expr, locale != .false else {
      return Locale.current
    }
    return Locale(identifier: try locale.asSymbol().identifier)
  }

  private func getTimeZone(_ expr: Expr?) -> TimeZone? {
    guard let timezone = expr else {
      return TimeZone.current
    }
    switch timezone {
      case .fixnum(let delta):
        if delta > Int64(Int.min) && delta < Int64(Int.max) {
          return TimeZone(secondsFromGMT: Int(delta))
        } else {
          return nil
        }
      case .flonum(let delta):
        if delta > Double(Int.min) && delta < Double(Int.max) {
          return TimeZone(secondsFromGMT: Int(delta))
        } else {
          return nil
        }
      case .symbol(let sym):
        return TimeZone(abbreviation: sym.identifier)
      case .string(let str):
        return TimeZone(identifier: str as String) ?? TimeZone(abbreviation: str as String)
      case .false:
        return TimeZone.current
      default:
        return nil
    }
  }

  private func asTimeZone(_ expr: Expr?) throws -> TimeZone {
    guard let tzone = expr else {
      return TimeZone.current
    }
    guard let timeZone = self.getTimeZone(tzone) else {
      throw RuntimeError.eval(.invalidTimeZone, tzone)
    }
    return timeZone
  }

  private func asDateComponents(_ expr: Expr) throws -> DateComponents {
    guard case .object(let obj) = expr, let box = obj as? NativeDateTime else {
      throw RuntimeError.type(expr, expected: [NativeDateTime.type])
    }
    return box.value
  }

  private func toDateTime(_ date: Date, in timeZone: TimeZone) -> Expr? {
    return .object(NativeDateTime(self.calendar.dateComponents(in: timeZone, from: date)))
  }

  private func toDateTime(_ dc: DateComponents) -> Expr? {
    guard let date = dc.date, let timeZone = dc.timeZone else {
      return nil
    }
    return self.toDateTime(date, in: timeZone)
  }

  private func asDateStyle(_ expr: Expr) throws -> DateFormatter.Style {
    switch try expr.asSymbol() {
      case self.dateTimeNone:
        return .none
      case self.dateTimeShort:
        return .short
      case self.dateTimeMedium:
        return .medium
      case self.dateTimeLong:
        return .long
      case self.dateTimeFull:
        return .full
      default:
        throw RuntimeError.eval(.invalidDateStyle, expr)
    }
  }
}

public final class NativeDateTime: AnyNativeObject<DateComponents> {

  /// Type representing date times.
  public static let type = Type.objectType(Symbol(uninterned: "date-time"))

  public override var type: Type {
    return NativeDateTime.type
  }

  public override var string: String {
    guard let date = self.value.date else {
      return "#<date-time malformed>"
    }
    let formatter = ISO8601DateFormatter()
    formatter.timeZone = self.value.timeZone ?? TimeZone.current
    return "#<date-time \(formatter.string(from: date))>"
  }
}
