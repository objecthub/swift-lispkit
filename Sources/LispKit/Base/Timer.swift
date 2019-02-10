//
//  Timer.swift
//  LispKit
//
//  Created by Matthias Zenger on 20/03/2016.
//  Copyright © 2016 ObjectHub. All rights reserved.
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

///
/// Timer utilities based on Darwin framework.
///
public struct Timer {

  private static let (TIME_NUMER, TIME_DENOM): (UInt64, UInt64) = {
    var info: mach_timebase_info = mach_timebase_info(numer: 0, denom: 0)
    mach_timebase_info(&info)
    return (UInt64(info.numer), UInt64(info.denom))
  }()

  /// Returns a current time measurement in seconds, as a Double. This is only useful for
  /// measuring short time intervals.
  public static var currentTimeInSec: Double {
    return Double(mach_absolute_time() * Timer.TIME_NUMER / Timer.TIME_DENOM) / 1e9
  }

  /// Returns a current time measurement in milliseconds, as a UInt64. This is only useful for
  /// measuring short time intervals.
  public static var currentTimeInMSec: UInt64 {
    return (mach_absolute_time() * Timer.TIME_NUMER) / (Timer.TIME_DENOM * 1000000)
  }
}
