//
//  BuildMetadata.m
//  LispKit
//
//  Created by Matthias Zenger on 05/05/2016.
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

#import <Foundation/Foundation.h>

NSDate *getBuildDate() {
  NSString *compileDate = [NSString stringWithUTF8String:__DATE__];
  NSDateFormatter *df = [[NSDateFormatter alloc] init];
  [df setDateFormat:@"MMM d yyyy"];
  NSLocale *usLocale = [[NSLocale alloc] initWithLocaleIdentifier:@"en_US"];
  [df setLocale:usLocale];
  return [df dateFromString:compileDate];
}

NSString *getBuildTime() {
  return [NSString stringWithUTF8String:__TIME__];
}
