//
//  BuildMetadata.m
//  LispKit
//
//  Created by Matthias Zenger on 05/05/2016.
//  Copyright © 2016 ObjectHub. All rights reserved.
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
