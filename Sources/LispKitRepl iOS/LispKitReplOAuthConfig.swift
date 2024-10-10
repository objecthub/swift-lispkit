//
//  LispKitReplOAuthConfig.swift
//  LispKit
//
//  Created by Matthias Zenger on 08/07/2024.
//  Copyright © 2024 ObjectHub. All rights reserved.
//

import OAuth2
import LispKit
import UIKit

public final class LispKitReplOAuthConfig: HTTPOAuthConfig {
  
  public init(context: Context) {
  }
  
  public func configureEmbeddedAuth(oauth2: OAuth2) {
    if Thread.isMainThread {
      if let context = UIApplication.shared.keyWindowPresentedController {
        oauth2.authConfig.authorizeContext = context
        oauth2.authConfig.authorizeEmbedded = true
        oauth2.authConfig.ui.useSafariView = false
        oauth2.authConfig.ui.useAuthenticationSession = true
      }
    } else {
      DispatchQueue.main.sync {
        if let context = UIApplication.shared.keyWindowPresentedController {
          oauth2.authConfig.authorizeContext = context
          oauth2.authConfig.authorizeEmbedded = true
          oauth2.authConfig.ui.useSafariView = false
          oauth2.authConfig.ui.useAuthenticationSession = true
        }
      }
    }
  }
  
  public func createLogger(level: OAuth2LogLevel) -> OAuth2Logger {
    return OAuth2DebugLogger(level)
  }
}
