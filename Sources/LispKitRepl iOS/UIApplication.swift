//
//  UIApplication.swift
//  LispKit
//
//  Created by Matthias Zenger on 08/07/2024.
//  Copyright © 2024 ObjectHub. All rights reserved.
//

import UIKit

extension UIApplication {
    
  public var keyWindowPresentedController: UIViewController? {
    // Find the key window
    let window = UIApplication.shared
                              .connectedScenes
                              .flatMap { ($0 as? UIWindowScene)?.windows ?? [] }
                              .last { $0.isKeyWindow }
    var viewController = window?.rootViewController
    // If root `UIViewController` is a `UITabBarController`
    if let presentedController = viewController as? UITabBarController {
      // Move to selected `UIViewController`
      viewController = presentedController.selectedViewController
    }
    // Go deeper to find the last presented `UIViewController`
    while let presentedController = viewController?.presentedViewController {
      // If root `UIViewController` is a `UITabBarController`
      if let presentedController = presentedController as? UITabBarController {
        // Move to selected `UIViewController`
        viewController = presentedController.selectedViewController
      } else {
        // Otherwise, go deeper
        viewController = presentedController
      }
    }
    return viewController
  }
}
