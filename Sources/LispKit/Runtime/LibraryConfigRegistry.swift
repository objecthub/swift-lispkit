//
//  LibraryConfigRegistry.swift
//  LispKit
//
//  Created by Matthias Zenger on 20/10/2026.
//  Copyright © 2026 ObjectHub. All rights reserved.
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
/// A `LibraryConfig` configures a library from the outside, e.g. by specifying
/// the locations of scripts and the parameters of renderers. A configuration is
/// a value that is registered with a `LibraryConfigRegistry`. It does not do
/// anything by itself: when the library needs what it has been configured with,
/// the registry asks the configuration to create the `Resource`, e.g. the renderers,
/// for a context (see `evaluate(in:)`).
///
/// There is at most one configuration for each type implementing this protocol.
///
public protocol LibraryConfig {

  /// What the configuration creates for a context, e.g. the renderers of a library.
  associatedtype Resource

  /// Creates the resource for the given context. The registry calls this method
  /// when the resource is requested for the first time (or for the first time
  /// since the resource was purged or the configuration got replaced), and then
  /// keeps the result.
  ///
  /// The registry holds its lock while this method runs. Thus, the method must
  /// not access the registry of the context.
  func evaluate(in: Context) -> Resource
}

///
/// A `LibraryConfigRegistry` manages the configurations of the libraries of a
/// context, and the resources that these configurations create for the context.
/// Every context has a registry, which is available as `Context.configs`. It
/// initially contains the global configurations (`Context.globalConfigs`, extended
/// via `Context.registerGlobalConfig`); later changes to the global configurations
/// do not affect existing contexts.
///
/// Resources are created lazily, when `resource(for:)` is called for the first time,
/// and are then kept for the lifetime of the context, so that all libraries using
/// a resource share it. Code that has obtained a resource keeps using it; registering
/// a new configuration (or purging a resource) only affects those who request the
/// resource afterwards. Hence, configurations need to be registered before the
/// resource is used for the first time, e.g. before the library it configures is
/// imported.
///
/// All methods are thread-safe.
///
public final class LibraryConfigRegistry {

  /// The context owning this registry; this reference is weak since the context
  /// owns the registry.
  private weak let context: Context?

  /// Protects `configs` and `resources`.
  private let lock = NSLock()

  /// The registered configurations, by their type.
  private var configs: [ObjectIdentifier : any LibraryConfig] = [:]

  /// The resources created by the configurations so far, by the type of the
  /// configuration.
  private var resources: [ObjectIdentifier : Any] = [:]

  /// Creates a registry for `context`, with the configurations `predef`
  /// registered already.
  internal init(for context: Context, including predef: [any LibraryConfig]) {
    self.context = context
    for config in predef {
      self.configs[ObjectIdentifier(type(of: config))] = config
    }
  }
  
  /// Returns the registered configuration of type `C`, or `nil` if
  /// there is none.
  public func config<C: LibraryConfig>(for type: C.Type) -> C? {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    return self.configs[ObjectIdentifier(C.self)] as? C
  }
  
  /// Returns the resource that the configuration of type `C` creates for the
  /// context. The configuration evaluates (see `LibraryConfig.evaluate(in:)`)
  /// the first time the resource is requested; the registry keeps the result
  /// and returns it for all later requests, until it gets purged or the
  /// configuration is replaced. Returns `nil` if no configuration of type
  /// `C` is registered, or if the resource needs to be created but the context
  /// does not exist anymore.
  public func resource<C: LibraryConfig>(for type: C.Type) -> C.Resource? {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    let key = ObjectIdentifier(C.self)
    if let resource = self.resources[key] as? C.Resource {
      return resource
    } else if let context,
              let config = self.configs[key] as? C {
      let resource = config.evaluate(in: context)
      self.resources[key] = resource
      return resource
    } else {
      return nil
    }
  }
  
  /// Registers `config`, replacing the configuration of the same type, if there is
  /// one. The resource that the replaced configuration created is discarded, so the
  /// next request for the resource evaluates the new configuration. Code that has
  /// obtained the old resource already is not affected.
  public func register<C: LibraryConfig>(config: C) {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    let key = ObjectIdentifier(C.self)
    self.configs[key] = config
    self.resources.removeValue(forKey: key)
  }
  
  /// Discards the resource that the configuration of type `C` created, if any. The
  /// configuration stays registered; the next request for the resource evaluates it
  /// again. Code that has obtained the discarded resource already is not affected.
  public func purgeResource<C: LibraryConfig>(for config: C.Type) {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    self.resources.removeValue(forKey: ObjectIdentifier(C.self))
  }
  
  /// Discards all resources that the registered configurations created. The
  /// configurations stay registered; the next requests for the resources
  /// evaluate them again.
  public func purgeAllResources() {
    self.lock.lock()
    defer {
      self.lock.unlock()
    }
    self.resources.removeAll()
  }
}
