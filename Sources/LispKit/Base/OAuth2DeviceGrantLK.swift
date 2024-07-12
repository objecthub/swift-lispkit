//
//  OAuth2DeviceGrantLK.swift
//  LispKit
//
//  Created by Matthias Zenger on 12/07/2024.
//  Copyright © 2024 ObjectHub. All rights reserved.
//

import Foundation
import OAuth2

/// https://www.ietf.org/rfc/rfc8628.html
open class OAuth2DeviceGrantLK: OAuth2 {
  public private(set) var deviceCode: String? = nil
  public private(set) var pollingInterval: Double = 5.0
  
	override open class var grantType: String {
		return "urn:ietf:params:oauth:grant-type:device_code"
	}
	
	override open class var responseType: String? {
		return ""
	}
		
	open func deviceAccessTokenRequest(with deviceCode: String) throws -> OAuth2AuthRequest {
		guard let clientId = clientConfig.clientId, !clientId.isEmpty else {
			throw OAuth2Error.noClientId
		}
		let req = OAuth2AuthRequest(url: (clientConfig.tokenURL ?? clientConfig.authorizeURL))
		req.params["device_code"] = deviceCode
		req.params["grant_type"] = type(of: self).grantType
		req.params["client_id"] = clientId
		return req
	}
	
	open func deviceAuthorizationRequest(params: OAuth2StringDict? = nil) throws -> OAuth2AuthRequest {
		guard let clientId = clientConfig.clientId, !clientId.isEmpty else {
			throw OAuth2Error.noClientId
		}
		guard let url = clientConfig.deviceAuthorizeURL else {
			throw OAuth2Error.noDeviceCodeURL
		}
		let req = OAuth2AuthRequest(url: url)
		req.params["client_id"] = clientId
		if let scope = clientConfig.scope {
			req.params["scope"] = scope
		}
		req.add(params: params)
		return req
	}
	
	open func parseDeviceAuthorizationResponse(data: Data) throws -> OAuth2JSON {
		let dict = try parseJSON(data)
		return try parseDeviceAuthorizationResponse(params: dict)
	}
	
	public final func parseDeviceAuthorizationResponse(params: OAuth2JSON) throws -> OAuth2JSON {
		try assureNoErrorInResponse(params)
		return params
	}
	
	/**
	Start the device authorization flow.
	
	- parameter params:   Optional key/value pairs to pass during authorize device request
	- parameter callback: The callback to call after the device authorization response has been received
	*/
	public func start(useNonTextualTransmission: Bool = false,
                    params: OAuth2StringDict? = nil,
                    queue: DispatchQueue? = .main,
                    completion: @escaping (DeviceAuthCodes?, Error?) -> Void) {
		authorizeDevice(params: params) { result, error in
			guard let result else {
				if let error {
					self.logger?.warn("OAuth2", msg: "Unable to get device code: \(error)")
				}
				completion(nil, error)
				return
			}
			guard let deviceCode = result["device_code"] as? String,
            let userCode = result["user_code"] as? String,
            let verificationUri = result["verification_uri"] as? String,
            let verificationUrl = URL(string: verificationUri),
            let expiresIn = result["expires_in"] as? Int else {
        let error = OAuth2Error.generic("The response doesn't contain all required fields.")
        self.logger?.warn("OAuth2", msg: String(describing: error))
        completion(nil, error)
        return
      }
      var verificationUrlComplete: URL?
      if let verificationUriComplete = result["verification_uri_complete"] as? String {
        verificationUrlComplete = URL(string: verificationUriComplete)
      }
      if useNonTextualTransmission, let url = verificationUrlComplete {
        do {
          try self.authorizer.openAuthorizeURLInBrowser(url)
        } catch let error {
          completion(nil, error)
        }
      }
      self.deviceCode = deviceCode
      self.pollingInterval = result["interval"] as? TimeInterval ?? 5.0
      if let queue {
        self.getDeviceAccessToken(deviceCode: deviceCode, interval: self.pollingInterval, queue: queue) { params, error in
          if let params {
            self.didAuthorize(withParameters: params)
          }
          else if let error {
            self.didFail(with: error.asOAuth2Error)
          }
        }
      }
      let deviceAuthorization = DeviceAuthCodes(
        deviceCode: deviceCode,
        userCode: userCode,
        verificationUrl: verificationUrl,
        verificationUrlComplete: verificationUrlComplete,
        expiresIn: expiresIn,
        interval: self.pollingInterval)
      completion(deviceAuthorization, nil)
		}
	}
	
	private func authorizeDevice(params: OAuth2StringDict?, completion: @escaping (OAuth2JSON?, Error?) -> Void) {
		do {
			let post = try deviceAuthorizationRequest(params: params).asURLRequest(for: self)
			logger?.debug("OAuth2", msg: "Obtaining device code from \(post.url!)")
			
			perform(request: post) { response in
				do {
					let data = try response.responseData()
					let params = try self.parseDeviceAuthorizationResponse(data: data)
					completion(params, nil)
				}
				catch let error {
					completion(nil, error.asOAuth2Error)
				}
			}
		}
		catch let error {
			completion(nil, error.asOAuth2Error)
		}
	}
	
	public func getDeviceAccessToken(deviceCode: String,
                                   interval: TimeInterval,
                                   queue: DispatchQueue = .main,
                                   completion: @escaping (OAuth2JSON?, Error?) -> Void) {
		do {
			let post = try deviceAccessTokenRequest(with: deviceCode).asURLRequest(for: self)
			logger?.debug("OAuth2", msg: "Obtaining access token for device with code \(deviceCode) from \(post.url!)")
			perform(request: post) { response in
				do {
					let data = try response.responseData()
					let params = try self.parseAccessTokenResponse(data: data)
					completion(params, nil)
				}
				catch let error {
					let oaerror = error.asOAuth2Error
					
					if oaerror == .authorizationPending(nil) {
						self.logger?.debug("OAuth2", msg: "AuthorizationPending, repeating in \(interval) seconds.")
						queue.asyncAfter(deadline: .now() + interval) {
              self.getDeviceAccessToken(deviceCode: deviceCode,
                                        interval: interval,
                                        queue: queue,
                                        completion: completion)
						}
					} else if oaerror == .slowDown(nil) {
						let updatedInterval = interval + 5 // The 5 seconds increase is required by the RFC8628 standard (https://www.rfc-editor.org/rfc/rfc8628#section-3.5)
						self.logger?.debug("OAuth2", msg: "SlowDown, repeating in \(updatedInterval) seconds.")
						queue.asyncAfter(deadline: .now() + updatedInterval) {
              self.getDeviceAccessToken(deviceCode: deviceCode,
                                        interval: updatedInterval,
                                        queue: queue,
                                        completion: completion)
						}
					} else {
						completion(nil, oaerror)
					}
				}
			}
		}
		catch let error {
			completion(nil, error.asOAuth2Error)
		}
	}
}

public struct DeviceAuthCodes {
  public let deviceCode: String
	public let userCode: String
	public let verificationUrl: URL
	public let verificationUrlComplete: URL?
	public let expiresIn: Int
  public let interval: Double
}
