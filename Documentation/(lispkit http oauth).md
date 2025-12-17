# LispKit HTTP OAuth

Library `(lispkit http oauth)` implements the OAuth 2.0 authorization framework as defined by [RFC 6749](https://datatracker.ietf.org/doc/html/rfc6749). The OAuth 2.0 authorization framework enables a third-party application to obtain limited access to an HTTP service, either on behalf of a resource owner by orchestrating an approval interaction between the resource owner and the HTTP service, or by allowing the third-party application to obtain access on its own behalf.

## Protocol overview

OAuth2 defines four roles:

  - **resource owner**: An entity capable of granting access to a protected resource. Often, the resource owner is a user who authorizes an application to access their account. The application's access to the user’s account is limited to the scope of the authorization granted.
  - **resource server**: The server hosting the protected resources, capable of accepting and responding to protected resource requests using access tokens.
  - **client**: An application making protected resource requests on behalf of the resource owner and with its authorization. Library `(lispkit http oauth)` facilitates primarily the implementation of clients.
  - **authorization server**: The server issuing access tokens to the client after successfully authenticating the resource owner and obtaining authorization.

The following diagram illustrates the interaction flow between the four roles.

![OAuth 2.0 Protocol Flow](x-devonthink-item://8713FF7E-46EF-4702-BEF3-E04058EA8187)

The protocol consists of the following steps:

  - **(A)** The client requests authorization from the resource owner. The authorization request can be made directly to the resource owner (as shown), or preferably indirectly via the authorization server as an intermediary.
  - **(B)** The client receives an authorization grant, which is a credential representing the resource owner's authorization, expressed using one of four grant types defined in this specification: _authorization code_, _implicit_, _resource owner password credentials_, and _client credentials_.
  - **(C)** The client requests an access token by authenticating with the authorization server and presenting the authorization grant.
  - **(D)** The authorization server authenticates the client and validates the authorization grant, and if valid, issues an access token.
  - **(E)** The client requests the protected resource from the resource server and authenticates by presenting the access token.
  - **(F)** The resource server validates the access token, and if valid, serves the request.

## OAuth2 flows

Library `(lispkit http oauth)` supports the major types of OAuth2 flows, i.e. instantiations of the abstract protocol outlined above. Flow types are specified via symbols. The following symbols listed below are supported. Some sites might not strictly adhere to the OAuth2 flows, from returning data differently to omitting mandatory return parameters. The library deals with those deviations by creating site-specific flow types.

  - `code-grant` (response type = code): This flow is typically used by applications that can guard their secrets, like server-side apps (not in distributed binaries). In case an application cannot guard its secret, such as a distributed app, you would use `implicit-grant` or, in some cases, still a `code-grant` but omitting the client secret. This flow automatically creates a `Basic` authorization header if the OAuth2 object defines a client secret (can be the empty string if there is none). If the site requires client credentials in the request body, set `secret_in_body` to `#t`.
  - `code-grant-basic-auth`
  - `code-grant-no-token-type`
  - `code-grant-azure`: Code grant flow for Azure.
  - `code-grant-facebook`: Code grant flow for Facebook.
  - `code-grant-linkedin`: Code grant flow for LinkedIn.
  - `implicit-grant` (response type = token): An implicit grant is suitable for apps that are not capable of guarding their secret, such as distributed binaries or client-side web apps. The client receives an access token and perform requests by providing this token.
  - `implicit-grant-query-params`
  - `client-credentials`: A 2-legged flow that lets an app authorize itself via its client id and secret. Both `client_id` and `client_secret` need to be provided in the corresponding settings.
  - `client-credentials-reddit`: A client credentials flow specifially for Reddit.
  - `password-grant`: This specifies the _Resource Owner Password Credentials Grant_. It requires that `username` and `password` settings are provided. Requests can then be authorized via procedure `oauth2-authorize!`.
  - `device-grant`: This Device Authorization Grant flow is designed for devices that either lack a browser to perform a user-agent-based authorization or are input constrained, it is also very useful for applications not allowed to start their own webserver (loopback URL) or register a custom URL scheme to finish the authorization code grant flow. To initiate the device grant flow, the setting `authorize_uri` needs to be correctly configured to point towards the device authorization endpoint. By calling procedure `oauth2-request-codes`, the client obtains all necessary details to complete the authorization on a secondary device or in the system browser.

## OAuth2 flow features

This library supports _dynamic client registration_. If during setup, `registration_url` is set but `client_id` is not, the `oauth2-authorize!` call automatically attempts to register the client before continuing to the actual authorization. Client credentials returned from registration are stored in the keychain. 

_PKCE_ support is controlled by the `use_pkce` setting. It is disabled by default. When enabled, a new code verifier string is generated for every authorization request.

This framework can transparently use the _keychain_. This feature is controlled by the `keychain` setting which is enabled by default. If it is not turned off initially, the keychain will be queried for tokens and client credentials related to the authorization URL. If it is turned off after initialization, the keychain will be queried for existing tokens, but new tokens will not be written to the keychain.

It is possible to delete the tokens from the keychain, i.e. log the user out completely by calling `oauth2-forget-tokens!`.

Ideally, access tokens get delivered with an `expires_in` parameter that tells you how long the token is valid. If it is missing, the framework will still use those tokens if one is found in the keychain and not re-perform the OAuth flow. You will need to intercept 401s and re-authorize if an access token has expired but the framework has still pulled it from the keychain. This behavior can be turned off by declaring the setting `token_assume_unexpired` to `#f`.

## OAuth2 usage example

The simplest way to use library `(lispkit http oauth)` is to first create an OAuth2 client by specifying the OAuth2 flow type and by providing settings that define the required parameters for the flow and the concrete API that is being targeted. Next, an OAuth2 session needs to be created by providing the OAuth2 client. OAuth2 sessions can be configured just like regular HTTP sessions as provided by library `(lispkit http)`. Finally, HTTP requests can be sent using this session with the framework performing the authorization flow as needed in the background.

Based on this usage pattern, the example below shows how library `(lispkit http oauth)` can be used to call the GitHub API.

```scheme
;; Define an OAuth2 client using a code grant flow
(define oauth
  (make-oauth2
    'code-grant            ; flow type
    '((client_id . "...")  ; flow settings
      (client_secret . "...")
      (authorize_uri . "https://github.com/login/oauth/authorize")
      (token_uri . "https://github.com/login/oauth/access_token")
      (redirect_uris . #("lisppad://oauth/callback"))
      (scope . "user repo:status")
      (auth_embedded . #t)
      (keychain . #t)
      (secret_in_body . #t)
      (log . 1))))

;; Set up OAuth2 session using the client to authorize requests
(define session (make-oauth2-session oauth))

;; Define a GET request for retrieving user data
(define request (make-http-request "https://api.github.com/user" "GET"))

;; Send the request via the session; this returns a future containing
;; the response of the request (or an error if the request failed)
(define result (oauth2-session-send request session))

;; Retrieve the body of the HTTP response from result; this will
;; block until the response has been received
(define response
  (bytevector->json (http-response-content (future-get result))))

;; Pretty print the response
(display (json->string response #t))
```

## OAuth2 settings

OAuth2 flows are configured with a settings association list that defines all parameters that influence a flow. As keys, typically symbols are used, but it is possible to use strings instead. As settings values, supported are the following data types: boolean, fixnum, flonum, vector (of strings), and association list (mapping symbols/strings to strings).

The following settings are supported:

   - `client_id` (string): Client identifier (e.g. application identifier)
   - `client_secret` (string): Usually only needed for code grant flows
   - `authorize_uri` (string): Authorization URL
   - `token_uri` (string): If omitted, `authorize_uri` will be used to obtain tokens
   - `refresh_uri` (string): If omitted, `token_uri` will be used to obtain tokens
   - `redirect_uris` (vector): redirect URLs
   - `scope` (string)
   - `custom_user_agent` (string)
   - `client_name` (string)
   - `registration_uri` (string)
   - `logo_uri` (string)
   - `keychain` (boolean): Use the system keychain; `#t` by default.
   - `keychain_access_mode` (string): A string value describing the keychain _access policy_. By default, this is `when-unlocked`.
   - `keychain_access_group` (string): This is referring to the access group identifier of the keychain to be used. This is unset by default.
   - `keychain_account_for_client_credentials` (string): The name to use to identify client credentials in the keychain, "clientCredentials" by default; `"clientCredentials"` by default.
   - `keychain_account_for_tokens` (string): The name to use to identify the tokens in the keychain; `"currentTokens"` by default.
   - `secret_in_body` (boolean): Forces the flow to use the request body for the client secret; `#f` by default.
   - `parameters` (association list): Custom request parameters to be added during authorization.
   - `token_assume_unexpired` (boolean): Determines whether to use access tokens that do not come with an `expires_in` parameter; `#t` by default.
   - `use_pkce` (boolean): Enable PKCE; `#f` is the default.
   - `log` (fixnum): Minimum log level (0 = trace, 1 = debug, 2 = warn, 3 = off).
   - `auth_embedded` (boolean): Use an embedded authorization mode;`#f` is the default.
   - `username` (string): Username of the user; used by password grant flows.
   - `password` (string): Password of the user; used by password grant flows.
   - `resource` (string): Resource used by `code-grant-azure` flow.
   - `basic` (string): Used by `code-grant-basic-auth`.
   - `device_id` (string): Used by flows such as `client-credentials-reddit`.

## OAuth2 clients

OAuth 2.0 defines a flexible authorization protocol. Library `(lispkit http oauth)` implements this protocol with a number of authorization _flows_. The configuration of an OAuth 2.0 flow is encapsulated in a `oauth2` client object which is defined in terms of a symbol identifying the flow type as well as settings which provide the parameters for the chosen type of flow. Such `oauth2` clients are then used to perform authorizations, sign HTTP requests, and create _OAuth2 sessions_, which can be used, just like regular _HTTP sessions_ to coordinate a group of related data-transfer tasks, e.g. via functionality provided by library `(lispkit http)`. As a side effect of such operations, state, such as _access_ and _refresh tokens_ are stored within `oauth2` client objects.

**oauth2-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `oauth2` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all oauth2 objects.

**(oauth2? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a oauth2 object; `#f` otherwise.

**(make-oauth2 _flow settings_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new OAuth2 client based on an authorization flow identifier _flow_ and settings which provide the parameters needed for executing the authorization flow. _flow_ is one of the following symbols:

  - `code-grant`
  - `code-grant-basic-auth`
  - `code-grant-no-token-type`
  - `code-grant-azure`
  - `code-grant-facebook`
  - `code-grant-linkedin`
  - `implicit-grant`
  - `implicit-grant-query-params`
  - `client-credentials`
  - `client-credentials-reddit`
  - `password-grant`
  - `device-grant`

_settings_ is an association list which maps symbols (the settings keys) to settings values, which are either booleans, fixnums, flonums, vectors (of strings), and association lists (mapping symbols/strings to strings). Procedure `make-oauth2` returns new `oauth2` objects.

**(oauth2-flow _oauth_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the authentication flow identifier (a symbol) for the flow specified by the `oauth2` object _oauth_.

**(oauth2-settings _oauth_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the authentication flow settings for the flow specified by the `oauth2` object _oauth_. Settings are association lists which map symbols (the settings keys) to settings values, which are either booleans, fixnums, flonums, vectors (of strings), and association lists (mapping symbols/strings to strings).

**(oauth2-setting _oauth key_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the settings value for the given settings _key_ (a symbol) in the `oauth2` object _oauth_. It is an error if _key_ is undefined in _oauth_.

**(oauth2-unexpired-access-token? _oauth_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if the _oauth_ client contains an access token that is not expired.

**(oauth2-access-token _oauth_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the access token contained in the _oauth_ client; `#f` otherwise.

**(oauth2-refresh-token _oauth_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the refresh token contained in the _oauth_ client; `#f` otherwise.

**(oauth2-forget-tokens! _oauth_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Discards access and refresh tokens encapsulated in the authorization client _oauth_.

**(oauth2-cancel-requests! _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(oauth2-cancel-requests! _obj timeout_)**  

Cancels outstanding requests, waiting for at most _timeout_ seconds for a response. The default for _timeout_ is 0 seconds. _obj_ either specifies an `oauth2` client, in which case all outstading requests related to this client are canceled, or it refers to a future, in which case only the request associated with initializing the future will be canceled.

**(oauth2-request-codes _oauth_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(oauth2-request-codes _oauth non-textual?_)**  
**(oauth2-request-codes _oauth non-textual? params_)**  

This procedure can only be used in combination with _oauth_ clients for `device-grant` authorization flows. It initiates the authorization flow returning a future which will, upon successful authorization, provide access to an association list containing the following attributes: `user-code`, `expires-in`, `verification-url`, `verification-url-complete`, `device-code`, and `interval` (polling interval for requesting the device access token). Should the flow fail, the corresponding error is stored in the future.

If boolean argument _non-textual?_ is set to true (default is false), the device grant flow will allow an authorization to be completed in a browser by opening the URL provided in the `verification-url-complete` parameter. _params_ is an association list mapping strings to strings. These are optionally defining HTTP headers that are passed through in the device authorization request.

**(oauth2-authorize! _oauth_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Initiates an OAuth 2.0-based authorization via the flow specified in the _oauth_ client. Procedure `oauth2-authorize!` returns a future which eventually contains an association list with the attributes contained in the successful JSON response to the authorization request. If the request failed, then the future will refer to the error that lead to the failure of the authorization.

**(oauth2-redirect! _redirect-url_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(oauth2-redirect! _oauth redirect-url_)**  

Handles an OAuth 2.0 redirect callback by processing the _redirect-url_ received from the authorization server. This procedure extracts the authorization code or access token from the redirect URL and completes the authorization flow for the _oauth_ client. Returns a boolean indicating whether the processing of the redirect was successfull. This is typically used in conjunction with custom URL scheme handlers or redirect URI interceptors.

**(http-request-sign! _request oauth_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Signs the HTTP request _request_ (see library `(lispkit http)`) by including an `Authorization` HTTP header that refers to the access token contained in the authorization client _oauth_. Procedure `http-request-sign!` returns `#t` if the header could successfully be added to _request_; otherwise `#f` is returned.


## OAuth2 sessions

_OAuth2 sessions_ can be used, just like regular _HTTP sessions_, to coordinate a group of related data-transfer tasks. Within each session, a series of tasks are created, each of which represents a request for a specific URL. The tasks within a given OAuth2 session share a common _session configuration_ and _OAuth2 client_. The configuration of a session defines connection behavior, like the maximum number of simultaneous connections to make to a single host, whether connections can use the cellular network, etc. As opposed to HTTP sessions, OAuth2 sessions automatically and transparently perform OAuth 2.0 authorization whenever needed via the encapsulated OAuth2 client.

**oauth2-session-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `oauth2-session` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all oauth2 session objects.

**(oauth2-session? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a oauth2 session object; `#f` otherwise.

**(make-oauth2-session _oauth_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(make-oauth2-session _oauth proto_)**  
**(make-oauth2-session _oauth proto host_)**  
**(make-oauth2-session _oauth proto host intercept403?_)**  
**(make-oauth2-session _oauth proto host intercept403? timeout_)**  
**(make-oauth2-session _oauth proto host intercept403? timeout cookies?_)**  
**(make-oauth2-session _oauth proto host intercept403? timeout cookies? cache_)**  
**(make-oauth2-session _oauth proto host intercept403? timeout cookies? cache maxcon_)**  
**(make-oauth2-session _oauth proto host intercept403? timeout cookies? cache maxcon pipe?_)**  
**(make-oauth2-session _oauth proto host intercept403? timeout cookies? cache maxcon pipe? cell?_)**  

Returns a new OAuth2 session object for the OAuth2 client _oauth_. The configuration is copied from the prototype session object _proto_. The arguments following _proto_ override individual settings of _proto_. If one of those arguments is set to `()`, then it is ignored.

If _proto_ is `#f` (or not provided at all), system-specific defaults are used. If _proto_ is `#t`, an _ephemeral_ session default is used which is not writing caches, cookies, or credentials to disk. Otherwise, it is assumed _proto_ is either an HTTP session or another OAuth2 session object whose configuration will be used for the newly created OAuth2 session.

_host_, if provided, will be used to handle redirects within the same domain. The default is `#f`. If _intercept403?_ is set to `#t`, a 403 HTTP response is treated like a 401 HTTP response. The default is `#f`. _timeout_ defines the time in seconds to wait for (additional) data. The default is 60. _cookies?_ is a boolean argument determining whether requests should automatically provide cookies from the shared cookie store. The default is `#t`. If set to `#f`, then cookie headers need to be provided manually.

_cache_ defines a cache policy. The following policies, specified as symbols, are supported:
  - `use-protocol-cache-policy`: Use the caching logic defined in the protocol implementation (default).
  - `reload-ignoring-local-cache`: The URL load should be loaded only from the originating source.
  - `reload-ignoring-local-remote-cache`: Ignore local cache data, and instruct proxies and other intermediates to disregard their caches so far as the protocol allows.
  - `return-cache-data-else-load`: Use existing cache data, regardless or age or expiration date, loading from originating source only if there is no cached data.
  - `return-cache-data-dont-load`: Use existing cache data, regardless or age or expiration date, and fail if no cached data is available.
  - `reload-revalidating-cache`: Use cache data if the origin source can validate it; otherwise, load from the origin.

Argument _maxcon_ specifies the maximum number of simultaneous connections made to each host by requests initiated by this session. The default value is 6. _pipe?_ is a boolean argument  determining whether HTTP pipelining should be used. _cell?_ is a boolean argument specifying whether connections should be made over a cellular network.

**(oauth2-session-oauth2 _session_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the OAuth2 client used by OAuth2 _session_.

**(oauth2-session-http-session _session_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

OAuth2 sessions are implemented in terms of HTTP sessions. `oauth2-session-oauth2` returns the HTTP session used to implement the given OAuth2 _session_.

**(oauth2-session-send _request session_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates a task that retrieves the contents of a URL via the specified HTTP request object _request_, and eventually stores a HTTP result object in the future returned by `oauth2-session-send`. In the background, the OAuth2 client embedded in _session_ is used to authorize the request.
