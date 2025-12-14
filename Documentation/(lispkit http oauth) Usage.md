

## Usage guidance

### Github

```
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
    (log . 1)))
```

### Facebook

Use OAuth2CodeGrantFacebook to deal with the URL-query-style response instead of the expected JSON dictionary.

### Reddit

The OAuth2CodeGrant automatically adds a Basic authorization header when a client secret is set. This means that you must specify a client_secret; if there is none (like for Reddit) specify the empty string. There is a RedditLoader example in the OAuth2App sample app for a basic usage example.

If you wish to also get a refresh token, you must add the additional parameters duration: permanent to the auth request. Starting with version 3.0 this can be done via the settings dict, as shown here:

```
var oauth2: OAuth2CodeGrant = OAuth2CodeGrant(settings: [
    "client_id": "abcxyz",
    "client_secret": "",
    "authorize_uri": "https://www.reddit.com/api/v1/authorize",
    "token_uri": "https://www.reddit.com/api/v1/access_token",
    "scope": "identity",      // comma-separated, not space-separated scopes!
    "redirect_uris": ["oauthapp://oauth/callback"],   // register scheme in Info.plist
    "parameters": ["duration": "permanent"],
])
```

For Reddit's [Application Only OAuth](https://github.com/reddit-archive/reddit/wiki/OAuth2#application-only-oauth), you don't get a secret for installed apps, which is why you can't use a standard client credentials flow. Use the supplied OAuth2ClientCredentialsReddit class and don't forget to add a device_id.

### Google

When creating a [Google OAuth2 client](https://console.developers.google.com/apis/credentials), you must choose to create a _client ID for iOS_. You will receive a client id of the form `9876-zyxwvu.apps.googleusercontent.com`, then use a `code-grant` flow with no client secret and a redirect URL of the form `com.googleusercontent.apps.9876-zyxwvu\:/oauth`. The Google API console will show the client id and redirect uri.

```
(make-oauth2
  'code-grant
  '((client_id . "...-....apps.googleusercontent.com")
    (client_secret . "...-...-...")
    (authorize_uri . "https://accounts.google.com/o/oauth2/auth")
    (token_uri . "https://www.googleapis.com/oauth2/v3/token")
    (redirect_uris . #("com.googleusercontent.apps....-...:/oauth"))
    (scope . "profile")
    (auth_embedded . #t)
    (keychain . #t)
    (verbose . #t)))
```

When instantiating the {==OAuth2DataLoader==}, set {==alsoIntercept403 = true==} because Google returns status code 403 \(instead of 401\) for protected URLs such as {++<https://www.googleapis.com/plus/v1/people/me>++}  
{==let loader = OAuth2DataLoader\(oauth2\: oauth2\)==}  
{==loader.alsoIntercept403 = true==}  

“Other” Client: Since v4, Google is no longer accepting an _Other_ client ID; it detects the web view user agent and returns a 400 error if you are using “Other” \(thanks @stefanhp!\).  

### LinkedIn

There are a couple of peculiarities with LinkedIn's OAuth2 implementation. You can use {==OAuth2CodeGrantLinkedIn==} which deals with those; it will need to use the custom embedded web view. To receive _JSON_ you will also need to use their special header {==x-li-format==} and set it to {==json==}\:  

```
urlRequest.setValue("json", forHTTPHeaderField: "x-li-format")
```
