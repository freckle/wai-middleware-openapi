# WAI OpenAPI Middleware

Validates request and response bodies against your service's OpenAPI spec.

This is useful in a non-Servant web application, where OpenAPI specs cannot be
generated from the API implementation itself, and so must be maintained
manually.

## Usage

```hs
import Network.Wai (Middleware)
import Network.Wai.Middleware.OpenApi qualified as OpenApi

middleware :: Middleware
middleware =
  thisMiddleware
    . thatMiddleware
    . OpenApi.validateRequestBody settings
    . OpenApi.validateResponseBody settings
    . theOtherMiddleware

settings :: OpenApi.Settings
settings = OpenApi.defaultSettings openApi

-- Defined in your /docs or /openapi.json Handler
openApi :: OpenApi
openApi = undefined
```

Default behavior:

- If a request body is invalid, a 400 is returned
- If a response body is invalid, a 500 is returned
- In both cases, the validation errors are included in the response body

This is useful if you,

1. Are confident your server is currently complying with spec
2. Trust your spec enough to reject all invalid-according-to-it requests
3. Trust your observability to catch the 5xx increase any future
   response-validity bug would cause

If all or some of these are not true, see the next section.

## Evaluation

When first implementing this, you probably want to log invalid cases but still
respond normally.

```hs
settings :: OpenApi.Settings
settings = (OpenApi.defaultSettings openApi)
  { OpenApi.onValidationErrors = {- metrics, logging, etc -}
  , OpenApi.evaluateOnly = True
  }
```

Once you address what you find in the logs, you can disable `evaluateOnly`.

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
