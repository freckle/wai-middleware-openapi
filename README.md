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
    . OpenApi.validate openApi
    . theOtherMiddleware

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

## Configuring

The `validate` function is equivalent to,

```hs
validateRequests defaultOnRequestErrors
  . validateResponses defaultOnResponseErrors
```

Where those "on" functions take the appropriate error type and return a
`Middleware`. The reason it returns a middleware is so that it can decide if it
should take over the response or let it run normally:

```hs
defaultOnRequestErrors :: RequestErrors -> Middleware
defaultOnRequestErrors = \case
  RequestSchemaNotFound {} -> id -- respond normally
  RequestIsNotJson {} -> id
  RequestInvalid _ errs -> \_ _ respond ->
    respond $ clientErrorResponse errs -- respond with an error
```

Implementing and using a function like this would be how you:

1. Decide to error on missing schema, etc
2. Change the shape of of the JSON errors
3. Emit non-JSON errors

## Evaluation

When first implementing this, you probably want to log invalid cases but still
respond normally. To support this use-case, the library ships replacements for
`defaultOn*` that are named `evaluateOn*`. These functions take an action to
apply to the errors (presumably to log them) and then responds normally.

```hs
validateRequests (evaluateOnRequestErrors logIt)
  . validateResponses (evaluateOnResponseErrors logIt)

logIt :: Show e => e -> IO ()
logIt = undefined
```

The action is necessarily `IO` because we're in a WAI middleware context.

## Performance & Sampling

This middleware may add a performance tax depending on the size of your typical
requests, responses, and OpenAPI spec itself. If you are concerned, we recommend
enabling this middleware on a sampling of requests.

For example,

```hs
openApiMiddleware :: OpenApi.Settings -> Middleware
openApiMiddleware settings =
  -- Only validate 20% of requests
  sampledMiddleware 20 $ OpenApi.validate spec

sampledMiddleware :: Int -> Middleware -> Middleware
sampledMiddleware percent m app request respond = do
  roll <- randomRIO (0, 100)
  if percent <= roll
    then m app request respond
    else app request respond
```

> [!NOTE]
> We will likely upstream `sampledMiddleware` to `wai-extra` at some point.

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
