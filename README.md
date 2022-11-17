# Circle

[![MIT LICENSE](https://img.shields.io/github/license/mashape/apistatus.svg)](https://github.com/dmarticus/circle/master/blob/LICENSE)

## [Circle API](https://developers.circle.com/developer/docs) Coverage for Haskell

This library provides Haskell bindings for [Circle's V1 API](https://developers.circle.com/developer/v1/reference/).  The supported operations include but aren't limited to:

* Fiat Payouts (for both personal and business accounts)
* Blockchain Transfers (for both personal and business accounts)
* Fiat Payments
* Fiat Payment cancellation and refunds
* Card issuing
* Chargebacks
* Subscriptions
* Settlements
* Wires
* ACHs
* Balances
* Crypto Payments
* Crypto Payouts
* Wallets

**NOTE:** This library does not currently support Circle V1.1 (which was released on 11/15/22).  It will soon, though!

## Usage

You'll need to set the environment variable `CIRCLE_API_KEY` (or something like that) to connect to Circle's sandbox (or production) environments.  Get your keys [here](https://developers.circle.com/docs/api-keys).

```hs
import Circle.Client
import Circle.Types
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
--
main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  config <- sandboxEnvConfig "CIRCLE_API_KEY" -- or whatever you ended up using for your environment variable
  result <- circle config manager getConfigurationInfo
  case result of
    Right CircleResponseBody b -> print bs
    Left CircleError e -> print e
```

## Optional Parameters

Stripe API calls can take multiple optional parameters.
`circle` supports optional parameters through the use of type-families and typeclasses.

In practice, the function to use is `(-&-)` to specify optional parameters on a request.

For a deeper dive into how this works, please see this [blog post](https://alexeyzabelin.com/haskell-api-wrapper).  [stripe-haskell](https://github.com/dmjio/stripe/blob/master/README.md#optional-parameters) does this too.

Example:

```hs
main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  config <- sandboxEnvConfig "CIRCLE_API_KEY"
  result <- circle config manager listAllBalances -&- PaginationQueryParams (PageBefore "a8899b8e-782a-4526-b674-0efe1e04526d")
  case result of
    Right CircleResponseBody b -> print bs
    Left CircleError e -> print e
```

## Testing

This method wraps all 59 methods supported by the V1 API, and there's an integration test (it calls the API sandbox with my token) for each method.  

```text
Finished in 14.9400 seconds
59 examples, 0 failures, 5 pending
```

The subscription tests are skipped for now (since the sandbox doesn't let me delete subscriptions and has a maximum of 3 subscriptions).  The unit tests are pretty simple; they're just making sure that the method calls the correct endpoint and then correctly parses the response into the appropriate types.  There could definitely be more test coverage, and that's coming in future versions.  But I've verified that at least simple happy-paths work for each method!

## Issues

Any feature requests or bugs can be reported on the GitHub issue tracker. Pull requests welcome!  I hope anyone uses this, frankly.

## Inspiration

This project came out of tinkering with writing a Haskell client for a USDC API at my old job, [Mercury](https://mercury.com/).  We often had to hand-write our API clients before integrating them into our business logic (since most APIs don't have existing open-source wrappers in Haskell), and even though we never ended up implementing the project at work, and I figured it would be a fun project to write a first-class Haskell client that wraps Circle's API.  I've since quit my job there, but maybe down the line folks might find this project useful!

In terms of implementation, I was heavily inspired by the following libraries

- (my old coworker) Alexey Zabelin's [shipwire](https://www.shipwire.com/) API wrapper: [ballast](https://github.com/alexeyzab/ballast)
- David Johnson's [stripe](https://github.com/dmjio/stripe) API wrapper (from which I'm pretty sure Alexey drew his inspiration).

As with many things in software development, this project was made possible by the hard work of the folks I mentioned (and the other contributors to those projects) who came before.  I'm eternally grateful to all of you.  Thanks for letting my stand on your shoulders.