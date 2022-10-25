# unknot

> In the mathematical theory of knots, the unknot, or trivial knot, is the least knotted of all knots. Intuitively, the unknot is a closed loop of rope without a knot tied into it, unknotted. To a knot theorist, an unknot is any embedded topological circle in the 3-sphere that is ambient isotopic (that is, deformable) to a geometrically round circle, the standard unknot.

[source](https://en.wikipedia.org/wiki/Unknot)

I'm not knot theorist, but it seemed like as good a name as any for this library, which is an API Client for [Circle](https://developers.circle.com/)

Inspired by Alexey Zabelin's [shipwire](https://www.shipwire.com/) API wrapper: [ballast](https://github.com/alexeyzab/ballast) and David Johnson's [stripe](https://github.com/dmjio/stripe) API wrapper.

You'll need to set the environment variable `CIRCLE_API_KEY` to test this library locally.  Get your keys [here](https://developers.circle.com/docs/api-keys).

## TODO

Since this project is still in dev, I'm including a todo list for the remaining things I want to finish.  I'll include completion of these items in commits + PR descriptions.

### Refactors + Ergonomic Changes

- [] Use custom types as much as possible.  Use smart constructors to give better type-safety.
- [] Split the code into more modules so it's easier to navigate around the various different bits.  Seems like better organization than one mega module.  Or, if you don't split it up, at _least_ put some comments breaking up different sections.
- [] Add CI pipeline to run tests
- [] Add some aeson utilities to reduce boilerplate + code reuse
- [] Look into sharing some request/response types if there's enough in common
- [] Add PR template and contribution guide

### Core Functionality

- [] Wrap API Overview endpoints (small)
- [] Wrap Transfers endpoint (small/medium [lots of new data being returned from the wallet info, but once it's done the endpoints will be easy])
- [] Wrap Addresses endpoint (medium)
- [] Wrap Deposits endpoint (small/medium)
- [] Wrap Payments endpoint (large, complex.  Probably the most complex endpoint to wrap.  Also likely the most useful)
- [] Wrap On-chain payments endpoint (medium/large.  Probably can reuse some work from payments but might be hard to get working)
- [] Wrap Cards endpoint (large)
- [] Wrap Wires endpoint (may be medium or small depending on if it's the same as the other wire work but with a different route)
- [] Wrap ACH endpoint (medium/large)
- [] Wrap Signet endpoints (this will be challenging to test bc they only work in prod)
- [] Wrap settlements endpoints (small/medium)
- [] Wrap chargebacks endpoint (medium)
- [] Wrap reversals endpoint (small)
- [] Wrap crypto payment intents API (medium/large)
- [] Wrap crypto payments API (small/medium)
- [] Wrap on-chain transfer API (medium)
- [] Wrap wallets endpoint (large)
- [] BETA Wrap SEPA endpoint (medium)
- [] BETA Wrap SEN endpoints (medium)
- [] Figure out a way to create balances for accounts via API to test payouts.  Right now all tests for payments/payouts/balances only test that the sad paths work when there's no money.
