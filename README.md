# unknot

> In the mathematical theory of knots, the unknot, or trivial knot, is the least knotted of all knots. Intuitively, the unknot is a closed loop of rope without a knot tied into it, unknotted. To a knot theorist, an unknot is any embedded topological circle in the 3-sphere that is ambient isotopic (that is, deformable) to a geometrically round circle, the standard unknot.

[source](https://en.wikipedia.org/wiki/Unknot)

I'm no knot theorist, but it seemed like as good a name as any for this library, which is an API Client for [Circle](https://developers.circle.com/)

Inspiration:

* Alexey Zabelin's [shipwire](https://www.shipwire.com/) API wrapper: [ballast](https://github.com/alexeyzab/ballast)
* David Johnson's [stripe](https://github.com/dmjio/stripe) API wrapper.

You'll need to set the environment variable `CIRCLE_API_KEY` to test this library locally.  Get your keys [here](https://developers.circle.com/docs/api-keys).

## TODO

Since this project is still in dev, I'm including a todo list for the remaining things I want to finish.  I'll include completion of these items in commits + PR descriptions.

### Refactors + Ergonomic Changes

- [x] Use custom types as much as possible.  Use smart constructors to give better type-safety.
- [x] Split the code into more modules so it's easier to navigate around the various different bits.  Seems like better organization than one mega module.  Or, if you don't split it up, at _least_ put some comments breaking up different sections.

### Infrastructure
- [ ] Add CI pipeline to run tests
- [ ] Add some aeson utilities to reduce boilerplate + code reuse
- [ ] Maybe add TemplateHaskell and do some `deriveJSON` to get rid of all those hand-rolled derivations
- [ ] Look into sharing some request/response types if there's enough in common
* [ ] Investigate [autocodec](https://github.com/NorfairKing/autodocodec#readme) and see if it would help you reduce some boilerplate for your aeson derivations


### README and Documentation

- [ ] everything exported has a doc.  Include types!  I've been good about the functions.
- [ ] figure out how haddock works, see if I can make something nice.
- [ ] add some usage examples to the README.
- [ ] Add PR template and contribution guide

### Core Functionality

- [x] differentiate between query params and the request body.  Right now I don't think I do that.
- [x] Wrap API Overview endpoints (small)
- [x] Wrap Transfers endpoint (small/medium [lots of new data being returned from the wallet info, but once it's done the endpoints will be easy])
- [x] Wrap Addresses endpoint (medium)
- [x] Wrap Deposits endpoint (small/medium)
- [ ] Wrap Payments endpoint (large, complex.  Probably the most complex endpoint to wrap.  Also likely the most useful)
- [ ] Wrap On-chain payments endpoint (medium/large.  Probably can reuse some work from payments but might be hard to get working)
- [ ] Wrap Cards endpoint (large)
- [ ] Wrap Wires endpoint (may be medium or small depending on if it's the same as the other wire work but with a different route)
- [ ] Wrap ACH endpoint (medium/large)
- [ ] Wrap settlements endpoints (small/medium)
- [ ] Wrap chargebacks endpoint (medium)
- [ ] Wrap reversals endpoint (small)
- [ ] Wrap crypto payment intents API (medium/large)
- [ ] Wrap crypto payments API (small/medium)
- [ ] Wrap on-chain transfer API (medium)
- [ ] Wrap wallets endpoint (large)
- [ ] Differentiate between business account and bank account endpoints.  There's the same calls for different endpoints.  Will be able to reuse all the types and just switch the path, though, so this can be done last.
- [ ] BETA Wrap SEPA endpoint (medium)
- [x] Wrap Signet endpoints (this will be challenging to test bc they only work in prod)
- [x] BETA Wrap SEN endpoints (medium)
- [ ] Figure out a way to create balances for accounts via API to test payouts.  Right now all tests for payments/payouts/balances only test that the sad paths work when there's no money.

### Extra goodies

- [ ] Add support for showing the `X-Request-Id` on requests so that consumers of this library can take advantage of debugging their requests with the Circle API team
