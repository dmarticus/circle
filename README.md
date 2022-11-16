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



## Local Development

You'll need to set the environment variable `CIRCLE_API_KEY` to test this library locally.  Get your keys [here](https://developers.circle.com/docs/api-keys).

## Inspiration

This project came out of tinkering with writing a Haskell client for a USDC API at my old job, [Mercury](https://mercury.com/).  We often had to hand-write our API clients before integrating them into our business logic (since most APIs don't have existing open-source wrappers in Haskell), and even though we never ended up implementing the project at work, and I figured it would be a fun project to write a first-class Haskell client that wraps Circle's API.  I've since quit my job there, but maybe down the line folks might find this project useful!

In terms of implementation, I was heavily inspired by the following libraries

- (my old coworker) Alexey Zabelin's [shipwire](https://www.shipwire.com/) API wrapper: [ballast](https://github.com/alexeyzab/ballast)
- David Johnson's [stripe](https://github.com/dmjio/stripe) API wrapper (from which I'm pretty sure Alexey drew his inspiration).

As with many things in software development, this project was made possible by the hard work of the folks I mentioned (and the other contributors to those projects) who came before.  I'm eternally grateful to all of you.  Thanks for letting my stand on your shoulders.