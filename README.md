# unknot

> In the mathematical theory of knots, the unknot, or trivial knot, is the least knotted of all knots. Intuitively, the unknot is a closed loop of rope without a knot tied into it, unknotted. To a knot theorist, an unknot is any embedded topological circle in the 3-sphere that is ambient isotopic (that is, deformable) to a geometrically round circle, the standard unknot.

[source](https://en.wikipedia.org/wiki/Unknot)

I'm not knot theorist, but it seemed like as good a name as any for this library, which is an API Client for [Circle](https://developers.circle.com/)

Inspired by Alexey Zabalin's [ballast](https://github.com/alexeyzab/ballast) API client wrapper.  

You'll need to set the environment variable `CIRCLE_API_KEY` to test this library locally.  Get your keys [here](https://developers.circle.com/docs/api-keys).

Actually, what happens when you use this library in a different project?  Will it also require a `CIRCLE_API_KEY`?  What if the client wants to use a different variable?  Is there a way to export that control?
