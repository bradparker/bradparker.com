---
title: You already know what Monads are
rss_guid: https://bradparker.com/content/posts/2017-01-06-you-already-know-what-monads-are.html
tags:
  - development
description: |
  I sought to understand what the fuss was all about and I've now added to the mass of Monad Explainer posts on the internet.

  ```javascript
  readAFile().then(function (content) {
    return writeAFile(content)
  }).then(function (report) {
    console.log(report)
  })
  ```
---

Assuming you're a javascript dev who's had to deal with anything aysncronous.

Firstly, we're going to talk about promises, so:

## What do we talk about when we talk about Promises?

It's very likely you've done something like this:

```javascript
doSomethingAsync(function (resultOfAsyncThing) {
  console.log(resultOfAsyncThing)
})
```

There's an interesting concept highlighted in that code above.

What if we tried to do the following:

```javascript
let resultOutsideCallback

doSomethingAsync(function (resultOfAsyncThing) {
  resultOutsideCallback = resultOfAsyncThing
})
```

How will we get to use `resultOutsideCallback`?

```javascript
let resultOutsideCallback

doSomethingAsync(function (resultOfAsyncThing) {
  resultOutsideCallback = resultOfAsyncThing
})

while(true) {
  if (resultOutsideCallback) {
    console.log(resultOutsideCallback)
    break
  }
}
```

That's... not awesome. Everything we try to execute after that while loop is blocked until the async function's callback is called. But if we want to work with that value "outside" of the callback, that's kind of how we'd have to do it.

So, it appears that the `resultOfAsyncThing` value is best left "inside" the callback. This is something that just kind of _happens_ when working with callbacks. A callback creates an (almost) inescapable context.

```javascript
someAsyncAction(function (a) {
  someOtherAsyncAction(function (b) {
    console.log(a + b)
  })
})
```

Promises are a way of turning this concept of "the eventual value of an async action" into an object we can interact with. The object is a container for this future value that grants you access to it through the `then` method. The `then` method is a way to get into the context in which the eventual value exists.

```javascript
someAsyncAction().then(function (resultOfAsyncAction) {
  console.log(resultOfAsyncAction)
})
```

Now, the example above doesn't really offer anything over using callbacks. But there is an aspect of promises that makes them very handy:

```javascript
someAsyncAction().then(function (value) {
  return value.toUppercase()
}).then(function (uppercaseValue) {
  return uppercaseValue.split('').reverse().join()
}).then(function (uppercaseAndReversedValue) {
  console.log(uppercaseAndReversed)
})
```

Each time we call `then` we get a new promise back. The new promise now "contains" the value returned by the function we passed in. So the above could be re-written as:

```javascript
const uppercased = someAsyncAction().then(function (value) {
  return value.toUppercase()
})

const uppercasedAndReversed = uppercased.then(function (uppercaseValue) {
  return uppercaseValue.split('').reverse().join()
})

uppercasedAndReversed.then(function (uppercasedAndReversedValue) {
  console.log(uppercasedAndReversedValue)
})
```

It looks a lot like performing a chain of transformations on an array to me:

```javascript
[1, 2, 3].map(function (value) {
  return value * 2
}).map(function (value) {
  return value + 2
})
```

## Ok, so what does this have to do with Monads?

Here we should highlight that `Array#map` is a lot like `Promise#then`. Where `then` is something you call on a container for a future value, `map` is something you call on a container for many values. They both kind of work the same way, you pass a function which will return a new value which goes back into the container.

```javascript
const a = Promise.resolve(1)
// Promise { 1 }
const b = a.then(function (a) { return a + 2 })
// Promise { 3 }

const as = [ 1, 1, 1 ]
// [ 1, 1, 1 ]
const bs = as.map(function (a) { return a + 2 })
// [ 3, 3, 3 ]
```

This concept of a container for a value which can have that value transformed by passing a function to the container has a name: Functor. That's a bit of a mouthful so let's pull it apart. Array is a functor because it has a `map` method which allows you to operate on and transform its contents. Promise is a functor because it has a `then` method which allows you to operate on and transform its eventual value.

The functions passed to `Array#map` and `Promise#then` both return the new contents of their respective containers. So you pass in a fuction which returns a "normal" value and when it's called that value will be wrapped up in a container for us.

```javascript
Promise.resolve(1).then(function (a) {
  return a + 2
//       ^^^^^
//       normal, un-wrapped value
})

```

This is not always what you want. Say, for example, that you need to perform two async actions in series, one will pass data to the next.

```javascript
readAFile().then(function (content) {
  return writeAFile(content)
}).then(function (report) {
  console.log(report)
})
```

See that? Interesting, right? You can return a new Promise from the function passed to `then`, when you do you get a new promise back, but it has the same contents as the one you returned.

```javascript
const a = Promise.resolve(1)
// Promise { 1 }

const b = Promise.resolve(2)
// Promise { 2 }

const c = a.then(function () {
  return b
})
// Promise { 2 }

c === b
// false
```

So you either return a "normal" value and it'll get wrapped up in a Promise for you, or you can return a Promise and its content will be the content of the new promise returned.

This second version of the function passed to `then`, where it accepts a normal value but returns a Promise has an array equivelant: `flatMap`.

```javascript
flatMap([1, 2, 3], function (elem) {
  return [elem, '-']
})
// [ 1, '-', 2, '-', 3, '-' ]
```

The function passed to `flatMap` takes a normal value but returns an Array. We don't get an array of arrays back because something happens to that returned value so that it results in a flattened array.

This extension to the idea of a functor, whereby the function passed to it returns a container of the same type has a name: Monad.

With the value-extracting and container-flattening parts of these operations abstracted away composing functions which return containers is made simpler. We can chain together async operations easily because the plumbing for “where did this value come from?” and “where should this new Promise go?” happens inside the then method. That’s what we get through good, composable abstractions.

***

This was intended as something of a practical introduction to the concept of a Monad, there is more to it than I’ve outlined here. If you’d like to dive deeper have a look at http://learnyouahaskell.com/a-fistful-of-monads. You may want to start from the begining of the book to gain some familiarity with Haskell.

[1] For more info on how promises perform their flattening: http://www.mattgreer.org/articles/promises-in-wicked-detail/

[2] A simple implementation of flatMap might look like: https://gist.github.com/bradparker/34c7bd8d627809333adcc3e02cd7f755
