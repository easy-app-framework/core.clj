# dar.container

Simple, composable dependency injection container for Clojure with
support for multiple runtime levels and async computations.

##Installation

Available via [Clojars](https://clojars.org/dar/container)

## Specification

Container is defined as a clojure map from keys to tasks,
i.e. instructions on how to compute corresponding values.
Tasks come in two forms:

  1. plain values (those known in advance)
  2. functions (computable values, which evaluated at runtime)

Below is an example of container defintion

```clojure
(def spec
  {:a {:value 1}
   :b {:value 2}
   :c {:args [:a :b]
       :fn (fn [a b]
             (+ a b))}})
```

Runtime instance of container is called `app`, although
this is an overloaded term and may refer to spec as well.
App is created with `start` function

```clojure
(def app (start spec))
```

Once an app is created you can evaluate tasks defined in container

```clojure
(evaluate app :a) ; => 1
(evaluate app :c) ; => 3
```

All results are cached on an app instance and each function is evaluated at most once.

###Runtime levels

App instance forms a runtime level. Levels might be named.

```clojure
(def app (start spec)) ; base level (has an :app name by default)
(def request (start app :request {})) ; next level instance, it shares spec with app and has a name :request
```

Now, each task might be marked as belonging to a particular level.
If so, we lookup entire app chain for a appropriate
instance and do computations exactly on that. So, for example, if we want
to share database connection between requests, we mark it as an `:app` level,
so even if evaluated on request instance it will always be computed on app,
thus shared between requests.

```clojure
(def spec
  {:db {:level :app
        :fn #(rand-int 10)}
   :response {:args [:db]
              :fn (fn [db] db)}})

(def app (start spec))
(def r1 (start app))
(def r2 (start app))

(evaluate r1 :response) ; => 5
(evaluate r2 :response) ; => 5
```

###Asynchrony

All computations performed by container are completely non-blocking.
It treats all return values as instances of
[dar.async.promise/IPromise](https://github.com/dar-clojure/async.promise/blob/master/src/dar/async/promise.clj)
protocol. Since `Object` and `nil` are already extended to be a special cases of promise,
you can perform sync computations seamlessly. However, in the case of async value, it will
wait for result and return undelivered promise accordingly.

###Cleanup

There is a notion of Closeable value.

```clojure
(def spec
  {:file {:close #(.close %) ; make a value closeable by providing cleanup function
          :fn (fn []
                (open "some-file.txt"))}})

(def app (start spec))
...
(stop! app) ; will close all closeable values of the current instance
```

###Spec API

There is a special API, that allows to define container in
clojure vars like manner.

```clojure
(application app)

(define :a 1)

(swap assoc :b {:value 2})

(define :c
  :args [:a :b]
  :fn +)

(evaluate (start app) :c) ; => 3
```

###Misc

All presented functions are defined in a single `dar.container` namespace.
Please refer to inline docs for complete list of options.


## License

Copyright © 2014 Eldar Gabdullin

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
