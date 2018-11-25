# EasyApp

The purpose of this library is to compile graph-based presentation
of a function to efficient code. 
In that graph, elementary operations are nodes, 
and their direct arguments/dependencies represented by edges.

Unlike with conventional dependency injection container, 
we introduce an automatic derivation of a value scope, 
which allows seamless incorporation of advanced code structures 
like loops and closures.

Please see [this article](https://medium.com/@eldargab/ioc-programming-framework-e792adc1cbfa) for detailed introductory rational.

## Examples

### Basic

```clj
(require '[ea.core :refer :all])

(def app (atom {}))

(define app :a 
  :fn (fn [] 1))

(define app :ab
  :args [:a :b]
  :fn (fn [a b]
        (+ a b)))
        
(define app :ac
  :args [:a :c]
  :fn (fn [a c]
        (+ a c)))
        
(define app :ab+ac
  :args [:ab :ac]
  :fn +)
  
(def ab+ac 
  (compile-app app      ; spec  
               :ab+ac   ; What we want to compute/run (main)
               [:b :c]  ; What we are ready to supply (seed arguments)
               ))
  
(ab+ac 1 2) ; => 5
```

`ab+ac` function generated above is literally equivalent to the following

```clj
(defn ab+ac [b c]
  (let [a (a-fn)
        ab (ab-fn a b)
        ac (ac-fn a c)]
    (+ ab ac)))   
```

### Multi-level (multi-scope) function

```clj
(define app :a
  :fn (fn [] 1))
  
(define app :ab
  :args [:a :b]
  :fn +)
  
(define-level app :ab-fn 
  :ab   ; main
  [:b]  ; seed arguments
  )

(define app :main
  :args [:ab-fn]
  :fn (fn [ab-fn]
        (+ (ab-fn 1) (ab-fn 2))))
  
(def main 
  (compile-app app :main []))
  
(main) ; => 5 (During `main` call `:a` was called only once)
```

## Credits

This work was initially inspired by [The-Kiln](https://github.com/straszheimjeffrey/The-Kiln)

## License

Copyright © 2018 Eldar Gabdullin

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
