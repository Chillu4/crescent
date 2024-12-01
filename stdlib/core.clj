(ns cres.core)

;; Declare docs for special forms
(def if
  "A special form which evaluates `test`. When `test` is evaluated to logical true value (values other than false or nil),
   `consequent` is evaluated, otherwise `alternate` is evaluated."
  {:type "special"
   :args '[test consequent alternate]}
  nil)

(def quote
  "Returns the given value unevaluated. The reader macro 'expr (single quote) is expanded into (quote arg). 
   Examples:
       `(quote (1 2 3 4))` returns a list without evaluating it.
       `(quote foo)` returns the symbol 'foo' (an unevaluated symbol)."
  {:type "special"
   :args '[expr]}
  nil)

(def def
  "Creates a var and associates with the given symbol in the current namespace."
  {:type "special"
   :args '[[name] [name value] [name doc-str value] [name metadata value] [name doc-str metadata value]]}
  nil)

(def fn
  "Creates and returns a new anonymous function with the given parameters and body. `parameters` must be a vector 
   of symbols and `body` can be any expression. 
   
   Variadic parameters can be defined using the `&` symbol followed by a symbol where the extra arguments will be 
   stored in. Variadic parameters must be defined at the end of the parameters. The variadic parameter is a list
   containing the excess arguments.
   
   Example:
     1. `(fn [x] (* x 2))` returns an anonymous function which accepts one argument and returns the product of its argument by 2.
     2. `(fn [x y & z] ...)` returns an anonymous function which accepts two or more arguments. The excess arguments are collected
        in a list and is bounded to the `z` symbol."
  {:type "special"
   :args '[parameters & body]}
  nil)


(def let
  {:type "special"
   :args '[bindings & body]}
  nil)

(def letrec
  {:type "special"
   :args '[bindings & body]}
  nil)

;; TODO: define documentations for true, false and nil
;; #_(def true
;;   "A boolean constant which represents a truthy value."
;;   {:type "special"}
;;   nil)

;; #_(def false
;;   "A boolean constant which represents a falsy value."
;;   {:type "special"}
;;   nil)

;; #_(def nil
;;   "A constant which signifies the absence of a value."
;;   {:type "special"}
;;   nil)

(def do
  "Evaluates each expression sequentially and returns the result of the last evaluated expression. If no expressions are given, it returns nil."
  {:type "special"
   :args '[& exprs]}
  nil)

(def set!
  {:type "special"
   :args '[name new-value]}
  nil)

(def comment
  "Used for 'commenting' out expressions, i.e. given expressions are ignored during evaluation. The reader macro `#_expr` expands to `(comment expr)`"
  {:type "special"
   :args '[& exprs]}
  nil)

(def ns
  "Interns and enters a given namespace."
  {:type "special"
   :args '([name] [name doc-str])}
  nil)

(def *import-path*
  "A vector of strings of directories Crescent uses to look for namespaces when importing."
  [""])

;; Core functions 

(def type?
  "Returns true if type of `x` is `typ`, otherwise returns false."
  {:inline (fn [x typ] (list 'lua.op/== (list 'type x) typ))}
  (fn [x typ] (lua.op/== (type x) typ)))

(def vec?
  "Returns true if `x` is a vector, otherwise false"
  {:inline (fn [x] (list 'type? x :vector))}
  (fn [x] (type? x :vector)))

(def list?
  "Returns true if `x` is a list, otherwise false"
  {:inline (fn [x] (list 'type? x :list))} 
  (fn [x] (type? x :list)))

(def number?
  "Returns true if `x` is a number, otherwise false"
  {:inline (fn [x] (list 'type? x :number))}
  (fn [x] (type? x :number)))

(def boolean?
  "Returns true if `x` is a boolean, otherwise false"
  {:inline (fn [x] (list 'type? x :boolean))}
  (fn [x] (type? x :boolean)))

(def symbol?
  "Returns true if `x` is a symbol, otherwise false"
  {:inline (fn [x] (list 'type? x :symbol))}
  (fn [x] (type? x :symbol)))

(def str?
  "Returns true if `x` is a string, otherwise false"
  {:inline (fn [x] (list 'type? x :string))}
  (fn [x] (type? x :string)))

(def hmap?
  "Returns true if `x` is a hmap, otherwise false"
  {:inline (fn [x] (list 'type? x :hmap))}
  (fn [x] (type? x :hmap)))

(def nil?
  "Returns true if `x` is nil, otherwise false"
  {:inline (fn [x] (list 'lua.op/== x nil))}
  (fn [x] (lua.op/== x nil)))

(def fn?
  "Returns true if `x` is a fn, otherwise false"
  {:inline (fn [x] (list 'type? x :function))}
  (fn [x] (type? x :function)))

(def true?
  "Returns true if `x` is true, otherwise false"
  {:inline (fn [x] (list 'lua.op/== x true))}
  (fn [x] (lua.op/== x true)))

(def false?
  "Returns true if `x` is false, otherwise false"
  {:inline (fn [x] (list 'lua.op/== x false))}
  (fn [x] (lua.op/== x false)))

(def some?
  "Returns true if `x` is not nil, otherwise false"
  {:inline (fn [x] 
             (list 'lua.op/~= x nil))}
  (fn [x] (lua.op/~= x nil)))

(def get
  "Returns the value associated with the given key for a collection."
  {:inline (fn [coll key] 
             (list 'lua.op/. coll key))}
  (fn [coll key]
    (lua.op/. coll key)))

(def assoc!
  "Associates a new value with the specified key in a hmap, or with the specified index in case of a vector.
  If the given value is nil, the specified key or index is removed from the given collection."
  (fn [coll key value & more]
    (lua.op/.= coll key value)
    coll))

(def count
  "Returns the number of elements in a vector or a list, otherwise if a hmap is passed, returns the number of entries."
  {:inline (fn [coll] 
             (list 'lua.op/# coll))}
  (fn [coll]
    (lua.op/# coll)))

(def last
  "Returns the last element in a vector"
  {:inline (fn [vector] 
             (list 'get vector (list 'count vector)))} 
  (fn [vector] 
    (get vector (count vector))))

(def head
  "Returns the first element of a list"
  (fn [list]
    (if (lua.op/or (list? list) (type? list :lazy-cons))
      (get list "head")
      (lua.fenv/error (lua.op/.. "attempt to get head of " (lua.fenv/tostring (type list))) 2))))

(def tail
  "Returns a list without its first element"
  (fn [list]
    (if (lua.op/or (list? list) (type? list :lazy-cons))
      (lua.op/or (get list "tail") '())
      (lua.fenv/error (lua.op/.. "attempt to get tail of " (lua.fenv/tostring (type list))) 2))))

(def empty?
  "Returns true if the given collection is empty, otherwise returns false"
  (fn [collection]
    (if (lua.op/or (list? collection) (type? collection :lazy-cons))
      (lua.op/== collection '())
      (lua.op/== (count collection) 0))))

(def vec->lazy-cons 
  (fn [vec i]
    (if (lua.op/< (count vec) i)
      '()
      (lazy-cons (fn [] (get vec i))
                 (fn [] (vec->lazy-cons vec (lua.op/+ i 1)))))))

(def hmap->lazy-cons
  (fn [hmap next-key]
    (let [pair [(lua.fenv/next hmap next-key)]]
      (if (lua.op/== (lua.op/# pair) 0)
        '()
        (lazy-cons (fn [] pair)
                   (fn [] (hmap->lazy-cons hmap (get pair 1))))))))

(def ->lazy-cons
  (fn [coll]
    (if (type? coll :lazy-cons)
      coll
      (if (vec? coll)
        (vec->lazy-cons coll 1)
        (if (hmap? coll)
          (hmap->lazy-cons coll nil)
          (if (list? coll)
            coll
            (lua.fenv/error (lua.op/.. (lua.op/.. "type " (lua.fenv/tostring (type coll)))
                                       " cannot be converted into :lazy-cons"))))))))

(def iter
  "Applies function `f` to every element in `seq`, and returns nil."
  (fn [f seq] 
    (if (lua.op/== seq '())
      nil
      (let [seq* (->lazy-cons seq)]
        (f (head seq*))
        (recur f (tail seq*))))))

(def reduce
  (fn [f & seq]
    (let [reduce* (fn [f init seq]
                    (let [acc init]
                      (iter (fn [x]
                              (set! acc (f acc x)))
                            seq)
                      acc))
          size (count seq)]
      (if (lua.op/== size 1)
        (reduce* f (head (head seq)) (tail (head seq)))
        (reduce* f (head seq) (head (tail seq)))))))

(def str
  "Converts all arguments into a string. If there are more than one argument, returns the concatenation of all of them.
  If no arguments are given, returns an empty string."
  {:inline (fn [a & more]
             (if (empty? more)
               (list 'lua.fenv/tostring a)
               (reduce (fn [acc expr]
                         (list 'lua.op/.. acc (list 'lua.fenv/tostring expr)))
                       (list 'lua.fenv/tostring a)
                       more)))}
  (fn [a & more]
    (if (empty? more)
      (lua.fenv/tostring a)
      (recur (lua.op/.. (lua.fenv/tostring a) (lua.fenv/tostring (head more))) 
             (tail more)))))

(def error
  "Throws an error with the given message."
  {:inline (fn [& strings]
             (list 'lua.fenv/error (cons 'str strings) 2))}
  (fn [& strings] 
    (lua.fenv/error (apply str strings) 2)))

(def populated?
  "Returns true if the given collection is not empty, otherwise returns false"
  {:inline (fn [coll] 
             (list 'lua.op/not (list 'empty? coll)))}
  (fn [collection] 
    (lua.op/not (empty? collection))))

(def defn
  "Defines a function and assigns it to the given var. Same as (def name ?doc-str ?meta (fn args body*))"
  {:macro true}
  (fn [name & more]
    (let [current (head more)
          next! (fn []
                  (let [temp current]
                    (set! more (tail more))
                    (set! current (head more))
                    temp))
          name (if (symbol? name)
                 name
                 (error "name of a function should be a symbol"))
          doc-str (if (str? current)
                    (next!)
                    "")
          meta (if (hmap? current)
                 (next!)
                 {})
          args (if (vec? current)
                 (next!)
                 (error "expected list of arguments in a vector"))
          body (if (some? current)
                 more
                 (error "body of a fn cannot be empty"))]
      (cons 'def (cons name (cons doc-str (cons meta (list (cons 'fn (cons args body))))))))))

(defn defmacro
  "Defines a macro function and assigns it to the given var. Same as calling the `defn` macro but sets `:macro true` in 
  its metatags. Macros are called during read time and the call is replaced with its return value."
  {:macro true}
  [name & more]
    (let [current (head more)
          next! (fn [] 
                  (let [temp current] 
                    (set! more (tail more)) 
                    (set! current (head more)) 
                    temp))
          name (if (symbol? name)
                    name
                    (error "name of a function should be a symbol"))
          doc-str (if (str? current)
                      (next!)
                      "")
          meta (if (hmap? current)
                    (assoc! (next!) :macro true)
                    {:macro true})
          args (if (vec? current)
                    (next!)
                    (error "expected list of arguments in a vector"))
          body (if (some? current)
                    more
                    (error "body of a fn cannot be empty"))]
      (cons 'def (cons name (cons doc-str (cons meta (list (cons 'fn (cons args body)))))))))

(defmacro if-not
  "Same as (if (not cond) consq alt)"
  [cond consq alt]
    (list 'if cond alt consq))

(defmacro if-some
  "Same as (if (some? cond) consq alt)"
  [cond consq alt]
    (list 'if (list 'some? cond) consq alt))

(defmacro if-nil
  "Same as (if (nil? cond) consq alt)"
  [cond consq alt]
    (list 'if (list 'nil? cond) consq alt))

(defmacro when
  "If `cond` evaluates to a truthy value, executes all the expression sequentially and returns the result of the last 
  expression, otherwise returns nil."
  [cond & body]
    (list 'if cond (cons 'do body) nil))

(defmacro when-not
  "If `cond` evaluates to a falsy value, executes all the expression sequentially, otherwise returns nil."
  [cond & body]
    (list 'if cond nil (cons 'do body)))

(defmacro cond
  "Evaluates a series of test/expression pairs sequentually and returns the value of the first expression whose test 
  evaluates to true. If no test evaluates to a truthy value, returns nil."
  [test expr & clauses]
    (list 'if
          test
          expr
          (if (empty? clauses)
              nil
              (apply cond clauses))))

(defn name
  "Returns the name of a symbol or keyword."
  [obj]
    (lua.fenv/rawget obj "__name"))

(defn boolean
  "Converts the given argument into a boolean. If the argument is a truthy value, returns true, otherwise 
  returns false."
  {:inline (fn [value] 
             (list 'lua.op/not (list 'lua.op/not value)))}
  [value]
    (if value true false))

(defn str->number
  "Converts a string representation of a number into its numeric form. Returns nil if the string cannot be parsed as a valid number."
  {:inline (fn [x]
             (list 'lua.fenv/tonumber x))}
  [x]
  (lua.fenv/tonumber x))

(defn assert
  "Throws an error with the given message if `test` is evaluated to a falsy value, otherwise returns `test`."
  [test message]
    (if test 
      test
      (error message)))

(defn not
  "Returns true if the given argument evaluates to false, otherwise returns false."
  {:inline (fn [value]
             (list 'lua.op/not value))}
  [value]
  (if value false true))

(defn list-append!*
  {:hidden true}
  [lst element]
  (let [cell (cons element '())]
    (lua.fenv/rawset lst "tail" cell)
    cell))

(defn =
  "Returns true if all the arguments are structurally equal to each other, otherwise false"
  {:inline {1 (fn [a] 
                true)
            2 (fn [a b]
                (list 'lua.op/== a b))}}
  [a & more]
  (if (empty? more)
    true
    (if (lua.op/== a (head more))
      (recur (head more) (tail more))
      false)))

(defn not=
  "Returns true if all the arguments are structurally not equal to each other, otherwise false"
  {:inline {1 (fn [a]
                false) 
            2 (fn [a b] 
                (list 'lua.op/~= a b))}}
  [& exprs]
  (not (apply = exprs)))

(defn >
  "Returns true if each argument is greater than the next, otherwise returns false."
  {:inline {1 (fn [a]
                true) 
            2 (fn [a b] 
                (list 'lua.op/> a b))}}
  [a & more]
  (if (empty? more)
    true
    (if (lua.op/> a (head more))
      (recur (head more) (tail more))
      false)))

(defn <=
  "Returns true if each argument is less than or equal to the next, otherwise returns false."
  {:inline {1 (fn [a]
                true)
            2 (fn [a b]
                (list 'lua.op/<= a b))}}
  [a & more]
  (if (empty? more)
    true
    (if (lua.op/<= a (head more))
      (recur (head more) (tail more))
      false)))

(defn <
  "Returns true if each argument is less than the next, otherwise returns false."
  {:inline {1 (fn [a]
                true)
            2 (fn [a b]
                (list 'lua.op/< a b))}}
  [a & more]
  (if (empty? more)
    true
    (if (lua.op/< a (head more))
      (recur (head more) (tail more))
      false)))

(defn >=
  "Returns true if each argument is greater than or equal to the next, otherwise returns false."
  {:inline {1 (fn [a]
                true)
            2 (fn [a b]
                (list 'lua.op/>= a b))}}
  [a & more]
  (if (empty? more)
    true
    (if (lua.op/>= a (head more))
      (recur (head more) (tail more))
      false)))

(defn and
  "Evaluates each argument sequentially and returns the last argument if all the arguments are a truthy value, 
  otherwise returns the first falsy value." 
  {:inline (fn [a & more]
             (if (empty? more)
               a
               (reduce (fn [acc expr] (list 'lua.op/and acc expr)) a more)))}
  [x & more]
  (if (lua.op/and x (populated? more))
    (recur (head more) (tail more))
    x))

(defn or
  "Evaluates each argument sequentially and returns the first argument that is a truthy value, otherwise returns 
  the last argument."
  {:inline (fn [a & more]
             (if (empty? more)
               a
               (reduce (fn [acc expr] (list 'lua.op/or acc expr)) a more)))}
  [x & more]
  (if (lua.op/or x (empty? more))
    x
    (recur (head more) (tail more))))

(defn +
  "Returns the sum of all the given numbers. If only one argument is applied, returns that argument, otherwise
  returns the sum of all the arguments."
  {:inline (fn [a & more]
             (if (empty? more)
               a
               (reduce (fn [acc expr] (list 'lua.op/+ acc expr)) a more)))}
  [a & more]
  (if (empty? more)
    a
    (recur (lua.op/+ a (head more)) (tail more))))

(defn -
  "Returns the difference of all the given numbers. If only one argument is applied, returns the negation of that 
  argument, otherwise returns the difference of all the arguments."
  {:inline (fn [a & more]
             (if (empty? more)
               (list 'lua.op/- a)
               (reduce (fn [acc expr] (list 'lua.op/- acc expr)) a more)))}
  [a & more]
  (if (empty? more)
    a
    (recur (lua.op/- a (head more)) (tail more))))

(defn *
  "Returns the product of all the given numbers. If only one argument is applied, returns that argument, otherwise
  returns the product of all the arguments."
  {:inline (fn [a & more]
             (if (empty? more)
               a
               (reduce (fn [acc expr] (list 'lua.op/* acc expr)) a more)))}
  [a & more]
  (if (empty? more)
    a
    (recur (lua.op/* a (head more)) (tail more))))

(defn /
  "Returns the division of all the given numbers. If only one argument is applied, returns the inverse of the argument, 
  otherwise returns the division of all the arguments."
  {:inline (fn [a & more]
             (if (empty? more)
               (list 'lua.op// 1 a)
               (reduce (fn [acc expr]
                         (list 'lua.op// acc expr)) 
                       a 
                       more)))}
  [a & more]
  (if (empty? more)
    (lua.op// 1 a)
    (let [aux (fn [a & more]
                (if (empty? more)
                  a
                  (recur (lua.op// a (head more)) (tail more))))]
      (aux a more))))

(defn %
  "Returns the remainder from dividing the first argument by the second."
  {:inline (fn [x y] 
             (list 'lua.op/% x y))}
  [x y]
    (lua.op/% x y))

(defn divmod
  "Returns a vector containing the quotient and remainder of `x` divided by `y`."
  [x y]
    [ (/ x y) (% x y) ])

(defn pow
  "Returns the result of raising `base` to the power of `exponent`."
  {:inline (fn [x y] 
             (list 'lua.op/^ x y))}
  [base exponent]
    (lua.op/^ base exponent))

(defn floor
  "Returns the largest integer less than or equal to the given number."
  [num]
    ((lua.op/. lua.fenv/math "floor") num))

(defn ceil
  "Returns the smallest integer greater than or equal to the given number."
  [num]
    ((lua.op/. lua.fenv/math "ceil") num))

(defn round
  "Rounds the given number to the nearest `nth` decimal places."
  [num nth]
    (let [factor (pow 10 nth)]
      (/ ((lua.op/. lua.fenv/math "round") (* num factor)) factor)))

(defn zero?
  "Returns true if the given number is zero, otherwise returns false."
  {:inline (fn [num]
             (list '= num 0))}
  [num]
  (= num 0))

(defn odd?
  "Returns true if the given number is odd, otherwise returns false."
  {:inline (fn [num] 
             (list 'and
                   (list 'not (list 'zero? num))
                   (list '= (list '% num 2) 1)))}
  [num]
    (and (not (zero? num))
         (= (% num 2) 1)))

(defn even?
  "Returns true if the given number is even, otherwise returns false."
  {:inline (fn [num]
             (list 'and 
                   (list 'not (list 'zero? num))
                   (list '= (list '% num 2) 0)))}
  [num]
  (and (not (zero? num)) 
       (= (% num 2) 0)))

(defn pos?
  "Returns true if the given number is positive (i.e. greater than zero), otherwise returns false."
  {:inline (fn [num]
             (list '> num 0))}
  [num]
  (> num 0))

(defn neg?
  "Returns true if the given number is negative (i.e. less than zero), otherwise returns false."
  {:inline (fn [num]
             (list '< num 0))}
  [num]
  (< num 0))

(defn sign
  "Returns -1 if the given number is negative, 1 if it's positive, otherwise returns 0 if it is zero or NaN."
  [num]
    (cond
      (pos? num) 1
      (neg? num) -1
      :else 0))

(def inf 
  "Returns a value larger than or equal to any other numerical value. The value is same as Lua's `math.huge`."
  (get lua.fenv/math "huge"))

(def -inf
  "Returns a value lesser than or equal to any other numerical value. Same as (- inf)."
  (- inf))

(def pi
  "The value of pi. Same as Lua's `math.pi`"
  (get lua.fenv/math "pi"))

(def pi
  "The value of pi. Same as Lua's `math.pi`"
  (get lua.fenv/math "pi"))

(def -pi
  "Same as (- pi)."
  (- pi))

(def nan
  (/ 0 0))

(defn nan?
  "Returns true if the given expression is NaN, otherwise returns false."
  [value]
    (= value nan))

(defn inc
  "Returns the increment of the given argument by 1. Same as (+ x 1)"
  {:inline (fn [v] 
             (list '+ v 1))}
  [v]
    (+ v 1))

(defn dec
  "Returns the decrement of the given argument by 1. Same as (- x 1)"
  {:inline (fn [v]
             (list '- v 1))}
  [v]
  (- v 1))

(defn nth
  "Returns the element of a sequence at the given index. If no elements exist at that index, returns nil.
  Throws an error if the first argument is not a valid sequence."
  [seq index]
  (let [seq-type (type seq)]
    (cond
      (= seq-type :vector) 
      (get seq index)
      
      (or (= seq-type :list) 
          (= seq-type :lazy-cons))
      (let [aux (fn [seq index]
                  (cond
                    (empty? seq) nil
                    (= index 1) (head seq)
                    :else (recur (tail seq) (- index 1))))]
        (aux seq index))
      
      :else (error "cannot use `nth` on type " seq-type))))

(defn str->bool
  "Returns true if the content of the given string is \"true\", returns false if the content of the string is \"false\", 
  otherwise returns nil."
  [string]
    (cond
      (= string "true") true
      (= string "false") false))

(defn append! [coll element]
  (if (not (vec? coll))
    (error "cannot append! on type " (type coll))
    (let [lua-insert (get lua.fenv/table "insert")]
      (lua-insert coll element)
      coll)))

;; (defn append [coll element]
;;   (case (type coll)
;;     :list (letrec [aux (fn [cell]
;;                       (if (empty? cell)
;;                         (cons element '())
;;                         (cons (head cell) (aux (tail cell)))))]
;;             (aux coll))
;;     :vector (let [cloned-vec ((get lua.fenv/table "clone") coll)]
;;               (append! cloned-vec element))
;;     else (error "cannot append on type " (type coll))))

;; Define functions for sequences

;; For performance reasons, the "main" function type convert the given collection into :lazy-cons
;; and the inner function will be the actual operation.
(defn repeat [x]
  (lazy-cons (fn [] x)
             (fn [] (repeat x))))

(defn iterate [f init]
  (lazy-cons (fn [] init)
             (fn [] (iterate f (f init)))))

(defn take [amount coll]
  (letrec [aux (fn [amount seq]
              (if (and (not (empty? seq)) (pos? amount))
                (lazy-cons (fn [] (head seq))
                           (fn [] (aux (dec amount) (tail seq))))
                '()))]
    (aux amount (->lazy-cons coll))))

(defn take-while [predicate coll]
  (letrec [aux (fn [f seq]
                 (if (and (not (empty? seq)) (f (head seq)))
                   (lazy-cons (fn [] (head seq))
                              (fn [] (aux f (tail seq))))
                   '()))]
    (aux predicate (->lazy-cons coll))))

(defn drop [amount coll]
  (let [seq (->lazy-cons coll)]
    (if (and (not (empty? seq)) (pos? amount))
      (recur (dec amount) (tail seq))
      seq)))

(defn drop-while [predicate coll]
  (let [seq (->lazy-cons coll)]
    (if (and (not (empty? seq)) (predicate (head seq)))
      (recur predicate (tail seq))
      seq)))

(defn cycle [coll]
  (letrec [root (->lazy-cons coll)
           aux (fn [cell]
                 (lazy-cons (fn [] (head cell))
                            (fn [] (let [next (tail cell)]
                                     (aux (if (empty? next) 
                                            root 
                                            next))))))]
    (aux root)))

(defn iter-indexed
  "Applies function `f` to every element in the given sequence along with its index, and returns nil. 
    The index and the element is passed to the function in that order."
  [f coll] 
  (if (hmap? coll)
    (iter f coll)
    (letrec [aux (fn [index seq]
                   (if (empty? seq)
                     '()
                     (do
                       (f index (head seq))
                       (aux (inc index) (tail seq)))))]
      (aux 1 (->lazy-cons coll)))))

(defn map
  "Applies function `f` to each element in the given collection."
  [f coll]
  (letrec [aux (fn [seq]
                 (if (empty? seq)
                   '()
                   (lazy-cons (fn [] (f (head seq)))
                              (fn [] (aux (tail seq))))))] 
    (aux (->lazy-cons coll))))

(defn map-indexed
  "Applies function `f` to every element in the given collection and replaces the element with the result of the 
  function. The index and the element are passed as arguments to the function in that order. "
  [f coll]
  (letrec [aux (fn [i seq]
                 (if (empty? seq)
                   '()
                   (lazy-cons (fn [] (f i (head seq)))
                              (fn [] (aux (inc i) (tail seq))))))]
    (aux 1 (->lazy-cons coll))))

(defn filter [predicate coll]
  (letrec [aux (fn [seq]
                 (cond
                   (empty? seq) '()
                   (not (predicate (head seq))) (aux (tail seq))
                   :else (lazy-cons (fn [] (head seq))
                                    (fn [] (aux (tail seq))))))]
    (aux (->lazy-cons coll))))

(defn filter-indexed [predicate coll]
  (letrec [aux (fn [i seq]
                 (cond
                   (empty? seq) '()
                   (not (predicate i (head seq))) (aux (inc i) (tail seq))
                   :else (lazy-cons (fn [] (head seq))
                                    (fn [] (aux (inc i) (tail seq))))))]
    (if (hmap? coll)
      (filter predicate coll)
      (aux 1 (->lazy-cons coll)))))

(defn range [& args]
  (let [len (count args)]
    (cond
      (= len 0) (iterate inc 1)
      (= len 1) (let [stop (head args)]
                  (take stop (iterate inc 1)))
      (= len 2) (let [start (head args)
                      stop (head (tail args))
                      length (inc (- stop start))]
                  (take length (iterate inc start)))
      (= len 3) (let [start (head args)
                      stop (head (tail args))
                      step (head (tail (tail args)))
                      length (inc (floor (/ (- stop start) step)))]
                  (take length
                        (iterate (fn [x] (+ x step)) start)))
      :else (error "fn expects 1..3 arguments but received " len))))

(defn all? 
  "Returns true if `predicate` returns true for all elements of `coll`, otherwise
   returns false. Returns true if `coll` is empty."
  [predicate coll]
  (letrec [all?* (fn [predicate seq]
                   (cond
                     (empty? seq) true
                     (predicate (head seq)) (recur predicate (tail seq))
                     :else false))]
    (all?* predicate (->lazy-cons coll))))

(defn any? 
  "Returns true if `predicate` returns true on some element of `coll`, otherwise 
   returns false. Returns false if `coll` is empty."
  [predicate coll]
  (letrec [any?* (fn [predicate seq]
                   (cond
                     (empty? seq) false
                     (predicate (head seq)) true
                     :else (recur predicate (tail seq))))]
    (any?* predicate (->lazy-cons coll))))

(defn foldr [f init coll]
  (letrec [aux (fn [f acc seq]
              (if (empty? seq)
                acc
                (f (aux f acc (tail seq)) (head seq))))]
    (aux f init (->lazy-cons coll))))

(defn ->vec [coll]
  (let [acc []
        seq (->lazy-cons coll)]
    (iter (fn [x]
            (append! acc x))
          seq)
    acc))

(defn ->list [coll]
  (if (list? coll)
    coll
    (letrec [seq (->lazy-cons coll)
             aux (fn [seq]
                   (let [tail (tail seq)]
                     (if (empty? tail)
                       (cons (head seq) '())
                       (cons (head seq) (aux tail)))))]
      (aux seq))))

(defn ->hmap [coll]
  (let [seq (->lazy-cons coll)
        acc {}]
    (iter (fn [pair]
            (let [key (get pair 1)
                  value (get pair 2)]
              (assoc! acc key value)))
          seq)
    acc))

(defn ->table [coll]
  (let [seq (->lazy-cons coll)
        acc (table)]
    (iter (fn [pair]
            (let [key (get pair 1)
                  value (get pair 2)]
              (assoc! acc key value)))
          seq)
    acc))

(defmacro import 
  "Imports one or more namespaces into the current namespace. All imported identifiers are namespace qualified by default, i.e. you must prefix the
     namespace's name before the identifier with a slash (for example, crescent.core/map). 
     
     The form accepts an optional `opts` hmap which can be used to control on how the import is done. The `opts` can contain the following keys:
     - `:expose` => A vector of symbols which can be accessed without qualifying it with the namespace. Use `:all` to expose every symbol.
     - `:as` => Imports the namespace with the given alternate symbol.
     - `:except` => A vector of symbols which are not to be imported from the given namespace.
     
     Example:
     1) Importing a single namespace
     `=> (import foo.bar)`
     
     2) Importing a namespace with the `opts` hmap
     `=> (import foo.bar {:expose [baz]})`
     
     3) Importing multiple namespaces
     `=> (import foo.bar ;; No need to specify an empty hmap as when it sees a symbol, it assumes to be the next namespace to be imported
                 cat.dog {:expose [food]})`"
  [& args]
  (let [quoted (->list
                (map (fn [x]
                       (list 'quote x))
                     args))]
    (cons 'cres.core/import* quoted)))

(defmacro case
  "Evaluates a test expression and matches its value against a series of case branches. 
  Returns the result of the first case branch which is equal to the value of the test expression. 
  If an 'else' symbol is encountered as one of the predicates, the value of that branch is returned if the branches 
  above it did not match, otherwise returns nil."
  [expr & predicates]
    (let [sym (gensym)
          body (->list
                (map-indexed (fn [i x]
                               (if (= (% i 2) 1)
                                 (cond
                                   (= x 'else) true
                                  ;;  (list? x) (cons 'or
                                  ;;                  (->list
                                  ;;                   (map (fn [datum]
                                  ;;                          (list '= sym datum))
                                  ;;                        x)))
                                   :else (list '= sym x))
                                 x)) predicates))]
      (list 'let [sym expr] (cons 'cond body))))

(defn list-insert
  "Returns a new list where the given value is inserted at the `nth` position."
  [lst element nth]
    (if (= nth 1)
        (cons element lst)
        (letrec [aux (fn [cell i]
                        (if (or (empty? cell) (= i 1))
                            (cons element cell)
                            (cons (head cell) (aux (tail cell) (- i 1)))))]
          (aux lst nth))))

(defn vector-insert!
  "Inserts the given value at the `nth` index of the given vector and returns the same vector."
  [vec element nth]
    ((lua.op/. lua.fenv/table "insert") vec element nth)
    vec)

(defn vector-insert
  "Returns a new vector where the given value is inserted at the `nth` index."
  [vec element nth]
    (let [copied []]
      (iter (fn [x] (append! copied x)) vec)
      ((lua.op/. lua.fenv/table "insert") copied element nth)
      copied))

(defn insert
  "Returns a new sequence of the same type where the given value is inserted at the `nth` position."
  [seq element index]
    ((case (type seq)
      :vector vector-insert
      :list list-insert
      else (error "could not insert in " (type seq))) seq element index))

(defn insert!
  "Inserts the given value at the `nth` position of the given sequence and returns the same sequence."
  [seq element index]
    (if (vec? seq)
        (vector-insert! element index)
        (error "cannot insert! for " (type seq))))

(defmacro ->?> [where init & forms]
  (reduce (fn [acc expr] 
            (list-insert expr acc where)) init forms))

(defmacro -> [init & forms]
  (apply ->?> 2 init forms))

(defmacro ->> [init & forms]
  (apply ->?> inf init forms))

(defn list-concat* [list-a list-b]
  (letrec [aux (fn [lst]
                 (if (empty? lst)
                   (if-not (list? list-b)
                     (list list-b)
                     list-b)
                   (cons (head lst) (aux (tail lst)))))]
          (aux list-a)))

(defn list-concat [a & lists]
  (reduce list-concat* a lists))

;; Define syntax-quote
(defn resolve
  "Takes a symbol without a namespace attached to it and returns a new symbol of the same name qualified with the current namespace."
  [symbol]
  (if true
    symbol
    (sym (str *ns* "/" (name symbol)))))

(def sq-gensym-map
  {:hidden true}
  {})

(defn syntax-quote*
  {:hidden true}
  [datum]
  (case (type datum)
    :symbol (list 'quote
                  (let [sym-name (name datum)]
                    (if (= ((get lua.fenv/string "sub") sym-name (count sym-name) (count sym-name)) "#")
                      (let [entry (get sq-gensym-map sym-name)]
                        (if entry
                          entry
                          (let [gensymed (gensym sym-name)]
                            (assoc! sq-gensym-map sym-name gensymed)
                            gensymed)))
                      (resolve datum))))

    :vector (->vec (map syntax-quote* datum))

    :hmap  (->hmap (map
                    (fn [pair]
                      [(syntax-quote* (get pair 1)) (syntax-quote* (get pair 2))])
                    datum))

    :list (if-not (empty? datum)
            (case (head datum)
              'unquote (head (tail datum))
              'unquote-splicing (error "cannot unquote-splice on a non-list")
              else (cons 'list-concat (->list
                                       (map (fn [x]
                                              (if (and
                                                   (list? x)
                                                   (populated? x)
                                                   (= (head x) 'unquote-splicing))
                                                (head (tail x))
                                                (list 'list (syntax-quote* x)))) datum))))
            (list))
    
    else datum))

(defmacro syntax-quote [x]
  ((get lua.fenv/table "clear") sq-gensym-map)
  (syntax-quote* x))

(defmacro loop
  [bindings & body]
  (let [args (->list (filter-indexed (fn [i _] (odd? i)) bindings))
        values (->list (filter-indexed (fn [i _] (even? i)) bindings))]
    (cons (cons 'fn (cons args body)) values)))

(defn identity
  "Returns the given argument."
  [x] x)

(defn constantly [x]
  "Returns a function which returns `x`."
  (fn [& _] x))

(defn partial [f & args]
  "Accepts a function `f` along with some arguments `args` and returns a new function which applys `args` and the extra arguments to the function `f`"
  (fn [& args2]
    (apply f (list-concat args args2))))

(defmacro get-in [coll & keys]
  `(-> ,coll ,@(map (fn [x] (list 'get x)) keys)))

(defn print
  "Prints the given arguments to the standard output."
  [& values]
  (let [transformed-values (->> values
                                (map (fn [value]
                                       (if (str? value)
                                         (str "\"" value "\"")
                                         (str value))))
                                (->list))]
    (apply lua.fenv/print transformed-values)))

(defmacro short-fn
  "The macro provides a short way to create an anonymous function via the reader macro `#()`.
   Arguments can be implicitly mentioned using a symbol whose name begins with $ followed by the argument number.
   For example:
   - `$1` or `$` refers to the first argument
   - `$2` refers to the second argument, and so on.

   The symbol `$&` is used to refer to the rest of the arguments as a list.
   The macro returns a functions with takes the specified arguments.
   
   Example:
   - `#(* $ 2)` is same as `(fn [x] (* x 2))`
   - `#(apply some-fn $&)` is same as `(fn [& xs] (apply some-fn xs))`"
  [datum]
  (if-not (list? datum)
    (error "datum must be a list")
    (letrec [arg-data {:arg-count 0
                       :varargs? false}

             make-body (fn [x]
                         (case (type x)
                           :list (->> x
                                      (map make-body)
                                      (->list))
                           
                           :vector (->> x
                                        (map make-body)
                                        (->vec))
                           
                           :hmap (->> x
                                      (map (fn [pair]
                                             [(make-body (get pair 1))
                                              (make-body (get pair 2))]))
                                      (->hmap))
                           
                           :symbol (let [sym-name (name x)]
                                     (if (= ((get lua.fenv/string "sub") sym-name 1 1) "$")
                                       (let [arg-type ((get lua.fenv/string "sub") sym-name 2)]
                                         (if (= arg-type "&")
                                           (assoc! arg-data :varargs? true)
                                           (let [n (if (= arg-type "")
                                                     1
                                                     (str->number arg-type))]
                                             (if (and (some? n)
                                                      (> n (get arg-data :arg-count)))
                                               (assoc! arg-data :arg-count n)
                                               nil)))
                                         (sym (str sym-name "#")))
                                       x))
                           else x))

             body (->> datum
                       (map make-body)
                       (->list))

             args (->> (range (get arg-data :arg-count))
                       (map (fn [x] (str "$" x "#")))
                       (->vec))]

            (when (get arg-data :varargs?)
              (append! args (sym "&"))
              (append! args (sym "$&#"))) 
            
            (let [form `(fn ,args ,body)]
              ;; Quick hack to evaluate the inner syntax-quoted form, as `eval` isn't available right now. 
              (apply list-concat (tail form))
              form))))

(defmacro defrecord [record-name fields]
  (assert (symbol? record-name) "name of a record should be a symbol")
  (assert (vec? fields) "fields of a record should be defined in a vector of symbols")
  (assert (all? symbol? fields) "fields of a record should be defined in a vector of symbols")

  (let [record-type (str *ns* "/" record-name)]
    `(defn ,record-name ,fields
       (let [record-mt (table
                        "__metatable" (table "__type" ,record-type)
                        "__index" (fn [self index]
                                    (if
                                                                        ;; Construct a list of (or (= index :field1) (= index :field2) ...)
                                     ,(->> fields
                                           (map (fn [field]
                                                  (keyword (name field))))
                                           (map (partial list '= 'index))
                                           (->list)
                                           (cons 'or))
                                      (lua.fenv/rawget index)
                                      (error ,(str "attempt to index record " record-type " with ") index)))

                        "__newindex" (fn [self index _value]
                                       (error "attempt to set invalid field " index ,(str " in record " record-type))))]
         (lua.fenv/setmetatable
                 ;; Construct a table in the form {:field field ...}
          ,(->> fields
                (map (fn [field]
                       [(keyword (name field)) field]))
                (->list)
                (list '->table))
           record-mt)))))