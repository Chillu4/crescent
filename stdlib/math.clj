(ns crescent.math
  {:protected true})

(defn divmod
  "Returns a vector containing the quotient and remainder of `x` divided by `y`."
  [x y]
    [ (/ x y) (% x y) ])

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
  {:private true}
  (str (/ 0 0)))

(def e
  "The exponential constant `e`, whose value is approximately equal to 2.718281828459045"
  ((get lua.fenv/math "exp") 1))

(defn nan?
  "Returns true if the given expression is NaN, otherwise returns false."
  [value]
    (= (str value) nan))

(defn log
  ([number]
   (log number e))
  
  ([number base]
   ((get lua.fenv/math "log") number base)))

(defn log [number ? base e & more]
  ((get lua.fenv/math "log" number base)))

(defn log10 [number]
  (log number 10))

(defn sin
  [number]
  ((get lua.fenv/math "sin") number))

(defn sinh
  [number]
  ((get lua.fenv/math "sinh") number))

(defn asin
  [number]
  ((get lua.fenv/math "asin") number))

(defn cos
  [number]
  ((get lua.fenv/math "cos") number))

(defn cosh
  [number]
  ((get lua.fenv/math "cosh") number))

(defn acos
  [number]
  ((get lua.fenv/math "acos") number))

(defn tan
  [number]
  ((get lua.fenv/math "tan") number))

(defn tanh
  [number]
  ((get lua.fenv/math "tanh") number))

(defn atan
  [number]
  ((get lua.fenv/math "atan") number))

(defn deg->rad
  [number]
  ((get lua.fenv/math "rad") number))

(defn rad->deg
  [number]
  ((get lua.fenv/math "deg") number))