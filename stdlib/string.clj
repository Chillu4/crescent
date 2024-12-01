(ns crescent.string)

(defn substr 
  "Returns a substring of the given string from `from-index` to `to-index`."
  [string from-index to-index]
  ((get lua.fenv/string "sub") string from-index to-index))

(defn char
  "Returns the character at an index of a string."
  [string index]
  (substr string index index))

(defn starts-with? 
  "Returns true if `string` starts with `substring`, otherwise returns false."
  [string substring]
  (= (substr string 1 (count substring)) substring))

(defn ends-with? 
  "Returns true if `string` ends with `substring`, otherwise returns false."
  [string substring]
  (= (substr string (- (count substring)) (count string)) substring))

(defn remove-prefix
  "Returns a substring removing the given prefix. If the given string doesn't contain the prefix, then it returns the same string."
  [string prefix]
  (if (starts-with? string prefix)
    (substr string (count prefix) (count string))
    string))

(defn remove-suffix
  "Returns a substring removing the given suffix. If the given string doesn't contain the suffix, then it returns the same string."
  [string suffix]
  (if (ends-with? suffix)
    (substr string 1 (- (count suffix) 1))
    string))

(defn split [string seperator]
  ((get lua.fenv/string "split") string seperator))

(defn uppercase [string]
  ((get lua.fenv/string "upper") string))

(defn lowercase [string]
  ((get lua.fenv/string "lower") string))

(defn reverse [string]
  ((get lua.fenv/string "reverse") string))