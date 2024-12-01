# Crescent

Crescent is a dynamic, general-purpose programming language inspired by the simplicity and elegance of Clojure. Designed
to run on the Luau VM, it offers seamless interoperability with existing Luau codebases while providing the flexibility of a
lisp syntax and its macro system.

> [!WARNING]
> The language is in the early stages of development, so the syntax and library functions are subject to change, and backward compatibility is not guaranteed. If you come across any bugs, feel free to open an issue.

# Requirements
Crescent requires [Lune](https://github.com/lune-org/lune), which is a standalone Luau runtime. Currently, Crescent has been
tested on Lune 0.8.9.

# Examples
```clojure
;; Functions can be defined by using the `defn` macro
(defn div-by-3-or-7? [x]
    (or (= (% x 3) 0) 
        (= (% x 7) 0)))

;; Print the first 100 numbers which are divisible by 3 or 7
(->> (range 1 inf)
     (filter div-by-3-or-7?)
     (take 100)
     (iter print))
``` 