(ns crescent.lua)

(defn lua-fn->fn [f]
  (let [debug-info (get (lua.fenv/debug) "info")
        fn-info [(debug-info f "a")]
        argc (nth fn-info 1)
        variadic? (nth fn-info 2)]
    (todo)))