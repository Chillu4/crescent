(ns crescent.set)

(defn set [& elements]
  (let [data (->> elements
                  (map #(vec $ true))
                  (->hmap))]
    (luau.fenv/setmetatable data 
                            (->table { "__type" "set" }))))

(defn contains? [set element]
  (or (get set element)
      false))

(defn add [set element]
  (assoc! set element true))

(defn remove [set element]
  (assoc! set element nil))