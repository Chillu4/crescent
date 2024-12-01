(ns crescent.reader)

(defn ->reader [file-name src]
  {:src src
   :offset 0
   :char ""
   :line 1
   :column 0
   :file-name file-name})

(defn current-pos [reader]
  (let [line-no (:line reader)
        column (:column reader)
        offset (:offset reader)]
    (Position line-no column offset)))


