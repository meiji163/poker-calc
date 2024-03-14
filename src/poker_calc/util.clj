(ns poker-calc.util)

(defn select-val [m val]
  (select-keys m (for [[k v] m :when (= v val)] k)))

(defn in?
  [coll elm]
  (some #(= elm %) coll))

(def first-key (comp first keys))

(defn max-with [cmp-fn coll]
  "max by comparison function"
  (letfn [(f [v1 v2]
            (let [cmp (cmp-fn v1 v2)]
              (cond (= 0 cmp) v1
                    (< 0 cmp) v1
                    (> 0 cmp) v2)))]
    (reduce f coll)))

(defn concatv [& xs]
  (into [] (apply concat xs)))
