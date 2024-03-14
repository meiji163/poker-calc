(ns poker-calc.core
  (:require [clojure.math.combinatorics :as combo]
            [clojure.core.async :as a]
            [poker-calc.util :refer [select-val in? first-key max-with concatv]])
  (:gen-class))

(def suits [:spade :club :diamond :heart])

(def full-deck
  (for [suit suits
        rank (range 2 15)]
    [rank suit]))

(defn rand-card []
  (let [rank (+ 2 (rand-int 12))
        suit (suits (rand-int 4))]
    [rank suit]))

(defn flush? [hand]
  (apply = (map second hand)))

(defn straight? [ranks]
  "highest card in straight, or nil if no straight."
  "`ranks` must be sorted desc"
  (cond
    ;; wheel
    (= [14 5 4 3 2] ranks)
    1

    (= [4 3 2 1 0] (map #(- % 2) ranks))
    (first ranks)

    :else nil))

(def hand-types
  {:hicard 0
   :pair 1
   :twopair 2
   :set 3
   :straight 4
   :flush 5
   :fullhouse 6
   :quads 7
   :strflush 8
   :rflush 9})

(defn classify-hand [hand]
  (let [ranks (->> hand
                   (map first)
                   sort
                   reverse
                   vec)
        rank-cnt (frequencies ranks)
        pairs (select-val rank-cnt 2)
        threes (select-val rank-cnt 3)
        quads (select-val rank-cnt 4)
        flush (flush? hand)
        straight (straight? ranks)]
    (cond
      ;; royal flush
      (and flush (= [10 11 12 13 14] ranks)) {:type :rflush}

      ;; straight flush
      (and flush straight) {:type :strflush
                            :card straight}

      ;; quads
      (not (empty? quads))
      {:type :quads
       :card (first-key quads)
       :kicker (first-key (select-val rank-cnt 1))}

      ;; fullhouse
      (and (not (empty? threes)) (not (empty? pairs)))
      {:type :fullhouse
       :card [(first-key threes) (first-key pairs)]}

      ;; flush
      flush {:type :flush
             :kicker ranks}

      ;; straight
      straight {:type :straight
                :card straight}

      ;; three of a kind / set
      (not (empty? threes))
      (let [r (first-key threes)
            kicker (vec
                    (filter #(not (= % r)) ranks))]
        {:type :set
         :card r
         :kicker kicker})

      ;; two pair
      (= 2 (count pairs))
      (let [cards (keys pairs)
            kicker (first
                    (filter #(not (in? cards %)) ranks))]
        {:type :twopair
         :card (vec (sort cards))
         :kicker kicker})

      ;; pair
      (= 1 (count pairs))
      (let [r (first-key pairs)
            kicker (vec
                    (filter #(not (= % r)) ranks))]
        {:type :pair
         :card r
         :kicker kicker})

      ;; high card
      :else {:type :hicard
             :kicker ranks})
    ))

(defn compare-types [t1 t2]
  (compare (hand-types t1) (hand-types t2)))

(defn compare-tie [h1 h2]
  "compare two hands of the same type"
  (let [cards-comp (compare (:card h1) (:card h2))]
    (if (not (= 0 cards-comp))
      cards-comp
      (compare (:kicker h1) (:kicker h2)))))

(defn compare-hands [h1 h2]
  "compare two hands after they are classified"
  (let [t1 (:type h1)
        t2 (:type h2)
        type-comp (compare-types t1 t2)]
    (if (not (= 0 type-comp))
      type-comp
      (compare-tie h1 h2))))

(defn best-hand
  ([hand-board]
   (let [hands ;; 7 choose 5 = 21 possible hands
         (map classify-hand (combo/combinations hand-board 5))]
     (max-with compare-hands hands)))
  ([hand board]
   (best-hand (concatv hand board))))

(defn play-sim [board & hands]
   (let [hand-board (concatv (concat hands) board)
         deck (filter #(not (in? hand-board %))
                      (shuffle full-deck))
         ;; draw rest of board
         n-draws (- 5 (count board))
         draws (into [] (take n-draws deck))
         sim-board (concatv board draws)
         results (map #(best-hand % sim-board) hands)]
     {:draws draws
      :result (if (= 1 (count results))
                (first results)
                results)}))

(defn run-n [n fn & args]
  (for [_ (range n)]
    (apply fn args)))

()

(let [runs (run-n 10000
                  play-sim
                  [[4 :diamond] [4 :spade] [14 :club]]
                  [[10 :diamond] [9 :diamond]])
      types (map #(get-in % [:result :type]) runs)]
  (frequencies types))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
