(ns bbe-clj.bbe
  (:require [bbe-clj.race :as race]
            [bbe-clj.order-book-v3 :as lob]
            [bbe-clj.bettor :as bettor]))

(defn make-bbe
  []
  (let [r (race/make-race)
        n (race/num-competitors r)]
    {:bettors (bettor/make-bettors n)
     :race r
     :lobs (into [] (take n
                          (repeat lob/make-lob)))}))
(defn step-bbe
  "Every 3 step/second we run a betting round
   Run simulation 100 times from current state
   Let each bettor create a decision
   Update lobs with descisions"
  [{:keys [bettors race lobs] :as bbe} n]
  (let [next-race (race/move-competitors race n)]
    (if
     (or (race/done? race)
         (not (zero? (mod n 3))))
      (assoc bbe :race next-race)

      (let [opinions (-> next-race (race/repeat-run 100) race/get-win-odds-each)
            decisions (map (fn [op] (op race lobs opinions)) bettors)
            next-lobs (reduce lob/execute-decision lobs decisions)]
        (-> bbe
            (assoc :race next-race)
            (assoc :lobs next-lobs))))))

(defn run
  "Run BBE until race is over"
  [bbe]
  (reduce (fn [b n]
            (if (race/done? (:race b))
              (reduced b)

              (step-bbe b n))) bbe (map inc (range))))

(comment
  (run (make-bbe))
  ;;
  )
