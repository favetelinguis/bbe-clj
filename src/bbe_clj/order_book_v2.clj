(ns bbe-clj.order-book-v2)

;;; Order-book is a map price->level where price is a float and level
;;; is [back lay matched] back/lay/matched are cells
;;; a cell contains bets. In the context of an order book a bet
;;; must have :size :price :side where side is :back|:lay
;;;
;;; TODO
;;; Currently a lay/back is only matched on the same level that is not how it works
;;;     A lay bet lower then the lowest current lay bet will just get added to size
;;;     A laybe higher then the highest laybet will be matched at the lowest back bet
;;;     A back bet higher then the highest current back bet will just get added to size
;;;     A back bet lower then the lowest current back bet will be matched against the highest lay bet
;;;
;;; OrderStatus will have to account for fill level something like
;;; :order-status [[price size] [price size]] getting added to bet
;; Dont fill up the whole book just have the levels where there is back/lay size and remove a level when all size is gone
;;; Dont keep matched at each level just have one matched in book and concat all matched there makes it simpler to remove a level
;;; When creating a bet check contains on set of all prices and trow exception if not there.
;;; Add support for market bets where one will keep filling will produce multiple order status if it fills over multiple levels
;;; Add support for fill-or-kill

(def price float)
(def level [[] [] []])
(def order-book
  (into (sorted-map-by >)
        (reduce (fn [acc x] (conj acc [(price x) level]))
                []
                (concat (range 1.01 2.01 0.01)
                        (range 2 3.02 0.02)
                        (range 3 4.05 0.05)
                        (range 4 6.1 0.1)
                        (range 6 10.2 0.2)
                        (range 10 20.5 0.5)
                        (range 20 31 1)
                        (range 30 52 2)
                        (range 50 105 5)
                        (range 100 1010 10)))))
(defn- order-book->level
  [order-book price]
  (get order-book price))

(defn- back?
  [side]
  (= side :back))

(defn- lay?
  [side]
  (= side :lay))

(defn- bet
  "TODO add validation that odds is a valid odds at betfair"
  [side competitor-id bettor-id p size]
  {:competitor-id competitor-id
   :bettor-id bettor-id
   :bet-id (random-uuid)
   :side side
   :price (price p)
   :size size})

(def lay (partial bet :lay))
(def back (partial bet :back))

(def matched 2)

(defn- size
  [side]
  (if (back? side) 0 1))

(defn- liquidity
  [side]
  (if (back? side) 1 0))

(defn- get-size
  [level side]
  (nth level (size side)))

(defn- get-liquidity
  [level side]
  (nth level (liquidity side)))

(defn add-size
  "Size is always added last
  For back add size to back side
  For lay add size to lay side"
  [level {:keys [side] :as bet}]
  (update level (size side) conj bet))

(defn add-liquidity
  "Liquidity is always added last
  For back add size to lay side
  For lay add size to back side"
  [level {:keys [side] :as bet}]
  (update level (liquidity side) conj bet))

(defn add-matched
  [level bet]
  (update level matched conj bet))

(defn- plan
  [level instructions side]
  (-> {}
      (assoc :level level)
      (assoc :side side)
      (assoc :instructions instructions)))

(defn- set-result
  [plan result]
  (assoc plan :result result))

(defn create-match-plan
  " instructions - 1 is how many needed for bet
    first in instruction is what need to be done
        neg? first split last liquidity abs first is what remains of liquidity
        pos? split bet first is what remains in bet
        zero? no split
  each entry in instruction is the size left on bet after each liquidity item, can be negative"
  [level side]
  (let [b (first (get-size level side))
        bs (get-liquidity level side)
        instructions (reduce (fn [acc curr-s]
                               (let [prev-s (-> acc first)
                                     y (- prev-s (:size curr-s))]
                                 (if (pos? y)
                                   (conj acc y)
                                   (reduced (conj acc y))))) (list (:size b)) bs)]
    (plan level instructions side)))

(defn bet->matched+unmatched
  "take something that has size and splits into matched+unmatched
   unmatched can be nil if fully matched
  if size2 is negative it mean unmatched size
  if sizes is posetive it mean matched size
   matched can be nil if size is bigger negative or match is bigger then size"
  [{:keys [size] :as bet} size2]
  (cond
    (or (> size2 size)
        (< size2 (* -1 size))) [nil nil]
    (= size size2) [(assoc bet :size size2) nil]
    (zero? size2) [nil bet]
    (neg? size2) [(update bet :size + size2) (assoc bet :size (abs size2))]
    (pos? size2) [(assoc bet :size size2) (update bet :size - size2)]))

(defn execute-match-plan
  "if n = 0 do nothing
   else
   set size to []
   take n from ls
   if split-liquidity? split last ls, put b-unmatched in fron of ls, put b-matched in l-matched
   if even-match? put ls in l-matched
   if split-size? split bet put b-unmatched in size, but b-matched in l-matched
  "
  [{:keys [level instructions side] :as plan}]
  (let [n (- (count instructions) 1)]
    (if (zero? n)
      level ; match not possible so do nothing
      (-> plan
          (set-result (let [d (first instructions)
                            matched (take n (get-liquidity level side))
                            bet (first (get-size level side))
                            split-liquidity? (neg? d)
                            split-size? (pos? d)
                            even-match? (zero? d)
                            clear-size (fn [l])
                            split-liquidity (fn [l])
                            split-size (fn [l])
                            even-match (fn [l])]
                        (cond-> level
                          true clear-size
                          split-liquidity? split-liquidity
                          split-size? split-size
                          even-match? even-match)))))))

(defn- get-match-result
  "result in new order-book
   take n from l-matched
   if first instruction pos get first in size as unmatched part of bet"
  [plan order-book])

(defn match
  "Return [updated-order-book bets-matched order-status]"
  [order-book {:keys [price side] :as bet}]
  (-> order-book
      (order-book->level price)
      (add-size bet)
      (create-match-plan side)
      execute-match-plan
      (get-match-result order-book)))

(defn cancel
  [order-book side price bet-id])

