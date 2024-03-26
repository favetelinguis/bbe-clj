(ns bbe-clj.order-book)

(def level {:back []
            :lay []
            :matched []})

(def order-book
  (into (sorted-map-by >)
        (reduce (fn [acc odds] (conj acc [odds level]))
                []
                (map float (concat (range 1.01 2.01 0.01)
                                   (range 2 3.02 0.02)
                                   (range 3 4.05 0.05)
                                   (range 4 6.1 0.1)
                                   (range 6 10.2 0.2)
                                   (range 10 20.5 0.5)
                                   (range 20 31 1)
                                   (range 30 52 2)
                                   (range 50 105 5)
                                   (range 100 1010 10))))))

(defn bet
  "TODO add validation that odds is a valid odds at betfair"
  [side competitor-id odds volume]
  {:competitor-id competitor-id
   :bet-id (random-uuid)
   :side side
   :odds (float odds) ; important to be float since keys in order-book is float
   :volume volume})

(def lay (partial bet :lay))
(def back (partial bet :back))

(defn lay?
  [k]
  (= k :lay))

(defn back?
  [k]
  (= k :back))

(defn get-opposite-side
  [k]
  (if (= k :back)
    :lay
    :back))

(defn bet->bet-status
  [m unmatched]
  ;; Super naive impl TODO
  (if (neg-int? unmatched)
    (-> m
        (assoc :unmatched (- (:volume m) (abs unmatched)))
        (assoc :matched (abs unmatched)))
    ;; Posetive
    (-> m
        (assoc :matched (- (:volume m) unmatched))
        (assoc :unmatched unmatched))))

(defn match
  "Return [updated-order-book bets-matched order-status]"
  [m {:keys [odds side] :as bet}]
  (let [opposite-side (get-opposite-side side)
        liquidity (get-in m [odds opposite-side])]
    (if (empty? liquidity)
      ;; No liquidity to match bet
      [(update-in m [odds side] conj bet) [] (bet->bet-status bet (:volume bet))]

      ;; Avaliable liquidity so match will be made
      (loop [remaining-liquidity liquidity
             matched-bets []
             unmatched-bet bet ; nil when bet is fully matched
             terminal? false]
        (let [[l & ls] remaining-liquidity
              l-volume (get l :volume 0)
              unmatched-volume (- (get unmatched-bet :volume 0) l-volume)
              split-bet? (pos? unmatched-volume)
              split-l? (neg? unmatched-volume)
              even-match? (zero? unmatched-volume)
              last-l? (empty? ls)]
          (cond
            ;; Terminal case
            terminal?
            [(cond-> m
               true (assoc-in [odds opposite-side] (into [] remaining-liquidity)) ; update avaliable liquidity, make sure can never be nil
               true (update-in [odds :matched] concat matched-bets) ; update volume matched at odds
               unmatched-bet (assoc-in [odds side] [unmatched-bet])) ; Add remaining of bet as liquidity on side
             matched-bets
             (bet->bet-status bet unmatched-volume)]

            ;; No more liquidity
            (nil? l)
            (recur ls matched-bets unmatched-bet true)

            ;; Bet is not filled and there are more liquidity, bet fully consumes l
            (and split-bet? (not last-l?))
            (recur ls (conj matched-bets l) (update unmatched-bet :volume - l-volume) false)

            ;; Bet is not filled and there is no more liquidity, bet fully consumes l
            (and split-bet? last-l?)
            ;; im counting on unmatched-bet be nil here but it was not
            (recur ls (conj matched-bets l) (update unmatched-bet :volume - l-volume) true)

            ;; Bet is filled but do not fully consume l
            split-l?
            (recur (into [(assoc l :volume (abs unmatched-volume))] ls) ; reduce matched volume from l and add back to front of avaliable liquidity
                   (conj matched-bets (assoc l :volume (+ l-volume unmatched-volume))) ; add the part filled as matched volume for l
                   nil true)

            ;; Bet is filled and fully consume l
            even-match?
            (recur ls (conj matched-bets l) nil true)))))))

(defn cancel-bet
  "return updated order-book and removed item if found"
  [m {:keys [odds side bet-id]}]
  (let [xs (get-in m [odds side])
        item (first (filter #(= (:bet-id %) bet-id) xs))
        updated-order-book (update-in m [odds side] (fn [ls] (filter #(not= (:bet-id %) bet-id) ls)))]
    [updated-order-book item]))
