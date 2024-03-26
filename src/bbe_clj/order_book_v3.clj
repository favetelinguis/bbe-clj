(ns bbe-clj.order-book-v3)

;; This will be a simplified book, only odds and total volume available will be used,
;; a real book would keep information about which bettor added the bet so that a bet can later be
;; canceled.
;; All stake is only 1 so I will never fill multiple levels in the book.

(defn book [order-fn]
  (sorted-map-by order-fn))

(def back-book (book >))
(def lay-book (book <))
(def tape (list)) ; conj to the front

(def ->odds float)

(defn side-idx
  [side]
  (if (= :back side) 1 0))

(def tape-idx 2)

(defn get-book
  [lob side]
  (get lob (side-idx side)))

(defn add-to-book
  [lob side odds stake]
  (update-in lob [(side-idx side) (->odds odds)]
             (fn [x y] (if (nil? x) y (+ x y))) stake))

(defn add-to-tape
  [lob m]
  (update lob tape-idx conj m))

(defn remove-from-book
  "Only time we remove from book is when there is a match then we remove stake from odds
   and update the tape"
  [lob side odds stake]
  (let [o (->odds odds)
        idx (side-idx side)
        new-lob (-> lob
                    (update-in [idx o] (fn [x y] (if (nil? x) y (- x y))) stake)
                    (add-to-tape {:stake stake :odds o}))]
    (if (<= (get-in new-lob [idx o]) 0)
      (update new-lob idx dissoc o)
      new-lob)))

(defn best-offer
  "Empty orderbook will return nil"
  [lob side]
  (-> lob
      (nth (side-idx side))
      first
      first))

(defn crossing-spread?
  "When a new back quote arrives at the exchange with a lower or equal odds then the current
   best-lay a trade happens, this is called crossing the spread.
   If the back odds is lower then multiple lay odds it can consume multiple levels
   of lays in an effort to fill. If there is not enogh volume to fill the whole back,
   the remaining volume will be put on the back side a a new best offer of back."
  [lob side odds]
  (if (= side :back)
    (when-let [bo (best-offer lob :lay)]
      (<= (->odds odds) bo))
    (when-let [bo (best-offer lob :back)]
      (>= (->odds odds) bo))))

(def make-lob
  "LOB - Limit Order Book"
  [lay-book back-book tape])

(defn other
  [side]
  (if (= side :back) :lay :back))

(defn execute-decision
  "Update books with decision and return the new books. Each competitor has a lob in lobs
   This is greatly simplified since stake is always 1 I will never fill
   multiple levels in a match, either I will fill or not."
  [lobs {:keys [competitor side odds stake]}]
  (if (crossing-spread? (nth lobs competitor) side odds)
    (update lobs competitor remove-from-book (other side) odds stake)
    (update lobs competitor add-to-book side odds stake)))
