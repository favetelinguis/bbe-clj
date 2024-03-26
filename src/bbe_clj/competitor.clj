(ns bbe-clj.competitor
  (:require [clojure.math :as math]))

(defn move
  "dc current distance of c as a list with the last distance first
   Sc c step function"
  [dc Sc]
  (let [d (first dc)
        dt (+ d (Sc))]
    (conj dc dt)))

(defn uniform-rand-cont
  "real value between min inclusive and max exclusive"
  [min max]
  (+ min (rand-int (- max min)) (rand)))

(defn uniform-rand-disc
  "min inclusive max exclusive"
  [min max]
  (rand-nth (range min max)))

(defn mean
  [xs]
  (/ (reduce + xs) (count xs)))

(defn std
  [xs]
  (math/sqrt (/
              (reduce + (map #(math/pow % 2) (map - xs (repeat (mean xs)))))
              (- (count xs) 1))))

(defn magnitude
  [xs]
  (->> xs
       (map #(math/pow % 2))
       (reduce +)
       math/sqrt))

(defn norm
  [xs]
  {:pre [(every? pos? xs)]}
  (let [x (magnitude xs)]
    (->> xs (map #(/ % x)))))

(defn euclidean-distance
  "https://en.wikipedia.org/wiki/Euclidean_distance
   This uses euclidean distance over normalized vectors
   which will bound the result between 0-2 (i think)
   Written |fr - fc|"
  [fr fc]
  {:pre [(= (count fr) (count fc))]}
  (->> (norm fc)
       (map - (norm fr))
       (map #(math/pow % 2))
       (reduce +)
       math/sqrt))

(defn preference
  "P-c Model how close the current race (fr) is to competitors preference (fc), bounded to 0-1"
  [fr fc]
  (let [k 2]
    (/ (- k (euclidean-distance fr fc)) k)))

(defn responsiveness
  "Rc - Model speed up and slow down, bounded to 0-1 during the race"
  [time dr]
  ;; TODO
  (rand))

(defn make-feature-vec
  []
  [(uniform-rand-disc 400 4000) ; distance
   (uniform-rand-disc 1 50) ; temperatur
   (uniform-rand-disc 1 100) ; undilation
   ])

(defn make-competitor
  [min-step max-step]
  (fn [fr]
    (let [fc (make-feature-vec)
          preference (preference fc fr)]
      (fn Sc [t dr]
        (* (responsiveness t dr) preference (uniform-rand-disc min-step max-step))))))
