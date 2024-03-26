(ns bbe-clj.race
  (:require
   [scicloj.kindly.v4.kind :as kind]
   [bbe-clj.competitor :as c]))

; race has competitors and hold the state for the race
; race is called to update the step function for each competitor
; race has uuid to have many races at the same time
; clock start at 0 and continue to loop until the last competior or first or n has
; a d(t) >= length

(defn num-competitors
  [race]
  (count (:competitors race)))

(defn make-race
  []
  (let [n (c/uniform-rand-disc 5 11)
        fr (c/make-feature-vec)
        distance (first fr)
        competitors (repeatedly n
                                #((c/make-competitor
                                   (c/uniform-rand-disc 5 10)
                                   (c/uniform-rand-disc 10 20)) fr))]
    {:distance distance
     :fr fr
     :competitors competitors ; n competitors
     :result nil ; winning horse
     :dcss (take (count competitors) (repeat (list 0.0))) ; n real-values denoting each competitors distance
     }))

(defn done?
  "I model win races so the betting stops after the first horse crosses the finnish line"
  [{:keys [result]}]
  (not (nil? result)))

(defn move-competitors
  "Step each competitor one step, once a competitor goes over distance
   race stops and no more moves are done."
  [{:keys [competitors dcss distance] :as race} n]
  (let [prev-dcs (map first dcss)
        next-dcss (map (fn [dcs Sc]
                         (let [dc (first dcs)]
                           (conj dcs (min distance
                                          (+ dc (Sc n prev-dcs)))))) dcss competitors)
        current-dcs (map first next-dcss)]
    (if (every? #(< % distance) current-dcs)
      (assoc race :dcss next-dcss)

      (let [winner (->> current-dcs
                        (map-indexed vector)
                        (filter #(= (second %) distance))
                        rand-nth ; break ties randomly
                        first ; horses will be zero indexed to 0-n
                        )]
        (-> race
            (assoc :dcss next-dcss)
            (assoc :result winner))))))

(defn run
  "Run a race until its done"
  [race]
  (reduce (fn [r n]
            (if (done? r)
              (reduced r)

              (move-competitors r n))) race (map inc (range))))

(defn repeat-run
  "Have a cap of max 100 runs to ensure not running forever"
  [race n]
  {:pre [(and (pos? n) (<= n 100))]}
  (repeatedly n #(run race)))

(defn decimal->betfair-odds
  [x]
  (apply min-key #(abs (- % x))
         (map float (concat (range 1.01 2.01 0.01)
                            (range 2 3.02 0.02)
                            (range 3 4.05 0.05)
                            (range 4 6.1 0.1)
                            (range 6 10.2 0.2)
                            (range 10 20.5 0.5)
                            (range 20 31 1)
                            (range 30 52 2)
                            (range 50 105 5)
                            (range 100 1010 10)))))

(defn prob->decimal
  [x]
  (/ 1 x))

(defn decimal->prob
  [x]
  (/ 1 x))

(defn ->prob
  [n1 tot]
  (/ n1 tot))

(defn get-win-odds
  "Calculate the win odds for each competitor as a map competitor->odds"
  [races]
  (let [results (map :result races)
        n (count results)]
    (-> (frequencies results)
        (update-vals #(->prob % n))
        (update-vals prob->decimal)
        (update-vals decimal->betfair-odds))))

(defn get-win-odds-each
  "Given a run of 100 races will split up into smaller runs and
   calculate winodds for 5 10 15 ... 100 races. Last item is the last simulation so
   the accuracy will increase with the index."
  [races]
  (reduce (fn [acc x] (conj acc (get-win-odds
                                 (take x races)))) []
          (range 5 (inc (count races)) 5)))

(defn run->vega-data
  [r]
  (->> r
       :dcss
       (map #(map-indexed vector (reverse %)))
       (map-indexed vector)
       (reduce (fn [acc [i xs]]
                 (concat acc
                         (map (fn [[x y]]
                                {:horse i
                                 :step x
                                 :distance y}) xs))) (list))))

(defn graph
  [data]
  (kind/vega-lite {:encoding
                   {:y {:field "distance" :type "quantitative"}
                    :x {:field "step" :type "quantitative"}
                    :color {:field "horse" :type "nominal"}}
                   :mark "line"
                   :data {:values data}}))

(comment
  (graph (run->vega-data (run (make-race))))
;;
  )
