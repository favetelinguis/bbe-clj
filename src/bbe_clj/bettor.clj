(ns bbe-clj.bettor)

; bettor run a different number of simulations at each time step to form an opinon
; it is the running of simulations that can be based on GPU
; each bettor run a different number of simulations to introduce noice

(defn select-competitor
  "Gets a list of split up simulations as produced by
  race/get-win-odds-each and randomly selects one.
  Return the number of the favorite competitor, whith the shortest odds"
  [win-odds]
  (->> win-odds
       rand-nth
       (apply min-key second)))

(defn make-decision
  "TODO this can be made much more complext where bettor also
   check exchange and race state before making decision.
   A bettor also need to know the bets that are made, and get updated when filled
   he should also be able to cancel bets"
  [race exchange win-odds]
  (let [[competitor odds] (select-competitor win-odds)]
    {:competitor competitor
     :odds odds
     :side (rand-nth [:lay :back]) ; just some simple to make it actual match sometime
     :stake 1}))

(defn make-bettors
  "Number of bettors needed to get action is B >= 4DN
   where D is wanted market depth and N is number of runners"
  [N]
  (let [wanted-market-depth 3
        x (* 4 N wanted-market-depth)]
    (take x (repeat make-decision))))
