(ns bbe-clj.order-book-test
  (:require [bbe-clj.order-book :as ob]
            [clojure.test :as t]))

(t/deftest adding-bets-when-no-match-and-removing
  (let [bet (ob/lay 123 2.16 500)]
    (-> ob/order-book
        (ob/match bet)
        ((fn [[new-book b c]]
           (t/is 1 (-> new-book (get-in [123 2.16 :lay]) count))
           new-book))
        (ob/cancel-bet bet)
        ((fn [[new-book removed]]
           (t/is (not (nil? removed)) "removed bet is returned")
           (t/is (= 0 (-> new-book (get-in [0 123 2.16 :lay]) count)) "liquidity has been updated"))))))

(t/deftest consume-all-liquidity-and-fully-match-bet
  (let [odds (float 2.16)
        back (partial ob/back 1 odds)
        lay (partial ob/lay 1 odds)
        get-back #(get-in % [odds :back])
        get-lay #(get-in % [odds :lay])
        get-matched #(get-in % [odds :matched])]
    (-> ob/order-book
        (ob/match (back 10))
        ((fn [[nob mb os]]
           (t/is (= 1 (-> nob get-back count)) "liquidity added to back side")
           (t/is (= 0 (-> mb count)) "no bets matched")
           (t/is (= 0 (-> os :matched)) "nothing was matched in bet")
           nob))
        (ob/match (back 5))
        ((fn [[nob mb os]]
           (t/is (= 2 (-> nob get-back count)) "liquidity added to back side")
           (t/is (= 0 (-> mb count)) "no bets matched")
           (t/is (= 0 (-> os :matched)) "nothing was matched in bet")
           nob))
        (ob/match (lay 15)) ; consume all liquidity and fully match bet
        ((fn [[nob mb os]]
           (t/is (= 0 (-> nob get-back count)) "back liquidity is gone")
           (t/is (= 0 (-> nob get-lay count)) "lay liquidity still empty")
           (t/is (= 2 (-> nob get-matched count)) "matched liquidity on odds is correct")
           (t/is (= 2 (-> mb count)) "bets matched")
           (t/is (= 15 (-> os :matched)) "all matched in bet")
           (t/is (= 0 (-> os :unmatched)) "nothing unmatched in bet")
           nob)))))

(t/deftest consume-partly-liquidity
  (let [odds (float 2.16)
        back (partial ob/back 1 odds)
        lay (partial ob/lay 1 odds)
        get-back #(get-in % [odds :back])
        get-lay #(get-in % [odds :lay])
        get-matched #(get-in % [odds :matched])]
    (-> ob/order-book
        (ob/match (lay 3))
        first
        (ob/match (lay 7))
        first
        (ob/match (back 5)) ; partly consume liquidity
        ((fn [[nob mb os]]
           (t/is (= 0 (-> nob get-back count)) "back liquidity")
           (t/is (= 1 (-> nob get-lay count)) "lay liquidity")
           (t/is (= 5 (-> nob get-lay first :volume)) "remaining lay liquidity")
           (t/is (= 2 (-> nob get-matched count)) "vol matched")
           (t/is (= 2 (-> mb count)) "bets matched")
           (t/is (= 2 (-> mb last :volume)) "partly matched liquidity matched")
           (t/is (= 5 (-> os :matched)) "matched in bet")
           (t/is (= 0 (-> os :unmatched)) "unmatched in bet")
           nob)))))

(t/deftest consume-partly-bet
  (let [odds (float 2.16)
        back (partial ob/back 1 odds)
        lay (partial ob/lay 1 odds)
        get-back #(get-in % [odds :back])
        get-lay #(get-in % [odds :lay])
        get-matched #(get-in % [odds :matched])]
    (-> ob/order-book
        (ob/match (lay 3))
        first
        (ob/match (back 12)) ; partly consume bet
        ((fn [[nob mb os]]
           (t/is (= 1 (-> nob get-back count)) "back liquidity")
           (t/is (= 9 (-> nob get-back first :volume)) "back liquidity")
           (t/is (= 0 (-> nob get-lay count)) "lay liquidity")
           (t/is (= 1 (-> nob get-matched count)) "vol matched")
           (t/is (= 1 (-> mb count)) "bets matched")
           (t/is (= 3 (-> os :matched)) "matched in bet")
           (t/is (= 9 (-> os :unmatched)) "matched in bet")
           nob)))))
