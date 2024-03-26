(ns bbe-clj.order-book-v2-test
  (:require [bbe-clj.order-book-v2 :as sut]
            [clojure.test :as t]))

(defn back [size] ((partial sut/back 1 1 2.16) size))
(defn lay [size] ((partial sut/lay 1 1 2.16) size))
(def lay-level (-> sut/level
                   (sut/add-liquidity (lay 5))
                   (sut/add-liquidity (lay 3))
                   (sut/add-liquidity (lay 1))))

(t/deftest create-match-plan
  (t/is (= '(14)
           (-> sut/level
               (sut/add-size (lay 14))
               (sut/create-match-plan :lay)
               :instructions)) "no liquidity avaliable")
  (t/is (= '(5 6 9 14)
           (-> lay-level
               (sut/add-size (lay 14))
               (sut/create-match-plan :lay)
               :instructions)) "consume all liquidity and bet is not filled")
  (t/is (= '(0 5)
           (-> lay-level
               (sut/add-size (lay 5))
               (sut/create-match-plan :lay)
               :instructions)) "even match")
  (t/is (= '(-2 1 6)
           (-> lay-level
               (sut/add-size (lay 6))
               (sut/create-match-plan :lay)
               :instructions)) "split liquidity and fill up bet"))

(t/deftest bet->matched+unmatched
  (let [[m1 um1] (sut/bet->matched+unmatched (lay 7) 0)
        [m2 um2] (sut/bet->matched+unmatched (lay 7) 7)
        [m3 um3] (sut/bet->matched+unmatched (lay 7) -6)
        [m4 um4] (sut/bet->matched+unmatched (lay 7) -70)
        [m5 um5] (sut/bet->matched+unmatched (lay 7) 4)
        [m6 um6] (sut/bet->matched+unmatched (lay 7) 70)]
    (t/is (and (nil? m1) um1) "handle zero")
    (t/is (= [7] [(:size um1)]))
    (t/is (and m2 (nil? um2)) "handle equal")
    (t/is (= [7] [(:size m2)]))
    (t/is (and m3 um3) "handle negative")
    (t/is (= [1 6] [(:size m3) (:size um3)]))
    (t/is (and (nil? m4) (nil? um4)) "handle larger negative")
    (t/is (and m5 um5) "handle smaller")
    (t/is (= [4 3] [(:size m5) (:size um5)]))
    (t/is (and (nil? m6) (nil? um6)) "handle larger")))

(t/deftest execute-match-plan
  ())
