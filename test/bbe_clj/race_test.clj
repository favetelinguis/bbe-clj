(ns bbe-clj.race-test
  (:require [bbe-clj.race :as sut]
            [clojure.test :as t]))

(t/deftest race
  (t/testing "when race is done result is avaliable"
    (t/is (not (nil? (:result (sut/run (sut/make-race)))))))
  (t/testing "can run many simulations of the same race"
    (let [xs (sut/repeat-run (sut/make-race) 100)]
      (t/is (= 100 (count xs)))
      (t/is (every? map? (sut/get-win-odds-each xs))))))
