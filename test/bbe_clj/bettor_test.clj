(ns bbe-clj.bettor-test
  (:require [bbe-clj.bettor :as sut]
            [clojure.test :as t]
            [bbe-clj.race :as race]))

(t/deftest make-decision
  (let [r (race/make-race)
        win-odds (race/get-win-odds-each (race/repeat-run r 50))]
    (t/testing "bettor can create decision"
      (t/is (map? (sut/make-decision r nil win-odds))))))
