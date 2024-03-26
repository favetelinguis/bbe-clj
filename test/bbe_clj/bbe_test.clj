(ns bbe-clj.bbe-test
  (:require [bbe-clj.bbe :as sut]
            [clojure.test :as t]))

(t/deftest bbe
  (t/testing "non mod 3 will just update simulation"
    (t/is (map? (sut/step-bbe (sut/make-bbe) 1))))
  #_(t/testing "mod 3 = 0 will run a betting run"
      (t/is (map?
             (-> (sut/make-bbe)
                 (sut/step-bbe 1)
                 (sut/step-bbe 2)
                 (sut/step-bbe 3))))))
