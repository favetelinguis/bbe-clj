(ns bbe-clj.competitor-test
  (:require [bbe-clj.competitor :as sut]
            [clojure.spec.alpha :as s]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test :as t]
            [clojure.math :as math]
            [clojure.set :as set]))

(defn close-to?
  [x y epsilon]
  (<= (abs (- x y)) epsilon))

(defspec minmax-rand
  (prop/for-all [low (gen/choose 0 100)
                 high (gen/choose 101 1000)]
                (let [results (repeatedly 100000 #(sut/minmax-rand low high))
                      expected-mean (/ (+ low high) 2)
                      expected-std (math/sqrt
                                    (* (/ 1 12) (math/pow (- high low) 2)))]
                  (and
                   (t/testing "Has the expected std"
                     (close-to? expected-std (sut/std results) 1.5))
                   (t/testing "Has the expected mean"
                     (close-to? expected-mean (sut/mean results) 5))))))

(defspec norm
  (prop/for-all [xs (gen/not-empty (gen/vector (gen/choose 1 2000)))]
                (let [result (sut/norm xs)]
                  (and
                   (t/testing "Length of a normalized vector is 1"
                     (close-to? (sut/magnitude result) 1 0.1))))))

(defspec euclidean-distance
  (prop/for-all [[xs ys] (gen/bind (gen/such-that pos? gen/nat)
                                   (fn [x]
                                     (gen/tuple
                                      (gen/not-empty (gen/vector (gen/choose 1 2000) x))
                                      (gen/not-empty (gen/vector (gen/choose 1 2000) x)))))]
                (let [s (sut/euclidean-distance xs ys)]
                  (and
                   (t/testing "For any two non-empty vectors of equal length with values > 0 s is bounded between 0-2"
                     (and (>= s 0) (<= s 2)))))))

(defspec preference-function
  (prop/for-all [[xs ys] (gen/bind (gen/such-that pos? gen/nat)
                                   (fn [x]
                                     (gen/tuple
                                      (gen/not-empty (gen/vector (gen/choose 1 2000) x))
                                      (gen/not-empty (gen/vector (gen/choose 1 2000) x)))))]
                (let [s (sut/preference xs ys)]
                  (and
                   (t/testing "Pc is bounded to 0-1"
                     (and (>= s 0) (<= s 1)))))))

(defspec responsivness-function
  (prop/for-all [n gen/nat
                 dcs (gen/not-empty (gen/vector gen/nat))]
                (let [s (sut/responsiveness n dcs)]
                  (and
                   (t/testing "Rc is bounded to 0-1"
                     (and (>= s 0) (<= s 1)))))))

(defspec step-forward-function
  (prop/for-all [min-step (gen/choose 0 100)
                 max-step (gen/choose 101 1000)
                 dcs (gen/not-empty (gen/vector gen/nat))
                 n gen/nat]
                (let [step-fn ((sut/make-competitor min-step max-step)
                               (sut/make-feature-vec))
                      s (step-fn n dcs)]
                  (and
                   (t/testing "Sc always greater then 0"
                     (> s 0))
                   (t/testing "Sc always less then max-step"
                     (<= s max-step))))))
