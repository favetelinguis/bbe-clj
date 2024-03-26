(ns bbe-clj.order-book-v3-test
  (:require [bbe-clj.order-book-v3 :as sut]
            [clojure.test :as t]))

(t/deftest book
  (let [lob (-> sut/make-lob
                (sut/add-to-book :lay 4 1)
                (sut/add-to-book :lay 3 1)
                (sut/add-to-book :lay 5 1)
                (sut/add-to-book :back 4 1)
                (sut/add-to-book :back 3 1)
                (sut/add-to-book :back 5 1))]
    (t/testing "back book is ascending"
      (t/is (->> (sut/get-book lob :back)
                 (map first)
                 (apply >))))
    (t/testing "lay book is decending"
      (t/is (->> (sut/get-book lob :lay)
                 (map first)
                 (apply <))))
    (t/testing "best back is highest"
      (t/is (= 5. (sut/best-offer lob :back))))
    (t/testing "best lay is lowest"
      (t/is (= 3. (sut/best-offer lob :lay))))))

(t/deftest crossing-the-spread
  (t/testing "empty lob will never cross spread"
    (t/is (not (-> sut/make-lob
                   (sut/crossing-spread? :back 3))))
    (t/is (not (-> sut/make-lob
                   (sut/crossing-spread? :lay 30)))))
  (t/testing "new back quote with lower odds then current best lay is a cross"
    (t/is (-> sut/make-lob
              (sut/add-to-book :lay 4 1)
              (sut/crossing-spread? :back 3)))
    (t/is (not (-> sut/make-lob
                   (sut/add-to-book :lay 4 1)
                   (sut/crossing-spread? :back 30)))))
  (t/testing "new lay quote with higher odds then current best back is a cross"
    (t/is (-> sut/make-lob
              (sut/add-to-book :back 4 1)
              (sut/crossing-spread? :lay 5)))
    (t/is (not (-> sut/make-lob
                   (sut/add-to-book :back 4 1)
                   (sut/crossing-spread? :lay 3))))))

(t/deftest best-offer
  (t/testing "Can handle empty lob with returning nil"
    (t/is (nil? (sut/best-offer sut/make-lob :lay)))))
