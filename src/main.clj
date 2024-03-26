(ns main
  (:require [bbe-clj.bbe :as bbe]))

;; This will actually not contain any core.async code
;; this will just be a library defining all the parts to build a bbe spec that then can be run.

(defn run-bbe
  [bbe]
;; race one go-loop
;; for each competitor in race go-loop
;; for each bettor in bbe go-loop
;; all have unbuffered channels?
;; can we setup each race so it has dedicated ch not having to share between races?
  )

(defn run-all-bbe
  [bbes]
  (doseq [bbe bbes]
    (run-bbe bbe)))
