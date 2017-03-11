;; A peak is any element that is larger or equal to both it's neighbours
;; Items that only have one neighbour (end items) are considered a peak if
;; they are larger or equal to their neighbour

(ns peakfinder
  (:require [clojure.core.match :refer [match]]))

(defn find-peak [input]
  "Returns all peaks in a sequence"
  (match [input]
         ;; checks input is a sequence of multiple numbers
         [(input :guard #(and (sequential? %)
                              (> (count %) 1)
                              (every? number? %)))]
         (let [all-nums (cons 0 input) ;; adds a zero to the head
               groups (partition 3 1 [0] all-nums)] ;; pads a zero to the tail
           (reduce (fn [memo group]
                     (let [middle (second group)]
                       (if (= (apply max group) middle)
                         (conj memo middle)
                         memo)))
                   []
                   groups))

         ;; checks input contains head only
         [([h] :seq)]
         input

         ;; other input
         :else
          nil))

;; Unit tests
(ns peakfinder-test
  (:use clojure.test)
  (:use peakfinder))

(deftest finding-a-peak
  (is (= [3 7 6] (find-peak [3 2 4 7 5 6 2])))
  (is (= [3] (find-peak '(3 2 1))))
  (is (= [3 4 6] (find-peak [3 2 4 3 5 6 2])))
  (is (= [3 6] (find-peak '(3 2 0 1 2 6 2))))
  (is (= [3 4] (find-peak [3 2 1 4])))
  (is (= [3] (find-peak '(1 3 2 1))))
  (is (= [3 3 2] (find-peak [3 3 2 2])))
  (is (= [3] (find-peak [3])))
  (is (= [3] (find-peak [3 1])))
  (is (= nil (find-peak "random")))
  (is (= nil (find-peak 3)))
  (is (= nil (find-peak [])))
  (is (= nil (find-peak '()))))

(run-tests 'peakfinder-test)
