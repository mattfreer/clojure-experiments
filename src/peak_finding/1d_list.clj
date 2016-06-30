;; A peak is any element that is larger or equal to both it's neighbours
;; Items that only have one neighbour (end items) are considered a peak if
;; they are larger or equal to their neighbour
;; This algorithm returns the first peak it finds.

(ns peakfinder)

(defn- item
  "returns a map that represents an item in a list. The data describes the
  index at which the item resides in the sequence and it's value. e.g. {index 1 :value 3}"
  [sequence, index]
  {:index index :value (nth sequence index nil)})

(defn- right-neighbour
  "returns the right neighbour of an item in a sequence"
  [sequence current-item]
  (item sequence (inc (:index current-item))))

(defn- left-neighbour
  "returns the left neighbour of an item in a sequence"
  [sequence current-item]
  (item sequence (dec (:index current-item))))

(defn- peak?
  "determines if an item is a peak compared to the supplied comparitors"
  [item & comparitors]
  (let [item-value (:value item)
        values (map :value comparitors)]
    (if (every? (fn [v] (>= item-value v)) values) item-value -1)))

(defn- middle-val
  "returns the middle value of a sequence"
  [sequence]
  (-> (count sequence) (- 1) (/ 2) (int)))

(defn find-peak
  "multiple arity recursive function, for finding a peak in a sequence"
  ([sequence] (->> (middle-val sequence) (item sequence) (find-peak sequence)))

  ([sequence current-item]
   (if (empty? sequence) -1
       (let [index (:index current-item)
             left (left-neighbour sequence current-item)
             right (right-neighbour sequence current-item)]
         (cond
           (-> sequence (count) (- 1) (= index)) ;; Furthest item right
           (peak? current-item left)

           (= index 0) ;; Furthest item left
           (peak? current-item right)

           :else
           (if (= (peak? current-item left right) -1)

             (if (> (:value left) (:value current-item))
               (find-peak sequence left) ;; Recursive
               (find-peak sequence right)) ;; Recursive

             (:value current-item)))))))


;; Unit tests
(ns peakfinder-test
  (:use clojure.test)
  (:use peakfinder))

(deftest finding-a-peak
  (is (contains? #{7} (find-peak [3 2 4 7 5 6 2])))
  (is (contains? #{3} (find-peak [3 2 1])))
  (is (contains? #{3 4 6} (find-peak [3 2 4 3 5 6 2])))
  (is (contains? #{3 6} (find-peak [3 2 0 1 2 6 2])))
  (is (contains? #{3 4} (find-peak [3 2 1 4])))
  (is (contains? #{3} (find-peak [1 3 2 1])))
  (is (contains? #{3 2} (find-peak [3 3 2 2])))
  (is (contains? #{-1} (find-peak []))))

(run-tests 'peakfinder-test)
