;;;; Translation of Peter Norvig's sudoku solver to Clojure
;;;; See http://norvig.com/sudoku.html

;; Original:
;; http://jkkramer.wordpress.com/2011/03/29/clojure-python-side-by-side/


;;; SEM converted representation to use bits for possible values and 0-80 for square
;;; identifiers.  This drifts away from original program, but gains about 3x performance.
;;; Many commments below are obsolete because of the changes.

;;;; [Obsolete notes]
;;;; Throughout this program we have:
;;;;   r is a row,     e.g. :a
;;;;   c is a column,  e.g. 3
;;;;   s is a square,  e.g. [:a 3]
;;;;   d is a digit,   e.g. 9
;;;;   u is a unit,    e.g. [[:a 1] [:b 1] [:c 1] ... [:i 1]]
;;;;   grid is a grid, e.g. 81 non-blank chars, e.g. starting with ".18...7..."
;;;;   values is a map of possible values, e.g. {[:a 1] #{1 2 3 9} [:a 2] #{8}}

;; Slightly modified by SEM 3/30/11
;;; 07/17/20  10:41 by miner -- modernized a bit
;;; 07/18/20  14:36 by miner -- new idea, use bits instead of small sets


(ns miner.bitsudoku
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io]))



(def dim 9)
(def sqcnt (* dim dim))

;; new representation
;; squares are vector of 81 values
;; row-major order  index = row * dim(9) + col
;; rows 0-8, cols 0-8 -- now zero-based instead of one-based

;; possible values are set by bits (powers of two), not digit.

;; bits 1-9 set
(def ninebits 1022)




(defn xrc [x]
  (case x
    (0 1 2 3 4 5 6 7 8 9) x
    :a 1
    :b 2
    :c 3
    :d 4
    :e 5
    :f 6
    :g 7
    :h 8
    :i 9
    nil))

;; just for testing
(defn convert-square-index [[r c]]
  (+ (* (dec (xrc r)) dim) (dec (xrc c))))

(def csi convert-square-index)

(defn sqi [r c]
  (+ (* r dim) c))
  
(defn bit-count [^long n]
  (Long/bitCount n))


(def rcs (range dim))

(def sqs (range sqcnt))

(def ulist  (concat (for [c rcs] (for [r rcs] (sqi r c)))
                   (for [r rcs] (for [c rcs] (sqi r c)))
                   (for [rs (partition 3 rcs) cs (partition 3 rcs)]
                     (for [r rs c cs] (sqi r c)))))

(def vun (vec (for [s sqs]
                (for [u ulist :when (some #{s} u)] u))))

(def vp (vec (for [s sqs]
               (-> (reduce into #{} (vun s)) (disj s)))))



(declare assign eliminate)

;;; Unit Tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn unit-tests
  "A set of tests that must pass"
  ;; NB: Tests normally go in separate files and and use clojure.test
  []
  (assert (= 81 (count sqs)))
  (assert (= 27 (count ulist)))
  (assert (every? #(= 3 (count (vun %))) sqs))
  (assert (every? #(= 20 (count (vp %))) sqs))
  (assert (= (vun (csi [:c 2]))
             [(mapv csi [[:a 2] [:b 2] [:c 2] [:d 2] [:e 2] [:f 2] [:g 2] [:h 2] [:i 2]])
              (mapv csi [[:c 1] [:c 2] [:c 3] [:c 4] [:c 5] [:c 6] [:c 7] [:c 8] [:c 9]])
              (mapv csi [[:a 1] [:a 2] [:a 3] [:b 1] [:b 2] [:b 3] [:c 1] [:c 2] [:c 3]])]))
  (assert (= (vp (csi [:c 2]))
             (into #{} (map csi) #{[:a 2] [:b 2] [:d 2] [:e 2] [:f 2] [:g 2] [:h 2] [:i 2]
               [:c 1] [:c 3] [:c 4] [:c 5] [:c 6] [:c 7] [:c 8] [:c 9]
               [:a 1] [:a 3] [:b 1] [:b 3]})))
  :passed)

;;; Parse a Grid ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn grid-values
  "Convert grid into a map of {square: digit}, with nil for empties"
  [grid]
  (assert (= sqcnt (count grid)))
  (mapv (fn [^Character c]
                    (case c
                      \1 2
                      \2 4
                      \3 8
                      \4 16
                      \5 32
                      \6 64
                      \7 128
                      \8 256
                      \9 512
                      nil))
        (seq grid)))


(def init-any-values (vec (repeat sqcnt ninebits)))

(defn parse-grid
  "Convert grid to a map of possible values, {square: digits}. Return false
  on contradiction"
  [grid]
  (reduce-kv
   (fn [values s d]
     (if (nil? d)
       values
       (or (assign values s d) (reduced nil))))
   init-any-values
   (grid-values grid)))

;;; Constraint Propagation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; SEM probably better to use Long/highestOneBit or lowestOneBit and keep everything in the
;; bit domain

;; (bit-and-not n c)  will clear bits in c



;; faster than marked-bits but returns bitfields (powers of 2) not bit indexes
(defn bitseq [^long n]
  (loop [n n bs nil]
    (if (zero? n)
      bs
      (let [h (Long/lowestOneBit n)]
        (recur (bit-and-not n h) (conj bs h))))))

(defn marked-bits [^long n]
  (map #(Long/numberOfTrailingZeros %) (bitseq n)))


(defn bit-single? [^long n]
  (= (Long/bitCount n) 1))
  

(defn assign
  "Whittle down the square at s to digit d by eliminating every digit
  except d from the square, and doing constraint propogation. Returns
  false if a contradiction results"
  [values s d]
  (reduce #(or (eliminate %1 s %2) (reduced nil))
          values
          (bitseq (bit-and-not (values s) d))))


(defn eliminate
  "Eliminate digit d from square s and do any appropriate constraint
  propogation"
  [values s d]
  (if (zero? (bit-and (values s) d))
    values ;already eliminated
    (when-not (= d (values s)) ;can't remove last value
      (let [values (update values s bit-and-not d)
            single (when (bit-single? (values s)) (values s))
            values (if single
                     ;; Only one digit left, eliminate it from peers
                     (reduce #(or (eliminate %1 %2 single) (reduced nil))
                             values
                             (vp s))
                     values)]
        (reduce
         (fn [values u]
           (if (nil? values)
             (reduced nil)
             (let [dplaces (for [s u :when (pos? (bit-and (values s) d))] s)]
               (if-not (zero? (count dplaces)) ;must be a place for this value
                 (if (= 1 (count dplaces))
                   ;; Only one spot remaining for d in a unit -- assign it
                   (assign values (first dplaces) d)
                   values)
                 (reduced nil)))))
         values
         (vun s))))))

;;; Display as 2D Grid ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn display
  "Display values as a 2D grid"
  [values]
  (let [width (inc (apply max (map (comp bit-count values) sqs)))
        line (str/join \+ (repeat 3 (str/join (repeat (* 3 width) \-))))]
    (doseq [r rcs]
      (println (str/join (for [c rcs]
                       (format (str "%-" width "s%s")
                               (str/join (marked-bits (values (sqi r c))))
                               (if (#{2 5} c) "|" "")))))
      (when (#{2 5} r) (println line)))))

;;; Search ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn search
  "Using depth-first search and propagation, try all possible values"
  [values]
  (when values
    (let [scount (comp bit-count values) ;digits remaining
          live-squares (remove #(= 1 (scount %)) sqs)]
      (if (empty? live-squares)
        values ;solved!
        (let [s (apply min-key scount live-squares)]
          (some identity (for [d (bitseq (values s))]
                           (search (assign values s d)))))))))



(defn solve [grid] (-> grid parse-grid search))

;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn from-file [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;;; System Test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn nine-bits? [xs]
  (= ninebits (reduce bit-or 0 xs)))

(defn solved?
  "A puzzle is solved if each unit is a permutation of the digits 1 to 9"
  [values]
  (and values
       (every? bit-single? values)
       (every? nine-bits? (map #(map values %) ulist))))

;; Use `(display values)` if you want to see state.
;; `squars81` converts a solution back to conventional digits.  Used for testing.
(defn squares81 [values]
  (when (solved? values)
    (mapv #(Long/numberOfTrailingZeros ^long %) values)))

#_
(defn random-puzzle
  "Make a random puzzle with N or more assignments. Restart on contradictions."
  ([] (random-puzzle 17))
  ([n]
     (let [done? (fn [values]
                   (let [ds (apply concat (filter #(= 1 (count %)) (vals values)))]
                     (and (<= n (count ds)) (<= 8 (count (distinct ds))))))
           steps (reductions #(assign %1 %2 (-> %2 %1 seq rand-nth))
                             (into {} (for [s squares] [s digits]))
                             (shuffle squares))
           values (first (filter #(or (not %) (done? %)) steps))]
       (if (nil? values)
         (recur n) ;contradiction - retry
         (str/join (for [ds (map values squares)]
                 (if (next ds) \. (first ds))))))))

(def grid1 "003020600900305001001806400008102900700000008006708200002609500800203009005010300")
(def grid2 "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")
(def hard1 ".....6....59.....82....8....45........3........6..3.54...325..6..................")

(def data-dir "resources/")

(defn solve-grids [solve grids]
  (assert (every? solved? (map solve grids))))

(defn run-bench
  ([] (run-bench solve))
  ([solve]
   (unit-tests)
   (solve-grids solve (from-file (io/file data-dir "easy50grids.txt")))
   (solve-grids solve (from-file (io/file data-dir "top95.txt")))
   (solve-grids solve (from-file (io/file data-dir "hardest.txt")))
   true))

  
(comment
  (require 'criterium.core)
  (require '[miner.sudoku :as s])
  (criterium.core/quick-bench (s/run-bench s/solve))
  )
