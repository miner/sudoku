(ns miner.taransud
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [tarantella.core :as t]))

;; For more information on Tarantella, see: https://github.com/Engelberg/tarantella

;; constraints for sudoku
;; each row 1-9
;; each col 1-9
;; each sub-square 1-9
;; a solution is basically 81 values 1-9 in an order satisfying the contraints

;; can also be thought of as 27 units each with a 1-9 sequence



(def dim 9)
(def sqcnt (* dim dim))

(defn sqi [r c]
  (+ (* r dim) c))

(def rcs (range dim))

(def sqs (range sqcnt))

(defn from-file [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))


;;; [from Tarantella README]
;;; The rules of Sudoku can be expressed as a matrix. Each row represents a possible choice
;;; of placing a given digit in a given cell. We'll use the row label [[i j] n] to mean we
;;; put the digit n in (row i,col j). Our row/col numbers i,j will range from 0-8 and our
;;; digits to place range from 1-9.
;;;
;;; Each column represents a constraint. We'll use the column label [i j] for the constraint
;;; that there is exactly one digit allowed in (row i,col j). We'll use the column
;;; label [:row i n] for the constraint that row i must contain exactly one n. [:col j n] is
;;; the constraint that col j must contain exactly one n. [:sector 0-2 0-2 n] is the
;;; constraint that each of the 9 sectors must contain exactly one n.


;;; SEM: I wasn't able to remember this constraint encoding off the top of my head.  Of
;;; course, it makes sense after I read it again.  There is an art to expressing the
;;; constraints appropriately and using a convenient encoding for the rows.  The "map"
;;; notation with the row labels lowers the cognitive overhead a bit as it's more expressive
;;; than encoding the matrix directly.  I suppose there's some overhead converting back and
;;; forth?  I should investigate that later.  (Famous last words.)
;;;
;;; Overall, the Tarantella solution is a bit slower than the (translated) Norvig approach.
;;; And it's much slower than my bitsudoku interpretation of the Norvig algorithm.  The
;;; "run-bench" numbers are about 3.5s for Taran, 3.0s for Norvig, and under 1.0s for
;;; bitsudoku.  And the Taran solution is less robust about confirming solutions so it's not
;;; really a fair comparison.  On the other hand, there's a lot less code for the Taran
;;; solution so that's a huge advantage -- if you can formulate the constraints correctly.


;;; Constraints are label with rc/n, indicate an value assignment [[r c] n].  The set of
;;; contraints associated with the row label are unique encodings to satisfy the row, col,
;;; and subsquare "sector" for a given square rc location.

(def sudoku-constraints
  (into {} (for [i (range 9), j (range 9), n (range 1 10)]
             [[[i j] n]
              #{[i j] [:row i n] [:col j n] [:sector (quot i 3) (quot j 3) n]}])))


 (defn ch-value [^Character c]
    (case c
      \1 1
      \2 2
      \3 3
      \4 4
      \5 5
      \6 6
      \7 7
      \8 8 
      \9 9
      0))

;; Note the rc/n label convention
(defn grid->filled-cells [grid]
  (let [values (mapv ch-value (seq grid))]
    (set (for [i (range 9), j (range 9)
               :let [n (values (+ (* i 9) j))]
               :when (pos? n)]
           [[i j] n]))))


(defn filled-cells->values [cells]
  (let [m (into {} cells)]
    (vec (for [i (range 9)
               j (range 9)]
           (get m [i j] 0)))))

;; cells->values is much faster than filled-cells->values

;; cells is vector rc-index value pairs  [[r c] n]
;; Note: the rc/n notation matches the constraint label
(defn cells->values [cells]
  (persistent!
   (reduce (fn [tv [[r c] n]] (assoc! tv (sqi r c) n))
           (transient (vec (repeat sqcnt 0)))
           cells)))


;; returns solution cells in rc/n
(defn solve [grid]
  (first (t/dancing-links sudoku-constraints
                          :select-rows (grid->filled-cells grid)
                          :limit 1)))



;; translation cells back to values isn't slow -- that was my mistaken assumption
;;
;; do it on demand with my cells->values

;; SEM FIXME -- cheap test, not robust
;; cells is vector rc-index value pairs  [[r c] n]
(defn solved? [cells]
  (and (= (count cells) sqcnt)
       (empty? (into nil (comp (map peek) (filter zero?)) cells))))


(defn display
  "Display values as a 2D grid"
  [values]
  (let [width 1
        line (str/join "-+-" (repeat 3 (str/join (repeat (* 3 width) \-))))]
    (doseq [r rcs]
      (println (str/join (for [c rcs]
                       (format (str "%-" width "s%s")
                               (values (sqi r c))
                               (if (#{2 5} c) " | " "")))))
      (when (#{2 5} r) (println line)))))


(defn display-cells [cells]
  (display (cells->values cells)))


(def grid1 "003020600900305001001806400008102900700000008006708200002609500800203009005010300")
(def grid2 "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")
(def hard1 ".....6....59.....82....8....45........3........6..3.54...325..6..................")

;; example from Rosetta Code
(def rcgrid "394002670000300400500690020045000900600000007007000580010067008009008000026400735")

(def data-dir "resources/")

(defn solve-grids [solve grids]
  (assert (every? solved? (map solve grids))))

(defn run-bench
  ([] (run-bench solve))
  ([solve]
   ;;(unit-tests)
   (solve-grids solve (from-file (io/file data-dir "easy50grids.txt")))
   (solve-grids solve (from-file (io/file data-dir "top95.txt")))
   (solve-grids solve (from-file (io/file data-dir "hardest.txt")))
   true))



  
