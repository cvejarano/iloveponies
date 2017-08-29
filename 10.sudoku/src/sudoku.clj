(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

; Ex 1
(defn value-at [board coord]
  (get-in board coord))

; Ex 2
(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

; Ex 3
(defn row-values [board coord]
  (let [[row _] coord]
    (set
     (get board row))))

; Ex 4
(defn col-values [board coord]
  (let [[_ col] coord]
    (set
     (map (fn [row] (get row col)) board))))

; Ex 5
(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

; Ex 6
(defn block-values [board coord]
  (defn aux-range [x]
    (cond
     (<= 0 x 2) [0 1 2]
     (<= 3 x 5) [3 4 5]
     (<= 6 x 8) [6 7 8]
     :else [0]))

  (defn aux-pairs [rows cols]
    (for [row rows
          col cols]
      [row col]))


  (let [[row col] coord]
    (set
     (map
      (partial value-at board)
      (aux-pairs (aux-range row) (aux-range col))))))

; Ex 7
(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (clojure.set/difference
     all-values
     (row-values board coord)
     (col-values board coord)
     (block-values board coord))))

; Ex 8
(defn filled? [board]
  (not
   (contains?
    (set (apply concat board))
    0)))


; Ex 9
(defn rows [board]
  (mapv
   (fn [x] (row-values board [x 0]))
   (range 9)))

(defn cols [board]
  (mapv
   (fn [x] (col-values board [0 x]))
   (range 9)))


; Ex 10
(defn blocks [board]
  (mapv
   (partial block-values board)
   (for [row [0 3 6]
         col [0 3 6]]
     [row col])))

; Ex 11
(defn valid? [func board]
  (apply
   = true
    (map
     (partial = (set (range 1 10)))
     (func board))))

(defn valid-rows? [board]
  (valid? rows board))

(defn valid-cols? [board]
  (valid? cols board))

(defn valid-blocks? [board]
  (valid? blocks board))


; Ex 12
(defn valid-solution? [board]
  (and
   (valid-rows? board)
   (valid-cols? board)
   (valid-blocks? board)))


; Ex 13
(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

; Ex 14
(defn find-empty-point [board]
  (first
    (for [row (range 9)
          col (range 9)
          :when (not (has-value? board [row col]))]
      [row col])))


; Ex 15
(defn solve-helper [board]
  (if (filled? board)

    (if (valid-solution? board)
      board
      '())

    (let [coord (find-empty-point board)
          options (valid-values-for board coord)]
      (for [elem options
            solution (solve-helper
                      (set-value-at board coord elem))]
        solution))))


(defn solve [board]
  (solve-helper board))
