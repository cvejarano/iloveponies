(ns p-p-p-pokerface)

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn rank [card]
  (let [[r _] card]
    (cond
     (Character/isDigit r)
       (Integer/valueOf (str r))

     :else
       (let [letter-map {\T 10 \J 11 \Q 12  \K 13 \A 14}]
         (get letter-map r)))))

(defn n-of-a-kind [n hand]
  (==
    n
    (apply max
           (vals
            (frequencies
             (map rank hand))))))

(defn pair? [hand]
  (n-of-a-kind 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind 4 hand))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (=
   (sort
    (vals
     (frequencies
      (map rank hand))))
   '(2 3)))

(defn two-pairs? [hand]
  (or
   (four-of-a-kind? hand)
   (= 2
      (get
       (frequencies
        (vals
         (frequencies
          (map rank hand))))
       2))))

; Exercise 9
(defn straight? [hand]

  ; Extract the ranks only
  (let [hand-ranks (map rank hand)]

    ; Auxiliary function for a given sequence.
    (defn straight?-aux [hand-ranks]
      (let [sorted-ranks (sort hand-ranks)]
        (=
         ; Build a [min, max] sequence
         (range
           (apply min sorted-ranks)
           (inc (apply max sorted-ranks)))

         sorted-ranks)))
    ; Try original sequence, or changing 1 by 14 or 14 by 1.
    (or
     (straight?-aux hand-ranks)
     (straight?-aux (replace {14 1} hand-ranks))
     (straight?-aux (replace {1 14} hand-ranks)))))


; Exercise 10
(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

; Exercise 11
(defn value [hand]
  (cond
   (straight-flush?  hand) 8
   (four-of-a-kind?  hand) 7
   (full-house?      hand) 6
   (flush?           hand) 5
   (straight?        hand) 4
   (three-of-a-kind? hand) 3
   (two-pairs?       hand) 2
   (pair?            hand) 1
   :else                   0))
