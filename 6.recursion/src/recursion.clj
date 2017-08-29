(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
    (*
     (first coll)
     (product (rest coll)))))

(defn singleton? [coll]
  (and
   (not (empty? coll))
   (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (empty? (rest coll))
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (if (empty? (rest a-seq)) (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))


(defn seq-max [seq-1 seq-2]
  (if
    (> (count seq-1) (count seq-2)) seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))


(defn my-filter [pred? a-seq]
  (if (empty? a-seq) a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (= (first a-seq) elem)
     true
   :else
     (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons
       (first a-seq)
       (my-take-while pred? (rest a-seq)))
      '())))


(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (not (pred? (first a-seq)))
      a-seq
      (my-drop-while pred? (rest a-seq)))))


(defn seq= [a-seq b-seq]
  ; Both are empty.
  (if (and (empty? a-seq) (empty? b-seq))
    true
    ; Only one is empty.
    (if (or (empty? a-seq) (empty? b-seq))
      false
      ; Test first element an the rest recursively.
      (if (= (first a-seq) (first b-seq))
        (seq= (rest a-seq) (rest b-seq))
        false))))


(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (conj
     (my-map f (rest seq-1) (rest seq-2))
     (f (first seq-1) (first seq-2)))))


(defn power [n k]
  (if
    (== k 0) 1
    (* n (power n (dec k)))))


(defn fib [n]
  (cond
   (<= n 0) 0
   (== n 1) 1
   :else (+
          (fib (- n 1))
          (fib (- n 2)))))


(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (conj
     (my-repeat (dec how-many-times) what-to-repeat)
     what-to-repeat)))


(defn my-range [up-to]
  (if
    (<= up-to 0) '()
    (conj
     (my-range (dec up-to))
     (dec up-to))))


(defn tails [a-seq]
  (if (empty? a-seq) '(())
    (cons
     (seq a-seq)
     (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse
    (map reverse
     (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if
    (empty? a-seq) '(())
    (map concat
        (rest (tails a-seq))
        (rest (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs

    (let [head (first a-seq)]

      (my-frequencies-helper
       (assoc freqs head
         (inc (or (get freqs head) 0)))

       (rest a-seq)))))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn un-frequencies [a-map]
  (if (empty? a-map) '()
    (let [head (first a-map)]
      (concat
       (repeat (second head) (first head))
       (un-frequencies (rest a-map))))))


(defn my-take [n coll]
  (if
    (or (empty? coll) (<= n 0)) '()

    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (empty? coll) '()
    (if (<= n 0) coll
      (my-drop (dec n) (rest coll)))))


(defn halve [a-seq]
  (let [len1 (int (/  (count a-seq) 2))]

    [(my-take len1 a-seq) (my-drop len1 a-seq)]
  ))


(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq) b-seq
    (if (empty? b-seq) a-seq
      (if (< (first a-seq) (first b-seq))
        (cons (first a-seq)
              (seq-merge (rest a-seq) b-seq))
        (cons (first b-seq)
              (seq-merge a-seq (rest b-seq)))))))


(defn merge-sort [a-seq]
  (if (empty? (rest a-seq))
    a-seq

    (apply seq-merge
           (map merge-sort (halve a-seq)))))



(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

