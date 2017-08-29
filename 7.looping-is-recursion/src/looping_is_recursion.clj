(ns looping-is-recursion)

(defn power [n k]
  ((fn [acc n k]
     (if (<= k 0) acc
       (recur (* acc n) n (dec k))))

    1 n k))

(defn last-element [seq1]
  (if (empty? seq1)
    nil
    (if (empty? (rest seq1))
      (first seq1)
      (recur (rest seq1)))))

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


(defn find-first-index [pred seq-1]
  (loop [ind 0
         seq-aux seq-1]
    (if (empty? seq-aux)
      nil
      (if (pred (first seq-aux))
        ind
        (recur (inc ind) (rest seq-aux))))))


(defn avg [a-seq]
  (loop [sum 0
         n 0
         seq-aux a-seq]
    (if (empty? seq-aux)
      (if (not= n 0)
        (/ sum n)
        0)
      (recur (+ sum (first seq-aux))
             (inc n)
             (rest seq-aux)))))


(defn parity [a-seq]

  ; Auxiliary toggle function
  (defn toggle [a-set elem]
    (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))

  ; Actual loop
  (loop [result-set #{}
         seq-aux a-seq]
    (if (empty? seq-aux)
      result-set
      (recur (toggle result-set (first seq-aux))
             (rest seq-aux)))))


(defn fast-fibo [n]
  (if (<= n 0) 0
    (if (== n 1) 1
      (loop [f-n-2 0
             f-n-1 1
             nn (- n 2)]
        (if (<= nn 0)
          (+ f-n-1 f-n-2)
          (recur f-n-1 (+ f-n-1 f-n-2) (dec nn)))))))

(defn cut-at-repetition [a-seq]
  (loop [aux-seq a-seq
         aux-set #{}
         result []]
    (let [head (first aux-seq)]
      (if (or
           (empty? aux-seq)
           (contains? aux-set head))
        result
        (recur (rest aux-seq)
               (conj aux-set head)
               (conj result head))))))

