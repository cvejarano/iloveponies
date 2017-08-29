(ns one-function-to-rule-them-all)

; Ex 1
(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

; Ex 2
(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce str (interpose " " a-seq))))

; Ex 3
(defn my-interpose [x a-seq]
  (rest
   (reduce
     (fn [seq-aux elem] (concat seq-aux [x elem]))
     '()
     a-seq)))

;Ex 4
(defn my-count [a-seq]
  (if (empty? a-seq) 0
    (reduce (fn [x y] (inc x)) 0 a-seq)))

; Ex 5
(defn my-reverse [a-seq]
  (reduce
   (fn [seq-aux elem]
     (conj seq-aux elem))
   '()
   a-seq))

; Ex 6
(defn min-max-element [a-seq]
  (reduce
   (fn [[min-elem max-elem] elem]
     [(min min-elem elem) (max max-elem elem)])
   [(first a-seq) (first a-seq)]
   a-seq))

; Ex 7
(defn insert [sorted-seq n]
  (if (empty? sorted-seq) (conj '() n)
    (if (< n (first sorted-seq))
      (cons n sorted-seq)
      (cons (first sorted-seq)
            (insert (rest sorted-seq) n)))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

; Ex 8
(defn parity [a-seq]

  ; Auxiliary toggle function
  (defn toggle [a-set elem]
    (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))

  (reduce toggle #{} a-seq))

; Ex 9
(defn minus
  ([x] (- x))
  ([x y] (- x y)))

; Ex 10
(defn count-params [& more]
  (count more))

; Ex 11
(defn my-*
  ([] 1)
  ([& args]
   (reduce * args)))

; Ex 12
(defn pred-and
  ([]
   (fn [x] true))
  ([& preds]
   (fn [x]
     (and
      ((first preds) x)
      ((apply pred-and (rest preds)) x)))))


; Ex 13
(defn my-map [f a-seq]
  [:-])

; Temporal, for ex. 13
(defn my-map [f a-seq & more-seqs]
  ; Empty sequence: easy
  (if (empty? a-seq)
    '()
    ; A single sequence a-seq: recursion over a-seq
    (if (empty? more-seqs)
      (cons
       (f (first a-seq))
       (my-map f (rest a-seq)))

      ;)))

      ; More than one sequence: another recursion level
      (apply f (my-map first more-seqs)))))

;    (cons
;     (f (first a-seq))
;     (my-map
;      f
;      (rest a-seq)
;      (my-map rest more-seqs)))))


(my-map inc [1 2 3 4])                  ;=> (2 3 4 5)
(my-map + [1 2 3] [4 5 6] [7 8 9])
(my-map + [1 1 1] [1 1 1] [1 1 1])      ;=> (3 3 3)
(my-map vector [1 2 3] [1 2 3] [1 2 3]) ;=> ((1 1 1) (2 2 2) (3 3 3))
