(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (+
   (get v 0)
   (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[a _ b]]
  (+ a b))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 _][x2 _]]]
  (- x2 x1))

(defn height [[[_ y1][_ y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (==
   (width rectangle)
   (height rectangle)))

(defn area [rectangle]
  (*
   (width rectangle)
   (height rectangle)))

(defn contains-point? [[[x1 y1][x2 y2]] [x y]]
  (and
   (<= x1 x x2)
   (<= y1 y y2)))

(defn contains-rectangle? [outer [p1 p2]]
  (and
   (contains-point? outer p1)
   (contains-point? outer p2)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (assoc book :authors
    (conj
     (:authors book)
     new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [helper (fn [x] (get x 1))]
    (map helper collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or
   (apply >= a-seq)
   (apply <= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (not=
   (count a-seq)
   (count (set a-seq))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
 (str
   (:name author)
   (if (:birth-year author)
     (do
       (str
         " ("
         (:birth-year author)
         " - "
         (:death-year author)
         ")")))))

(defn authors->string [authors]
  (apply
   str
   (interpose
    ", "
    (map author->string authors))))

(defn book->string [book]
  (str
   (:title book)
   ", written by "
   (authors->string (:authors book))))

(defn books->string [books]
  (str
   (case (count books)
         0 "No books"
         1 "1 book. "
         (str (count books) " books. "))
   (apply str
          (interpose ". "
                     (map book->string books)))
   "."))

(defn books-by-author [author books]
  (filter
   (fn [book]
     (has-author? book author))
   books))

(defn author-by-name [name authors]
  (first
    (filter
     (fn [author]
       (= (:name author) name))
     authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (first
   (apply
     (fn [& x] (or x))
     (map alive? (:authors book)))))


(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
