(ns structured-data)

(defn do-a-thing [x]
  (let [double-x (+ x x)]
    (Math/pow double-x double-x)
))


(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3")
  )

(cutify ["a" "b"])

(defn spiff-destructuring [[first second third]]
    (+ first third))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[left] [right]]]
  (- right left))

(width [[0 0] [3 4]])

(defn height [[[_ bottom] [_ top]]]
  (- top bottom))

(height [[0 0] [3 4]])

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(square? [[0 0] [3 3]])

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(area [[0 0] [2 2]])

(defn contains-point? [[[left bottom] [right top]] [x y]]
  (and (<= left x right) (<= bottom y top)))

(contains-point? [[0 0] [2 2]] [1 2])

(defn contains-rectangle? [outer [corner-1 corner-2]]
  (and
    (contains-point? outer corner-1)
    (contains-point? outer corner-2)))

(contains-rectangle? (rectangle [0 0] [3 3])
                     (rectangle [1 1] [2 2]))
(contains-rectangle? (rectangle [0 0] [2 2])
                     (rectangle [1 1] [3 3]))

(defn title-length [book]
    (count (:title book)))


(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [curr-authors (:authors book)]
    (assoc book :authors (conj curr-authors new-author))))

(add-author {:authors ["lauri"]} "mike")

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(element-lengths ["foo" "bar" "" "quux"])

(defn second-elements [collection]
  (let [second (fn [vec] (get vec 1))]
    (map second collection)))

(second-elements [[1 2] [2 3] [3 4]])

(use 'structured-data)

(defn titles [books]
  (map :title books))

(set '("foo" "bar" "bar"))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(monotonic? [5 7 4])

(defn stars [n]
  (apply str (repeat n "*")))


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(toggle #{:a :b :c} :d)
(toggle #{:a :b :c} :a)

(defn contains-duplicates? [a-seq]
  (not (= (count (set (set a-seq))) (count a-seq))))

(contains-duplicates? [1 1 2 3 -40])
(contains-duplicates? [1 2 3 -40])
(contains-duplicates? [1 2 3 "a" "a"])


(defn old-book->new-book [book]
  (let [old-authors (:authors book)]
    (assoc book :authors (set old-authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        dob (:birth-year author)
        dod (:death-year author)
        years (if (nil? dob)
                ""
                (str " (" dob " - "  dod ")"))]
    (str name years)))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))


(defn book->string [book]
  (str
    (:title book)
    ", written by "
    (authors->string (:authors book))))

(defn books->string [books]
  (let [n (count books)
        books (apply str (interpose ". " (map book->string books)))]
    (case n
      0 "No books."
      1 (str "1 book. " books ".")
      (str n " books. " books "."))))

(defn has-author-by-name [book name]
  (contains? (set (map :name (:authors book))) name))

(defn books-by-author [author books]
  (let [by-author (fn [b] (contains? (:authors b) author))]
    (filter by-author books)))

(defn has-name [author name]
  (= (:name author) name))



(defn author-by-name [name authors]
  (let [authors (filter (fn [author] (has-name author name)) authors)]
    (first authors)))

(defn alive? [author]
  (nil? (:death-year author)))

(defn living-authors [authors]
  (filter alive? authors))


(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))


(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
