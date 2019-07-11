(ns structured-data)

(defn do-a-thing [x]
  (let [ x (+ x x)]
    (Math/pow x x)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))


(defn cutify [v]
  (conj v "v3"))

(defn spiff-destructuring [v]
   (let [[first second third] v]
     (+ first third)))


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [value (- x2 x1)]
      (neg? value) (* -1 value) value)))


(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [value (- y2 y1)]
      (neg? value) (* -1 value) value)))


(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))


(defn area [rectangle]
  (* (width rectangle) (height rectangle)))


(defn contains-point? [rectangle point]
  (let [[x y] point]
    (let [[[x1 y1] [x2 y2]] rectangle]
      (or (= x1 y1 x2 y2 x y) 
          (and (and (pos? x) (<= x (width rectangle)) 
                    (and (pos? y) (<= y (height rectangle)))))))))
       

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer [x1 y1])) (contains-point? outer [x2 y2])))

(defn title-length [book]
  (count (:title book)))


(defn author-count [book]
  (count (:authors book)))


(defn multiple-authors? [book]
  (> (count (:authors book)) 1))


(defn add-author [book new-author]
  (let [{authors :authors} book]
    assoc book :authors (conj authors new-author)))


(defn alive? [author]
  (not (contains? author :death-year)))


(defn element-lengths [collection]
  (map count collection))


(defn second-elements [collection]
  (let [take-second (fn [x] (get x 1))]
    (map take-second collection)))


(defn titles [books]
  (map :title books))


(defn monotonic? [a-seq]
  (or (apply >= a-seq) (apply <= a-seq)))


(defn stars [n]
  (apply str (repeat n "*")))
    

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (not (= (count (set a-seq)) (count a-seq))))


(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors #{china}})
(def wild-seed {:title "Wild Seed", :authors #{octavia}})
(def embassytown {:title "Embassytown", :authors #{china}})
(def little-schemer {:title "The Little Schemer"
                     :authors #{friedman, felleisen}})

(def books [cities, wild-seed, embassytown, little-schemer])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn has-author? [book author]
  (contains? (set (:authors book)) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))


(authors [cities, wild-seed])              ;=> #{china, octavia}
(authors [cities, wild-seed, embassytown]) ;=> #{china, octavia}
(authors [little-schemer, cities])         ;=> #{china, friedman, felleisen}

(defn all-author-names [books]
  (apply clojure.set/union (map :name (authors books))))

(all-author-names books)
;=> #{"Matthias Felleisen" "China Miéville"
;     "Octavia E. Butler" "Daniel Friedman"}
(all-author-names [cities, wild-seed])
;=> #{"China Miéville" "Octavia E. Butler"}
(all-author-names []) ;=> #{}

(defn author->string [author]
  (let [name (author :name)
        death (author :death-year)
        birth (author :birth-year)
        years (str " (" birth " - " death ")")]
      
    (str name (if (contains? author :birth-year) years))))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))
  

(defn book->string [book]
  (let [title (book :title)
        authors (authors->string (book :authors))]
    (str title ", written by " authors)))


(defn books->string [books]
  (let [amount (count books)]
    (apply str 
      (cond 
        (= amount 0) "No books. " 
        (= amount 1) "1 book. "
        :else (str amount " books. "))
      (interpose ", " (map book->string books)))))
      


(books->string []) ;=> "No books."
(books->string [cities])
;=> "1 book. The City and the City, written by China Miéville (1972 - )."
(books->string [little-schemer, cities, wild-seed])
;=> "3 books. The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen. 
;The City and the City, written by China Miéville (1972 - ). Wild Seed, written by Octavia E. Butler (1947 - 2006)."

(has-author? cities china)             ;=> true
(has-author? cities felleisen)         ;=> false
(has-author? little-schemer felleisen) ;=> true
(has-author? little-schemer friedman)  ;=> true
(has-author? little-schemer octavia)   ;=> false

(defn books-by-author [author books]
  (filter (fn [book author] (contains? (:authors book)) author))) books

(books-by-author china books)   ;=> (cities embassytown)
(books-by-author octavia books) ;=> (wild-seed)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
