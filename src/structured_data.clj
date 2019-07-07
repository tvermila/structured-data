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
  :-)

(defn has-author? [book author]
  :-)

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
