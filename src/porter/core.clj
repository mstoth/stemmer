(ns porter.core)
(use 'clojure.repl) ;; so doc is available

(defstruct stemmer :word :index)

(defn make-stemmer
  "This returns a stemmer structure for a given word"
  [w]
  (struct stemmer (vec w) (dec (count w))))

(defn reset-index
  "creates a new stemmer from a vectored word"
  [wv] (struct stemmer wv (dec (count wv))))

(defn get-index 
  "returns a valid index for a stemmer"
  [s] (if-let [j (:index s)]
        (min j (dec (count (:word s))))
        (dec (count (:word s)))))

(defn subword 
  "returns the word before the index"
  [s] 
  (let [b (:word s) j (inc (get-index s))]
    (if (< j (count b))
       (subvec b 0 j)
      b)))

(defn index-char
  "returns the character pointed to by the index"
  [s] (nth (:word s) (get-index s)))

(defn pop-word
  "returns a stemmer with the last character removed"
  [s]
  (assoc s :word (pop (:word s))))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
