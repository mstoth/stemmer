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

(defn pop-stemmer-on
  "This is an amalgam of a number of
  different functions: pop (it walks
  through the :word sequence using pop);
  drop-while (it drops items off while
  testing the sequence against drop-while);
  and maplist from Common Lisp (the
  predicate is tested against the entire
  current stemmer, not just the first
  element)."
  [predicate stemmer]
  (if (and (seq (:word stemmer)) (predicate stemmer))
    (recur predicate (pop-word stemmer))
    stemmer))

(defn member? [sequence item]
         (if (not (seq sequence))    
           nil
           (if (= (peek sequence) item)
             sequence
             (member? (pop sequence) item))))

(defn cond-member? [sequence item]
  (cond (not (seq sequence)) nil
        (= (peek sequence) item) sequence
        :else (recur (pop sequence) item)))

(def vowel-letter? #{\a \e \i \o \u})

(defn consonant? 
  "returns true if index is pointing to a consonant"
  ([sequence] (consonant? sequence (:index sequence)))
  ([sequence i] 
    (let [c (nth (:word sequence) i)]
      (cond (vowel-letter? c) false
            (= c \y) (if (zero? i) true 
                       (not (consonant? stemmer (dec i))))
            :else true))))
(def vowel? (complement consonant?))

(defn vowel-in-stem?
  "returns true if a vowel precedes the index"
  [stemmer]
  (let [j (get-index stemmer)]
    (loop [i 0]
      (cond (> i j) false
            (consonant? stemmer i) (recur (inc i))
            :else true ))))

(defn double-c?
  "returns true if a double consonant" 
  ([stemmer] 
    (double-c? stemmer (:index stemmer)))
  ([stemmer j] 
    (and (<= j 1) 
         (= (nth (:word stemmer) j)
            (nth (:word stemmer) (dec j)))
         (consonant? stemmer j))))


    
      

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
