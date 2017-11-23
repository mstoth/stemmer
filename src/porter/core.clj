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


(defn m
  "Measures the number of consonant sequences between
  the start of word and position j. If c is a consonant
  sequence and v a vowel sequence, and <...> indicates
  arbitrary presence,
    <c><v>       -> 0
    <c>vc<v>     -> 1
    <c>vcvc<v>   -> 2
    <c>vcvcvc<v> -> 3
    ...
  "
  [stemmer]
  (let [
        j (get-index stemmer)
        count-v (fn [n i]
                  (cond (> i j) [:return n i]
                        (vowel? stemmer i) [:break n i]
                        :else (recur n (inc i))))
        count-c (fn [n i]
                  (cond (> i j) [:return n i]
                        (consonant? stemmer i) [:break n i]
                        :else (recur n (inc i))))
        count-cluster (fn [n i]
                        (let [[stage1 n1 i1] (count-c n i)]
                          (if (= stage1 :return)
                            n1
                            (let [[stage2 n2 i2] (count-v (inc n1) (inc i1))]
                              (if (= stage2 :return)
                                n2
                                (recur n2 (inc i2)))))))
        [stage n i] (count-v 0 0)
        ]
    (if (= stage :return)
      n
      (count-cluster n (inc i)))))    
      
(defn ends?
  "true if the word ends with s."
  [stemmer s]
  (let [word (subword stemmer), sv (vec s), j (- (count word) (count sv))]
    (if (and (pos? j) (= (subvec word j) sv))
      [(assoc stemmer :index (dec j)) true]
      [stemmer false])))

(defmacro if-ends?
  "Instead of the function ends?, I'm using this:
  (if-ends? x (make-stemmer \"names\") [\\s]
            (println x \"no longer has a plural suffix\")
            (println x \"never had a plural suffix\"))
  "
  ([var stemmer end true-expr]
   (let [vend (vec end)]
     `(let [stemmer# ~stemmer,
            end# ~vend,
            word# (subword stemmer#),
            j# (- (count word#) (count end#))]
        (if (and (pos? j#) (= (subvec word# j#) end#))
          (let [~var (assoc stemmer# :index (dec j#))]
            ~true-expr)
          stemmer#))))
  ([var stemmer end true-expr false-expr]
   (let [vend (vec end)]
     `(let [stemmer# ~stemmer,
            end# ~vend,
            word# (subword stemmer#),
            j# (- (count word#) (count end#))]
        (if (and (pos? j#) (= (subvec word# j#) end#))
          (let [~var (assoc stemmer# :index (dec j#))]
            ~true-expr)
          (let [~var stemmer#]
            ~false-expr))))))


(defn set-to
  "This sets the last j+1 characters to x and readjusts the length of b."
  [stemmer new-end]
  (reset-index (into (subword stemmer) new-end)))

(defn r
  "This is used further down."
  [stemmer orig-stemmer s]
  (if (pos? (m stemmer))
    (set-to stemmer s)
    orig-stemmer))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
