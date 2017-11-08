(ns porter.core-test
  (:require [clojure.test :refer :all]
            [porter.core :refer :all]))

(deftest test4stemmer
  (testing "stemmer struct exists"
    (is (= (type stemmer) clojure.lang.PersistentStructMap$Def))))
(deftest test-making-stemmers
  (testing "making stemmer from word"
           (is (= clojure.lang.PersistentStructMap (type (make-stemmer "looking"))))))
(deftest test-accessing-word
  (testing "accesses the word of the structure"
           (is (= "looking" (:word (make-stemmer "looking"))))))

(deftest test-reset-index
  (testing "resets index to end of vectored word"
           (let [s (make-stemmer "all")]
                  (is (= 2 (:index (reset-index (:word s))))))))

(deftest test-get-index
  (testing "getting an index from a stemmer" 
           (let [s (make-stemmer "all")]
             (is (= 2 (get-index s))))))


(deftest test-subword
  (testing "getting the subword"
           (let [s (struct stemmer "looking" 3)]
             (is (= "look" (subword s))))))

(run-tests)