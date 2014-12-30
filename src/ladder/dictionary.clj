(ns ladder.dictionary
  (require [clojure.string :as str]
           [clojure.test :refer [is testing with-test]]))

(def filename "/usr/share/dict/words")

(defn parse
  "Parse dictionary file into list by splitting on new lines"
  [file]
  (-> file
      slurp
      (str/split #"\n")))

(with-test
  (defn- valid?
    "Returns true if the word is valid.

    A valid word is non-empty, not a proper noun (no capital letters),
    and is not a possessive (no apostrophes)."
    [word]
    (not (or (= 0 (count word))
             (Character/isUpperCase (first word))
             (.contains word "'"))))

  (is (valid? "cat"))
  (testing "invalid words"
    (is (not (valid? "")))
    (is (not (valid? "cat's")))
    (is (not (valid? "Maru")))))

(with-test
  (defn clean
    "Filter invalid words from given list"
    [words]
    (filter valid? words))

  (is (= (clean ["cat" "cat's" "Maru"]) ["cat"])))

; A list of all clean words
(def all (-> filename
             parse
             clean))

(with-test
  (defn length-n
    "Return only those of the given words with the specified length"
    [words n]
    (filter #(= n (count %)) words))

  (is (= (length-n ["abc" "defgh" "hij"] 3)
         ["abc" "hij"])))

(with-test
  (defn hamming
    [one two]
    (loop [current-one one
           current-two two
           distance 0]
      (if (empty? current-one)
        (if (empty? current-two) distance) ; return nil for unequal strings
        (recur (rest current-one)
               (rest current-two)
               (+ distance (if (= (first current-one) (first current-two))
                             0
                             1))))))

  (is (= (hamming "karolin" "kathrin") 3))
  (is (= (hamming "karolin" "kerstin") 3))
  (is (= (hamming "1011101" "1001001") 2))
  (is (= (hamming "2173896" "2233796") 3))
  (testing "unequal lengths"
    (is (nil? (hamming "123" "1234")))))
