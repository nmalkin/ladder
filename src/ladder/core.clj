(ns ladder.core
  (require clojure.set
           [clojure.test :refer [is testing with-test]]
           [ladder.dictionary :as dictionary])
  (:gen-class))

(with-test
  (defn- mask-nth
    "Masks the given letter of the provided word"
    [word n]
    (str (subs word 0 n)
         "_"
         (subs word (inc n))))

  (is (= (mask-nth "word" 0) "_ord"))
  (is (= (mask-nth "word" 3) "wor_")))

(with-test
  (defn- mask
    "Given a word, returns a list, with each letter sequentially masked"
    [word]
    (loop [n 0
           masks []]
      (if (= n (count word))
        masks
        (recur (inc n)
               (conj masks (mask-nth word n))))))

  (is (= (mask "word") ["_ord" "w_rd" "wo_d" "wor_"])))


(with-test
  (defn match-map
    "Returns a map from a word mask to a set of all matching words.

    There will be a mask for each letter in each input word.
    e.g., if the input is [ab cd], the map will have the keys [_b a_ _d c_]

    Each key will map to the strings in the input that follow the pattern.
    e.g., _b -> #{ab cb}

    The keys in the map are strings, the values are sets."
    [words]
    (loop [words words
           nodes {}]
      (if (empty? words)
        nodes
        (let [current-word (first words)
              current-masks (mask current-word)]
          (recur (rest words)
                 ; For each mask, add current-word to nodes with mask as the key
                 (reduce (fn [nodes mask]
                           (assoc nodes mask (conj (get nodes mask #{})
                                                   current-word)))
                         nodes
                         current-masks))))))

  (is (= (match-map ["ab"]) {"_b" #{"ab"}
                             "a_" #{"ab"}}))
  (is (= (match-map ["ab" "cb"])
         {"_b" #{"ab" "cb"}
          "a_" #{"ab"}
          "c_" #{"cb"}})))

(with-test
  (defn- get-neighbors
    "Provided an adjacency map, returns all valid neighbors of the given word.
    i.e., all words that can be made by changing one letter in the given word

    An adjacency map is of the form produced by match-map."
    [adjacency origin]
    (->> origin
         mask
         (map #(get adjacency %)) ; get neighbors from each mask
         (map #(disj % origin)) ; filter out the current word
         (apply clojure.set/union))) ; join results into a single set

  (is (= (get-neighbors {"_b" #{"ab" "cb"} "a_" #{"ab" "ac"}} "ab")
         #{"ac" "cb"})))

; Node represents a vertex in the graph used by A* search.
; It contains three crucial pieces of information:
;   1. value (payload)
;   2. distance, the past-path cost - g(x)
;   3. weight, the future-path cost - f(x) = g(x) + h(x)
(defrecord Node [value distance weight]
  ; Nodes are compared based on their weight f(x).
  ; The Comparable interface is implemented so that they can be used inside a
  ; Priority Queue.
  Comparable
  (compareTo [this other]
    (- (:weight this) (:weight other))))

(with-test
  (defn a*
    "Given an adjacency map of edges, finds the shortest path from start to end
    using the A* search algorithm"
    [start end adjacency]
    (let
      ; Hamming distance is used as the heuristic to estimate how far off from
      ; the answer we are.
      [heuristic (fn [word] (dictionary/hamming word end))
       ; The start word becomes the root node, with 0 as the distance to origin
       root (Node. start 0 (heuristic start))
       ; Nodes that need visited are stored in a priority queue.
       upcoming (doto (java.util.PriorityQueue.)
                  (.add root))]
      (loop [; Keep track of visited words in a set
             visited #{}
             ; path maps a word to its previous word (how it was found).
             ; We can follow these links to trace the path from the origin.
             path {start nil}]
        ; Start each round by removing the lowest-priority (most promising)
        ; element from the priority queue
        (if-let [current-node (.poll upcoming)]
          (let [current-word (:value current-node)
                current-dist (:distance current-node)
                next-node (fn [word]
                            (Node. word
                                   ; Any new node's distance to the origin is
                                   ; one greater than the current node's.
                                   (inc current-dist)
                                   ; A node's weight (in A*) is the sum of its
                                   ; distance to origin and its heuristic score.
                                   (+ 1 current-dist (heuristic word))))]
            ; Start by checking if the current word is the one we're looking for
            (if (= end current-word)
              ; If it is, return all the links we've collected.
              ; They include the correct path.
              path
              (let [; identify the neighbors of the current word
                    neighbors (get-neighbors adjacency current-word)
                    ; We're only interested in those we haven't seen yet.
                    unvisited-neighbors (filter #(not (contains? visited %))
                                                neighbors)
                    ; Create a node for each one of them
                    neighbor-nodes (map next-node unvisited-neighbors)]
                ; Add each new neighbor node to the queue to visit
                (dotimes [n (count neighbor-nodes)]
                  (.add upcoming (nth neighbor-nodes n)))
                (recur
                  ; Mark the current word as visited
                  (conj visited current-word)
                  ; Record the current word as previous to each of the neighbors
                  (reduce (fn [path neighbor] (assoc path neighbor current-word))
                          path
                          unvisited-neighbors)))))))))

  (is (= (a* "ab" "cd" {"_b" #{"ab" "cb"}
                        "a_" #{"ab"}
                        "c_" #{"cb" "cd"}})
         {"ab" nil "cb" "ab" "cd" "cb"})
      "the minimum necessary")
  (is (= (a* "ab" "cd" {"_b" #{"ab" "cb" "zb"}
                        "a_" #{"ab" "az"}
                        "c_" #{"cb" "cd" "cz"}
                        "_z" #{"az" "cz"}})
         {"ab" nil "cb" "ab" "cd" "cb" ; relevant path
          "az" "ab" "zb" "cb" "cz" "cb"}) ; irrelevant path
      "irrelevant data")
  (is (clojure.set/subset?
        #{["ab" nil] ["cb" "ab"] ["cd" "cb"]}
        (set (a* "ab" "cd" {"_b" #{"ab" "cb" "zb"}
                            "a_" #{"ab" "az"}
                            "c_" #{"cb" "cd" "cz"}
                            "_z" #{"az" "cz"}})))
      "irrelevant data")
  (is (clojure.set/subset?
        #{["ab" nil] ["ad" "ab"] ["ed" "ad"]}
        (set (a* "ab" "ed" {"_b" #{"ab" "cb"}
                            "a_" #{"ab" "ad"}
                            "c_" #{"cb" "cd"}
                            "_d" #{"ad" "cd" "ed"}})))
      "shortest path"))

(with-test
  (defn reconstruct-path
    "Given a map from node to previous node (where the root maps to nil)
    and a finish point, returns the path from start to finish."
    [links from]
    (loop [head from
           path nil]
      (if (nil? head)
        path
        (recur (get links head)
               (conj path head)))))

  (is (= (reconstruct-path {1 nil 2 1 3 2} 3) (list 1 2 3)))
  (is (= (reconstruct-path {"ab" nil "cb" "ab" "cd" "cb"} "cd")
         (list "ab" "cb" "cd"))))

(with-test
  (defn ladder
    "Finds the path from start to end using words from the dictionary.

    Returns a sequential list of words in the path or nil if there is none."
    [start end]
    (if (= (count start) (count end)) ; only defined for equal-length words
      (let [words (dictionary/length-n dictionary/all (count start))
            adjacency (match-map words)]
        (if-let [path (a* start end adjacency)]
          (reconstruct-path path end)))))

  (testing "known paths. WARNING: system dictionaries may differ"
    (is (= (ladder "bank" "bark") (list "bank" "bark")))
    (is (= (ladder "band" "bark") (list "band" "bank" "bark")))
    (is (= (ladder "mask" "past") (list "mask" "mast" "past")))
    (is (= (ladder "mark" "cash") (list "mark" "mask" "cask" "cash"))))
  (testing "no known path. WARNING: system dictionaries may differ"
    (is (nil? (ladder "paths" "exist")))
    (is (nil? (ladder "abc" "xyz"))))
  (testing "invalid inputs"
    (is (nil? (ladder "different" "length")))))

(defn- read-word
  "Read and return a line from stdin"
  [title]
  (do (print title)
      (flush)
      (try (-> (read-line) clojure.string/trim)
           (catch Exception e (System/exit 0)))))

(defn -main
  "Print ladders based on user input.
  EOF to exit"
  []
  (let [start (read-word "start: ")
        end (read-word "end: ")]
    (if-let [path (ladder start end)]
      (println path)
      (println (str "No path from " start " to " end)))
    (recur)))
