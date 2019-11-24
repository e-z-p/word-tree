(ns word-tree.suffix-tree
  (:require [clojure.string :as str]))

(defn sentence-split
  "Splits a body of text on delimiters: ('.'|'!'|'?')"
  [text]
  (map str/trim
       (re-seq #"\(?[^\.\?\!]+[\.!\?]\)?" text)))           ; In backend use "clojure-opennlp" lib to tokenize text into sentences

(defn get-fork
  "Returns the index where two strings diverge."
  [a b]
  (loop [a a
         b b
         i 0]
    (if (or (some empty? [a b])
            (not= (first a) (first b)))
      i
      (recur (rest a)
             (rest b)
             (inc i)))))

(defn get-prefix
  "Gets substring of S from zero up to index I"
  [s i]
  (subs s 0 i))

(defn get-suffix
  "Gets rest of string S after and including the character at index I"
  [s i]
  (subs s i))

(defn cut-at
  "Cuts string at the given index."
  [s i]
  [(get-prefix s i) (get-suffix s i)])

(defn get-branch
  "Finds the index of the string whose first char matches the target char."
  [coll target]
  (loop [coll coll
         i 0]
    (let [ch (first (:text (first coll)))]
      (cond
        (empty? coll) nil
        (= target ch) [i (first coll)]
        :else (recur (rest coll) (inc i))))))

(defn suffix-tree
  ([] {:text "", :branches []})
  ([p] {:text p, :branches []})
  ([p b] {:text p, :branches b}))

(defn prune
  "Removes empty trees from a collection."
  [coll]
  (letfn [(is-bare? [t] (every? empty? (vals t)))]
    (remove #(or (nil? %) (is-bare? %)) coll)))

(defn vec-remove
  "Removes element from vector."
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn into-tree
  "Adds string, s, to suffix tree, t."
  ([s] (suffix-tree s))
  ([s t]
   (let [{text :text, branches :branches} t
         fork (get-fork text s)
         [prefix suffix-a] (cut-at text fork)
         suffix-b (get-suffix s fork)]
     (if (empty? suffix-b)
       t
       (let [[i n] (get-branch branches (first suffix-b))]
         (suffix-tree prefix
                      (prune (concat [(suffix-tree suffix-a)
                                      (if-not n
                                        (suffix-tree suffix-b)
                                        (into-tree suffix-b n))]
                                     (if-not n
                                       branches
                                       (vec-remove (vec branches) i))))))))))

; 1. split text into sentences
; 2. filter sentences that contain the search-term
; 3. for each sentence, chop off everything before the search-term
; 4. build tree
; pattern to match whole word (def patter (re-pattern (str "\\b" search-term "\\b")))
(defn gen-tree                                              ;; Filter out regex match whole word instead of substring, something like #"\b(re-pattern search-term)\b"
  "Builds a word-tree out of body of text and a search-term."
  [sentences search-term]
  (let [patter (re-pattern (str "(?i)\\b" search-term "\\b")) ; insensitive match of whole words
        phrases (map #(subs % (.indexOf % search-term)) (remove #(nil? (re-find patter %)) sentences))] ; This ignores  multiple occurrences in the same sentence
    (reduce #(into-tree %2 %1) (suffix-tree search-term) phrases)))

(defn re-each-match-to-eos
  "Get position of regex match."
  [re s]
  (let [re (js/RegExp. (.-source re) "gi" )]
    (loop [res []]
      (if-let [m (.exec re s)]
        (recur (conj res (subs s (.-index m))))
        res))))

(defn get-suffixes
  [sentences prefix]
  (let [re (re-pattern (str "\\b" prefix "\\b"))]
    (mapcat #(re-each-match-to-eos re %) sentences)))

(defn gen-suffix-tree
  "Generates a suffix tree from a collection of strings."
  [coll pre]
  (reduce #(into-tree %2 %1) (suffix-tree pre) (get-suffixes coll pre)))

(defn render-tree
  "Renders a suffix tree as html."
  [t]
  (let [{text :text, branches :branches} t]
    ^{:key (gensym)} [:ul {:class "suffix-tree"} text (map render-tree branches)]))
