(ns word-tree.suffix-tree
  (:require [clojure.string :as str]))

(defn sentence-split
  "Splits a body of text on delimiters: ('.'|'!'|'?')"
  [text]
  (map str/trim
       (re-seq #"\(?[^\.\?\!]+[\.!\?]\)?" text)))           ; In backend use "clojure-opennlp" lib to tokenize text into sentences

;       \W+ matches any succession of non-word characters

(defn get-fork
  "Returns the index where two strings diverge."
  [s1 s2]
  (loop [a s1
         b s2
         i 0]
    (if (or (or (empty? a) (empty? b))
            (not= (first a) (first b)))
      i                                                     ;; get len of matchuptobeginningoflastword of s1[0:i]
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

(defn suffix-tree
  ([] {:text "", :branches []})
  ([p] {:text p, :branches []})
  ([p b] {:text p, :branches b}))

(defn vec-remove
  "Removes element from vector."
  [coll pos]
  (vec (concat (subvec (vec coll) 0 pos) (subvec (vec coll) (inc pos)))))

(defn first-word
  "Gets substring of s up to end of first word."
  [s]
  (re-find #"[\W+]*\w+" s))

(defn get-branch
  "Finds the index of the string whose first word matches the target word."
  [branches v]
  (loop [coll branches]
    (let [b (:text (first coll))]
      (cond
        (empty? coll) nil
        (str/starts-with? b v) (- (count branches) (count coll))
        :else (recur (rest coll))))))

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
       (let [i (get-branch branches (first-word suffix-b))]
         (suffix-tree prefix
                      (concat [(suffix-tree suffix-a)]
                              (if-not i
                                [(suffix-tree suffix-b) branches]
                                [(into-tree suffix-b (get branches i)) (vec-remove branches i)]))))))))

(defn re-each-match-to-eos
  "Get position of regex match."
  [re s]
  (let [re (js/RegExp. (.-source re) "g" )]
    (loop [res []]
      (if-let [m (.exec re s)]
        (recur (conj res (subs s (.-index m))))
        res))))

(defn get-phrases
  [sentences prefix]
  (let [re (re-pattern (str "\\b" prefix "\\b"))]
    (remove nil? (mapcat #(re-each-match-to-eos re %) sentences))))

(defn gen-suffix-tree
  "Generates a suffix tree from a collection of strings."
  [text pre]
  (let [sentences (sentence-split text)
        phrases (get-phrases sentences pre)]
    (reduce (fn [t s] (into-tree s t))
            (suffix-tree pre) phrases)))

(defn render-tree
  "Renders a suffix tree as html."
  [t]
  (let [{text :text, branches :branches} t]
    ^{:key (gensym)} [:ul {:class "suffix-tree"} text (map render-tree branches)]))
