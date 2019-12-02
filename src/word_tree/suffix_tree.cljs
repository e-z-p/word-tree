(ns word-tree.suffix-tree
  (:require [clojure.string :as str]
            [cljs.pprint :as pp]))

(defn- sentence-split
  "Splits a body of text on delimiters: ('.'|'!'|'?')"
  [text]
  (map str/trim
       (re-seq #"\(?[^\.\?\!]+[\.!\?]\)?" text)))           ; In backend use "clojure-opennlp" lib to tokenize text into sentences

(defn- get-fork
  "Returns the index where two strings diverge."
  [s1 s2]
  (loop [a s1
         b s2]
    (if (or (or (empty? a) (empty? b))
            (not= (first a) (first b)))
      (- (count s1) (count a))                                            ;; get len of matchuptobeginningoflastword of s1[0:i]
      (recur (rest a)
             (rest b)))))

(defn- get-prefix
  "Gets substring of S from zero up to index I"
  [s i]
  (subs s 0 i))

(defn- get-suffix
  "Gets rest of string S after and including the character at index I"
  [s i]
  (subs s i))

(defn- cut-at
  "Cuts string at the given index."
  [s i]
  [(get-prefix s i) (get-suffix s i)])

(defn- suffix-tree
  ([] {:text "", :branches []})
  ([p] {:text p, :branches []})
  ([p b] {:text p, :branches b}))

(defn- vec-remove
  "Removes element from vector."
  [coll pos]
  (vec (concat (subvec (vec coll) 0 pos) (subvec (vec coll) (inc pos)))))

(defn- first-word
  "Gets substring of s up to end of first word."
  [s]
  (first (re-find #",?\s?\w+(\'[a-z])?|." s)))

(defn- get-branch
  "Finds the index of the string whose first word matches the target word."
  [branches val]
  (loop [brs branches]
    (let [text (:text (first brs))]
      (cond
        (empty? brs) nil
        (str/starts-with? text val) (- (count branches) (count brs))
        :else (recur (rest brs))))))

(defn- into-tree
  "Adds string, s, to suffix tree, t."
  ([s] (suffix-tree s))
  ([s t]
   (let [{text :text, branches :branches} t
         fork (get-fork text s)
         [prefix suffix-a] (cut-at text fork)
         suffix-b (get-suffix s fork)]
     (if (empty? suffix-b)
       t
       (let [path (get-branch branches (first-word suffix-b))]
         (suffix-tree prefix
                      (remove nil? (flatten [(when suffix-a (suffix-tree suffix-a branches))
                                             (if path [(into-tree suffix-b (nth branches path)) (vec-remove branches path)]
                                                      [(suffix-tree suffix-b)])]))))))))

(defn- re-each-match-to-eos
  "Get position of regex match."
  [re s]
  (let [re (js/RegExp. (.-source re) "g")]
    (loop [res []]
      (if-let [m (.exec re s)]
        (recur (conj res (subs s (.-index m))))
        res))))

(defn- get-phrases
  [sentences prefix]
  (let [re (re-pattern (str "(?<![\\'\\w\\d])" prefix "(?![\\w\\d\\'])"))]
    (remove nil? (mapcat #(re-each-match-to-eos re %) sentences))))

(defn gen-suffix-tree
  "Generates a suffix tree from a collection of strings."
  [text prefix]
  (let [sentences (sentence-split text)
        phrases (get-phrases sentences prefix)]
    (reduce #(into-tree %2 %1) (suffix-tree (first phrases)) (rest phrases))))

(defn render-suffix-tree
  "Renders a suffix tree as html."
  [t]
  (let [{text :text, branches :branches} t]
    ^{:key (gensym)} [:ul {:class "suffix-tree"} text (map render-suffix-tree branches)]))
