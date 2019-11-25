(ns word-tree.suffix-tree
  (:require [clojure.string :as str]))

(defn sentence-split
  "Splits a body of text on delimiters: ('.'|'!'|'?')"
  [text]
  (map str/trim
       (re-seq #"\(?[^\.\?\!]+[\.!\?]\)?" text)))           ; In backend use "clojure-opennlp" lib to tokenize text into sentences

; (re-split #"((\b[^\s]+\b)((?<=\.\w).)?)" sentences) splits text into words (needs test)
;       \W+ matches any succession of non-word characters

(defn get-fork
  "Returns the index where two strings diverge."
  [s1 s2]
  (loop [a s1
         b s2
         i 0]
    (if (or (some empty? [a b])
            (not= (first a) (first b)))
      i                                                     ;; get len of matchuptolastword of s1[0:i]
      (recur (rest a)
             (rest b)
             (inc i)))))

; (get-fork "A blue bird lives in my heart." "A blue bird dies.")
;            012345678901234567890123456789   01234567890123456
;                      11111111112222222222             1111111
;
; ===> 12

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

(defn prune
  "Removes empty trees from a collection."
  [coll]
  (letfn [(is-bare? [t] (every? empty? (vals t)))]
    (remove #(or (nil? %) (is-bare? %)) coll)))

(defn vec-remove
  "Removes element from vector."
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn first-word
  "Gets the first word from a string."
  [s]
  (re-find #"\w+" s))

(defn get-branch
  "Finds the index of the string whose first char matches the target char."
  [coll target]
  (loop [indices-and-first-words (map vector (range 0 (count coll)) (map #(first-word (:text %)) coll))]
    (let [i&fw (first indices-and-first-words)]
      (cond
        (empty? indices-and-first-words) nil
        (= target (get i&fw 1)) i&fw
        :else (recur (rest indices-and-first-words))))))

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
       (let [[i n] (get-branch branches (first-word suffix-b))]
         (suffix-tree prefix
                      (prune (concat [(suffix-tree suffix-a)
                                      (if-not n
                                        (suffix-tree suffix-b)
                                        (into-tree suffix-b n))]
                                     (if-not n
                                       branches
                                       (vec-remove (vec branches) i))))))))))

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
    (reduce #(into-tree %2 %1) (suffix-tree pre) phrases)))

(defn render-tree
  "Renders a suffix tree as html."
  [t]
  (let [{text :text, branches :branches} t]
    ^{:key (gensym)} [:ul {:class "suffix-tree"} text (map render-tree branches)]))
