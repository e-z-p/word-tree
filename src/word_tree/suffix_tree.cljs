(ns word-tree.suffix-tree
  (:require [clojure.string :as str]))

(defn sentence-split
  "Splits a body of text on delimiters: ('.'|'!'|'?')"
  [text]
  (map str/trim
       (re-seq #"\(?[^\.\?\!]+[\.!\?]\)?" text)))

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
  "remove elem in coll"
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn into-tree
  "Adds string, S, to suffix tree, T."
  ([s] (suffix-tree s))
  ([s t]
   (let [{text :text, branches :branches} t
         fork (get-fork text s)
         [prefix suffix-a] (cut-at text fork)
         suffix-b (get-suffix s fork)]
     (println "tree: " t)
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

(defn gen-suffix-tree
  "Creates a suffix tree from a body of text."
  [text]
  (let [sentences (sentence-split text)]
    (reduce into-tree sentences)))

(defn render-suffix-tree
  "Renders a suffix tree as html."
  [t]
  (let [{text :text, branches :branches} t]
    ^{:key (gensym)}
    [:ul text
     (map render-suffix-tree branches)]))
