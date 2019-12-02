(ns word-tree.component.textarea
  (:require
    [reagent.core :as r]))

;; (def war-and-peace-url "http://www.gutenberg.org/files/2600/2600-0.txt")
;; (def war-and-peace-corpus (slurp war-and-peace))

(defn atom-textarea [value]
  [:textarea {:styles    {:font-size ".7"}
              :value     @value
              :on-change #(reset! value (-> % .-target .-value))}])
