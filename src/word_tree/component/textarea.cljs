(ns word-tree.component.textarea
  (:require
    [reagent.core :as r]))

(defn atom-textarea [value]
  [:textarea {:styles    {:font-size ".7"}
              :value     @value
              :on-change #(reset! value (-> % .-target .-value))}])
