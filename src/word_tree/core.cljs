(ns word-tree.core
  (:require
    [reagent.core :as r]
    [word-tree.suffix-tree :as sfx]
    [clojure.string :as str]))

;; -------------------------
;; Views

(def sentences "Let's go to the movies. Let me go.")
; TODO:
; 1. change `sentences`                   [x]
; 2. merge with `--squash`                [ ]
; 3. delete branch locally and remotely   [ ]

(defn atom-input [value]
  [:input {:type      "text"
           :style     {:font-size ".75em"}
           :value     @value
           :on-change #(reset! value (-> % .-target .-value))}]) ; where '%' is the event object
; TODO:
; drop down suggestions as you begin typing or "no word beginning with '<search-term>' exists in this corpus."

(defn atom-textarea [value]
  [:textarea {:styles    {:font-size ".7"}
              :value     @value
              :on-change #(reset! value (-> % .-target .-value))}])

(defn home-page []
  (let [search-term (r/atom "")
        corpus (r/atom sentences)]
    (fn []
      [:div
       [:p [:strong "Search term: "] [atom-input search-term]]
       ;[:p [:strong "Corpus: "] [atom-textarea corpus]]
       (if (empty? @search-term)
         [:p @corpus]
         (sfx/render-suffix-tree (sfx/gen-suffix-tree @corpus @search-term)))])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
