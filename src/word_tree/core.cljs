(ns word-tree.core
  (:require
    [reagent.core :as r]
    [word-tree.suffix-tree :as sfx]
    [word-tree.component.search-bar :as sb]
    [word-tree.component.corpus-select :as cs]))

;; -------------------------
;; Views

(def sentences "Let's go to the movies. Let me go. 'hiya. hiya. 'hi.")
; TODO:
; clean git tree
;   1. change `sentences`                   [x]
;   2. merge with `--squash`                [ ]
;   3. delete branch locally and remotely   [ ]
; highlight root of word-tree
; decrease font-size as depth increases
; (-> event .-target .-dataset .-<keyword>) to access key :data-<keyword>
(defonce search-term (r/atom ""))
(defonce corpus (r/atom sentences))
(defn home-page []
  (fn []
    [:div
     [:strong "Search term: "] [sb/atom-search-bar search-term corpus]
     [cs/atom-select corpus]
     (if (empty? @search-term)
       [:div @corpus]
       (sfx/render-suffix-tree (sfx/gen-suffix-tree @corpus @search-term)))]))

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
