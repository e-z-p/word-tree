(ns word-tree.core
  (:require
    [reagent.core :as r]
    [word-tree.suffix-tree :as sfx]
    [clojure.string :as str]))

;; -------------------------
;; Views

(def sentences "Let's go to the movies. Let me go. 'hiya. hiya. 'hi.")
; TODO:
; 1. change `sentences`                   [x]
; 2. merge with `--squash`                [ ]
; 3. delete branch locally and remotely   [ ]


(defn atom-textarea [value]
  [:textarea {:styles    {:font-size ".7"}
              :value     @value
              :on-change #(reset! value (-> % .-target .-value))}])

(defn atom-input
  [value show-suggestions? ]
  [:input {:type      "text"
           :style     {:font-size ".75em"}
           :value     @value
           :on-change #(reset! value (-> % .-target .-value))}]) ; where '%' is the event object

; TODO:
; drop down suggestions as you begin typing or "no word beginning with '<search-term>' exists in this corpus."'
; highlight root of word-tree
; decrease font-size as depth increases
(defn atom-search-bar
  [value text]
  (let [suggestions-list (r/atom [])
        active-suggestion (r/atom 0)
        show-suggestions? (r/atom false)
        get-suggestions #(let [pattern (re-pattern (str "(?<!\\w)" @value "[\\w\\']*"))]
                           (distinct (re-seq pattern text)))
        on-input-change (fn [event]
                          (do (reset! suggestions-list get-suggestions)
                              (reset! active-suggestion 0)
                              (reset! show-suggestions? true)
                              (reset! value (-> event .-target .-value))))]
    [:<>
     [:input {:type "text"
              :class "search-input"
              :value @value
              :on-change on-input-change}]]
    ))

(defn home-page []
  (let [search-term (r/atom "")
        corpus (r/atom sentences)]
    (fn []
      [:div
       [:p [:strong "Search term: "] [atom-input search-term]]
       (if (empty? @search-term)
         [:div @corpus]
         (sfx/render-suffix-tree (sfx/gen-suffix-tree @corpus @search-term)))])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
