(ns word-tree.core
    (:require
      [reagent.core :as r]
      [word-tree.suffix-tree :as suff]))

;; -------------------------
;; Views

(def text "I am peeling honeycrisps for apple crisp. Within minutes, my hands drip with juice. My father is at the counter, making the 'crisp' part of the dessert, with cane sugar, cinnamon, lemon juice, almonds, and oats. We are listening to jazz, something calm yet dramatic, with big pockets of pregnant silence.")

(defn atom-input [value]
  [:input {:type "text"
           :style {:font-size ".75em"}
           :value @value
           :on-change #(reset! value (-> % .-target .-value))}])

(defn home-page []
  (let [search-term (r/atom "with")]
    (fn []
      [:div
       [:p [:strong "Search term: "] [atom-input search-term]]
       (suff/render-tree (suff/gen-tree text @search-term))])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
