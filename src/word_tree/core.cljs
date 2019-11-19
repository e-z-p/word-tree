(ns word-tree.core
    (:require
      [reagent.core :as r]
      [word-tree.suffix-tree :as s-tree]))

;; -------------------------
;; Views

(def text "I am peeling honeycrisps for apple crisp. Within minutes, my hands drip with juice. My father is at the counter, making the 'crisp' part of the dessert, with cane sugar, cinnamon, lemon juice, almonds, and oats. We are listening to jazz, something calm yet dramatic, with big pockets of pregnant silence.")

(defn home-page []
  (let [f (s-tree/get-fork "I am peeling the honey crisps." "I am not happy.")
        [p s1] (s-tree/cut-at "I am peeling the honey crisps." f)
        s2 (s-tree/get-suffix "I am not happy." f)
        t (s-tree/into-tree "You are something else entirely.")
        t2 (s-tree/into-tree "You are the best." t)
        t3 (s-tree/into-tree "You are the dumbest person." t2)
        t4 (s-tree/into-tree "You are something." t3)
        t5 (s-tree/gen-tree text "with")]
    [:div
     [:ul p
      [:li s1]
      [:li s2]]
     (s-tree/render-suffix-tree t)
     (s-tree/render-suffix-tree t2)
     (s-tree/render-suffix-tree t3)
     (s-tree/render-suffix-tree t4)
     [:div {:style {:color "red"}} (s-tree/render-suffix-tree t5)]]))

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
