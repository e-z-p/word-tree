(ns word-tree.component.search-bar
  (:require
    [reagent.core :as r]))

(defonce suggestions-list (r/atom []))
(defonce active-suggestion (r/atom 0))
(defonce show-suggestions? (r/atom false))
(defn atom-search-bar
  [value text]
  (let [get-suggestions #(let [pattern (re-pattern (str "(?<![\\'\\w\\d])" @value "[\\w\\']*"))]
                           (remove empty? (distinct (re-seq pattern @text))))
        on-input-change #(do (reset! active-suggestion 0)
                             (reset! show-suggestions? true)
                             (reset! value (-> % .-target .-value))
                             (if (empty? @value)
                               (do (reset! suggestions-list [])
                                   (reset! show-suggestions? false))
                               (reset! suggestions-list (get-suggestions))))
        on-input-key-down (fn [event]
                            (when show-suggestions?
                              (let [keycode (-> event .-keyCode)]
                                (case keycode
                                  13 (do (reset! value (nth @suggestions-list @active-suggestion))                                              ; enter
                                         (reset! show-suggestions? false)
                                         (reset! suggestions-list [])
                                         (reset! active-suggestion 0))
                                  27 (do (reset! show-suggestions? false)                                                                       ; escape
                                         (reset! suggestions-list [])
                                         (reset! active-suggestion 0))
                                  38 (when (pos? (count @suggestions-list)) (swap! active-suggestion #(mod (dec %) (count @suggestions-list)))) ; up
                                  40 (when (pos? (count @suggestions-list)) (swap! active-suggestion #(mod (inc %) (count @suggestions-list)))) ; down
                                  nil))))
        on-suggestion-click #(do (reset! value (nth suggestions-list active-suggestion))
                                 (reset! show-suggestions? false)
                                 (reset! suggestions-list [])
                                 (reset! active-suggestion 0))
        render-suggestion (fn [i s] ^{:key (gensym)}
                            [:li.suggestion {:class    (str "suggestion" (when (= @active-suggestion i) "-active"))
                                             :style    {:display (if (= @value s) "none" "")}
                                             :on-click on-suggestion-click} s])]
    [:div
     [:input.search-bar {:type        "text"
                         :value       @value
                         :on-change   on-input-change
                         :on-key-down on-input-key-down}]
     (when @show-suggestions?
       (if (not (empty? @suggestions-list))
         [:ul.suggestions-list
          (doall (map-indexed render-suggestion @suggestions-list))]
         [:div.no-suggestions (str "There are no words beginning with '" @value "' in this corpus.")]))]))
