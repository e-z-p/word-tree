(ns word-tree.core
  (:require
    [reagent.core :as r]
    [word-tree.suffix-tree :as sfx]
    [clojure.string :as str]
    [clojure.pprint :as pp]
    [component.search-bar]))

;; -------------------------
;; Views

;; (def war-and-peace-url "http://www.gutenberg.org/files/2600/2600-0.txt")
;; (def war-and-peace-corpus (slurp war-and-peace))

(def sentences "Let's go to the movies. Let me go. 'hiya. hiya. 'hi.")
; TODO:
; 1. change `sentences`                   [x]
; 2. merge with `--squash`                [ ]
; 3. delete branch locally and remotely   [ ]


(defn atom-textarea [value]
  [:textarea {:styles    {:font-size ".7"}
              :value     @value
              :on-change #(do (pp/pprint %) (reset! value (-> % .-target .-value)))
              :on-key-down #(reset! value (-> % .-target .-value))}])

(defn atom-input
  [value]
  [:input {:type      "text"
           :style     {:font-size ".75em"}
           :value     @value
           :on-change #(reset! value (-> % .-target .-value))}]) ; where '%' is the event object

; TODO:
; drop down suggestions as you begin typing or "no word beginning with '<search-term>' exists in this corpus."'
; highlight root of word-tree
; decrease font-size as depth increases
; (-> event .-target .-dataset .-index) to access key :data-index
(defonce suggestions-list (r/atom []))
(defonce active-suggestion (r/atom 0))
(defonce show-suggestions? (r/atom false))
(defn atom-search-bar
  [value text]
  (let [get-suggestions #(let [pattern (re-pattern (str "(?<!\\w)" @value "[\\w\\']*"))]
                           (remove empty? (distinct (re-seq pattern @text))))
        on-input-change #(do (reset! active-suggestion 0)
                             (reset! show-suggestions? true)
                             (reset! value (-> % .-target .-value))
                             (if (empty? @value)
                               (do (reset! suggestions-list [])
                                   (reset! show-suggestions? false))
                               (reset! suggestions-list (get-suggestions))))
        on-input-key-down (fn [event]
                           (let [keycode (-> event .-keyCode)]
                             (case keycode
                               13 (do (reset! value (nth @suggestions-list @active-suggestion)) ; enter
                                      (reset! show-suggestions? false)
                                      (reset! suggestions-list [])
                                      (reset! active-suggestion 0))
                               27 (do (reset! show-suggestions? false) ; escape
                                      (reset! suggestions-list [])
                                      (reset! active-suggestion 0))
                               38 (when (pos? (count @suggestions-list)) (swap! active-suggestion #(mod (dec %) (count @suggestions-list))))
                               40 (when (pos? (count @suggestions-list)) (swap! active-suggestion #(mod (inc %) (count @suggestions-list))))
                               nil)))
        on-suggestion-click #(do (reset! value (nth suggestions-list active-suggestion))
                                 (reset! show-suggestions? false)
                                 (reset! suggestions-list [])
                                 (reset! active-suggestion 0))
        render-suggestion (fn [i s] ^{:key (gensym)}
                            [:li.suggestion {:class (str "suggestion" (when (= @active-suggestion i) "-active"))
                                             :style {:display (if (= @value s) "none" "")}
                                             :on-click on-suggestion-click} s])]
    [:div
     [:input.search-bar {:type "text"
                         :value @value
                         :on-change on-input-change
                         :on-key-down on-input-key-down}]
     (when @show-suggestions?
      (if (not (empty? @suggestions-list))
       [:ul.suggestions-list
        (doall (map-indexed render-suggestion @suggestions-list))]
       [:div.no-suggestions (str "There are no words beginning with '" @value "' in this corpus.")]))]))

(defn home-page []
  (let [search-term (r/atom "")
        corpus (r/atom sentences)]
    (fn []
      [:div
       [:strong "Search term: "] [atom-search-bar search-term corpus]
       (if (empty? @search-term)
         [:div @corpus]
         (sfx/render-suffix-tree (sfx/gen-suffix-tree @corpus @search-term)))])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
