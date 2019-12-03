(ns word-tree.component.corpus-select
  (:require [reagent.core :as r]
            [goog.net.XhrIo :as xhr]
            [clojure.string :as str]
            [cljs.core.async :as a :refer [chan close! go >! <!]]))

(defn GET [url]
  (let [ch (chan 1)]
    (xhr/send url
              (fn [event]
                (let [res (-> event .-target .getResponseText)]
                  (go (>! ch res)
                      (close! ch)))))
    ch))

(defn log [s]
  (js/console.log s))

(defn proxy [url] (str "https://cors-anywhere.herokuapp.com/" url))

;(go
;(log (<! (GET (proxy "http://www.textfiles.com/etext/NONFICTION/word")))))

(defonce ^:private
         books (sort-by :author
                        [{:author "H. G. Welles" :title "War of the Worlds" :link "http://www.textfiles.com/etext/FICTION/war_worlds"}
                         {:author "The Buddha" :title "The Eight-Fold Path" :link "http://www.textfiles.com/etext/NONFICTION/word"}
                         {:author "Charles Darwin" :title "The Origin of Species" :link "http://www.textfiles.com/etext/NONFICTION/origin_species"}
                         {:author "Leo Tolstoy" :title "War and Peace" :link "http://www.gutenberg.org/files/2600/2600-0.txt"}
                         {:author "Oscar Wilde" :title "The Picture of Dorian Gray" :link "http://www.textfiles.com/etext/AUTHORS/WILDE/wilde-picture-615.txt"}
                         {:author "Sun Tzu" :title "The Art of War" :link "http://www.textfiles.com/etext/NONFICTION/sunzu10.txt"}
                         {:author "Niccolo Machiavelli" :title "The Prince" :link "http://www.textfiles.com/etext/NONFICTION/machiavelli-prince-123.txt"}
                         {:author "John Locke" :title "An Essay Concerning Human Understanding" :link "http://www.textfiles.com/etext/NONFICTION/locke-essay-113.txt"}
                         {:author "Frederick Douglas" :title "Narrative of the Life of Frederick Douglas" :link "http://www.textfiles.com/etext/NONFICTION/douglass"}
                         {:author "John Stuart Mill" :title "Utilitarianism" :link "http://www.textfiles.com/etext/NONFICTION/mill-utilitarianism-218.txt"}
                         {:author "Jack London" :title "The Call of the Wild" :link "http://www.textfiles.com/etext/FICTION/callwild"}
                         {:author "Joseph Conrad" :title "The Heart of Darkness" :link "http://www.textfiles.com/etext/FICTION/conrad-heart-372.txt"}]))

(defonce ^:private book-links (zipmap (map :title books) (map :link books)))

(defonce ^:private corpus-active (r/atom 0))
(defn atom-select
  [text]
  (let [on-select-change #(do (reset! corpus-active (.. % -target -value))
                              (.persist %)
                              (go (reset! text (<! (GET (-> % .-target .-value book-links proxy)))))
                              ;(log (-> % .-target .-value book-links))
                              )
        render-option (fn [i {author :author, title :title, link :link}]
                        (let [class (when (= i @corpus-active) "-active")
                              inner-text (str/join ", " [author, title])]
                          [:option.corpus
                           {:class class :key (gensym) :value title :data-url link}
                           inner-text]))]
    [:<>
     [:label.selector-label "Select text to search: "]
     [:select.corpora-selection
      {:on-change on-select-change}
      [:optgroup {:label "Corpora"} (doall (map-indexed render-option books))]]]))
