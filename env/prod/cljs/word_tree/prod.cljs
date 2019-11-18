(ns word-tree.prod
  (:require
    [word-tree.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
