(ns acme.frontend.app
  (:require #_["@codemirror/language" :as language]
            #_[acme.frontend.eval-region :as eval-region]))

(defn init []
  (println "cljs started"))

(defn hello []
  (js/console.log "hello  ee world"))