(ns acme.frontend.app
  (:require #_[nextjournal.clojure-mode :as cm-clj]
            ["@codemirror/language" :as language]
            [acme.frontend.eval-region :as eval-region]))

(defn init []
  (println "Hello World"))

(defn hello []
  (js/console.log "hello world"))