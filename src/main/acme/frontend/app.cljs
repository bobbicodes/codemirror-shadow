(ns acme.frontend.app
  (:require ["@codemirror/language" :as language]
            [acme.frontend.eval-region :as eval-region]))

(defn init []
  (println "Hello World"))

(defn hello []
  (js/console.log "hello world"))