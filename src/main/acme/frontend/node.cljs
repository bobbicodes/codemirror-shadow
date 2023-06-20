(ns acme.frontend.node
  (:require #_["@codemirror/language" :as language]))

(defn ^boolean top-type? [node-type] (.-isTop ^js node-type))
(defn top? [node] (top-type? (type node)))
(defn ^js up [node] (.-parent ^js node))

(defn ^number start [^js node]
  {:pre [(.-from node)]}
  (.-from node))

(defn ^number end [^js node]
  {:pre [(.-to node)]}
  (.-to node))