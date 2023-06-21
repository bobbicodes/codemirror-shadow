(ns acme.frontend.node
  (:require 
   ["@codemirror/state" :refer [EditorSelection]]
   ["@codemirror/language" :as language]
   [applied-science.js-interop :as j]
   ["@nextjournal/lezer-clojure" :as lezer-clj]
   ["@lezer/common" :as lz-tree]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Node props are marked in the grammar and distinguish categories of nodes

;; primitive collection
(def coll-prop (.-coll lezer-clj/props))
;; prefix collection - a prefix token that wraps the next element
(def prefix-coll-prop (.-prefixColl lezer-clj/props))
;; the prefix edge itself
(def prefix-edge-prop (.-prefixEdge lezer-clj/props))
;; prefix form - pair of [metadata, target]
(def prefix-container-prop (.-prefixContainer lezer-clj/props))
;; edges at the beginning/end of collections, + "same" edges (string quotes)
(def start-edge-prop (.-closedBy lz-tree/NodeProp))
(def end-edge-prop (.-openedBy lz-tree/NodeProp))
(def same-edge-prop (.-sameEdge lezer-clj/props))

;; used when instantiating the parser
(defn node-prop [prop-name]
  (case prop-name "prefixColl" prefix-coll-prop
        "coll" coll-prop
        "prefixEdge" prefix-edge-prop
        "prefixContainer" prefix-container-prop
        "sameEdge" same-edge-prop))

;; these wrapping functions exist mainly to avoid type hints
;; & are mostly compiled away

(defn ^lz-tree/NodeType node-type [^js node] (.-type node))

(defn ^number start [^js node]
  {:pre [(.-from node)]}
  (.-from node))

(defn ^number end [^js node]
  {:pre [(.-to node)]}
  (.-to node))

;; a more zipper-like interface
(defn ^js up [node] (.-parent ^js node))

(defn ^js down [node]
  {:pre [(not (fn? (.-lastChild ^js node)))]}
  (.-firstChild ^js node))

(defn ^js down-last [node]
  {:pre [(not (fn? (.-lastChild ^js node)))]}
  (.-lastChild ^js node))

(defn ^number depth [^js node]
  (loop [node node
         i 0]
    (if-some [parent (up node)]
      (recur parent (inc i))
      i)))

(defn ^js left [^js node]
  (.childBefore (up node) (start node))
  #_(.-prevSibling node))

(defn lefts [node]
  (take-while identity (iterate left (left node))))

(defn ^js right [node]
  (.childAfter (up node) (end node))
  #_(.-nextSibling node))

(defn rights [node]
  (take-while identity (iterate right (right node))))

;; category predicates

(defn coll-type? [^js node-type]
  (or (.prop node-type coll-prop)
      #_(.prop node-type prefix-coll-prop)))

(defn ^boolean prefix-type? [node-type] (.prop ^js node-type prefix-coll-prop))
(defn ^boolean prefix-edge-type? [node-type] (.prop ^js node-type prefix-edge-prop))
(defn ^boolean prefix-container-type? [node-type] (.prop ^js node-type prefix-container-prop))
(defn ^boolean same-edge-type? [node-type] (.prop ^js node-type same-edge-prop))
(defn ^boolean start-edge-type? [node-type] (.prop ^js node-type start-edge-prop))
(defn ^boolean end-edge-type? [node-type] (.prop ^js node-type end-edge-prop))
(defn ^boolean top-type? [node-type] (.-isTop ^js node-type))
(defn ^boolean error-type? [node-type]  (.-isError ^js node-type))

(defn ^boolean prefix? [n] (prefix-type? (node-type n)))
(defn ^boolean prefix-edge? [n] (prefix-edge-type? (node-type n)))
(defn ^boolean prefix-container? [n] (prefix-container-type? (node-type n)))
(defn ^boolean same-edge? [n] (same-edge-type? (node-type n)))
(defn ^boolean start-edge? [n]
  (start-edge-type? (node-type n)))
(defn ^boolean end-edge? [n] (end-edge-type? (node-type n)))

(defn ^boolean left-edge-type? [t]
  (or (start-edge-type? t)
      (same-edge-type? t)
      (prefix-edge-type? t)))

(defn ^boolean left-edge? [n]
  (left-edge-type? (node-type  n)))

(defn ^boolean right-edge-type? [t]
  (or (end-edge-type? t)
      (same-edge-type? t)))

(defn ^boolean right-edge? [n]
  (right-edge-type? (node-type n)))

(defn ^boolean edge? [n]
  (let [t (node-type n)]
    (or (start-edge-type? t)
        (end-edge-type? t)
        (same-edge-type? t)
        (prefix-type? t))))

(defn closed-by [n]
  (some-> (.prop (node-type n) (.-closedBy lz-tree/NodeProp))
          (aget 0)))
(defn opened-by [n]
  (some-> (.prop (node-type n) (.-openedBy lz-tree/NodeProp))
          (aget 0)))

(defn ^string node-name [^js node] (.-name node))

;; specific node types

(defn error? [^js node] (error-type? node))
(defn top? [node] (top-type? (node-type node)))

(defn program? [node] (identical? "Program" (node-name node)))
(defn string-node? [node] (identical? "String" (node-name node)))
(defn regex-node? [node] (identical? "RegExp" (node-name node)))
(defn line-comment? [node] (identical? "LineComment" (node-name node)))
(defn discard? [node] (identical? "Discard" (node-name node)))


(defn coll-node? [node]
  (coll-type? (node-type node)))

(defn terminal-type? [^js node-type]
  (cond (top-type? node-type) false
        (.prop node-type prefix-coll-prop) false
        (.prop node-type coll-prop) false
        (identical? "Meta" (node-name node-type)) false
        (identical? "TaggedLiteral" (node-name node-type)) false
        (identical? "ConstructorCall" (node-name node-type)) false
        :else true))

(j/defn balanced? [^:js {:as node :keys [^js firstChild ^js lastChild]}]
  (if-let [closing (closed-by firstChild)]
    (and (= closing (node-name lastChild))
         (not= (end firstChild) (end lastChild)))
    true))

(defn node-ancestors [^js node]
  (when-some [parent (up node)]
    (cons parent
          (lazy-seq (node-ancestors parent)))))

(defn ^js closest [node pred]
  (if (pred node)
    node
    (reduce (fn [_ x]
              (if (pred x) (reduced x) nil)) nil (node-ancestors node))))

(defn ^js highest [node pred]
  (reduce (fn [found x]
            (if (pred x) x (reduced found))) nil (cons node (node-ancestors node))))

(defn children
  ([^js parent from dir]
   (when-some [^js child (case dir 1 (.childAfter parent from)
                               -1 (.childBefore parent from))]
     (cons child (lazy-seq
                  (children parent (case dir 1 (end child)
                                         -1 (start child)) dir)))))
  ([^js subtree]
   (children subtree (start subtree) 1)))

(defn eq? [^js x ^js y]
  (and (== (start x) (start y))
       (== (end x) (end y))
       (== (depth x) (depth y))))

(defn empty-node?
  "Node only contains whitespace"
  [^js node]
  (let [type-name (node-name node)]
    (cond (coll-node? node)
          (eq? (-> node down right) (-> node down-last))

          (= "String" type-name)
          (== (-> node down end) (-> node down-last start))
          :else false)))

(defn from-to
  ([from to] #js{:from from :to to})
  ([node]
   (from-to (start node) (end node))))

(defn sel-range
  ([from to] (.range EditorSelection from to))
  ([^js range] (.range EditorSelection (.-from range) (.-to range))))

(defn node-range [node]
  (sel-range (start node) (end node)))

(defn string
  ([^js state node]
   (string state (start node) (end node)))
  ([^js state from to]
   (.sliceString (.-doc state) from to \newline)))

(defn ancestor-node? [parent child]
  (boolean
   (and (<= (start parent) (start child))
        (>= (end parent) (end child))
        (< (depth parent) (depth child)))))

(defn move-toward
  "Returns next loc moving toward `to-path`, skipping children"
  [node to-node]
  (when-not (eq? node to-node)
    (case (compare (start to-node) (start node))
      0 (cond (ancestor-node? to-node node) (up node)
              (ancestor-node? node to-node) (down node))
      -1 (if (ancestor-node? node to-node)
           (down-last node)
           (or (left node)
               (up node)))
      1 (if (ancestor-node? node to-node)
          (down node)
          (or (right node)
              (up node))))))

(defn nodes-between [node to-node]
  (take-while identity (iterate #(move-toward % to-node) node)))

(defn- require-balance? [node]
  (or (coll? node)
      (string-node? node)
      (regex-node? node)))

(defn ^js tree
  "Returns a (Tree https://lezer.codemirror.net/docs/ref/#common.Tree) for editor state
  or the SyntaxNode at pos.

  If pos is given and we're using Clojure language support embedded in other languages (e.g. markdown)
  enters overlaid Clojure nodes (https://lezer.codemirror.net/docs/ref/#common.MountedTree)."
  ([^js state] (language/syntaxTree state))
  ([^js state pos] (-> state language/syntaxTree (.resolveInner pos)))
  ([^js state pos dir] (-> state language/syntaxTree (.resolveInner pos dir))))

(defn ^js cursor
  ([^js tree] (.cursor tree))
  ([^js tree pos] (.cursorAt tree pos))
  ([^js tree pos dir] (.cursorAt tree pos dir)))

(defn ^js terminal-cursor
  [^js tree pos dir]
  (loop [i pos]
    (let [^js c (cursor tree i dir)
          type (.-type c)]
      (cond (top-type? type) nil
            (terminal-type? (.-type c)) c
            :else (recur (+ dir i))))))

(defn ^js up-here
  "Returns topmost node at same starting position"
  [node]
  (let [from (start node)]
    (or (highest node #(= from (start %)))
        node)))

(defn topmost-cursor [state from]
  (-> (tree state from 1) .-node up-here .cursor))

(defn terminal-nodes [state from to]
  (let [^js cursor (topmost-cursor state from)]
    (loop [found []]
      (let [node-type (node-type cursor)]
        (cond (> (start cursor) to) found
              (or (terminal-type? node-type)
                  (error? node-type))
              (let [found (conj found #js{:type node-type
                                          :from (start cursor)
                                          :to  (end cursor)})]
                (.lastChild cursor)
                (if (.next cursor)
                  (recur found)
                  found))
              :else (if (.next cursor)
                      (recur found)
                      found))))))

(j/defn balanced-range
  ([state ^js node] (balanced-range state (start node) (end node)))
  ([state from to]
   (let [[from to] (sort [from to])
         from-node (tree state from 1)
         to-node (tree state to -1)
         from (if (require-balance? from-node)
                (start from-node)
                from)
         to (if (require-balance? to-node)
              (end to-node)
              to)
         [left right] (->> (nodes-between from-node to-node)
                           (map #(cond-> % (edge? %) up))
                           (reduce (fn [[left right] ^js node-between]
                                     [(if (ancestor-node? node-between from-node) (start node-between) left)
                                      (if (ancestor-node? node-between to-node) (end node-between) right)])
                                   [from to]))]
     (sel-range left right))))

(j/defn inner-span
  "Span of collection not including edges"
  [^:js {:as node :keys [firstChild lastChild]}]
  #js{:from (if (left-edge? firstChild)
              (end firstChild)
              (start node))
      :to (if (right-edge? lastChild)
            (start lastChild)
            (end node))})

(defn within?< "within (exclusive of edges)"
  [parent child]
  (let [c1 (compare (start parent) (start child))
        c2 (compare (end parent) (end child))]
    (and (or (pos? c1) (neg? c2))
         (not (neg? c1))
         (not (pos? c2)))))

(defn within? "within (inclusive of edges)"
  [parent child]
  (and (not (neg? (compare (start parent) (start child))))
       (not (pos? (compare (end parent) (end child))))))

(defn follow-edges [node]
  (if (edge? node)
    (up node)
    node))

(defn guard [x f] (when (f x) x))

(defn prefix [node]
  (when-some [parent (up node)]
    (or (guard parent prefix-container?)
        (guard (down parent) prefix-edge?))))

(defn left-edge-with-prefix [state node]
  (str (some->> (prefix node) (string state))
       (node-name (down node))))

(defn with-prefix [node]
  (cond-> node
    (prefix node) up))

(defn node|
  "Node ending immediately to the left of pos"
  [state pos]
  (some-> (tree state pos -1)
          (guard #(= pos (end %)))))

(defn |node
  "Node starting immediately to the right of pos"
  [state pos]
  (some-> (tree state pos 1)
          (guard #(= pos (start %)))))

(defn nearest-touching [^js state pos dir]
  (let [L (some-> (tree state pos -1)
                  (guard (j/fn [^:js {:keys [to]}] (= pos to))))
        R (some-> (tree state pos 1)
                  (guard (j/fn [^:js {:keys [from]}]
                             (= pos from))))
        mid (tree state pos)]
    (case dir 1 (or (guard R (every-pred some? #(or (same-edge? %) (not (right-edge? %)))))
                    L
                    R
                    mid)
          -1 (or (guard L (every-pred some? #(or (same-edge? %) (not (left-edge? %)))))
                 R
                 L
                 mid))))