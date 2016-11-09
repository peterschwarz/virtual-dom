(ns virtual-dom.core
  (:require [virtual-dom.dom :as dom]
            [clojure.string :refer [join]]
            [clojure.data :refer [diff]]))

(defn primitive? [x]
  (or (string? x)
      (number? x)
      (= (type x) (type true))))

(defn create-element
  "Given a virtual dom tree node, create a real dom element."
  [node]
  (when node
    (cond

      (primitive? node)
      (dom/create-text (str node))

      (map? node)
      (let [{:keys [tag children]} node
            ; NOTE: Ww're just going to ignore our attributes
            el (dom/create (name tag))]
        (->> children
             (map create-element)
             (apply dom/append el)))

      :default
      (dom/create-text (str node)))))

(defn apply-state
  "Given a state snapshot, realize the virual dom tree."
  [node state]
  node)

(defn update-elements
  "Given a the old (left) and new (right) tree, update the elements in the dom.
  `index` is the current element's index relative to its siblings."
  [el left right index])

(defn h
  "Creates a virtual dom representation of an element"
  [tag attr & children]
  {:tag tag
   :attributes attr
   :children (vec children)})

(defn update-tree
  "Given an old-state and an new state, produce the virtual dom trees and update
  the dom accordingly"
  [el node old-state new-state]
  (let [before (apply-state node old-state)
        after (apply-state node new-state)
        [left right same] (diff before after)]
    (when (and left right)
      (update-elements el before after 0))))

(defn- wrap-state [value]
  (if (satisfies? IAtom value)
    value
    (atom value)))

(defn attach!
  [component-or-fn initial-value parent]
  (let [state (wrap-state initial-value)
        initial-node (apply-state component-or-fn @state)]
    (dom/remove-children parent)
    (add-watch state ::virtua-component
               (fn [_ _ old-state new-state]
                 (update-tree parent component-or-fn old-state new-state)))
    (dom/append
      parent
      (create-element initial-node))))
