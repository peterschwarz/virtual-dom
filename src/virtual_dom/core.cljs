(ns virtual-dom.core
  (:require [virtual-dom.dom :as dom]
            [clojure.string :refer [join]]
            [clojure.data :refer [diff]]))

(defn create-element
  "Given a virtual dom tree node, create a real dom element."
  [node])

(defn apply-state
  "Given a state snapshot, realize the virual dom tree."
  [node state])

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
