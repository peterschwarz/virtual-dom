(ns virtual-dom.main
  (:require [virtual-dom.core :as v :refer [h]]))

(enable-console-print!)

(defonce app-state (atom {:content "Hello world!"}))

(defn a-component [state]
  (h :div {:class "row"}
    (h :p nil "I'm a sub component")
    (h :p "My state of interest: " (:content state))))

(v/attach!
  (let [state @app-state]
    (h :div {:class "container"}
           (h :h1 nil "Hello, Virtua")
           (h :p nil
                  "Hello there, we're using pure CLJS virtual dom
                  to render this page.")
           (a-component state)
           (h :ul nil
                  (map (fn [i] (h :li nil i)) (:list state)))))
  app-state
  (. js/document (getElementById "app")))

;; define your app data so that it doesn't get over-written on reload


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
