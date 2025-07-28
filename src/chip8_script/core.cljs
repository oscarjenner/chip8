(ns ^:figwheel-hooks chip8-script.core
  (:require
   [goog.dom :as gdom]))

(println "This text is printed from src/chip8_script/core.cljs. Go ahead and edit it and see reloading in action.")

(defn multiply [a b] (* a b))

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:text "Hello world!"}))

(defn getCanvas []
  (gdom/getElement "my-canvas"))
(defn getContext [canvas]
  (.getContext canvas "2d"))
(defn createImage [ctx pixels]
  (let [imageData (.createImageData ctx 64 32) pixelArray (.-data imageData)]
    (doseq [i (range 2048)]
      (let [i4 (* 4 i) value (if (true? (get pixels i)) 0 255)]
        (aset pixelArray i4 value)
        (aset pixelArray (+ i4 1) value)
        (aset pixelArray (+ i4 2) value)
        (aset pixelArray (+ i4 3) 255)))
    imageData))

(defn paintCanvas [pixels]
  (let [ctx (getContext (getCanvas))]
    (.putImageData ctx (createImage ctx pixels) 0 0)))



;; specify reload hook with ^:after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
