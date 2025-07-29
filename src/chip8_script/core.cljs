(ns ^:figwheel-hooks chip8-script.core
  (:require
   [goog.dom :as gdom]
   [goog.events :as events]
   [chip8-script.memory :as mem]
   [chip8-script.cpu :as cpu]))

(println "This text is printed from src/chip8_script/core.cljs. Go ahead and edit t and see reloading in action.")

(def emulator-state
  (atom {:memory nil
         :registers (vec (repeat 16 0))
         :pc 0x200
         :i 0
         :stack '() ; IMPORTANT: Stack pointer does not overflow
         :display (vec (repeat (* 64 32) false))
         :timers {:delay 0 :sound 0}
         :keys #{}}))

(def pressedKeys (atom #{}))

(defn handeKeypress [event]
  (let [key (.-key event)]
    (swap! pressedKeys conj key)
    (println "Pressed keys:" @pressedKeys)))

(defn handleKeyRelease [event]
  (let [key (.-key event)]
    (swap! pressedKeys disj key)))

(events/listen js/document "keydown" handeKeypress)
(events/listen js/document "keyup" handleKeyRelease)

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




(defonce game-running (atom true))

(defn game-loop [state]
  (when @game-running
    (swap! state cpu/fetch-decode-execute)
    (let [disp (:display @state)]
      (paintCanvas disp))
    (js/requestAnimationFrame #(game-loop state))))
(println "Loading memory and fonts...")
(-> (mem/clean-memory)
    (mem/load-fonts "roms/font.csv")
    (.then #(do (js/console.log "Fonts loaded") %))  ; Debug print
    (.then #(mem/load-rom % "roms/ibm.ch8"))
    (.then #(do (js/console.log "ROM loaded") %))    ; Debug print
    (.then #(swap! emulator-state assoc :memory %))
    (.then #(js/console.log "Done!"))
    (.then #(game-loop emulator-state))) ; Error handling


;; specify reload hook with ^:after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
