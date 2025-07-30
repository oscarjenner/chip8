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

(def key-values
  {"1" 0x1 "2" 0x2 "3" 0x3 "4" 0xC
   "q" 0x4 "w" 0x5 "e" 0x6 "r" 0xD
   "a" 0x7 "s" 0x8 "d" 0x9 "f" 0xE
   "z" 0xA "x" 0x0 "c" 0xB "v" 0xF})

(defn handeKeypress [event]
  (when-let [key (key-values (.-key event))]
    (swap! emulator-state #(assoc %1 :keys (conj (:keys %1) %2)) key)
    (println "Pressed keys:" (@emulator-state :keys))
    ))

(defn handleKeyRelease [event]
  (when-let [key (key-values (.-key event))]
    (swap! emulator-state #(assoc %1 :keys (disj (:keys %1) %2)) key)))

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

(def audio-context (js/AudioContext.))
(def oscillator (.createOscillator audio-context))
(def gain-node (.createGain audio-context))
(def previous-stop (atom false))
(def sound-playing (atom false))

(.connect oscillator gain-node)
(.connect gain-node (.-destination audio-context))
(set! (.-frequency.value oscillator) 440)
(set! (.-value (.-gain gain-node)) 0) 
(.start oscillator) 

(defn play-sound [] 
    (println "Playing sound")
    (set! (.-value (.-gain gain-node)) 0.3) 
    (reset! sound-playing true)
    (reset! previous-stop false))

(defn stop-sound []
  (when @sound-playing
    (if (true? @previous-stop)
      (do
        (println "Stopping sound")
        (set! (.-value (.-gain gain-node)) 0)
        (reset! sound-playing false))
      (reset! previous-stop true))))



(defn game-loop [state]
  (swap! state #(cpu/nx-fetch-decode-execute % 15))
  (let [current-state @state]
      ;(println "Timers:" (:timers current-state))
    (paintCanvas (:display current-state))
      ;(println (pos? (:sound (:timers current-state))))
    (if (pos? (:sound (:timers current-state)))
      (play-sound)
      (stop-sound)))
  (js/requestAnimationFrame #(game-loop state)))

(defn render-loop [state]
  (swap! state cpu/decrease-timers)
  (println "Timers:" (:timers @state))
  (if (pos? (:sound (:timers @state)))
    (play-sound)
    (stop-sound))
  (let [disp (:display @state)]
    (paintCanvas disp))
  (js/requestAnimationFrame #(render-loop state)))

(defn game [state]
  (js/setInterval (fn []
                    
                      (swap! state #(cpu/nx-fetch-decode-execute % 15)))
                  16)
  (render-loop state))


(println "Loading memory and fonts...")
(-> (mem/clean-memory)
    (mem/load-fonts "roms/font.csv")
    (.then #(do (js/console.log "Fonts loaded") %))  ; Debug print
    (.then #(mem/load-rom % "roms/7-beep.ch8"))
    (.then #(do (js/console.log "ROM loaded") %))    ; Debug print
    (.then #(swap! emulator-state assoc :memory %))
    (.then #(do 
              (js/console.log "Done!") 
              (game-loop emulator-state)))
    )


;; specify reload hook with ^:after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
