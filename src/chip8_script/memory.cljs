(ns chip8-script.memory
  (:require 
   [clojure.string :as s]
   ))

(defn clean-memory
  "Creates clean 4kB memory as a vector"
  []
  (vec (repeat 4096 0x00)))

(defn unsigned-byte
  "Converts signed byte (read in by Java) to unsigned"
  [signed-byte]
  (bit-and signed-byte 0xff))



(defn read-rom
  "Gives sequence of unsigned bytes"
  [filename]
  (-> (js/fetch filename)
      (.then (fn [response]
               (.arrayBuffer response)))
      (.then (fn [buffer]
               (let [ba (js/Uint8Array. buffer)]
                 (mapv unsigned-byte ba))))))

(defn write-at
  [memory position value]
  (if (< position (count memory))
   (assoc memory position value)
    memory))

(defn write-from
  "Writes a sequence to memory beginning at a certain point"
  [memory position values]
  (if-let [[first & rest] values]
    (recur (write-at memory position first) (inc position) rest)
    memory))

(defn load-rom
  [memory rom-path]
  (.then (read-rom rom-path)
         (fn [rom]
           ;; Write the ROM to memory starting at 0x200
           (write-from memory 0x200 rom))))

(defn parse-hex
  [number]
  (js/parseInt (last (s/split number #"0x")) 16))

(defn read-fonts
  [font-path]
  (-> (js/fetch font-path)
      (.then (fn [response]
               (.text response)))
      (.then (fn [csv]
               (->> csv
                    (s/split #"\n")
                    (map #(first (s/split % #" //")))
                    (map #(s/split % #","))
                    flatten
                    (map s/trim)
                    (map parse-hex))))))

(defn load-fonts
  "Loads fonts from the csv to memory at 050 as recommended by Tobias Langhoff"
  [memory font-path]
  (.then (read-fonts font-path)
         (fn [font]
           ;; Write the font to memory starting at 0x050
           (write-from memory 0x050 font))))

(defn read-instruction
  [memory position]
  (+ (bit-shift-left (get memory position) 8)
     (get memory (inc position))))

(defn read-n-bytes
  ([memory position n]
   (read-n-bytes memory position n []))
  ([memory position n have-read]
   (if (> n 0)
     (recur memory (inc position) (dec n) (conj have-read (get memory position)))
     have-read)))