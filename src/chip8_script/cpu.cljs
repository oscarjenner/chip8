(ns chip8-script.cpu
  (:require
   [chip8-script.memory :as mem]
   [clojure.string :as s]))

(defn read-nibble
  "Reads n:th nibble (n in 1 2 3 4)"
  [opcode n]
  (let [shift (* 4 (- 4 n))]
    (-> opcode
        (bit-and (bit-shift-left 0xF shift))
        (bit-shift-right shift))))

(defn decode
  "Takes opcode and determines which command to run"
  [opcode]
  ; Check the first nibble:
  (case (read-nibble opcode 1)
    0 (case opcode
        0x00E0 :CLS
        0x00EE :RET
        nil)
    1 :JP-addr
    2 :CALL-addr
    3 :SE-Vx-byte
    4 :SNE-Vx-byte
    5 :SE-Vx-Vy
    6 :LD-Vx-byte
    7 :ADD-Vx-byte
    8 (case (read-nibble opcode 4)
        0 :LD-Vx-Vy
        1 :OR-Vx-Vy
        2 :AND-Vx-Vy
        3 :XOR-Vx-Vy
        4 :ADD-Vx-Vy
        5 :SUB-Vx-Vy
        6 :SHR-Vx-Vy
        7 :SUBN-Vx-Vy
        0xE :SHL-Vx-Vy)
    9 (when (= 0 (read-nibble opcode 4)) :SNE-Vx-Vy)
    0xA :LD-I-addr
    0xB :JP-V0-addr
    0xC :RND-Vx-byte
    0xD :DRW-Vx-Vy-nibble
    ; E F TODO
    0xE nil
    0xF nil))

(defn byte->pixels
  [byte]
  (let [byte-string (.toString byte 2)]
    (map #(= "1" (str %))
         (str
          (apply str (take (- 8 (count byte-string)) (repeat "0"))) ; This is just padding
          byte-string))))

(defn bytes->sprite
  ([bytes x y]
   (bytes->sprite bytes x y (vec (take (* 32 64) (repeat false)))))
  ([bytes x y sprite]
   (if-let [[first & rest] bytes]
     (recur rest x (inc y)
            (mem/write-from sprite (+ x (* 64 y)) (byte->pixels first)))
     sprite)))

(defn xor
  [a b]
  (not= (boolean a) (boolean b)))


(def commands
  {:CLS (fn [state opcode] ; Clear display
          (assoc state :display (vec (repeat (* 64 32) false))))
   :RET (fn [state opcode] ; Return from subroutine
          (let [[first & rest] (:stack state)]
            (-> state
                (assoc :pc first)
                (assoc :stack rest))))

   :JP-addr (fn [state opcode] ; Jump to address
              ;(println state)
              ;(println (:pc state))
              ;(println (+ 1 (bit-and 0x0FFF opcode)))
              (assoc state :pc (bit-and 0x0FFF opcode)))

   :CALL-addr (fn [state opcode] ; Call subroutine at NNN
                (-> state
                    (assoc :stack (conj (:stack state) (:pc state)))
                    (assoc :pc (bit-and 0x0FFF opcode))))

   :SE-Vx-byte (fn [state opcode] ; Skip if equal
                 (if (= (get-in state [:registers (read-nibble opcode 2)]) (bit-and 0x00FF opcode))
                   (update state :pc #(+ 2 %))
                   state))

   :SNE-Vx-byte (fn [state opcode] ; Skip if not equal
                  (if-not (= (get-in state [:registers (read-nibble opcode 2)]) (bit-and 0x00FF opcode))
                    (update state :pc #(+ 2 %))
                    state))

   :SE-Vx-Vy (fn [state opcode] ; Skip if Vx = Vy
               (if (= (get-in state [:registers (read-nibble opcode 2)]) (get-in state [:registers (read-nibble opcode 3)]))
                 (update state :pc #(+ 2 %))
                 state))

   :LD-Vx-byte (fn [state opcode] ; Load byte to register
                 (assoc-in state [:registers (read-nibble opcode 2)] (bit-and 0x00FF opcode)))
   :ADD-Vx-byte (fn [state opcode] ; Add byte to register
                  (assoc-in state [:registers (read-nibble opcode 2)]
                            (-> opcode
                                (bit-and 0x00FF)
                                (+ (get-in state [:registers (read-nibble opcode 2)]))
                                (bit-and 0x00FF))))
   :LD-Vx-Vy (fn [state opcode] ; Load register Vy to Vx
               (assoc-in state [:registers (read-nibble opcode 2)]
                         (get-in state [:registers (read-nibble opcode 3)])))
   :OR-Vx-Vy (fn [state opcode]; Bitwise or between Vx and Vy, stores in Vx
               (assoc-in state [:registers (read-nibble opcode 2)]
                         (bit-or (get-in state [:registers (read-nibble opcode 2)])
                                 (get-in state [:registers (read-nibble opcode 3)]))))
   :AND-Vx-Vy (fn [state opcode] ; Bitwise and between Vx and Vy, stores in Vx
                (assoc-in state [:registers (read-nibble opcode 2)]
                          (bit-and (get-in state [:registers (read-nibble opcode 2)])
                                   (get-in state [:registers (read-nibble opcode 3)]))))
   :XOR-Vx-Vy (fn [state opcode] ; Bitwise xor between Vx and Vy, stores in Vx
                (assoc-in state [:registers (read-nibble opcode 2)]
                          (bit-xor (get-in state [:registers (read-nibble opcode 2)])
                                   (get-in state [:registers (read-nibble opcode 3)]))))
   :ADD-Vx-Vy (fn [state opcode] ; Add Vy to Vx, stores in Vx
                (let [sum (+ (get-in state [:registers (read-nibble opcode 2)])
                             (get-in state [:registers (read-nibble opcode 3)]))
                      carry (if (> sum 255) 1 0)]
                  (-> state
                      (assoc-in [:registers (read-nibble opcode 2)] (bit-and sum 0xFF))
                      (assoc-in [:registers 0xF] carry))))
   :SUB-Vx-Vy (fn [state opcode] ; Subtract Vy from Vx, stores in Vx
                (let [diff (- (get-in state [:registers (read-nibble opcode 2)])
                              (get-in state [:registers (read-nibble opcode 3)]))]
                  (-> state
                      (assoc-in [:registers (read-nibble opcode 2)] (bit-and diff 0xFF))
                      (assoc-in [:registers 0xF] (if (neg? diff) 0 1))))) ; Set VF to 1 if no borrow, else 0
   :SHR-Vx-Vy (fn [state opcode] ; Shift Vx right by 1, stores in Vx, sets VF to LSB of Vx before shift
                (let [vx (get-in state [:registers (read-nibble opcode 2)])
                      lsb (bit-and vx 0x01)]
                  (-> state
                      (assoc-in [:registers (read-nibble opcode 2)] (bit-shift-right vx 1))
                      (assoc-in [:registers 0xF] lsb))))
   :SUBN-Vx-Vy (fn [state opcode] ; Subtract Vx from Vy, stores in Vx
                 (let [diff (- (get-in state [:registers (read-nibble opcode 3)])
                               (get-in state [:registers (read-nibble opcode 2)]))]
                   (-> state
                       (assoc-in [:registers (read-nibble opcode 2)] (bit-and diff 0xFF))
                       (assoc-in [:registers 0xF] (if (neg? diff) 0 1))))) ; Set VF to 1 if no borrow, else 0
   :SHL-Vx-Vy (fn [state opcode] ; Shift Vx left by 1, stores in Vx, sets VF to MSB of Vx before shift
                (let [vx (get-in state [:registers (read-nibble opcode 2)])
                      msb (bit-shift-right (bit-and vx 0x80) 7)] ; Get the most significant bit
                  (-> state
                      (assoc-in [:registers (read-nibble opcode 2)] (bit-shift-left vx 1))
                      (assoc-in [:registers 0xF] msb))))
   
   :SNE-Vx-Vy (fn [state opcode] ; Skip if Vx != Vy
                (if-not (= (get-in state [:registers (read-nibble opcode 2)]) 
                           (get-in state [:registers (read-nibble opcode 3)]))
                  (update state :pc #(+ 2 %))
                  state)) 


   :LD-I-addr (fn [state opcode] ; Set I to address
                (assoc state :i (bit-and 0x0FFF opcode)))
   
   :JP-V0-addr (fn [state opcode] ; Jump to address + V0
                 (assoc state :pc (+ (bit-and 0x0FFF opcode) (get-in state [:registers 0]))))
   

   :DRW-Vx-Vy-nibble (fn [state opcode] ; 
                       (let [[vx vy] (map #(get-in state [:registers (read-nibble opcode %)]) '(2 3))
                             bytes (mem/read-n-bytes (:memory state) (:i state) (read-nibble opcode 4))]
                         (let [old-display (:display state) sprite (bytes->sprite bytes vx vy)]
                           (-> state
                               (assoc :display (vec (map xor old-display sprite)))
                               (assoc-in [:registers 0xF] (if (some true? (map #(= %1 %2) old-display sprite)) 1 0))))))})


(defn fetch-decode-execute
  [state]
  ;(println "DEBUG: state parameter is:" state)
  ;(println "DEBUG: type of state:" (type state))
  ;(println "DEBUG: (:pc state) is:" (:pc state))
  (let [opcode (mem/read-instruction (:memory state) (:pc state))]
    ;(println "The opcode is " opcode)
    (let [updated-state (assoc state :pc (+ 2 (:pc state)))]
      (if-let [com (decode opcode)]
        (if-let [fun (get commands com)]
          (fun updated-state opcode)
          updated-state)
        updated-state))))