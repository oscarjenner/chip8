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
    0xE (case (bit-and 0xFF opcode)
          0x9E :SKP-Vx
          0xA1 :SKNP-Vx)
    0xF (case (bit-and 0xFF opcode)
          0x07 :LD-Vx-DT
          0x0A :LD-Vx-K
          0x15 :LD-DT-Vx
          0x18 :LD-ST-Vx
          0x1E :ADD-I-Vx
          0x29 :LD-F-Vx
          0x33 :LD-B-Vx
          0x55 :LD-I-Vx
          0x65 :LD-Vx-I)))


(defn byte->pixels
  [byte]
  (let [byte-string (.toString byte 2)] 
    (map #(= "1" (str %))
         (str
          (apply str (take (- 8 (count byte-string)) (repeat "0"))) ; This is just padding
          byte-string))))

(defn bytes->sprite
  ([bytes x y]
   (bytes->sprite bytes (mod x 64) (mod y 32) (vec (take (* 32 64) (repeat false)))))
  ([bytes x y sprite]
   (let [[first & rest] bytes]
     (if first
       (recur rest x (inc y)
            (mem/write-from sprite (+ x (* 64 y)) (byte->pixels first)))
     sprite))))

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
                      (assoc-in [:registers (read-nibble opcode 2)] (bit-and (bit-shift-left vx 1) 0xFF))
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

   :RND-Vx-byte (fn [state opcode] ; Random number AND byte, stores in Vx
                  (assoc-in state [:registers (read-nibble opcode 2)]
                            (bit-and (rand-int 256) (bit-and 0x00FF opcode))))

   :DRW-Vx-Vy-nibble (fn [state opcode] ; 
                       (let [[vx vy] (map #(get-in state [:registers (read-nibble opcode %)]) '(2 3))
                             bytes (mem/read-n-bytes (:memory state) (:i state) (read-nibble opcode 4))]
                         (let [old-display (:display state) sprite (bytes->sprite bytes vx vy)]
                           (-> state
                               (assoc :display (vec (map xor old-display sprite)))
                               (assoc-in [:registers 0xF] (if (some true? (map #(= %1 %2) old-display sprite)) 1 0))))))
   :SKP-Vx (fn [state opcode] ; Skip next instruction if key Vx is pressed
             (if (contains? (:keys state)  (get-in state [:registers (read-nibble opcode 2)]))
               (update state :pc #(+ 2 %))
               state))
   :SKNP-Vx (fn [state opcode] ; Skip next instruction if key Vx is not pressed
              (if-not (contains? (:keys state) (get-in state [:registers (read-nibble opcode 2)]))
                (update state :pc #(+ 2 %))
                state))
   :LD-Vx-DT (fn [state opcode] ; Load delay timer to Vx
               (assoc-in state [:registers (read-nibble opcode 2)] (bit-and (:delay (:timers state)) 0xFF)))
   :LD-Vx-K (fn [state opcode] ; Wait for key press, store in Vx
              (if (empty? (:keys state))
                (update state :pc #(- 2 %)) ; This handles "waiting", maybe does not work
                (assoc-in state [:registers (read-nibble opcode 2)]
                          (first (:keys state)))))
   :LD-DT-Vx (fn [state opcode] ; Load Vx to delay timer
               (assoc-in state [:timers :delay]
                         (get-in state [:registers (read-nibble opcode 2)])))
   :LD-ST-Vx (fn [state opcode] ; Load Vx to sound timer
               (assoc-in state [:timers :sound]
                         (get-in state [:registers (read-nibble opcode 2)])))
   :ADD-I-Vx (fn [state opcode] ; Add Vx to I
               (assoc state :i
                      (bit-and (+ (:i state) (get-in state [:registers (read-nibble opcode 2)])) 0xFFF)))
   :LD-F-Vx (fn [state opcode] ; Load font address for Vx
              (assoc state :i
                     (+ 0x050 (* 5 (get-in state [:registers (read-nibble opcode 2)])))))
   :LD-B-Vx (fn [state opcode] ; Store BCD representation of Vx at I, I+1, I+2
              (let [vx (get-in state [:registers (read-nibble opcode 2)])
                    hundreds (mod (quot vx 100) 10)
                    tens (mod (quot vx 10) 10)
                    units (mod vx 10)]
                (update state :memory #(mem/write-from % (:i state)
                                                       [hundreds tens units]))))
   :LD-I-Vx (fn [state opcode] ; Load Vx registers to memory starting at I
              (reduce (fn [s k]
                        (assoc-in s [:memory (+ (:i state) k)]
                               (get-in state [:registers k])))
                      state
                      (range (inc (read-nibble opcode 2))))) 
    :LD-Vx-I (fn [state opcode] ; Load memory starting at I to Vx registers
               (reduce (fn [s k]
                         (assoc-in s [:registers k] (get-in state [:memory (+ (:i state) k)])))
                       state
                       (vec (range (inc (read-nibble opcode 2))))))
   
   })

   (comment (fn [state opcode] ; Load memory starting at I to Vx registers
              (reduce (fn [s k]
                        (println "memory at I" (get-in state [:memory (:i state)]) "v0" (get-in state [:registers 0]))
                        (let [byte (get-in state [:memory (+ (:i state) k)])]
                          (println "byte is" byte)
                          (println "k is" k)
                          (println "v0 is" (get-in s [:registers 0]))
                          (assoc-in s [:registers k] byte)))
                      state
                      (vec (range (read-nibble opcode 2))))))

(defn decrease-timers
  [state]
  (let [timers (:timers state)]
    (-> state
        (assoc-in [:timers :delay] (max 0 (dec (:delay timers))))
        (assoc-in [:timers :sound] (max 0 (dec (:sound timers)))))))

(defn fetch-decode-execute
  [state]
  (let [opcode (mem/read-instruction (:memory state) (:pc state))]
    ;(println "The opcode is " opcode)
    (let [updated-state (-> state
                            (update :pc #(+ 2 %)))]
      (if-let [com (decode opcode)]
        (if-let [fun (get commands com)]
          (fun updated-state opcode)
          updated-state)
        updated-state))))