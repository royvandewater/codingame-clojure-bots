(ns Player
  (:gen-class))


; debug prints arguments to standard error
(defn debug [& args]
  (binding [*out* *err*]
    (apply println args)))

; debug-board prints a board out to standard error
(defn debug-board [board]
  (map debug board))

(defn read-one-line [i] (read))

(defn read-n-lines [n]
  (doall (map read-one-line (range 0 n))))

(defn number-of-lines [opponentCount]
  (let [gameLines 4
        opponentLines (* opponentCount 3)
        boardLines 20]
    (+ gameLines opponentLines boardLines)))

; parse-board assumes the last 20 indexes of input
; represent the board and returns them
(defn parse-board [input]
  (take-last 20 input))

; parse-game assumes the first 4 indexes of input
; are metadata about the current game state and builds
; a hash-map with the relevant data
(defn parse-game [input]
  { :game-round        (nth input 0)
    :x                 (nth input 1)
    :y                 (nth input 2)
    :back-in-time-left (nth input 3)})

; parse-opponents assumes that whatever ain't the game
; or the board must be the opponents. Returns an array
; of hashmaps, each representing one opponent
(defn parse-opponents [input]
  (let [opponents (drop 4 (drop-last 20 input))]
  (for [i (range 0 (count opponents) 3)]
    { :x                 (nth opponents (+ i 0))
      :y                 (nth opponents (+ i 1))
      :back-in-time-left (nth opponents (+ i 2))})))

(defn parse-input [input]
  [(parse-game input) (parse-opponents input) (parse-board input)])

(defn -main [& args]
  (let [opponentCount (read)]
    (while true
      (let [input (read-n-lines (number-of-lines opponentCount))
           [game opponents board] (parse-input input)]

        (debug "game:" game)
        (debug "opponents" opponents)
        (debug "board" board)
        (println "17 10")))))
