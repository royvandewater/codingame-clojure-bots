(ns Player
  (:gen-class))

; debug prints arguments to standard error
(defn debug [& args]
  (binding [*out* *err*]
    (apply println args)))

; debug-board prints a board out to standard error
(defn debug-board [board]
  (map debug board))

(defn read-one-line [i] (read-line))

(defn read-n-lines [n]
  (debug "read-n-lines" n)
  (doall (map read-one-line (range 0 n))))

; number-of-lines assumes a board has:
;   * 1 game state line
;   * 1 player state line
;   * n opponent state lines where n is opponentCount
;   * 20 board state lines
(defn number-of-lines [opponentCount]
  (let [game 1
        player 1
        opponents opponentCount
        board 20]
    (+ game player opponents board)))

; parse-board assumes the last 20 indexes of input
; represent the board and returns them
(defn parse-board [input]
  (take-last 20 input))

; parse-game assumes the first 4 indexes of input
; are metadata about the current game state and builds
; a hash-map with the relevant data
(defn parse-game [input]
  (let [game (nth input 0)]
  ; (debug "game:" game)
  { :game-round        (nth input 0)
    :x                 (nth input 1)
    :y                 (nth input 2)
    :back-in-time-left (nth input 3)}))

; parse-opponents assumes that whatever ain't the game
; or the board must be the opponents. Returns an array
; of hashmaps, each representing one opponent
(defn parse-opponents [input]
  (let [opponents (drop 4 (drop-last 20 input))]
  (for [i (range 0 (count opponents) 3)]
    { :x                 (nth opponents (+ i 0))
      :y                 (nth opponents (+ i 1))
      :back-in-time-left (nth opponents (+ i 2))})))

; parse-input takes a sequence of strings and makes
; a lot of assumptions about what they contain
(defn parse-input [input]
  (debug "parse-input" input)
  [(parse-game input) (parse-opponents input) (parse-board input)])

; is-origin returns true if the x & y coordinates
; are 0 & 0. false otherwise
(defn is-origin [coords]
  (and (= 0 (coords :x))
       (= 0 (coords :y))))

; target-cell returns the cell that the player would
; be best served by moving towards
(defn target-cell [game opponents board]
  (if (is-origin game)
    { :x 10 :y 10}
    { :x 0  :y 0}))

(defn -main [& args]
  (let [opponentCount (read)]
    (while true
      (let [input (read-n-lines (number-of-lines opponentCount))
           [game opponents board] (parse-input input)
           cell   (target-cell game opponents board)]
           (println (format "%s %s" (cell :x) (cell :y)))))))
