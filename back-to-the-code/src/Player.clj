(ns Player
  (:gen-class))

(require '[clojure.string :as str])

; debug prints arguments to standard error
(defn debug [& args]
  (binding [*out* *err*]
    (apply println args)))

; debug-board prints a board out to standard error
(defn debug-board [board]
  (doall (map debug board)))

(defn read-one-line [i]
  (read-line))

(defn read-n-lines [n]
  (doall (for [i (range 0 n)]
    (read-one-line i))))
  ; (doall (map read-one-line (range 0 n))))

; number-of-lines assumes a board has:
;   * 1 game state line
;   * 1 player state line
;   * n opponent state lines where n is opponentCount
;   * 20 board state lines
(defn number-of-lines [opponentCount]
  (let [game      1
        player    1
        opponents opponentCount
        board     20]
    (+ game player opponents board)))

; parse-board assumes the last 20 indexes of input
; represent the board and returns them
(defn parse-board [input]
  (take-last 20 input))

; parse-game assumes the first 2 indexes of input
; are metadata about the current game state and builds
; a hash-map with the relevant data
(defn parse-game [input]
  (let [round  (nth input 1)
        player (nth input 2)
        [x y backInTimeLeft] (str/split player #"\s")]
  { :game-round        (read-string round)
    :x                 (read-string x)
    :y                 (read-string y)
    :back-in-time-left (read-string backInTimeLeft)}))

; parse-opponents assumes that whatever ain't the game
; or the board must be the opponents. Returns an array
; of hashmaps, each representing one opponent
(defn parse-opponents [input]
  (let [opponents (drop 3 (drop-last 20 input))]
  (for [opponent opponents]
    (let [[x y backInTimeLeft] (str/split opponent #"\s")]
    { :x                 (read-string x)
      :y                 (read-string y)
      :back-in-time-left (read-string backInTimeLeft)}))))

; parse-input takes a sequence of strings and makes
; a lot of assumptions about what they contain
(defn parse-input [input]
  [(parse-game input) (parse-opponents input) (parse-board input)])

(defn visited? [board x y]
  (let [row  (nth board y)
        cell (nth row x)]
        (debug "cell" cell x y)
        (= \0 cell)))

; cell-owned-by-me? returns true if the cell
; is owned by me (is equal to \0)
(defn cell-owned-by-me? [cell] (= cell \0))

; go-down returns the cell just one down from
; the one passed in
(defn go-down [game]
  { :x (:x game)
    :y (+ (:y game) 1)})

; go-left returns the cell just one left from
; the one passed in
(defn go-left [game]
  { :x (- (:x game) 1)
    :y (:y game)})

; go-right returns the cell just one right from
; the one passed in
(defn go-right [game]
  { :x (+ (:x game) 1)
    :y (:y game)})

; go-up returns the cell just one up from
; the one passed in
(defn go-up [game]
  { :x (:x game)
    :y (- (:y game) 1)})

(defn going-down? [game board] true)
(defn going-left? [game board] true)
(defn going-right? [game board] true)
(defn going-up? [game board] true)

; number-owned returns the number of cells
; owned by the player in a given row
(defn number-owned [row]
  (count (filter cell-owned-by-me? row)))

; partially-owned? returns true if at least one
; cell in the row is owned by the player
(defn partially-owned? [row] (some cell-owned-by-me? row))

(defn max-full-edge-length [board]
  (let [rowsOwnedByMe (filter partially-owned? board)]
    (cond
      (empty? rowsOwnedByMe) 0
      :else (apply min (map number-owned rowsOwnedByMe)))))

(defn stay-here [game])

(defn target-edge-length [board]
  (+ 1 (max-full-edge-length board)))

(defn spiral-outwards [game board]
  (cond
    (going-up? game board) (go-up game)
    (going-right? game board) (go-right game)
    (going-down? game board) (go-down game)
    (going-left? game board) (go-left)
    :else (stay-here game)))
  ; (let [targetEdgeLength (target-edge-length board)]
  ;   (debug "targetEdgeLength" targetEdgeLength)
  ;   (visit-origin)))

; ........
; ...0....
; ........

; target-cell returns the cell that the player would
; be best served by moving towards
(defn target-cell [game opponents board]
  (spiral-outwards game board))

(defn -main [& args]
  (let [opponentCount (read-string (read-line))]
    (while true
      (let [input (read-n-lines (number-of-lines opponentCount))
           [game opponents board] (parse-input input)
           cell   (target-cell game opponents board)]
          ;  (debug-board board)
           (println (format "%s %s" (cell :x) (cell :y)))))))
