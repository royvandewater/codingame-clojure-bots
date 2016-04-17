(ns Player
  (:gen-class))

(require '[clojure.string :as str])

(defn debug
  "prints arguments to standard error"
  [& args]
  (binding [*out* *err*]
    (apply println args)))

(defn debug-board
  "prints a board out to standard error"
  [board]
  (doall (map debug board)))

(defn debug-cell
  "prints a cell to standard error in the format '{x: <x>, y: <y>}'"
  [msg cell]
  (debug msg (format "{x: %s, y: %s}" (:x cell) (:y cell))))

(defn read-one-line
  "calls read line, throws away the arg"
  [i]
  (read-line))

(defn read-n-lines
  "reads n lines and returns them"
  [n]
  (doall (map read-one-line (range 0 n))))

(defn number-of-lines
  "returns the number of lines in a game board assuming:
    * 1 game state line
    * 1 player state line
    * n opponent state lines where n is opponentCount
    * 20 board state lines"
  [opponentCount]
  (let [game      1
        player    1
        opponents opponentCount
        board     20]
    (+ game player opponents board)))

(defn parse-board
  "assumes the last 20 indexes of input represent
  the board and returns them"
  [input]
  (take-last 20 input))

(defn parse-game
  "assumes the first 2 indexes of input are
  metadata about the current game state and
  builds a hash-map with the relevant data"
  [input]
  (let [round  (nth input 0)
        player (nth input 1)
        [x y backInTimeLeft] (str/split player #"\s")]
    { :game-round        (read-string round)
      :x                 (read-string x)
      :y                 (read-string y)
      :back-in-time-left (read-string backInTimeLeft)}))

(defn parse-opponents
  "assumes that whatever ain't the game or the board
  must be the opponents. Returns an array of hashmaps,
  each representing one opponent"
  [input]
  (let [opponents (drop 2 (drop-last 20 input))]
    (for [opponent opponents]
      (let [[x y backInTimeLeft] (str/split opponent #"\s")]
        { :x                 (read-string x)
          :y                 (read-string y)
          :back-in-time-left (read-string backInTimeLeft)}))))

; parse-input
;
(defn parse-input
  "takes a sequence of strings and makes a lot of
  assumptions about what they contain"
  [input]
  [(parse-game input) (parse-opponents input) (parse-board input)])

(defn extract-column
  "returns a list of characters representing the nth
  column of the board"
  [n board]
  (map #(nth % n) board))

(defn to-columns
  "returns a list of lists of characters representing
  representing each column of the board"
  [board]
  (let [numCols (count (first board))]
    (for [n (range numCols)]
      (extract-column n board))))

(defn cell-owned-by-me?
  "returns true if the cell is equal to '0'"
  [cell]
  (= cell \0))

(defn owned?
  "returns true if the x and y coordinates are
  owned by the player"
  [board x y]
  (let [row  (nth board y)
        cell (nth row x)]
        (cell-owned-by-me? cell)))

(defn go-down
  "returns the cell just one down from the one passed in"
  [game]
  { :x (:x game)
    :y (+ (:y game) 1)})

(defn go-left
  "returns the cell just one left from the one passed in"
  [game]
  ; (debug "going-left")
  { :x (- (:x game) 1)
    :y (:y game)})

(defn go-right
  "returns the cell just one right from the one passed in"
  [game]
  ; (debug "going-right")
  { :x (+ (:x game) 1)
    :y (:y game)})

(defn go-up
  "returns the cell just one up from the one passed in"
  [game]
  ; (debug "going-up")
  { :x (:x game)
    :y (- (:y game) 1)})

(defn number-owned
  "returns the number of cells owned by the player
  in a given row"
  [row]
  (count (filter cell-owned-by-me? row)))

(defn partially-owned?
  "returns true if at least one cell in the row
  is owned by the player"
  [row]
  (some cell-owned-by-me? row))

(defn center-of-largest-free-square
  "Returns the coordinates of the cell in the center
  of the largest free square"
  [board]
  {
    :x (int (/ (count board) 2))
    :y (int (/ (count board) 2))
  })

(defn target-cell
  "Returns a hashmap with the :x and :y coordinates of where
  the player should move next"
  [game opponents board]
  (center-of-largest-free-square board))

(defn -main [& args]
  (let [opponentCount (read-string (read-line))]
    (while true
      (let [input (read-n-lines (number-of-lines opponentCount))
           [game opponents board] (parse-input input)
           cell   (target-cell game opponents board)]
          ;  (println (format "%s %s" 4 3))))))
           (println (format "%s %s" (:x cell) (:y cell)))))))
