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

(defn print-board
  "prints a board out to standard out"
  [board]
  (doall (map println board)))

(defn print-hash
  "print a hash out nice"
  [hash]
  (doall (for [key (keys hash)]
    (println key (get hash key)))))

(defn in?
  "returns true if the element is in the collection"
  [collection elem]
  (some #(= elem %) collection))

(defn max-by
  "returns the element in the collection with the greatest
  values as defined by the result of passing that element
  to f"
  [f coll]
  (reduce #(if (> (f %1) (f %2)) %1 %2) coll))

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

(defn board-height
  "returns the height of the board"
  [board]
  (count board))

(defn board-width
  "returns the width of the board"
  [board]
  (count (first board)))

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

(defn subsequence
  "returns a subsequence of the on passed in"
  [sequence start end]
  (drop start (take end sequence)))

(defn extract-board-subset
  "returns the subset of the board indicated
  by the rectangular coordinates"
  [board rect]
  (let [{:keys [x1 y1 x2 y2]} rect
        relevantRows (subsequence board y1 y2)]
    (map #(subs % x1 x2) relevantRows)))

(defn extract-cell
  "returns the rune of the cell at the coordinates"
  [board {:keys [x y]}]
  (nth (nth board y) x))

(defn owned?
  "returns true if the x and y coordinates are
  owned by the player"
  [board x y]
  (let [row  (nth board y)
        cell (nth row x)]
        (cell-owned-by-me? cell)))

(defn cell-free?
  "returns true if the cell is '.'"
  [cell]
  (= cell \.))

(defn origin-free?
  "returns true if the cell at the given coordinates
  is '.'"
  [board origin]
  (cell-free? (extract-cell board origin)))

(defn row-free?
  "returns true if every cell in the row is a '.'"
  [row]
  (every? cell-free? row))

(defn is-square?
  "returns true if the rectangle is a square"
  [{:keys [x1, x2, y1, y2]}]
  (let [width  (- x2 x1)
        height (- y2 y1)]
    (and
      (<= 0 height)
      (<= 0 width)
      (= height width))))

(defn all-squares
  "returns a lazy sequence of all possible square combinations
  in the board"
  [board]
  (filter is-square?
    (let [numRows (count board)
          numCols (count (first board))]
      (for [x1 (range numCols), y1 (range numRows)
            x2 (range numCols), y2 (range numRows)]
        {:x1 x1, :y1 y1, :x2 x2, :y2 y2}))))

(defn rectangle-free?
  "returns true if the described rectangle is completely
  inside the board and contains no non '.' squares within"
  [board rectangle]
  (cond
    (<= (board-width board)  (:x2 rectangle)) false
    (<= (board-height board) (:y2 rectangle)) false
    :else
      (let [subset (extract-board-subset board rectangle)]
        (every? row-free? subset))))

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

(defn center-of-square
  "Returns a hash containing the x and y
  coordinates of the center of the square passed in"
  [{:keys [x1, x2, y1, y2]}]
  (let [halfWidth  (/ (- x2 x1) 2)
        halfHeight (/ (- y2 y1) 2)]
    {
      :x (int (+ x1 halfWidth))
      :y (int (+ y1 halfHeight))
    }))

(defn rectangle-height
  "returns the height of the rectangle"
  [{:keys [y1 y2]}]
  (int (- (inc y2) y1)))

(defn rectangle-width
  "returns the width of the rectangle"
  [{:keys [x1 x2]}]
  (int (- (inc x2) x1)))

(defn rectangle-area
  "returns the area of the rectangle"
  [rectangle]
  (*
    (rectangle-width rectangle)
    (rectangle-height rectangle)))

(defn next-origin
  "returns the next origin on the board, nil if no more
  origins exist"
  [board {:keys [x y]}]
  (cond
    (< (inc x) (board-width board))  {:x (inc x), :y y}
    (< (inc y) (board-height board)) {:x 0, :y (inc y)}
    :else nil))

(defn first-free-origin
  "returns the first free point on the board"
  [board]
  (loop [origin {:x 0, :y 0}]
    (cond
      (nil? origin) nil
      (origin-free? board origin) origin
      :else (recur (next-origin board origin)))))

(defn origin-to-square
  "generates a 1x1 square for the given coordinates"
  [{:keys [x y]}]
  {:x1 x, :y1 y, :x2 x, :y2 y})

(defn larger-free-square-for-origin
  "return a larger free square for the same origin. Returns
  nil if no bigger squares exist"
  [board origin square]
  (let [{:keys [x y]} origin
        height (rectangle-height square)
        width  (rectangle-width square)
        largerSquare {:x1 x, :y1 y, :x2 (+ x width), :y2 (+ y height)}]
    (if (rectangle-free? board largerSquare)
      largerSquare)))

(defn largest-free-square
  "Returns a hash containing the x1, y1, x2, y2 coordinates
  that define the largest un-owned square on the board"
  [board]
  (loop [origin (first-free-origin board)
         square (origin-to-square origin)]
    (let [largerSquare (larger-free-square-for-origin board origin square)
          nextOrigin   (next-origin board origin)]
      (cond
        (some? largerSquare) (recur origin largerSquare)
        (some? nextOrigin)   (recur nextOrigin square)
        :else square))))

(defn center-of-largest-free-square
  "Returns the coordinates of the cell in the center
  of the largest free square"
  [board]
  (center-of-square
    (largest-free-square board)))

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
           (println (format "%s %s" (:x cell) (:y cell)))))))
