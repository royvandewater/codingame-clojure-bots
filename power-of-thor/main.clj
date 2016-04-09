(ns Player
  (:gen-class))

; debug prints arguments to standard error
(defn debug [& args]
  (binding [*out* *err*]
    (apply println args)))

; x-component returns:
; * [currentX+1, "E"] if the target is East of you
; * [currentX-1, "W"] if the target is West of you
; * [currentX,   ""] otherwise
(defn x-component [targetX currentX]
  (cond
    (< currentX targetX) [(+ currentX 1) "E"]
    (> currentX targetX) [(- currentX 1) "W"]
    :else [currentX ""]))

; y-component returns:
; * [currentX+1, "S"] if the target is South of you
; * [currentX-1, "N"] if the target is North of you
; * [currentX,   ""] otherwise
(defn y-component [targetY currentY]
  (cond
    (< currentY targetY) [(+ currentY 1) "S"]
    (> currentY targetY) [(- currentY 1) "N"]
    :else [currentY ""]))

; thor-move reads the garbage standard in and moves, baby
(defn thor-move [targetX targetY currentX currentY]
  (read) ; throw away remaining turns line
  (let [
    [newY componentY] (y-component targetY currentY)
    [newX componentX] (x-component targetX currentX)]
    (println (clojure.string/join "" [componentY componentX]))
    (thor-move targetX targetY newX newY)))

; Auto-generated code below aims at helping you parse
; the standard input according to the problem statement.
; ---
; Hint: You can use the debug stream to print initialTX and initialTY, if Thor seems not follow your orders.
(defn -main [& args]
  (let [lightX (read) lightY (read) initialTX (read) initialTY (read)]
    ; lightX: the X position of the light of power
    ; lightY: the Y position of the light of power
    ; initialTX: Thor's starting X position
    ; initialTY: Thor's starting Y position
    (thor-move lightX lightY initialTX initialTY)))
