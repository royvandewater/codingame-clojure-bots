(ns Player
  (:gen-class))

(require '[clojure.pprint :as pprint])

(defn debug
  "prints arguments to standard error"
  [& args]
  (binding [*out* *err*]
    (apply println args)))

(defn debug-map
  [map]
  (binding [*out* *err*]
    (pprint/pprint map)))

; Auto-generated code below aims at helping you parse
; the standard input according to the problem statement.

(def gravity -3.711)

(defn sum
  "calculate the sum of the values in the col"
  [col]
  (reduce + col))

(defn mean
  "calculate the mean value of the col"
  [col]
  (/ (sum col) (count col)))

(defn simulate-turn
  "Returns a vector with the new height and speed
  after one turn passes at the given thrust."
  [height speed thrust]

  (let [nextSpeed    (+ speed gravity thrust)
        averageSpeed (mean [speed nextSpeed])
        nextHeight   (+ height averageSpeed)]
    [nextHeight nextSpeed]))

(defn inc-thrust
  "returns thrust + 1 to an upper limit of 4"
  [thrust]
  (let [newThrust (inc thrust)]
    (if (> newThrust 4) 4 newThrust)))

(defn do-i-die?
  "returns true if initiating full throttle at this point
  causes death"
  [height speed thrust]
  ; (debug "\ndo-i-die?")
  (loop [height height
         speed  speed
         thrust (inc-thrust thrust)]
    (let [[newHeight newSpeed] (simulate-turn height speed thrust)]
      ; (if (or (< newHeight 0) (> newSpeed 0))
      ;   (debug-map {:height newHeight, :speed newSpeed}))
      (cond
        (< newHeight 0) true
        (> newSpeed  0) false
        :else (recur newHeight newSpeed (inc-thrust thrust))))))

(defn last-chance?
  "returns true if this is the last turn where the lander
  can survive a fall, provided it start boosting right away.
  This is calculated by simulating a boost starting the following
  turn and seeing if it smashes into the surface of Mars"
  [height speed]

  (let [[nextHeight nextSpeed] (simulate-turn height speed 0)]
    (do-i-die? nextHeight nextSpeed 0)))

(defn -main [& args]
  (let [surfaceN (read)]
    ; surfaceN: the number of points used to draw the surface of Mars.
    (loop [i surfaceN]
      (when (> i 0)
        (let [landX (read) landY (read)]
          ; landX: X coordinate of a surface point. (0 to 6999)
          ; landY: Y coordinate of a surface point. By linking all the points together in a sequential fashion, you form the surface of Mars.
        (recur (dec i)))))
    (while true
      (let [X (read) Y (read) hSpeed (read) vSpeed (read) fuel (read) rotate (read) power (read)]
        ; hSpeed: the horizontal speed (in m/s), can be negative.
        ; vSpeed: the vertical speed (in m/s), can be negative.
        ; fuel: the quantity of remaining fuel in liters.
        ; rotate: the rotation angle in degrees (-90 to 90).
        ; power: the thrust power (0 to 4).

        ; (binding [*out* *err*]
        ;   (println "Debug messages..."))

        ; 2 integers: rotate power. rotate is the desired rotation angle (should be 0 for level 1), power is the desired thrust power (0 to 4).
        (let [height Y
             [nextHeight nextSpeed] (simulate-turn height vSpeed 0)]
          (if (last-chance? height vSpeed)
            (println "0 4")
            (println "0 0")))))))
