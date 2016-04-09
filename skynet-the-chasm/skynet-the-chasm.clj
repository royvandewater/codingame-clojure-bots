(ns Player
  (:gen-class))

; debug prints arguments to standard error
(defn debug [& args]
  (binding [*out* *err*]
    (apply println args)))

; gap-crossed? returns true if the
; motorbike is already past the gap
(defn gap-crossed? [road gap coordX]
  (>= coordX (+ road gap)))

; gap-next-turn? returns true if the
; motorbike will be in (or past) the gap
; within the next turn, assuming the speed
; stays constant
(defn gap-next-turn? [road coordX speed]
  (< road (+ coordX speed)))

; need-more-speed? returns true if the current
; speed is insufficient to cross the gap
(defn need-more-speed? [gap speed]
  (< speed (+ gap 1)))

; too-much-speed? returns true if the current
; speed is more than the minimum needed to cross
; the gap
(defn too-much-speed? [gap speed]
  (> speed (+ gap 1)))

; make-a-decision returns one of SPEED SLOW JUMP WAIT
(defn make-a-decision [road gap platform speed coordX]
  (debug road gap platform speed coordX)
  (cond
    (gap-crossed? road gap coordX) "SLOW"
    (gap-next-turn? road coordX speed) "JUMP"
    (need-more-speed? gap speed) "SPEED"
    (too-much-speed? gap speed) "SLOW"
    :else "WAIT"))


(defn -main [& args]
  (let [road (read) gap (read) platform (read)]
    (while true
      (let [speed (read) coordX (read)]
        (println (make-a-decision road gap platform speed coordX))))))
