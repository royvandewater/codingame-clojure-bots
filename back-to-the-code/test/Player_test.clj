(ns Player-test
  (:require [clojure.test :refer :all]
            [Player :refer :all]))

(deftest all-squares-test
  (testing "When called with a 1x1 board"
    (let [board ["."]
          squares (all-squares board)]
      (is (in? squares {:x1 0, :y1 0, :x2 0, :y2 0}))))

  (testing "When called with a 1x2 board"
    (let [board ["."
                 "."]
          squares (all-squares board)]
      (is (in? squares {:x1 0, :y1 0, :x2 0, :y2 0}))
      (is (in? squares {:x1 0, :y1 1, :x2 0, :y2 1}))
      (is (not (in? squares {:x1 0, :y1 1, :x2 0, :y2 0})))))

  (testing "When called with a 2x2 board"
    (let [board [".."
                 ".."]
          squares (all-squares board)]
      (is (in? squares {:x1 0, :y1 0, :x2 0, :y2 0}))
      (is (in? squares {:x1 0, :y1 1, :x2 0, :y2 1}))
      (is (in? squares {:x1 1, :y1 0, :x2 1, :y2 0}))
      (is (in? squares {:x1 1, :y1 1, :x2 1, :y2 1}))
      (is (in? squares {:x1 0, :y1 0, :x2 1, :y2 1}))))

  (testing "When called with a 35x20 board"
    (let [board (repeat 20 "...................................")
          squares (all-squares board)]
      (is (= 6020 (count squares)))))
)

(deftest cell-owned-by-me?-test
  (testing "When called with '0'"
    (is (= true  (cell-owned-by-me? \0))))
  (testing "When called with '.'"
    (is (= false (cell-owned-by-me? \.)))))

(deftest center-of-largest-free-square-test
  (testing "When called with an empty 1x1 board"
    (let [board ["."]]
      (is (= {:x 0, :y 0} (center-of-largest-free-square board)))))

  (testing "When called with an empty 2x2 board"
    (let [board [".."
                 ".."]]
      (is (= {:x 0, :y 0} (center-of-largest-free-square board)))))

  (testing "When called with an empty 3x3 board"
    (let [board ["..."
                 "..."
                 "..."]]
      (is (= {:x 1, :y 1} (center-of-largest-free-square board)))))

  (testing "When called with an 3x3 board with me in a corner"
    (let [board ["0.."
                 "..."
                 "..."]]
      (is (= {:x 1, :y 1} (center-of-largest-free-square board)))))

  (testing "When called with a 35x20 board"
    (let [board (repeat 20 "...................................")]
      (println "\ncenter-of-largest-free-square")
      (is (= {:x 24, :y 9} (time (center-of-largest-free-square board))))))
 )

(deftest center-of-square-test
  (testing "When called with a top left 1x1 square"
    (let [square {:x1 0, :y1 0, :x2 0, :y2 0}]
      (is (= {:x 0, :y 0} (center-of-square square)))))

  (testing "When called with a top left 2x2 square"
    (let [square {:x1 0, :y1 0, :x2 1, :y2 1}]
      (is (= {:x 0, :y 0} (center-of-square square)))))

  (testing "When called with a top left 3x3 square"
    (let [square {:x1 0, :y1 0, :x2 2, :y2 2}]
      (is (= {:x 1, :y 1} (center-of-square square)))))

  (testing "When called with a 1x1 square one unit right and down"
    (let [square {:x1 1, :y1 1, :x2 1, :y2 1}]
      (is (= {:x 1, :y 1} (center-of-square square)))))

  (testing "When called with a 3x3 square two unit right and three down"
    (let [square {:x1 2, :y1 3, :x2 4, :y2 5}]
      (is (= {:x 3, :y 4} (center-of-square square)))))
)

(deftest go-down-test
  (testing "When called with 0,0"
    (is (= {:x 0, :y 1} (go-down {:x 0, :y 0}))))
  (testing "When called with 4,4"
    (is (= {:x 4, :y 5} (go-down {:x 4, :y 4})))))

(deftest go-left-test
  (testing "When called with 1,1"
    (is (= {:x 0, :y 1} (go-left {:x 1, :y 1}))))
  (testing "When called with 4,4"
    (is (= {:x 3, :y 4} (go-left {:x 4, :y 4})))))

(deftest go-right-test
  (testing "When called with 1,1"
    (is (= {:x 2, :y 1} (go-right {:x 1, :y 1}))))
  (testing "When called with 4,4"
    (is (= {:x 5, :y 4} (go-right {:x 4, :y 4})))))

(deftest go-up-test
  (testing "When called with 1,1"
    (is (= {:x 1, :y 0} (go-up {:x 1, :y 1}))))
  (testing "When called with 4,4"
    (is (= {:x 4, :y 3} (go-up {:x 4, :y 4})))))

(deftest largest-free-square-test
  (testing "When called with an 3x3 board with me in a corner"
    (let [board ["0.."
                 "..."
                 "..."]]
      (is (= {:x1 1, :y1 1, :x2 2, :y2 2} (largest-free-square board)))))

  (testing "When called with a 35x20 board"
    (let [board (repeat 20 "...................................")]
      (println "\nlargest-free-square")
      (is (= {:x1 15, :y1 0, :x2 34, :y2 19} (time (largest-free-square board))))))
)

(deftest max-by-test
  (testing "When called with a key"
    (is (= {:a 1, :b 2} (max-by :a [{:a 1, :b 2},{:a 0, :b 3}]))))

  (testing "When called with a lambda"
    (is (= {:a 0, :b 4} (max-by #(reduce + (vals %)) [{:a 1, :b 2},{:a 0, :b 4}]))))
)

(deftest number-owned-test
  (testing "When called with '.....0....'"
    (is (= 1 (number-owned ".....0...."))))
  (testing "When called with '...000....'"
    (is (= 3 (number-owned "...000....")))))

(deftest partially-owned?-test
  (testing "When called with '.........'"
    (is (not (partially-owned? "........."))))
  (testing "When called with '...1.....'"
    (is (not (partially-owned? "...1....."))))
  (testing "When called with '.....0...'"
    (is (partially-owned? ".....0..."))))

(deftest rectangle-area-test
  (testing "When called with a top left 1x1 square"
    (let [rectangle {:x1 0, :y1 0, :x2 0, :y2 0}]
      (is (= 1 (rectangle-area rectangle)))))
  )

(deftest rectangle-free?-test
  (testing "When called with an 3x3 board with me in a corner"
    (let [board ["0.."
                 "..."
                 "..."]]
      (testing "given the resulting function the bottom right 2x2 square"
        (is (= true (rectangle-free? board {:x1 1, :y1 1, :x2 2, :y2 2}))))
      (testing "given the resulting function the top left 2x2 square"
        (is (= false (rectangle-free? board {:x1 0, :y1 0, :x2 1, :y2 1}))))))
)
