(ns Player-test
  (:require [clojure.test :refer :all]
            [Player :refer :all]))

(deftest cell-owned-by-me?-test
  (testing "When called with '0'"
    (is (cell-owned-by-me? \0)))
  (testing "When called with '.'"
    (is (not (cell-owned-by-me? \.)))))

(deftest edge-length-down-test
  (testing "When called with an initial board state"
    (let [board ["....."
                 "..0.."
                 "....."]]
      (is (= 1 (edge-length-down board)))))

  (testing "When called with a normal 3rd turn board state"
    (let [board ["....."
                 "..00."
                 "..0.."]]
      (is (= 1 (edge-length-down board)))))

  (testing "When called with a normal 4th turn board state"
    (let [board ["....."
                 "..00."
                 "..00."]]
      (is (= 2 (edge-length-down board)))))
  )

(deftest edge-length-right-test
  (testing "When called with a normal 2nd turn board state"
    (let [board ["....."
                 "..0.."
                 "..0.."]]
      (is (= 1 (edge-length-right board)))))

  (testing "When called with a normal 3rd turn board state"
    (let [board ["....."
                 "..00."
                 "..0.."]]
      (is (= 2 (edge-length-right board)))))

  (testing "When called with a normal 4th turn board state"
    (let [board ["....."
                 "..00."
                 "..00."]]
      (is (= 2 (edge-length-right board)))))
  )

(deftest edge-length-up-test
  (testing "When called with an initial board state"
    (let [board ["....."
                 "..0.."
                 "....."]]
      (is (= 1 (edge-length-up board)))))

  (testing "When called with a normal 2nd turn board state"
    (let [board ["....."
                 "..0.."
                 "..0.."]]
      (is (= 2 (edge-length-up board)))))

  (testing "When called with a normal 3rd turn board state"
    (let [board ["....."
                 "..00."
                 "..0.."]]
      (is (= 2 (edge-length-up board)))))
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

(deftest going-down?-test
  (testing "When called with a normal 3rd turn board state"
    (let [board ["....."
                 "..00."
                 "..0.."]]
      (is (= true (going-down? board)))))

  (testing "When called with a normal 4th turn board state"
    (let [board ["....."
                 "..00."
                 "..00."
                 "....."]]
      (is (= true (going-down? board)))))

  (testing "When called with a normal 5th turn board state"
    (let [board ["....."
                 "..00."
                 "..00."
                 "...0."]]
      (is (= false (going-down? board)))))
)

(deftest going-left?-test
  (testing "When called with a normal 5th turn board state"
    (let [board ["....."
                 "..00."
                 "..00."
                 "...0."]]
      (is (= true (going-left? board)))))

  (testing "When called with a normal 6th turn board state"
    (let [board ["....."
                 "..00."
                 "..00."
                 "..00."]]
      (is (= true (going-left? board)))))

  (testing "When called with a normal 7th turn board state"
    (let [board ["....."
                 "..00."
                 "..00."
                 ".000."]]
      (is (= false (going-left? board)))))
)

(deftest going-right?-test
  (testing "When called with a normal 2nd turn board state"
    (let [board ["....."
                 "..0.."
                 "..0.."]]
      (is (= true (going-right? board)))))

  (testing "When called with a normal 3rd turn board state"
    (let [board ["....."
                 "..00."
                 "..0.."]]
      (is (not (going-right? board)))))

  (testing "When called with a normal 4th turn board state"
    (let [board ["....."
                 "..00."
                 "..00."]]
      (is (not (going-right? board)))))
)

(deftest going-up?-test
  (testing "When called with an initial board state"
    (let [board ["....."
                 "..0.."
                 "....."]]
      (is (= true (going-up? board)))))

  (testing "When called with a normal 2nd turn board state"
    (let [board ["....."
                 "..0.."
                 "..0.."]]
      (is (= false (going-up? board)))))

  (testing "When called with a normal 3rd turn board state"
    (let [board ["....."
                 "..00."
                 "..0.."]]
      (is (= false (going-up? board)))))

  (testing "When called with a normal 4th turn board state"
    (let [board ["....."
                 "..00."
                 "..00."]]
      (is (= false (going-up? board)))))
)

(deftest made-a-square?-test
  (testing "When called with a normal 1st round board state"
    (is (= true (made-a-square? ["......."
                                 "...0..."
                                 "......."]))))

  (testing "When called with a normal 2nd round board state"
    (is (= false (made-a-square? ["......."
                                  "...0..."
                                  "...0..."]))))
)

(deftest max-full-edge-length-test
  (testing "When called with ['.........']"
    (is (= 0 (max-full-edge-length ["........."]))))

  (testing "When called with ['.........', '...00....']"
    (is (= 1 (max-full-edge-length ["........."
                                    "...00...."]))))

  (testing "When called with ['.........', '...00....']"
    (is (= 2 (max-full-edge-length ["...00...."
                                    "...00...."]))))
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

(deftest spiral-outwards-test
  (testing "When called with a normal 3rd turn board state"
    (let [game  {:x 3, :y 1}
          board ["....."
                 "..00."
                 "..0.."]]
      (is (= {:x 3, :y 2} (spiral-outwards game board)))))

  (testing "When called with a normal 4th turn board state"
    (let [game  {:x 3, :y 2}
          board ["....."
                 "..00."
                 "..00."]]
      (is (= {:x 3, :y 3} (spiral-outwards game board)))))
)
