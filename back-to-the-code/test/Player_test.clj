(ns Player-test
  (:require [clojure.test :refer :all]
            [Player :refer :all]))

(deftest is-mine?-test
  (testing "When called with '0'"
    (is (is-mine? \0)))
  (testing "When called with '.'"
    (is (not (is-mine? \.)))))

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

(deftest max-full-edge-length-test
  (testing "When called with ['.........']"
    (is (= 0 (max-full-edge-length ["........."]))))

  (testing "When called with ['.........', '...00....']"
    (is (= 2 (max-full-edge-length ["........." "...00...."])))))
