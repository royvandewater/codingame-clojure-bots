(ns Player-test
  (:require [clojure.test :refer :all]
            [Player :refer :all]))

(deftest cell-owned-by-me?-test
  (testing "When called with '0'"
    (is (cell-owned-by-me? \0)))
  (testing "When called with '.'"
    (is (not (cell-owned-by-me? \.)))))

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
