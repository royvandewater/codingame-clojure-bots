(ns Player-test
  (:require [clojure.test :refer :all]
            [Player :refer :all]))

(deftest is-mine?-when-it-is-not
  (testing "When called with '.'"
    (is (not (is-mine? \.)))))
