(ns Player-test
  (:require [clojure.test :refer :all]
            [Player :refer :all]))

(deftest sum-test
  (testing "adding one number"
    (is (= 1 (sum [1]))))

  (testing "adding two numbers"
    (is (= 6 (sum [1 5]))))

  (testing "adding two hundred numbers"
    (is (= 200 (sum (repeat 200 1)))))
)

(deftest mean-test
  (testing "taking the mean of 1 number"
    (is (= 1 (mean [1]))))

  (testing "taking the mean of 2 numbers"
    (is (= 2 (mean [1 3]))))
)
