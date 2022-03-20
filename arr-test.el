;;;  arr-test.el --- Tests for arr -*- lexical-binding: t -*-

;;; Commentary:
;; cool
;;; Code:


(require 'arr)

(ert-deftest arr->test ()
  (should (= (arr-> 10 (/ 5)) 2)))

(ert-deftest arr->>test ()
  (should (= (arr->> 10 (/ 5)) 0)))

(ert-deftest arr-<>test ()
  (should (equal (arr-<> 10
                         (list 9 <> 11)
                         (seq-elt 1))
                 10)))

(ert-deftest arr-<>>test ()
  (should (equal (arr-<>> 10
                          (list 9 <> 11)
                          (seq-map #'1+)
                          (seq-elt <> 1))
                 11)))

(ert-deftest arr-fn->test ()
  (should (equal (seq-map (arr-fn-> (1+) (number-to-string)) '(1 2 3))
                 '("2" "3" "4"))))
;;; arr-test.el ends here
