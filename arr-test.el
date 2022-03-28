;;;  arr-test.el --- Tests for arr -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for arrow macros
;;; Code:

(add-to-list 'load-path ".")  ;; hack to get the library to load.

(require 'arr)

(ert-deftest arr->test ()
  (should (= (arr-> 10 (/ 5)) 2)))

(ert-deftest arr->>test ()
  (should (= (arr->> 10 (/ 5)) 0)))

(ert-deftest arr->*test ()
  (should (= (arr->> 3
                  (/ 12)
                  (arr->* (/ 2)))
             2)))

(ert-deftest arr-?>test ()
  (should (null (arr-?> 3
                        (+ 5)
                        (member '(2 5 9))
                        cl-first
                        (* 9))))
  (should (= (arr-?> 3
                     (+ 5)
                     (member '(2 5 8 9))
                     cl-first
                     (* 9))
             72))
  (should (= (arr-?> 3
                     (+ 5)
                     (member '(2 5 8 9))
                     cl-second
                     (* 9))
             81))
  (should (null (arr-?> 3
                        (+ 5)
                        (member '(2 5 8 9))
                        cl-third
                        (* 9))))
  (should (null (arr-?> '(:a 1)
                        (cl-getf :b)
                        1+))))

(ert-deftest arr-?>>test ()
  (should (= (arr-?>> '((:a . 3) (:b . 5))
                      (assoc :a)
                      cdr
                      1+)
             4))
  (should (null (arr-?>> '((:a . 3) (:b . 5))
                         (assoc :c)
                         cdr
                         1+))))

(ert-deftest arr-<>test ()
  (should (equal (arr-<> 10
                         (list 9 <> 11)
                         (seq-elt 1))
                 10))

  (should (equal (arr-<> (_ 10)
                         (list 9 _ 11)
                         (seq-elt 1))
                 10)))

(ert-deftest arr-<>>test ()
  (should (equal (arr-<>> 10
                          (list 9 <> 11)
                          (seq-map #'1+)
                          (seq-elt <> 1))
                 11))

  (should (equal (arr-<>> (dwayne-the-rock-johnson 10)
                          (list 9 dwayne-the-rock-johnson 11)
                          (seq-map #'1+)
                          (seq-elt dwayne-the-rock-johnson 1))
                 11)))

(ert-deftest arr-fn->test ()
  (should (equal (seq-map (arr-fn-> (1+) (number-to-string)) '(1 2 3))
                 '("2" "3" "4"))))
;;; arr-test.el ends here
