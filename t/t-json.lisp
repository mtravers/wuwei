(in-package :wu)
;;; This was in swframes but it doesn't really belong there, since it tests code in wuwei/cl-json-patches.lisp

(use-package :lisp-unit)

(defun json-round-trip-test (s)
  (assert-equal s (json:decode-json-from-string (json:encode-json-to-string s))))

(define-test basic-json
    (json-round-trip-test '((:foo . "bar")))
    (json-round-trip-test "blther")
    (json-round-trip-test '(1 2 3))
    (json-round-trip-test nil))

(define-test json-local-enhancements
    (assert-equal "{}" (json:encode-json-to-string :empty-dict))
  (assert-equal "[]" (json:encode-json-to-string :empty-list))
  (assert-equal "{\"foo\":testing}" (json:encode-json-to-string '((:foo . (:raw "testing")))))
  )

  
