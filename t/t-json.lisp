(in-package :wu)
;;; This was in swframes but it doesn't really belong there, since it tests code in wuwei/cl-json-patches.lisp

(5am:def-suite :json :in :wuwei)

(5am:in-suite :json)

(defun json-round-trip-test (s)
  (5am:is (equal s (json:decode-json-from-string (json:encode-json-to-string s)))))

(5am:test basic-json
    (json-round-trip-test '((:foo . "bar")))
    (json-round-trip-test "blther")
    (json-round-trip-test '(1 2 3))
    (json-round-trip-test nil))

(5am:test json-local-enhancements
  (5am:is (equal "{}" (json:encode-json-to-string :empty-dict)))
  (5am:is (equal "[]" (json:encode-json-to-string :empty-list)))
  (5am:is (equal "{\"foo\":testing}" (json:encode-json-to-string '((:foo . (:raw "testing"))))))
  )

  
