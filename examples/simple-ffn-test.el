(require 'ert)
(require 'simple-ffn)

(ert-deftest test-simple-ffn-structure ()
  "Test the structure of the simple feedforward network."
  (let ((network (create-simple-ffn)))
    (should (= (length network) 2))
    (should (= (length (nth 0 (nth 0 network))) 10))
    (should (= (length (nth 1 (nth 0 network))) 20))
    (should (= (length (nth 0 (nth 1 network))) 20))
    (should (= (length (nth 1 (nth 1 network))) 5))
    (should (eq (nth 2 (nth 0 network)) 'ops-relu))
    (should (eq (nth 2 (nth 1 network)) 'identity))))

(provide 'simple-ffn-test)
