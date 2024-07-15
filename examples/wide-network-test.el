(require 'ert)
(require 'wide-network)

(ert-deftest test-wide-network-structure ()
  "Test the structure of the wide network."
  (let ((network (create-wide-network)))
    (should (= (length network) 2))
    (should (= (length (nth 0 (nth 0 network))) 10))
    (should (= (length (nth 1 (nth 0 network))) 100))
    (should (= (length (nth 0 (nth 1 network))) 100))
    (should (= (length (nth 1 (nth 1 network))) 5))
    (should (eq (nth 2 (nth 0 network)) 'ops-relu))
    (should (eq (nth 2 (nth 1 network)) 'identity))))

(provide 'wide-network-test)
