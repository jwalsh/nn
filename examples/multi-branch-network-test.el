(require 'ert)
(require 'multi-branch-network)

(ert-deftest test-multi-branch-network-structure ()
  "Test the structure of the multi-branch network."
  (let ((network (create-multi-branch-network)))
    (should (= (length network) 2))
    (should (= (length (nth 0 network)) 2))
    (should (= (length (nth 0 (nth 0 (nth 0 network)))) 10))
    (should (= (length (nth 1 (nth 0 (nth 0 network)))) 20))
    (should (= (length (nth 0 (nth 1 (nth 0 network)))) 10))
    (should (= (length (nth 1 (nth 1 (nth 0 network)))) 15))
    (should (= (length (nth 0 (nth 1 network))) 35))
    (should (= (length (nth 1 (nth 1 network))) 5))
    (should (eq (nth 2 (nth 0 (nth 0 network))) 'ops-relu))
    (should (eq (nth 2 (nth 1 (nth 0 network))) 'ops-relu))
    (should (eq (nth 2 (nth 1 network)) 'identity))))

(provide 'multi-branch-network-test)
