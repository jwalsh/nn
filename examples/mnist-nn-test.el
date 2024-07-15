(require 'ert)
(require 'mnist-nn)

(ert-deftest test-mnist-nn-structure ()
  "Test the structure of the MNIST neural network."
  (let ((network (create-mnist-nn)))
    (should (= (length network) 3))
    (should (= (length (nth 0 (nth 0 network))) 784))
    (should (= (length (nth 1 (nth 0 network))) 128))
    (should (= (length (nth 0 (nth 1 network))) 128))
    (should (= (length (nth 1 (nth 1 network))) 64))
    (should (= (length (nth 0 (nth 2 network))) 64))
    (should (= (length (nth 1 (nth 2 network))) 10))
    (should (eq (nth 2 (nth 0 network)) 'ops-relu))
    (should (eq (nth 2 (nth 1 network)) 'ops-relu))
    (should (eq (nth 2 (nth 2 network)) 'identity))))

(provide 'mnist-nn-test)
