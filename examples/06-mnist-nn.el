(require 'nn)

(defun create-mnist-nn ()
  "Create a neural network for MNIST digit recognition."
  (list
   (nn-layer 784 128 'ops-relu)
   (nn-layer 128 64 'ops-relu)
   (nn-layer 64 10 'identity)))

(provide 'mnist-nn)
