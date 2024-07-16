(require 'nn)

(defun create-simple-ffn ()
  "Create a simple feedforward neural network."
  (list
   (nn-layer 10 20 'ops-relu)
   (nn-layer 20 5 'identity)))

(provide 'simple-ffn)
