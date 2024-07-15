(require 'nn)

(defun create-deep-ffn ()
  "Create a deep feedforward neural network."
  (list
   (nn-layer 10 20 'ops-relu)
   (nn-layer 20 15 'ops-relu)
   (nn-layer 15 10 'ops-relu)
   (nn-layer 10 5 'identity)))

(provide 'deep-ffn)
