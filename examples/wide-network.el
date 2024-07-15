(require 'nn)

(defun create-wide-network ()
  "Create a wide neural network."
  (list
   (nn-layer 10 100 'ops-relu)
   (nn-layer 100 5 'identity)))

(provide 'wide-network)
