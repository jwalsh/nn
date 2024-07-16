(require 'nn)

(defun create-bottleneck-network ()
  "Create a bottleneck neural network."
  (list
   (nn-layer 10 20 'ops-relu)
   (nn-layer 20 5 'ops-relu)
   (nn-layer 5 20 'ops-relu)
   (nn-layer 20 10 'identity)))

(provide 'bottleneck-network)
