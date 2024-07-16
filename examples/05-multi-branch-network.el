(require 'nn)

(defun create-multi-branch-network ()
  "Create a multi-branch neural network."
  (list
   (list
    (nn-layer 10 20 'ops-relu)
    (nn-layer 10 15 'ops-relu))
   (nn-layer 35 5 'identity)))

(provide 'multi-branch-network)
