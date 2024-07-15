(require 'nn)

(defun create-digit-identification-nn ()
  "Create a neural network for digit identification."
  (list
   (nn-layer 100 80 'ops-relu)
   (nn-layer 80 10 'ops-relu)
   (nn-layer 10 8 'ops-relu)
   (nn-layer 8 3 'identity)))

(provide 'digit-identification-nn)
