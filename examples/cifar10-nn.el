(require 'nn)

(defun create-cifar10-nn ()
  "Create a neural network for CIFAR-10 image classification."
  (list
   ;; Input layer: 32x32x3 = 3072 neurons (for RGB images)
   (nn-layer 3072 1024 'ops-relu)
   ;; Hidden layers
   (nn-layer 1024 512 'ops-relu)
   (nn-layer 512 256 'ops-relu)
   ;; Output layer: 10 neurons (one for each class)
   (nn-layer 256 10 'identity)))

(provide 'cifar10-nn)
