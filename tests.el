;;; -*- lexical-binding: t -*-

;; (add-to-list 'load-path "~/sandbox/nn")
(require 'nn)
(require 'matrix)
(require 'ops)

(setq model `(,(nn-layer 100 80 'ops-relu)
              ,(nn-layer 80 10 'ops-relu)
              ,(nn-layer 10 8 'ops-relu)
              ,(nn-layer 8 3)))

(setq x (matrix-random 100 2))
(setq y (matrix-transpose (vconcat [[1 0 0]] [[0 0 1]])))

(let* ((s (ops-softmax (nn-forward-layers x model)))
       (loss (nn-crossentropy y s)))
  (message "Initial loss: %s" loss))

(defun nn--apply-gradient-sgd-layer (grad layer)
  "Apply GRAD to LAYER using gradient descent."
  (let ((w (nth 0 layer))
        (b (nth 1 layer))
        (wg (nth 0 grad))
        (bg (nth 1 grad)))
    (setf (nth 0 layer) (matrix-subtract w (matrix-scalar-mul 0.01 wg)))
    (setf (nth 1 layer) (matrix-subtract b (matrix-scalar-mul 0.01 bg)))))

(dotimes (counter 100)
  ;; This is the train step; essentially just do this as many times
  ;; as you want to train the model.
  (let ((grads (nn-gradient x y model)))
    (seq-mapn #'nn--apply-gradient-sgd-layer grads model))
  (message "Training step: %s ; Loss: %s" counter (nn-crossentropy y (ops-softmax (nn-forward-layers x model)))))

(let* ((s (ops-softmax (nn-forward-layers x model)))
       (loss (nn-crossentropy y s)))
  (message "Post training loss: %s" loss))
