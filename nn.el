;;; -*- lexical-binding: t -*-

;; nn.el --- Neural Network Library


(require 'matrix)
(require 'ops)
(require 'seq)
(require 'cl-lib)

;; Define custom types
(cl-deftype matrix () '(satisfies matrixp))
(cl-deftype activation-function () '(or symbol function))

(defun matrixp (obj)
  "Check if OBJ is a matrix (vector of vectors)."
  (and (vectorp obj)
       (> (length obj) 0)
       (vectorp (aref obj 0))))

(cl-defun nn-layer (in out &optional (activation 'identity))
  "Return a layer with weight matrix of shape IN x OUT and
bias with optional ACTIVATION function.

(fn (IN integer) (OUT integer) &optional (ACTIVATION activation-function)) -> list"
  (list (matrix-random in out) 
        (matrix-random out 1) 
        activation))

(cl-defun nn--copy-bias-vector (bias times)
  "Copy the BIAS column vector in R^n TIMES number of times to
produce a matrix of shape n x TIMES, where each of the columns
are the input BIAS column vector.

(fn (BIAS matrix) (TIMES integer)) -> matrix"
  (matrix-transpose (make-vector times (aref (matrix-transpose bias) 0))))

(cl-defun nn--fill (shape value)
  "Return a matrix of SHAPE with all elements set to VALUE.

(fn (SHAPE vector) VALUE) -> matrix"
  (let ((nrows (aref shape 0))
        (ncols (aref shape 1)))
    (make-vector nrows (make-vector ncols value))))

(cl-defun nn-forward (x layer)
  "Return the result of the forward pass on LAYER on input X.

(fn (X matrix) (LAYER list)) -> matrix"
  (let* ((input-shape (matrix-shape x))
         (batch-size (aref input-shape 1))
         (weight-matrix (nth 0 layer))
         (bias-matrix (nn--copy-bias-vector (nth 1 layer) batch-size))
         (activation (nth 2 layer)))
    (funcall activation (matrix-add (matrix-matmul (matrix-transpose weight-matrix) x) bias-matrix))))

(cl-defun nn-forward-layers (x layers)
  "Return the result of the sequential forward pass through LAYERS on input X.

(fn (X matrix) (LAYERS list)) -> matrix"
  (seq-reduce #'nn-forward layers x))

(cl-defun nn-crossentropy (label pred)
  "Calculate the crossentropy loss between LABEL and PRED.

(fn (LABEL matrix) (PRED matrix)) -> matrix"
  (let* ((sum (ops-reduce-sum (matrix-hadamard label (ops-log pred)) 1))
         (minus-one (nn--fill (matrix-shape sum) -1)))
    (matrix-hadamard minus-one sum)))

(cl-defun nn--build-gradient (a-idx d-idx)
  "Build gradient from activation and delta matrices.

(fn (A-IDX matrix) (D-IDX matrix)) -> list"
  (let ((batch-size (aref (matrix-shape d-idx) 0)))
    (list (matrix-scalar-mul (/ 1.0 batch-size) (matrix-matmul a-idx d-idx))
          (matrix-transpose (ops-reduce-mean d-idx 1)))))

(cl-defun nn-gradient (x y model)
  "Return the gradient of training example(s) X Y evaluated at MODEL
using crossentropy loss.

(fn (X matrix) (Y matrix) (MODEL list)) -> list"
  (let* ((s (ops-softmax (nn-forward-layers x model))))
    (setq nn--DL (matrix-transpose (matrix-subtract s y))))
  (setq nn--D-matrices (list nn--DL))
  (setq nn--A-matrices (list x))
  ;; calculate the activation matrices
  (dotimes (idx (1- (length model)))
    (push (nn-forward-layers x (seq-take model (1+ idx))) nn--A-matrices))
  (setq nn--A-matrices (nreverse nn--A-matrices))
  ;; calculate the d matrices
  (dotimes (count (1- (length model)))
    (let* ((idx (- (1- (length model)) count))
           (a-idx (nth idx nn--A-matrices))
           (h-idx (ops-heaviside (matrix-transpose a-idx)))
           (wt-idx-plus-one (matrix-transpose (nth 0 (nth idx model))))
           (d-idx-plus-one (car nn--D-matrices))
           (d-idx (matrix-hadamard (matrix-matmul d-idx-plus-one wt-idx-plus-one)
                                   h-idx)))
      (push d-idx nn--D-matrices)))
  (seq-mapn #'nn--build-gradient nn--A-matrices nn--D-matrices))

(provide 'nn)
