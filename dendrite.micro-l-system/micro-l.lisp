(in-package :dendrite.micro-l-system)

(define-compiler-macro mapcat (function &rest lists)
  `(apply #'concatenate 'list (mapcar ,function ,@lists)))

(defun mapcat (function &rest lists)
  (apply #'concatenate 'list (apply #'mapcar function lists)))

;; recurse n times
(defun rec-n (func n d)
  (if (> n 0)
      (rec-n func (- n 1) (funcall func d))
      d))

;; lsystem
(defun l (x lang) (mapcat (lambda (_) (cdr (assoc _ lang))) x))

;; example
;; -------
;; one lsystem language
;; --------------------
;; (defparameter l1 '((a a b) (b c c) (c a b)))

;; 6 recursions
;; ------------
;; (rec-n λ(l _ l1) 6 '(a))
