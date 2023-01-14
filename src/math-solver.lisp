(defpackage #:math-solver
  (:use #:cl)
  (:export #:solve-math))


(in-package :math-solver)


(defparameter *operands* '(("+" . +)
                           ("-" . -)
                           ("x" . *)
                           (":" . /)))


;;; (equation-to-sexp "1 + 2 - 3")
;;; -> (- (+ 1 2) 3)
;;;
;;; (equation-to-sexp "1 - 2 : 3")
;;; -> (- 1 (/ 2 3))
;;;
;;; (equation-to-sexp "1 x 2 + 3")
;;; -> (+ (* 1 2) 3)
;;;
;;; (equation-to-sexp "1 : 2 x 3")
;;; -> (* (/ 1 2) 3)
;;;
;;; (equation-to-sexp "1 : 2 x 3 : 4")
;;; -> (/ (* (/ 1 2) 3) 4)
(defun equation-to-sexp (equation)
  (loop
    with sexp = nil
    for (arg op next-arg) on (excl:split-re " " equation) by #'cddr
    while op
    for previous-operand = nil then operand
    for operand = (cdr (assoc op *operands* :test 'equal))
    for arg-num = (parse-integer arg)
    for next-arg-num = (parse-integer next-arg)
    do (cond
         ((and sexp
               (member operand '(* /))
               (not (member previous-operand '(* /))))
          (destructuring-bind (s-op s-arg1 s-arg2) sexp
            (setq sexp `(,s-op ,s-arg1 (,operand ,s-arg2 ,next-arg-num)))))
         (sexp
          (setq sexp `(,operand ,sexp ,next-arg-num)))
         (t
          (setq sexp `(,operand ,arg-num ,next-arg-num))))
    finally (return sexp)))


(defun try-equations (args result)
  (labels ((%try-equations (args result equation-acc)
             (destructuring-bind (arg &rest rest) args
               ;; Initialize the equation or include the new argument (the
               ;; operator must have been included before).
               (if equation-acc
                   (setq equation-acc (format nil "~a ~d" equation-acc arg))
                   (setq equation-acc (format nil "~d" arg)))
               (if rest
                   (loop
                     for (op . operand) in *operands*
                     ;; For every operand, include it in the equation and
                     ;; continue digging. Careful about dividing by zero!
                     unless (and (eq operand '/)
                                 (zerop (car rest)))
                       do (%try-equations rest result (format nil "~a ~a"
                                                              equation-acc op)))
                   ;; We've got an equation. Check if it's correct and, if so,
                   ;; return it.
                   (let ((sexp (equation-to-sexp equation-acc)))
                     (when (= (eval sexp) result)
                       (return-from try-equations equation-acc)))))))
    (%try-equations args result nil)))


;;; Entrypoint

(defun solve-math (args result)
  (assert (listp args))
  (assert (>= (length args) 2))
  (assert (every 'integerp args))
  (assert (integerp result))
  (try-equations args result))


;;; Tests

(defun test-solve-math ()
  (loop
    for (equation args result)
      in '( ; Two arguments
           ("1 + 2" (1 2)  3)
           ("1 - 2" (1 2) -1)
           ("1 x 2" (1 2)  2)
           ("2 : 2" (2 2)  1)
           ;; Three arguments
           ("1 + 2 + 3" (1 2 3)  6)
           ("1 + 2 - 3" (1 2 3)  0)
           ("1 + 2 x 3" (1 2 3)  7)
           ("1 + 6 : 2" (1 6 2)  4)
           ("1 - 2 + 3" (1 2 3)  2)
           ("1 - 2 - 3" (1 2 3) -4)
           ("1 - 2 x 3" (1 2 3) -5)
           ("1 - 9 : 3" (1 9 3) -2)
           ("1 x 2 + 3" (1 2 3)  5)
           ("1 x 2 - 3" (1 2 3) -1)
           ("1 x 2 x 4" (1 2 4)  8)
           ("1 x 9 : 3" (1 9 3)  3)
           ("2 : 2 + 3" (2 2 3)  4)
           ("2 : 2 - 3" (2 2 3) -2)
           ("4 : 2 x 3" (4 2 3)  6)
           ("9 : 3 : 3" (9 3 3)  1)
           ;; Avoid dividing by zero
           ("2 - 2 + 0" (2 2 0) 0)
           ;; Ensure we evaluate multiplication and division from left to right
           ("3 x 4 : 2 x 10" (3 4 2 10) 60)
           ("16 : 2 : 2 : 2 : 2" (16 2 2 2 2) 1)
           )
    for result-equation = (solve-math args result)
    unless (equal equation result-equation)
      do (format t "~2&For args ~a and result ~d expected equation ~s but got ~
                    ~s~2&"
                 args result equation result-equation)))
