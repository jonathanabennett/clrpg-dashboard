;;;; clrpg-dashboard.lisp

(in-package #:clrpg-dashboard)

(defstruct die-code
  dice
  sides
  mod)

(defun roll (dice mods)
  (let ((sum 0))
    (dotimes (i (die-code-dice dice) sum)
      (setf sum (+ sum (1+ (random (die-code-sides dice))))))
    (incf sum (die-code-mod dice))
    (incf sum mods)
    sum))

(defun string-to-die-code (string)
  (let ((dice (or (parse-integer string :junk-allowed t) 1))
        (sides (or (parse-integer string :start (1+ (position #\D string :test #'char-equal)) :junk-allowed t) 6))
        (mods (cond
               ((position #\+ string :test #'char-equal)
                (setf mods (parse-integer string :start (position #\+ string :test #'char-equal) :junk-allowed t)))
               ((position #\- string :test #'char-equal)
                (setf mods (parse-integer string :start (position #\- string :test #'char-equal) :junk-allowed t)))
               (t
                (setf mods 0)))))
    (make-die-code :dice dice :sides sides :mod mods)))

(defun output-die-code (die-code)
  (cond
    ((= 0 (die-code-mod die-code)) (format t "~aD~a" (die-code-dice die-code) (die-code-sides die-code)))
    ((> 0 (die-code-mod die-code)) (format t "~aD~a~a" (die-code-dice die-code) (die-code-sides die-code) (die-code-mod die-code)))
    ((< 0 (die-code-mod die-code)) (format t "~aD~a+~a" (die-code-dice die-code) (die-code-sides die-code) (die-code-mod die-code)))))

(define-application-frame dashboard ()
  ()
  (:pointer-documentation t)
  (:panes

   ;; Let's add an additional pane
   (app :application

        ;; When should this pane be displayed in the command loop.
        ;; Note that the refresh is pane-specific, not
        ;; application-wide.
        :display-time nil
        :height 400
        :width 600)

   (int :interactor
        :height 200
        :width 600))

  (:layouts

   ;; This time we explicitly specify that the 2 defined panes
   ;; should be stacked vertically.
   (default (vertically ()
              app int))))

;;
;; Let's also define commands that will act on the application.
;;

;; How to leave the application.
;; Note the '-superapp-' part of the command definition, coming from
;; the name of the application frame.
(define-dashboard-command (com-quit :name t) ()
  (frame-exit *application-frame*))


;; This is an additional command that will be used in the next
;; example, so it's content is not important. However, it is useful
;; to describe some aspect of the command loop. See below.
(define-dashboard-command (com-parity :name t) ((number 'integer))
  (format t "~a is ~a~%" number
          (if (oddp number)
              "odd"
              "even")))

(define-dashboard-command (com-roll :name t) ((dice-string 'string))
  "Take a string in the format x[d|D]s[+|-]y and roll x dice with s sides,
applying the modifier + or - y to the final result."
  ())

(defun run-app ()
  (run-frame-top-level (make-application-frame 'dashboard)))
