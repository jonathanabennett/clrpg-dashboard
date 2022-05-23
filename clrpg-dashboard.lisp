;;;; clrpg-dashboard.lisp

(in-package #:clrpg-dashboard)

(defclass roll ()
  ((descriptor
    :initarg :descriptor
    :accessor descriptor)
   (target
    :initarg :target
    :accessor target)
   (dice
    :initarg :dice
    :initform (make-instance 'raw-dice :dice 3 :mods 0)
    :accessor dice)))

(defclass raw-dice ()
    ((num-dice
      :initarg :num-dice
      :accessor num-dice)
     (mods
      :initarg :mods
      :initform 0
      :accessor mods))
  (:documentation "Class for rolls."))

(defun string-to-raw-dice (string)
  (let ((dice (or (parse-integer string :junk-allowed t) 1))
        (mods (cond
                ((position #\+ string :test #'char-equal)
                 (setf mods (parse-integer string :start (position #\+ string :test #'char-equal) :junk-allowed t)))
                ((position #\- string :test #'char-equal)
                 (setf mods (parse-integer string :start (position #\- string :test #'char-equal) :junk-allowed t)))
                (t
                 (setf mods 0)))))
    (make-instance 'raw-dice
                   :dice dice
                   :mods mods)))

(defmethod print-object ((roll raw-dice) stream)
  (with-slots (num-dice mods) roll
    (cond
      ((= mods 0) (format stream "~aD" num-dice))
      ((< mods 0) (format stream "~aD~a" num-dice mods))
      ((> mods 0) (format stream "~aD+~a" num-dice mods)))))

(defmethod roll ((roll raw-dice) mods)
  (let ((sum 0))
    (dotimes (i (num-dice roll) sum)
      (setf sum (+ sum (1+ (random 6)))))
    (incf sum (mods roll))
    (incf sum mods)
    sum))

(define-application-frame dashboard ()
  ((die-lists :initform '()
             :accessor die-lists))
  (:pointer-documentation t)
  (:panes

   ;; Let's add an additional pane
   (app :application

        ;; When should this pane be displayed in the command loop.
        ;; Note that the refresh is pane-specific, not
        ;; application-wide.
        :display-function 'display-app
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

(defun display-app (frame pane)
  (format pane "~{~a~%~}" (die-lists *application-frame*)))

(define-presentation-type raw-dice-string ()
  :inherit-from 'string)
;;
;; Let's also define commands that will act on the application.
;;

(define-dashboard-command (com-add-die-code :name t) ((raw-dice-string 'string))
  (push (string-to-raw-dice raw-dice-string) (die-lists *application-frame*)))
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
