;;;; snekeyes.lisp

(in-package #:snekeyes)

;;; A dice rolling bot for Granolin

(defclass snekeyes (client auto-joiner) ())

(defvar *snekeyes* nil
  "Dynamic variable holding the bot instance. Bound by HANDLE-EVENT.")

(defparameter +die-faces+ '(nil ⚀ ⚁ ⚂ ⚃ ⚄ ⚅)
  "List of unicode characters standard die faces. The face corresponds to the element number")

(defparameter +dice-regex+ (ppcre:create-scanner "([0-9]+)[dD]([0-9]+)")
  "Looks for a `dice-string' form like 3d6 or 1d20. The number of N sided dice to roll.")

(defparameter +snekeyes-commands+ '("roll!" "craps!")
  "List of strings that are valid snekeyes commands.")

;; Looks at every text message in the room and responds if the first word is one
;; of the +snekeyes-commands+
(defmethod handle-event :after ((*snekeyes* snekeyes) (event text-message-event))
  (let* ((words (ppcre:split " " (msg-body event)))
         (command (string-downcase (car words)))
         (dice-string (cadr words)))
    (if (dice-command-p command)
        (send-text-message *snekeyes* *room-id* (handle-dice-command command dice-string)))))

(defun dice-command-p (word)
  "Returns true if the WORD argument is one of the snekeyes commands."
  (subsetp (list word) +snekeyes-commands+ :test #'equal))

(defun handle-dice-command (command dice-string)
  "Returns a string result of rolling dice according to COMMAND and DICE-STRING."
  (cond ((equal "roll!" command) (roll! dice-string))
        ((equal "craps!" command) (craps))
        (t "This should not be!!!!")))

(defun roll! (dice-string)
  (let* ((parsed (parse-dice-string dice-string))
         (num (or (car parsed) 1))
         (sides (cdr parsed)))
    (format-dice-rolls
     (if sides (roll-dice num sides)
         (roll-dice num)))))

(defun roll-dice (num &optional (sides 6))
  "Rolls the NUM dice with SIDES faces."
  (loop for i upto (- num 1)
        collect (roll sides)))

(defun format-dice-rolls (rolls)
  (let ((total (total-dice-rolls rolls)))
    (if (consp (car rolls))
        (format nil "You rolled ~a, for a total of ~a." (mapcar 'cdr rolls) total)
        (format nil "You rolled ~a, for a total of ~a." rolls total))))

(defun total-dice-rolls (rolls)
  (if (consp (car rolls))
      (reduce '+ (mapcar 'car rolls))
      (reduce '+ rolls)))

(defun parse-dice-string (str)
  "Parses strings like 3d6 and returns a cell with (NUMBER . SIDES)"
  (multiple-value-bind (res matches) (ppcre:scan-to-strings +dice-regex+ str)
    (when res (cons (read-from-string (elt matches 0)) (read-from-string (elt matches 1))))))

(defun roll (sides)
  "Dice rolling function. Returns an integer from 1 to SIDES.
   If it is a six sided die, returns the result of calling d6"
  (if (= sides 6) (d6)
      (+ 1 (random sides))))

(defun d6 ()
  "Rolls a six sided die and returns (VALUE . FACE)"
  (let ((result (+ 1 (random 6))))
    (cons result (elt +die-faces+ result))))

(defun craps ()
  "Plays the game of craps. Rolls two six-sided dice and returns the result as a string."
  (let* ((die-1 (d6))
         (die-2 (d6))
         (total (+ (car die-1) (car die-2)))
         (result (if (= 7 total) "Lucky 7! You win!" "Better luck next time...")))
    (format nil "You rolled ~a ~a. ~a" (cdr die-1) (cdr die-2) result)))

(defun start-snekeyes ()
  "A start function to pass in as the :toplevel to SAVE-LISP-AND-DIE"
  (let* ((config (if (uiop:file-exists-p "snekeyes.config")
                     (with-open-file (input "snekeyes.config")
                       (read input))
                     (progn (format  t "I think you need a snekeyes.config~%~%")
                            (return-from start-snekeyes))))
         (bot (make-instance 'snekeyes
                             :ssl (if (member :ssl config)
                                      (getf config :ssl)
                                      t)
                             :hardcopy (getf config :hardcopy)
                             :user-id (getf config :user-id)
                             :homeserver (getf config :homeserver))))
    (when (not (logged-in-p bot))
      (login bot (getf config :user-id) (getf config :password)))
    (start bot)))
