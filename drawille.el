;;; Package --- Drawille implementation in elisp

;;; Commentary:

;; This is an experimental drawille implementation im emacs lisp.

;;; Code:

(message "%s" #o24174)

(defconst drawille-braille-unicode-offset #x2800
  "Offeset to reach the first braille char in unicode encoding.")

(defconst drawille-braille-table
  [#x01 #x08 #x02 #x10 #x04 #x20 #x40 #x80]
  "Table to convert coordinates to braille character.")

(defun drawille-vector-to-char (vector)
  "Translate a VECTOR to a corresponding braille character."
  (char-to-string
   (let ((char-codes nil))
     (cl-dotimes (i 8) (push (* (aref vector i)
				(aref drawille-braille-table i))
			     char-codes))
     (apply '+ drawille-braille-unicode-offset char-codes))))

(drawille-vector-to-char [0 0
			  1 0
			  0 0
			  1 1])


'(((0 1)   ((0 1)   ((0 1)   ((0 1)   ((0 1)
   (1 0)  / (1 0)  / (1 0)  / (1 0)  / (1 0)
   (1 1) /  (1 1) /  (1 1) /  (1 1) /  (1 1)
   (0 1))   (0 1))   (0 1))   (0 1))   (0 1)))

Cut the below one so that it become the above

      |   |
'((0 1 0 1 0 1)
  (1 0 1 0 1 0)
  (1 1 1 1 1 1)
_ (0 1 0 1 0 1) _
  (0 1 0 1 0 1)
  (1 0 1 0 1 0)
  (1 1 1 1 1 1)
  (0 1 0 1 0 1))
      |   |

(provide 'drawille)
;;; drawille.el ends here
