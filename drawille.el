;;; Package --- Drawille implementation in elisp

;;; Commentary:

;; This is an experimental drawille implementation im emacs lisp.

;;; Code:

(message "%s" #o24174)

(defconst drawille-braille-unicode-offset #x2800
  "Offeset to reach the first braille char in unicode encoding.")

(defconst drawille-braille-table
  [[#x01 #x08]
   [#x02 #x10]
   [#x04 #x20]
   [#x40 #x80]]
  "Table to convert coordinates to braille character.")

(defun drawille-offsets-to-char (&rest offsets)
  "Return a character corresponding to a sum of OFFSETS."
  (char-to-string (apply '+ drawille-braille-unicode-offset
			 (delete-dups offsets))))

(defun drawille-coordinates-to-offsets (&rest coordinates)
  "Return an offeset that reach braille matching COORDINATES.
COORDINATES is a list of cons like: (X . Y) (X . Y) ...
X is an int between 0 and 1, and Y is an int between 0 and 3."
  (mapcar (lambda (x) (apply 'drawille-coordinate-to-offset x))
	  coordinates))

(defun drawille-coordinate-to-offset (x y)
  "Convert X, Y coordinates to the equivalent braille offset to apply.
The offset is defined by `drawille-braille-table'
two-dimensionnal vector."
  (aref (aref drawille-braille-table y) x))

(defun drawille-coordinates-to-char (&rest coordinates)
  "Return a braille character matching the COORDINATES."
  (apply 'drawille-offsets-to-char
	 (apply 'drawille-coordinates-to-offsets coordinates)))

(drawille-coordinates-to-char '(0 0) '(1 0) '(1 1) '(1 2))

(defun drawille-coordinates-to-string (coordinates)
  "Convert COORDINATES to a string of braille characters."
  ;; For each row return a string, for each cell of the row, return a
  ;; char of the string.  Then join the strings together to return the
  ;; unicode Drawille image.
  )

(defvar drawille-matrix [[1 2 3 4 5][1 2 3 4 5][1 2 3 4 5]])

(defun drawille-global-to-local-coordinate (x y)
  "Convert a table of X, Y coordinates to a set of local ones.
The returned set is a list of cons with the car the coordinates
of the character and the cdr a list of coordinates for the dots
within the braille character at this position."
  (let* ((super-x (floor x 2))
	 (super-y (floor y 4))
	 (row (aref drawille-matrix super-y))
	 (sub-x (% x 2))
	 (sub-y (% y 4))
	 (cell (cons sub-x sub-y)))
    (aset row super-x cell)
    (aset drawille-matrix super-y row)
    drawille-matrix))

(drawille-global-to-local-coordinate 5 4)

(provide 'drawille)
;;; drawille.el ends here
