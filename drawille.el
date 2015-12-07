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

(defvar drawille-matrix [[nil nil][nil nil]])

(defun drawille-global-to-local-coordinate (x y)
  "Convert a table of X, Y coordinates to a set of local ones.
The returned set is a list of cons with the car the coordinates
of the character and the cdr a list of coordinates for the dots
within the braille character at this position."
  (let* ((super-x (floor x 2))
	 (super-y (floor y 4))
	 (row (aref drawille-matrix super-y))
	 (cell (aref row super-x))
	 (sub-x (% x 2))
	 (sub-y (% y 4)))
    (aset row super-x (push (list sub-x sub-y) cell))
    (aset drawille-matrix super-y row)))

(defun drawille-global-to-local-coordinates (&rest coordinates)
  "From global COORDINATES, return a matrix of local coordinates.
It will initiallize `drawille-matrix' with greatests values from
coordinates."
  (mapc (lambda (x) (apply 'drawille-global-to-local-coordinate x))
	coordinates)
  drawille-matrix)


(defun drawille-coordinates-to-string (&rest coordinates)
  "Convert COORDINATES to a string of braille characters."
  ;; For each row return a string.
  ;; For each cell of the row, return a char of the string.
  ;; Then join the strings together to return the Drawille text.
  (let* ((max-x (cl-loop for row in coordinates
  			 maximize (car row)))
  	 (max-y (cl-loop for row in coordinates
  			 maximize (cadr row))))
    (setq drawille-matrix (make-vector max-y (make-vector max-x nil)))
  (apply 'drawille-global-to-local-coordinates coordinates)
  ;; drawille-matrix)
  )

(drawille-global-to-local-coordinates '(1 2) '(2 1))
(defvar drawille-matrix [[nil nil][nil nil]])
(drawille-coordinates-to-string '(1 2) '(2 1)) ;; '(0 0) '(9 0) '(1 1)
				)

(provide 'drawille)
;;; drawille.el ends here
