;;; Package --- Drawille implementation in elisp

;;; Commentary:

;; This is an experimental drawille implementation im emacs lisp.

;; This will result into transforming a matrices:

;; [[a0 a1 a2 a3 a4 a5]   [[[a0 a1   [a2 a3   [a4 a5  ╮<- One braille
;;  [b0 b1 b2 b3 b4 b5]      b0 b1  / b2 b3  / b4 b5  │   character
;;  [c0 c1 c2 c3 c4 c5]      c0 c1 /  c2 c3 /  c4 c5  │
;;  [d0 d1 d2 d3 d4 d5]      d0 d1]   d2 d3]   d4 d5]]╯
;;  [e0 e1 e2 e3 e4 e5]    [[e0 e1   [e2 e3   [e4 e5
;;  [f0 f1 f2 f3 f4 f5]      f0 f1  / f2 f3  / f4 f5
;;  [g0 g1 g2 g3 g4 g5]      g0 g1 /  g2 g3 /  g4 g5
;;  [h0 h1 h2 h3 h4 h5]]     h0 h1]   h2 h3]   h4 h5]]]

;; Which is more correctly written as:

;; [[[a0 a1 b0 b1 c0 c1 d0 d1] <- One braille character
;;   [a2 a3 b2 b3 c2 c3 d2 d3]
;;   [a4 a5 b4 b5 c4 c5 d4 d5]] <- One row of braille characters
;;  [[e0 e1 f0 f1 g0 g1 h0 h1]
;;   [e2 e3 f2 f3 g2 g3 h2 h3]
;;   [e4 e5 f4 f5 g4 g5 h4 h5]]] <- Two row of braille characters

;; With each row a vector of 0 or 1, that is multiplied pairwise, and
;; aditionned to #x2800 produce a braille character keycode.

;;; Code:

(require 'cl)

(defconst drawille-braille-unicode-offset #x2800
  "Offeset to reach the first braille char in unicode encoding.")

(defconst drawille-braille-table
  [#x01 #x08 #x02 #x10 #x04 #x20 #x40 #x80]
  "Table to convert coordinates to braille character.")

(defun drawille-vector-to-char (vector)
  "Translate a VECTOR to a corresponding braille character."
  (let (char-codes)
    (cl-dotimes (i 8)
      (push (* (aref vector i)
	       (aref drawille-braille-table i))
	    char-codes))
    (char-to-string
     (apply '+ drawille-braille-unicode-offset char-codes))))

(drawille-vector-to-char [0 0 1 1 0 0 1 0])

(defun drawille-char-at-pos (matrix x y)
  "Return a braille char corresponding to MATRIX at X, Y."
  (let (sub-matrix)
    (dotimes (i 4)
      (dotimes (j 2)
	(set 'sub-matrix
	     (vconcat sub-matrix
		      (vector (aref (aref matrix (+ x i)) (+ y j)))))))
    (drawille-vector-to-char sub-matrix)))

(drawille-char-at-pos [[0 0 1][0 0 1][0 0 1][0 0 1][1 1 1][1 1 1]] 1 0)

(setq grid [[a0 a1 a2 a3 a4 a5]		;For debugging
	    [b0 b1 b2 b3 b4 b5]
	    [c0 c1 c2 c3 c4 c5]
	    [d0 d1 d2 d3 d4 d5]
	    [e0 e1 e2 e3 e4 e5]
	    [f0 f1 f2 f3 f4 f5]
	    [g0 g1 g2 g3 g4 g5]
	    [h0 h1 h2 h3 h4 h5]])

(defun drawille-fill-matrix (matrix)
  "Return a MATRIX filled until there are a multiple of 4 of rows."
  (let* ((width (length (aref matrix 0)))
	 (height (length matrix)))
    (vconcat
     matrix
     (make-vector (- 4 (% height 4))
		  (make-vector width 0)))))

(drawille-fill-matrix [[1 1] [1 1]])

(defun drawille-matrix (matrix)
  "Subdivises a MATRIX into a vectortransform a 3d MATRIX .
It will then call `drawille-char-at-pos' to fill rows, then columns."
  (let ((width (length (aref matrix 0)))
	(height (length matrix))
	(result))
    (dotimes (i (floor height 4))
      (dotimes (j (floor width 2))
	(setq result (concat result (drawille-char-at-pos
				     matrix (* 4 i) (* 2 j)))))
      (setq result (concat result "\n")))
    result))

(drawille-matrix
 (drawille-fill-matrix
  [[0 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 0 0 ]
   [0 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 0 0 ]
   [0 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 0 0 ]
   [0 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 0 0 ]
   [0 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 0 0 ]
   [0 1 1 1 0 0 1 1 1 0 0 0 1 1 1 0 0 1 1 1 0 0 ]
   [0 1 1 0 0 0 0 1 1 0 0 0 1 1 0 0 0 0 1 1 0 0 ]
   [0 1 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 1 0 0 ]
   [0 1 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 1 0 0 ]
   [0 1 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 1 0 0 ]
   [0 1 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 1 0 0 ]
   [0 1 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 1 0 0 ]
   [0 1 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 1 0 0 ]
   [0 1 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 1 0 0 ]
   [0 1 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 1 0 0 ]
   [0 1 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 1 0 0 ]
   [0 1 1 0 0 0 0 1 1 0 0 0 1 1 0 0 0 0 1 1 0 0 ]
   [0 1 1 1 0 0 1 1 1 0 0 0 1 1 1 0 0 1 1 1 0 0 ]
   [0 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 0 0 ]]))

(provide 'drawille)
;;; drawille.el ends here
