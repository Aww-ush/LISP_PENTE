(load "/Users/aayushshrestha/Desktop/LispProject/board.lisp")
(load "/Users/aayushshrestha/Desktop/LispProject/Player.lisp")
(defun GenerateRandomPosition (board)
  (let ((position (cons (random 19) (random 19))))
    (cond
      ((or (string= (GetPiece board (first position) (rest position)) "W")
           (string= (GetPiece board (first position) (rest position)) "B"))
       (GenerateRandomPosition board))
      (t
       (print position)))))
; this is for counting points
(defun GenerateValidSecondPosition()
)
(defun Generate)