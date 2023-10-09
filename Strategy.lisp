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
(defun CountPoints(row column colour board)

)
(defun ReturnTotalPointAndIfWon (listPointsAllDirection totalPoints winnerFlag)
  (cond
    ((null listPointsAllDirection) (cons totalPoints winnerFlag))
    (t 
      (let ((tmpPoint (car listPointsAllDirection)))
        (cond 
          ((>= tmpPoint 4)
            (cond 
              ((>= (- tmpPoint 5) 0)
               (if (= (- tmpPoint 5) 0)
                   (ReturnTotalPointAndIfWon (rest listPointsAllDirection) (+ 5 totalPoints) t)
                   (ReturnTotalPointAndIfWon (rest listPointsAllDirection) (+ (- tmpPoint 4) 5) totalPoints t)))
              (t (ReturnTotalPointAndIfWon (rest listPointsAllDirection) (+ 1 totalPoints) nil)))
          )
          (t (ReturnTotalPointAndIfWon (rest listPointsAllDirection) totalPoints nil))
        )
      )
    )
  )
)

(defun GetOppositeColour(colour)
  (cond
    ((string= colour "W") "B")
    (t "W")
  )
)
; starting from one position ahead to avoid counting the starting position twice
(defun CheckRightDiagonalForPoint (startingRow startingColumn colour board)
  (let ((rightUpPoints (NumPiecesInRightUp (- startingRow 1) (+ startingColumn 1) colour board 0))
        (rightDownPoints (NumPiecesInRightDown (+ startingRow 1) (- startingColumn 1) colour board 0)))
    (+ rightDownPoints rightUpPoints)
  )
)

(defun NumPiecesInRightUp (row column colour board counter)
  (cond 
    ((or (not (CheckIfPieceWithinBoard row column)) (string= (GetPiece board row column) (GetOppositeColour colour))) counter)
    (t (NumPiecesInRightUp (- row 1) (+ column 1) colour board (+ counter 1)))
  )
)
    
(defun NumPiecesInRightDown (row column color board counter)
  (cond 
    ((or (not (CheckIfPieceWithinBoard row column)) (string= (GetPiece board row column) (GetOppositeColour color))) counter)
    (t (NumPiecesInRightDown (+ row 1) (- column 1) color board (+ counter 1)))
  )
)



;;;;;;;;;;;;;;;;;
;left diagonal
;;;;;;;;;;;;;;;;;

(defun CheckLeftDiagonalForPoint (startingRow startingColumn colour board)
  (let ((upPoints (NumPiecesInLeftUp (- startingRow 1) (- startingColumn 1) colour board 0))
        (downPoints (NumPiecesInLeftDown (+ startingRow 1) (+ startingColumn 1) colour board 0)))
    (+ upPoints downPoints)
  )
)

(defun NumPiecesInLeftUp (row column colour board counter)
  (cond 
    ((or (not (CheckIfPieceWithinBoard row column)) (string= (GetPiece board row column) (GetOppositeColour colour))) counter)
    (t (NumPiecesInLeftUp (- row 1) (- column 1) colour board (+ counter 1)))
  )
)
    
(defun NumPiecesInLeftDown (row column color board counter)
  (cond 
    ((or (not (CheckIfPieceWithinBoard row column)) (string= (GetPiece board row column) (GetOppositeColour color))) counter)
    (t (NumPiecesInLeftDown (+ row 1) (+ column 1) color board (+ counter 1)))
  )
)


;;;;;;;;;;;;;;;;;
;vertical
;;;;;;;;;;;;;;;;;
(defun CheckVerticalForPoint (startingRow startingColumn colour board)
  (let ((upPoints (NumPiecesInVerticalUp (- startingRow 1) startingColumn colour board 0))
        (downPoints (NumPiecesInVerticalDown (+ startingRow 1) startingColumn colour board 0)))
    (+ upPoints downPoints)
  )
)

(defun NumPiecesInVerticalUp (row column colour board counter)
  (cond 
    ((or (not (CheckIfPieceWithinBoard row column)) (string= (GetPiece board row column) (GetOppositeColour colour))) counter)
    (t (NumPiecesInVerticalUp (- row 1) column colour board (+ counter 1)))
  )
)
    
(defun NumPiecesInVerticalDown (row column color board counter)
  (cond 
    ((or (not (CheckIfPieceWithinBoard row column)) (string= (GetPiece board row column) (GetOppositeColour color))) counter)
    (t (NumPiecesInLeftDown (+ row 1) column color board (+ counter 1)))
  )
)

;;;;;;;;;;;;;;;;;
;horizontal
;;;;;;;;;;;;;;;;;
(defun CheckHorizontalForPoint (startingRow startingColumn colour board)
  (let ((upPoints (NumPiecesInLeftHorizontal startingRow (- startingColumn 1) colour board 0))
        (downPoints (NumPiecesInRightHorizontal startingRow (+ startingColumn 1) colour board 0)))
    (+ upPoints downPoints)
  )
)

(defun NumPiecesInLeftHorizontal (row column colour board counter)
  (cond 
    ((or (not (CheckIfPieceWithinBoard row column)) (string= (GetPiece board row column) (GetOppositeColour colour))) counter)
    (t (NumPiecesInLeftHorizontal (- row 1) (- column 1) colour board (+ counter 1)))
  )
)
    
(defun NumPiecesInRightHorizontal (row column color board counter)
  (cond 
    ((or (not (CheckIfPieceWithinBoard row column)) (string= (GetPiece board row column) (GetOppositeColour color))) counter)
    (t (NumPiecesInRightHorizontal (+ row 1) (+ column 1) color board (+ counter 1)))
  )
)
