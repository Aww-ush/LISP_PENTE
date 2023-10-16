(load "/Users/aayushshrestha/Desktop/LispProject/board.lisp")
(load "/Users/aayushshrestha/Desktop/LispProject/Player.lisp")
(defun GenerateRandomPosition (board)
  (let ((position (list (random 19) (random 19))))
    (cond
      ((or (string= (GetPiece board (nth 0 position) (nth 1 position)) "W")
           (string= (GetPiece board (nth 0 position) (nth 1 position)) "B"))
       (GenerateRandomPosition board))
      (t
        position))
  )
)

; this is for counting points
(defun GenerateValidSecondPosition(board)
    
    (let((position(GenerateRandomPosition board)))
          (cond
            ((not(CheckIfPlaceEmpty board (nth 0 position) (nth 1 position))) (GenerateValidSecondPosition board))
            ((IsSecondPositionValid board (nth 0 position) (nth 1 position)) (print "This is the best position accoring to game rules") position)
            (t (GenerateValidSecondPosition board))
          )
    )
)

(defun CanRtDiagonalUpBeCaptured(row column colour board)
    (cond 
      ((CheckIfPieceWithInBoard  (- row 3) (+ column 3))
        ( cond 
         ( (string= (GetPiece board (- row 3) (+ column 3)) colour)
            ;now check if count equals 2
            (cond 
              ((= (NumPiecesInRightUp (- row 1) (+ column 1) (GetOppositeColour colour) board 0) 2) 1)
              (t 0)
            )
            
         )
         ( t 0)
          
        )
      )
      (t 0)
    )
)
(defun CanRtDiagonalDownBeCaptured(row column colour board)
    (cond 
      ((CheckIfPieceWithInBoard  (+ row 3) (- column 3))
        ( cond 
         ( (string= (GetPiece board (+ row 3) (- column 3)) colour)
            ;now check if count equals 2
            (cond 
              ((= (NumPiecesInRightDown (+ row 1) (- column 1) (GetOppositeColour colour) board 0) 2) 1)
              (t 0)
            )
            
         )
         ( t 0)
          
        )
      )
      (t 0)
    )
)
(defun CanLtDiagonalUpBeCaptured(row column colour board)
      (cond 
      ((CheckIfPieceWithInBoard  (- row 3) (- column 3))
        ( cond 
         ( (string= (GetPiece board (- row 3) (- column 3)) colour)
            ;now check if count equals 2
            (cond 
              ((= (NumPiecesInLeftUp (- row 1) (- column 1) (GetOppositeColour colour) board 0) 2) 1)
              (t 0)
            )
            
         )
         ( t 0)
          
        )
      )
      (t 0)
    )
)
(defun CanLtDiagonalDownBeCaptured(row column colour board)
      (cond 
      ((CheckIfPieceWithInBoard  (+ row 3) (+ column 3))
        ( cond 
         ( (string= (GetPiece board (+ row 3) (+ column 3)) colour)
            ;now check if count equals 2
            (cond 
              ((= (NumPiecesInLeftUp (+ row 1) (+ column 1) (GetOppositeColour colour) board 0) 2) 1)
              (t 0)
            )
            
         )
         ( t 0)
          
        )
      )
      (t 0)
    )
)
(defun CanHorizontalRightBeCaptured(row column colour board)
      (cond 
      ((CheckIfPieceWithInBoard  row (+ column 3))
        ( cond 
         ( (string= (GetPiece board row (+ column 3)) colour)
            ;now check if count equals 2
            (cond 
              ((= (NumPiecesInRightHorizontal row (+ column 1) (GetOppositeColour colour) board 0) 2) 1)
              (t 0)
            )
            
         )
         ( t 0)
          
        )
      )
      (t 0)
    )
)
(defun CanHorizontalLeftBeCaptured(row column colour board)
      (cond 
      ((CheckIfPieceWithInBoard  row (- column 3))
        ( cond 
         ( (string= (GetPiece board row (- column 3)) colour)
            ;now check if count equals 2
            (cond 
              ((= (NumPiecesInLeftUp row (- column 1) (GetOppositeColour colour) board 0) 2) 1)
              (t 0)
            )
            
         )
         ( t 0)
          
        )
      )
      (t 0)
    )
)
;;vertical
(defun CanVerticalUptBeCaptured(row column colour board)
      (cond 
      ((CheckIfPieceWithInBoard  (- row 3) column)
        ( cond 
         ( (string= (GetPiece board (- row 3) column) colour)
            ;now check if count equals 2
            (cond 
              ((= (NumPiecesInRightHorizontal (- row 1) column (GetOppositeColour colour) board 0) 2) 1)
              (t 0)
            )
            
         )
         ( t 0)
          
        )
      )
      (t 0)
    )
)
(defun CanVerticalDownBeCaptured(row column colour board)
      (cond 
      ((CheckIfPieceWithInBoard  (+ row 3) column)
        ( cond 
         ( (string= (GetPiece board (+ row 3) column) colour)
            ;now check if count equals 2
            (cond 
              ((= (NumPiecesInLeftUp (+ row 1) column (GetOppositeColour colour) board 0) 2) 1)
              (t 0)
            )
            
         )
         ( t 0)
          
        )
      )
      (t 0)
    )
)
(defun CountPoints (row column colour board)
  (let ((listPointsAllDirection (list (CheckRightDiagonalForPoint row column colour board)
                                      (CheckLeftDiagonalForPoint row column colour board)
                                      (CheckHorizontalForPoint row column colour board)
                                      (CheckVerticalForPoint row column colour board))))
    (ReturnTotalPoint listPointsAllDirection 0 0))

)


(defun ReturnTotalPoint (listPointsAllDirection totalPoints counter)
  (cond
    ((= counter 4) totalPoints )
    (t 
      (let ((tmpPoint (first listPointsAllDirection)))
        (cond 
          ((>= tmpPoint 4)
            (cond 
              ((>= (- tmpPoint 5) 0)
               (cond 
                 ((= (mod tmpPoint 5) 0)
                  (ReturnTotalPoint (rest listPointsAllDirection) (+ tmpPoint totalPoints) (+ counter 1)))
                 (t
                  (let ((result (mod tmpPoint 5)))
                    (cond 
                      ((> result 4)
                        (ReturnTotalPoint (rest listPointsAllDirection)
                                                   (+ (* (/ tmpPoint 5) 5) (/ result 4) totalPoints) (+ counter 1)))
                      (t
                        (ReturnTotalPoint (rest listPointsAllDirection)
                                                   (+ (* (/ tmpPoint 5) 5) totalPoints) (+ counter 1))))))))
              (t (ReturnTotalPoint (rest listPointsAllDirection) (+ 1 totalPoints) (+ counter 1)))))
          (t (ReturnTotalPoint (rest listPointsAllDirection) totalPoints (+ counter 1)))
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
    (+ (+ rightDownPoints 1) rightUpPoints)
  )
)

(defun NumPiecesInRightUp (row column colour board counter)
  (cond 
    ((or (not (CheckIfPieceWithinBoard row column)) (or (string= (GetPiece board row column) (GetOppositeColour colour)) (string= (GetPiece board row column) "O"))) counter)
    (t (NumPiecesInRightUp (- row 1) (+ column 1) colour board (+ counter 1)))
  )
)
    
(defun NumPiecesInRightDown (row column colour board counter)
  (cond 
    ((or (not (CheckIfPieceWithinBoard row column)) (or (string= (GetPiece board row column) (GetOppositeColour colour)) (string= (GetPiece board row column) "O"))) counter)
    (t (NumPiecesInRightDown (+ row 1) (- column 1) colour board (+ counter 1)))
  )
)



;;;;;;;;;;;;;;;;;
;left diagonal
;;;;;;;;;;;;;;;;;

(defun CheckLeftDiagonalForPoint (startingRow startingColumn colour board)
  (let ((upPoints (NumPiecesInLeftUp (- startingRow 1) (- startingColumn 1) colour board 0))
        (downPoints (NumPiecesInLeftDown (+ startingRow 1) (+ startingColumn 1) colour board 0)))
    (+ (+ upPoints 1) downPoints)
  )
)

(defun NumPiecesInLeftUp (row column colour board counter)
  (cond 
    ((or (not (CheckIfPieceWithinBoard row column)) (or (string= (GetPiece board row column) (GetOppositeColour colour)) (string= (GetPiece board row column) "O"))) counter)
    (t (NumPiecesInLeftUp (- row 1) (- column 1) colour board (+ counter 1)))
  )
)
    
(defun NumPiecesInLeftDown (row column colour board counter)
  (cond 
    ((or (not (CheckIfPieceWithinBoard row column)) (or (string= (GetPiece board row column) (GetOppositeColour colour)) (string= (GetPiece board row column) "O"))) counter)
    (t (NumPiecesInLeftDown (+ row 1) (+ column 1) colour board (+ counter 1)))
  )
)


;;;;;;;;;;;;;;;;;
;vertical
;;;;;;;;;;;;;;;;;
(defun CheckVerticalForPoint (startingRow startingColumn colour board)
  (let ((upPoints (NumPiecesInVerticalUp (- startingRow 1) startingColumn colour board 0))
        (downPoints (NumPiecesInVerticalDown (+ startingRow 1) startingColumn colour board 0)))
    (+ (+ upPoints 1) downPoints)
  )
)

(defun NumPiecesInVerticalUp (row column colour board counter)
  (cond 
    ((or (not (CheckIfPieceWithinBoard row column)) (or (string= (GetPiece board row column) (GetOppositeColour colour)) (string= (GetPiece board row column) "O"))) counter)
    (t (NumPiecesInVerticalUp (- row 1) column colour board (+ counter 1)))
  )
)
    
(defun NumPiecesInVerticalDown (row column colour board counter)
  (cond 
    ((or (not (CheckIfPieceWithinBoard row column)) (or (string= (GetPiece board row column) (GetOppositeColour colour)) (string= (GetPiece board row column) "O"))) counter)
    (t (NumPiecesInVerticalDown (+ row 1) column colour board (+ counter 1)))
  )
)

;;;;;;;;;;;;;;;;;
;horizontal
;;;;;;;;;;;;;;;;;
(defun CheckHorizontalForPoint (startingRow startingColumn colour board)
  (let ((upPoints (NumPiecesInLeftHorizontal startingRow (- startingColumn 1) colour board 0))
        (downPoints (NumPiecesInRightHorizontal startingRow (+ startingColumn 1) colour board 0)))
    (+ (+ upPoints 1)  downPoints)
  )
)

(defun NumPiecesInLeftHorizontal (row column colour board counter)
  (cond 
    ((or (not (CheckIfPieceWithinBoard row column)) (or (string= (GetPiece board row column) (GetOppositeColour colour)) (string= (GetPiece board row column) "O"))) counter)
    (t (NumPiecesInLeftHorizontal row (- column 1) colour board (+ counter 1)))
  )
)
    
(defun NumPiecesInRightHorizontal (row column colour board counter)
  (cond 
    ((or (not (CheckIfPieceWithinBoard row column)) (or (string= (GetPiece board row column) (GetOppositeColour colour)) (string= (GetPiece board row column) "O"))) counter)
    (t (NumPiecesInRightHorizontal row (+ column 1) colour board (+ counter 1)))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;removes if there is valid capture point and returns the total point
(defun CalculateTotalCapturePoint(row column colour board counter totalPoints)
  (cond
    ; right diagonal 
    ((= counter 0) 
      (cond 
        ((= (CanRtDiagonalUpBeCaptured row column colour board) 1)
          (let((newBoard(InsertPiece board (- row 1) (+ column 1) "O")))
              (CalculateTotalCapturePoint row column colour (InsertPiece newBoard (- row 2) (+ column 2) "O") (+ counter 1) (+ totalPoints 1))
          )
        )
        (t (CalculateTotalCapturePoint row column colour board (+ counter 1) totalPoints) )
      )
    )
    ((= counter 1) 
      (cond 
        ((= (CanRtDiagonalDownBeCaptured row column colour board) 1)
          (let((newBoard(InsertPiece board (+ row 1) (- column 1) "O")))
              (CalculateTotalCapturePoint row column colour (InsertPiece newBoard (+ row 2) (- column 2) "O") (+ counter 1) (+ totalPoints 1))
          )
        )
        (t (CalculateTotalCapturePoint row column colour board (+ counter 1) totalPoints) )
      )
    )
    ;; left diagonal
    ((= counter 2) 
      (cond 
        ((= (CanLtDiagonalUpBeCaptured row column colour board) 1)
          (let((newBoard(InsertPiece board (- row 1) (- column 1) "O")))
              (CalculateTotalCapturePoint row column colour (InsertPiece newBoard (- row 2) (- column 2) "O") (+ counter 1) (+ totalPoints 1))
          )
        )
        (t (CalculateTotalCapturePoint row column colour board (+ counter 1) totalPoints) )
      )
    )
    ((= counter 3) 
      (cond 
        ((= (CanLtDiagonalDownBeCaptured row column colour board) 1)
          (let((newBoard(InsertPiece board (+ row 1) (+ column 1) "O")))
              (CalculateTotalCapturePoint row column colour (InsertPiece newBoard (+ row 2) (+ column 2) "O") (+ counter 1) (+ totalPoints 1))
          )
        )
        (t (CalculateTotalCapturePoint row column colour board (+ counter 1) totalPoints) )
      )
    )
    ; horizontal
    ((= counter 4) 
      (cond 
        ((= (CanHorizontalLeftBeCaptured row column colour board) 1)
          (let((newBoard(InsertPiece board row (- column 1) "O")))
              (CalculateTotalCapturePoint row column colour (InsertPiece newBoard  row (- column 2) "O") (+ counter 1) (+ totalPoints 1))
          )
        )
        (t (CalculateTotalCapturePoint row column colour board (+ counter 1) totalPoints) )
      )
    )
    ((= counter 5) 
      (cond 
        ((= (CanHorizontalRightBeCaptured row column colour board) 1)
          (let((newBoard(InsertPiece board row (+ column 1) "O")))
              (CalculateTotalCapturePoint row column colour (InsertPiece newBoard row (+ column 2) "O") (+ counter 1) (+ totalPoints 1))
          )
        )
        (t (CalculateTotalCapturePoint row column colour board (+ counter 1) totalPoints) )
      )
    )
    ; vetical
    ((= counter 6) 
      (cond 
        ((= (CanVerticalUptBeCaptured row column colour board) 1)
          (let((newBoard(InsertPiece board (- row 1) column "O")))
              (CalculateTotalCapturePoint row column colour (InsertPiece newBoard (- row 2) column "O") (+ counter 1) (+ totalPoints 1))
          )
        )
        (t (CalculateTotalCapturePoint row column colour board (+ counter 1) totalPoints) )
      )
    )
    ((= counter 7) 
      (cond 
        ((= (CanVerticalDownBeCaptured row column colour board) 1)
          (let((newBoard(InsertPiece board (+ row 1) column "O")))
              (CalculateTotalCapturePoint row column colour (InsertPiece newBoard (- row 2) column "O") (+ counter 1) (+ totalPoints 1))
          )
        )
        (t (CalculateTotalCapturePoint row column colour board (+ counter 1) totalPoints) )
      )
    )
    (t (cons board totalPoints))
  )
)
; BestPosition to place
; to capture
; to block
; to score
; calculate the round point
; calculate the round points
; this should return updated humana nc computer point
; thia can also be used to block
(defun BestPositionForPoint(board playerColour rowCounter maxPoints bestRow bestColumn)
  (cond
    ((< rowCounter 19)
     (let* ((result (CalculateBestPositionForPoint board playerColour rowCounter 0 maxPoints bestRow bestColumn)))
        (cond
            ((> (nth 0 result) maxPoints)(BestPositionForPoint board playerColour (+ rowCounter 1) (nth 0 result) (nth 1 result) (nth 2 result)))
            (t (BestPositionForPoint board playerColour (+ rowCounter 1) maxPoints bestRow bestColumn))
        )

      )
    )
    (t
     (cond 
        ((= maxPoints 0) '( -1 -1 -1))
        (t (list maxPoints bestRow bestColumn))
     )
    )
  )
)
(defun CalculateBestPositionForPoint(board playerColour rowCounter columnCounter maxPoints bestRow bestColumn)
  (cond
    ((< columnCounter 19)
     (let ((piece (GetPiece board rowCounter columnCounter)))
       (cond
         ((string= piece "O")
            (let ((points(CountPoints rowCounter columnCounter playerColour board)))
                  (cond 
                      ((> points maxPoints)(CalculateBestPositionForPoint board playerColour rowCounter (+ columnCounter 1) points rowCounter columnCounter))
                      (t (CalculateBestPositionForPoint board playerColour rowCounter (+ columnCounter 1) maxPoints bestRow bestColumn))
                  )
            )
        )
        (t (CalculateBestPositionForPoint board playerColour rowCounter (+ columnCounter 1) maxPoints bestRow bestColumn))
       )
     )
    )
    (t
     (list maxPoints bestRow bestColumn)
    )
  )
)
; best postition for capture
(defun BestPositionForCapture(board playerColour rowCounter maxCapturePoints bestRow bestColumn)
  (cond
    ((< rowCounter 19)
     (let ((result (CalculateBestPositionForCapture board playerColour rowCounter 0 maxCapturePoints bestRow bestColumn)))
        (cond
            ((< maxCapturePoints (nth 0 result))(BestPositionForPoint board playerColour (+ rowCounter 1) (nth 0 result) (nth 1 result) (nth 2 result)))
            (t (BestPositionForCapture board playerColour (+ rowCounter 1) maxCapturePoints bestRow bestColumn))
        )

      )
    )
    (t
     (cond 
        ((= maxCapturePoints 0) '(-1 -1))
        (t (list bestRow bestColumn))
     )
    )
  )
)
(defun CalculateBestPositionForCapture(board playerColour rowCounter columnCounter maxCapturePoints bestRow bestColumn)
  (cond
    ((< columnCounter 19)
     (let ((piece (GetPiece board rowCounter columnCounter)))
       (cond
        ((string= piece "O")
            (let ((capturePoints(rest(CalculateTotalCapturePoint rowCounter columnCounter playerColour board 0 0))))
                  (cond 
                      ((> capturePoints maxCapturePoints)(CalculateBestPositionForCapture board playerColour rowCounter (+ columnCounter 1) capturePoints rowCounter columnCounter))
                      (t (CalculateBestPositionForCapture board playerColour rowCounter (+ columnCounter 1) maxCapturePoints bestRow bestColumn))
                  )
            )
        )
        (t (CalculateBestPositionForCapture board playerColour rowCounter (+ columnCounter 1) maxCapturePoints bestRow bestColumn))
       )
     )
    )
    (t
     (list maxCapturePoints bestRow bestColumn)
    )
  )
)

;creating iniitative
; if the score is equal and there is nthg to block for point 
; block iniitiative
;; (defun BestPositionForConsecutive(board playerColour rowCounter bestRow bestColumn)
;;   (cond
;;     ((< rowCounter 19)
;;      (let ((result (CalculateBestPositionForCapture board playerColour rowCounter 0 maxCapturePoints bestRow bestColumn)))
;;         (cond
;;             ((< maxCapturePoints (nth 0 result))(BestPositionForPoint board playerColour (+ rowCounter 1) (nth 0 result) (nth 1 result) (nth 2 result)))
;;             (t (BestPositionForConsecutive board playerColour (+ rowCounter 1) maxCapturePoints bestRow bestColumn))
;;         )

;;       )
;;     )
;;     (t
;;      (cond 
;;         ((= maxCapturePoints 0) '(-1 -1))
;;         (t (list bestRow bestColumn))
;;      )
;;     )
;;   )
;; )
;; (defun CalculateBestPositionForConsecutive(board playerColour rowCounter columnCounter maxCapturePoints bestRow bestColumn)
;;   (cond
;;     ((< columnCounter 19)
;;      (let ((piece (GetPiece board rowCounter columnCounter)))
;;        (cond
;;         ((string= piece "O")
;;             (let ((capturePoints(rest(BuildingConsecutive rowCounter columnCounter playerColour))))
;;                   (cond 
;;                       ((> capturePoints maxCapturePoints)(CalculateBestPositionForConsucutive board playerColour rowCounter (+ columnCounter 1) capturePoints rowCounter columnCounter))
;;                       (t (CalculateBestPositionForConsucutive board playerColour rowCounter (+ columnCounter 1) maxCapturePoints bestRow bestColumn))
;;                   )
;;             )
;;         )
;;         (t (CalculateBestPositionForConsucutive board playerColour rowCounter (+ columnCounter 1) maxCapturePoints bestRow bestColumn))
;;        )
;;      )
;;     )
;;     (t
;;      (list maxCapturePoints bestRow bestColumn)
;;     )
;;   )
;; )

(defun FindLargest (lst max)
  (cond
    ((null lst) max) 
    (t (let ((num (first lst)))
         (cond
           ((> num max) (FindLargest (rest lst) num))
           (t (FindLargest (rest lst) max))
         )
       )
    )
  )
)
(defun GetPoint(board colour)
  (let ((fillInitiativePoints (CheckForFillingInitiativeRow 0 0 colour board)))
    (cond
      ((>= (first fillInitiativePoints) 0)
       (print "This is the best location where you can start filling 4 in a row to gain points")
       fillInitiativePoints)
      (t
       (let ((buildInitiative (CheckForBuildingInitiative 0 0 colour board)))
         (cond
           ((>= (first buildInitiative) 0)
            (print "This is the best location where you can start building 4 in a row to gain points")
            buildInitiative)
           (t
            (GenerateValidSecondPosition board)))))
      )
    )
  )
(defun CheckForBuildingInitiative(rowCounter columnCounter colour board)
    (cond 
      (
        (< rowCounter 19)
        (let((newRowCol(CheckForBuildingInitiativeColumn board colour rowCounter 0)))
            (cond
                   ((>= (first newRowCol) 0) newRowCol)
                   (t (CheckForBuildingInitiative (+ rowCounter 1) 0 colour board))
            )
        )
      )
      (t '(-1 -1))
    )
)
(defun CheckForBuildingInitiativeColumn(board playerColour rowCounter columnCounter)
  (cond
      ((< columnCounter 19)
        (let ((piece (GetPiece board rowCounter columnCounter)))
          (cond
            ((string= piece playerColour)
              (let ((newRowCol(MakeInitiative rowCounter columnCounter playerColour board 0)))
                  (cond
                    ((>= (first newRowCol) 0) newRowCol)
                    (t (CheckForBuildingInitiativeColumn board playerColour rowCounter (+ columnCounter 1)))
                  )
              )
            )
            (t (CheckForBuildingInitiativeColumn board playerColour rowCounter (+ columnCounter 1)))
          )
        )
      )
      (t '(-1 -1))
    )
)
; check on all direction if there is same colour piece 4 intesection away and
(defun CheckForFillingInitiativeRow(rowCounter columnCounter colour board)
    (cond 
      ((< rowCounter 19)
        (let((newRowCol(CheckForFillingInitiativeColumn board colour rowCounter columnCounter)))
            (cond
                   ((>= (first newRowCol) 0) newRowCol)
                   (t (CheckForFillingInitiativeRow (+ rowCounter 1) 0 colour board))
            )
        )
      )
      (t '(-1 -1))
    )
)
(defun CheckForFillingInitiativeColumn(board playerColour rowCounter columnCounter)
  (cond
      ((< columnCounter 19)
        (let ((piece (GetPiece board rowCounter columnCounter)))
          (cond
            ((string= piece playerColour)
              (let ((newRowCol(FillInitiative rowCounter columnCounter playerColour board 0)))
                  (cond
                    ((>= (first newRowCol) 0) newRowCol)
                    (t (CheckForFillingInitiativeColumn board playerColour rowCounter (+ columnCounter 1)))
                  )
              )
            )
            (t (CheckForFillingInitiativeColumn board playerColour rowCounter (+ columnCounter 1)))
          )
        )
      )
      (t '(-1 -1))
    )
)
(defun NumEmptyPiecesInRightUp (row column board counter)
  (cond 
    ((or (not (CheckIfPieceWithinBoard row column)) (or (string= (GetPiece board row column) "B") (string= (GetPiece board row column) "W"))) counter)
    (t (NumEmptyPiecesInRightUp (- row 1) (+ column 1) board (+ counter 1)))
  )
)
(defun NumEmptyPiecesInRightDown (row column board counter)
  (cond 
    ((or (not (CheckIfPieceWithinBoard row column)) (or (string= (GetPiece board row column) "B") (string= (GetPiece board row column) "W"))) counter)
    (t (NumEmptyPiecesInRightDown (+ row 1) (- column 1)  board (+ counter 1)))
  )
)

(defun NumEmptyPiecesInLeftUp (row column board counter)
  (cond 
    ((or (not (CheckIfPieceWithinBoard row column)) (or (string= (GetPiece board row column) "B") (string= (GetPiece board row column) "W"))) counter)
    (t (NumEmptyPiecesInLeftUp (- row 1) (- column 1)  board (+ counter 1)))
  )
)
(defun NumEmptyPiecesInLeftDown (row column board counter)
  (cond 
    ((or (not (CheckIfPieceWithinBoard row column)) (or (string= (GetPiece board row column) "B") (string= (GetPiece board row column) "W"))) counter)
    (t (NumEmptyPiecesInLeftDown (+ row 1) (+ column 1) board (+ counter 1)))
  )
)
(defun NumEmptyPiecesInLeftHorizontal (row column board counter)
  (cond 
    ((or (not (CheckIfPieceWithinBoard row column)) (or (string= (GetPiece board row column) "B") (string= (GetPiece board row column) "W"))) counter)
    (t (NumEmptyPiecesInLeftHorizontal row (- column 1) board (+ counter 1)))
  )
)
    
(defun NumEmptyPiecesInRightHorizontal (row column board counter)
  (cond 
   ((or (not (CheckIfPieceWithinBoard row column)) (or (string= (GetPiece board row column) "B") (string= (GetPiece board row column) "W"))) counter)
    (t (NumEmptyPiecesInRightHorizontal row (+ column 1) board (+ counter 1)))
  )
)
(defun NumEmptyPiecesInVerticalUp (row column board counter)
  (cond 
     ((or (not (CheckIfPieceWithinBoard row column)) (or (string= (GetPiece board row column) "B") (string= (GetPiece board row column) "W"))) counter)
    (t (NumEmptyPiecesInVerticalUp (- row 1) column board (+ counter 1)))
  )
)
    
(defun NumEmptyPiecesInVerticalDown (row column board counter)
  (cond 
   ((or (not (CheckIfPieceWithinBoard row column)) (or (string= (GetPiece board row column) "B") (string= (GetPiece board row column) "W"))) counter)
    (t (NumEmptyPiecesInVerticalDown (+ row 1) column board (+ counter 1)))
  )
)

;fill in the initiative
; fill in the bridge
; how to fill in the bridge
; first check if there is bridge int hat direction
; then fill the peice in that direction
; to know if it is abridge the peice 4 piece away must be of same colour
; then the position should be empty
; this can also be use to block inititive
(defun FillInitiative(row column colour board counter)
  (cond ((= counter 0)
          (cond
            ((and (CheckIfPieceWithinBoard (- row 4) (+ column 4))
                 (and (= (NumEmptyPiecesInRightUp (- row 1) (+ column 1) board counter) 2)
                      (string= (GetPiece board (- row 4) (+ column 4)) colour)))
            (list (- row 1) (+ column 1)))
            (t (FillInitiative row column colour board (+ counter 1)))
          ))
        ((= counter 1)
          (cond
            ((and (CheckIfPieceWithinBoard (+ row 4) (- column 4))
                 (and (= (NumEmptyPiecesInRightDown (+ row 1) (- column 1) board counter) 2)
                      (string= (GetPiece board (+ row 4) (- column 4)) colour)))
            (list (+ row 1) (- column 1)))
            (t (FillInitiative row column colour board (+ counter 1)))
          ))
        ((= counter 2)
          (cond
            ((and (CheckIfPieceWithinBoard (- row 4) (- column 4))
                 (and (= (NumEmptyPiecesInLeftUp (- row 1) (- column 1) board counter) 2)
                      (string= (GetPiece board (- row 4) (- column 4)) colour)))
            (list (- row 1) (- column 1)))
            (t (FillInitiative row column colour board (+ counter 1)))
          ))
        ((= counter 3)
          (cond
           ( (and (CheckIfPieceWithinBoard (+ row 4) (+ column 4))
                 (and (= (NumEmptyPiecesInLeftDown (+ row 1) (+ column 1) board counter) 2)
                      (string= (GetPiece board (+ row 4) (+ column 4)) colour)))
            (list (+ row 1) (+ column 1)))
            (t (FillInitiative row column colour board (+ counter 1)))
          ))
        ((= counter 4)
          (cond
            ((and (CheckIfPieceWithinBoard (- row 4) column)
                 (and (= (NumEmptyPiecesInVerticalUp (- row 1) column board counter) 2)
                      (string= (GetPiece board (- row 4) column) colour)))
            (list (- row 1) column))
            (t (FillInitiative row column colour board (+ counter 1)))
          ))
        ((= counter 5)
          (cond
            ((and (CheckIfPieceWithinBoard (+ row 4) column)
                 (and (= (NumEmptyPiecesInVerticalDown (+ row 1) column board counter) 2)
                      (string= (GetPiece board (+ row 4) column) colour)))
            (list (+ row 1) column))
            (t (FillInitiative row column colour board (+ counter 1)))
          ))
        ((= counter 6)
          (cond
            ((and (CheckIfPieceWithinBoard row (- column 4))
                 (and (= (NumEmptyPiecesInLeftHorizontal row (- column 1) board counter) 2)
                      (string= (GetPiece board row (- column 4)) colour)))
            (list row (- column 1)))
            (t (FillInitiative row column colour board (+ counter 1)))
          ))
        ((= counter 7)
          (cond
            ((and (CheckIfPieceWithinBoard row (+ column 4))
                 (and (= (NumEmptyPiecesInRightHorizontal row (+ column 1) board counter) 2)
                      (string= (GetPiece board row (+ column 4)) colour)))
            (list row (+ column 1)))
            (t (FillInitiative row column colour board (+ counter 1)))
          ))
          (t '(-1 -1))
    )
    
)

; always create bridge in 
; gives point 4 intersection away
(defun MakeInitiative(row column colour board counter)
  (let* ((listEmpty (list (NumEmptyPiecesInRightUp (- row 1) (+ column 1) board counter)
                         (NumEmptyPiecesInRightDown (- row 1) (- column 1) board counter)
                         (NumEmptyPiecesInLeftUp (- row 1) (- column 1) board counter)
                         (NumEmptyPiecesInLeftDown (+ row 1) (+ column 1) board counter)
                         (NumEmptyPiecesInVerticalUp (- row 1) column board counter)
                         (NumEmptyPiecesInVerticalDown (+ row 1) column board counter)
                         (NumEmptyPiecesInLeftHorizontal row (- column 1) board counter)
                         (NumEmptyPiecesInRightHorizontal row (+ column 1) board counter)))
         (largestEmptyDir (FindLargest listEmpty 0)))
    (cond 
      ((< largestEmptyDir 3) '(-1 -1))
      ((= largestEmptyDir (nth 0 listEmpty)) (list (- row 4) (+ column 4)))
      ((= largestEmptyDir (nth 1 listEmpty)) (list (+ row 4) (- column 4)))
      ((= largestEmptyDir (nth 2 listEmpty)) (list (- row 4) (- column 4)))
      ((= largestEmptyDir (nth 3 listEmpty)) (list (+ row 4) (+ column 4)))
      ((= largestEmptyDir (nth 4 listEmpty)) (list (- row 4) column))
      ((= largestEmptyDir (nth 5 listEmpty)) (list (+ row 4) column))
      ((= largestEmptyDir (nth 6 listEmpty)) (list row (- column 4)))
      ((= largestEmptyDir (nth 7 listEmpty)) (list row (+ column 4)))
      (t '(-1 -1))
    )
  )
)



(defun GetBestPosition(board totalPlayerScore playerColour totalOpponentScore)
  (cond 
    ((<= totalPlayerScore totalOpponentScore)
      ; never let opponet score 5 in a row first
      (let ((fiveInRow(BestPositionForPoint board (GetOppositeColour playerColour) 0 0 0 0)))
      (cond
        ((>= (first fiveInRow) 5) (print "This is the best position to block five in a row to prevent opponent form wining") (rest fiveInRow))
        (t (let ((maxPointRowCol (BestPositionForPoint board playerColour 0 0 0 0)))
          (cond 
            ((>= (first maxPointRowCol) 0)
              (print "This is the best position because current score is less or equal to opponent's score and placing here will help you gain points")
              (rest maxPointRowCol))
            (t 
              (let ((maxCaptureRowCol (BestPositionForCapture board playerColour 0 0 0 0)))
                (cond
                  ((>= (first maxCaptureRowCol) 0)
                    (print "This is the best position because current score is less or equal to opponent's score and placing here will help you capture points")
                    maxCaptureRowCol)
                  (t 
                    (let ((maxBlockPointRowCol (BestPositionForPoint board (GetOppositeColour playerColour) 0 0 0 0)))
                      (cond
                        ((>= (first maxBlockPointRowCol) 0)
                          (print "This is the best position because current score is less or equal to opponent's score and placing here will help you block the opponent from scoring more")
                          (rest maxBlockPointRowCol))
                        (t 
                          ;; (let ((blockConseqRowCol (BestPositionForConsecutive board (GetOppositeColour playerColour) 0 0 0 0)))
                          ;;   (cond 
                          ;;     ((>= (first blockConseqRowCol) 0)
                          ;;       (print "This is the best position because your score is less than the opponent's score, so you need to block the opponent's move")
                          ;;       blockConseqRowCol)
                          ;;     (t (GetPoint board playerColour))
                          ;;   )
                          ;; )
                          (GetPoint board playerColour)
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ))
      ))
    )
    (t
     (let ((fiveInRow(BestPositionForPoint board (GetOppositeColour playerColour) 0 0 0 0)))
      (cond
        ((>= (first fiveInRow) 5) (print "This is the best position to block five in a row to prevent opponent form wining") (rest fiveInRow))
        (t (let ((maxPointRowCol (BestPositionForPoint board playerColour 0 0 0 0)))
        (cond 
          ((>= (first maxPointRowCol) 0)
            (print "This is the best position because current score is greater than opponent's score and placing here will help you gain more advantage")
            (rest maxPointRowCol))
          (t 
            (let ((maxCaptureRowCol (BestPositionForCapture board playerColour 0 0 0 0)))
              (cond
                ((>= (first maxCaptureRowCol) 0)
                  (print "This is the best position because current score is greater than opponent's score and capturing piece here will help you gain more advantage")
                  maxCaptureRowCol)
                (t 
                  (let ((maxBlockPointRowCol (BestPositionForPoint board (GetOppositeColour playerColour) 0 0 0 0)))
                    (cond
                      ((>= (first maxBlockPointRowCol) 0)
                        (print "This is the best position because current score is greater than opponent's score and placing here will help you block the opponent from catching up")
                        (rest maxBlockPointRowCol))
                      (t 
                        (GetPoint board playerColour)
                      )
                    )
                  )
                )
              )
            )
          )
        )
        ))
      )
     )
    )
  )
)

;; (defun BuildingConsecutive(row column colour board)
;;   (let ((listPointsAllDirection (list (CheckRightDiagonalForPoint row column colour board)
;;                                       (CheckLeftDiagonalForPoint row column colour board)
;;                                       (CheckHorizontalForPoint row column colour board)
;;                                       (CheckVerticalForPoint row column colour board)))
;;           (listConseq(FindLargest listPointsAllDirection 0))
;;         )
  
  
;;   (cond 
;;       ((< listConseq 1) '(-1 -1))
;;       ((= listConseq (nth 0 listEmpty)) (list (- row 4) (+ column 4)))
;;       ((= listConseq (nth 1 listEmpty)) (list (+ row 4) (- column 4)))
;;       ((= listConseq (nth 2 listEmpty)) (list (- row 4) (- column 4)))
;;       ((= listConseq (nth 3 listEmpty)) (list (+ row 4) (+ column 4)))
;;       ((= listConseq (nth 4 listEmpty)) (list (- row 4) column))
;;       ((= listConseq (nth 5 listEmpty)) (list (+ row 4) column))
;;       ((= listConseq (nth 6 listEmpty)) (list row (- column 4)))
;;       ((= listConseq (nth 7 listEmpty)) (list row (+ column 4)))
;;       (t '(-1 -1))
;;     )
  
;;   )
;; )