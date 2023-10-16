;(load "/Users/aayushshrestha/Desktop/LispProject/Strategy.lisp")

(defun CreateRow (n)
  (cond 
    ((= n 0) nil)
    (t (cons "O" (CreateRow (- n 1))))))

(defun CreateBoardRow (n)
  (cond
    ((= n 0) nil)
    (t (cons (CreateRow 19) (CreateBoardRow (- n 1))))))

(defun GetEmptyBoard ()
  (CreateBoardRow 19))



(defun PrintRow (row)
  (cond
    ((null row) nil)
    (t
     (princ (first row))
     (princ " ")
     (PrintRow (rest row)))))

(defun PrintColumnIndex (n)
  (cond 
    ((= n 0) nil)
    (t 
     (princ (code-char (+ (char-code #\A) (- 19 n))))
     (princ " ")
     (PrintColumnIndex (- n 1)))))

(defun PrintBoard (board n)
  (cond
    ((null board) nil)
    (t
     (princ n)
     (cond 
       ((< n 10) (princ "  "))
       (t (princ " ")))
     (PrintRow (first board))
     (terpri) ; Move to the next line for the next row
     (PrintBoard (rest board) (- n 1)))))

(defun PrintBoardWithIndex(board)
  (princ "   ")
  (PrintColumnIndex 19)
  (terpri)
  (PrintBoard board 19))

(defun CheckIfPieceWithInBoard( row column)
  (cond
  ((or (< row 0) (> row 18)) nil)
  ((or (< column 0) (> column 18)) nil)
  (t t)
  )
)
(defun CheckIfPieceValid(colour)
    (cond
      ((or(string= colour "W")(string= colour "B")) t)
      (t nil)
    )
)


(defun InsertColumn (row column colour)
  (cond
    ((= column 0)
     (cons colour (rest row)))
    (t (cons (first row) (InsertColumn (rest row) (- column 1) colour)))))

(defun InsertRow (board row column colour)
  (cond
    ((= row 0)
     (cons (InsertColumn (first board) column colour) (rest board)))
    (t (cons (first board) (InsertRow (rest board) (- row 1) column colour)))))


(defun ConvertAlphaToColumn (alphaColumn)
  (let ((column (- (char-code (char (string alphaColumn) 0)) (char-code #\A)))) ; Fixed char usage here
    (cond
      ((< column 0)
       (print "The column position is too small")
       nil)
      ((> column 19)
       (print "The column position is too large")
       nil)
      (t
       column))))

(defun ConvertAlphaToRow (alphaRow)
  (let ((row (parse-integer (string alphaRow))))
    (cond
      ((not row)
       (print "The row position is invalid")
       nil)
      ((<= row 0)
       (print "The row position is too small")
       nil)
      ((> row 19)
       (print "The row position is too large")
       nil)
      (t
       (- 19 row)))))
(defun ChangeToRowColumn (position)
  (let ((row (ConvertAlphaToRow (subseq position 1)))
        (column (ConvertAlphaToColumn (char position 0))))
    (cond
      ((and row column) (cons row column))
      (t nil))))



       
(defun FindRowInBoard (n rows)
  (cond
    ((= n 0) (first rows))
    (t (FindRowInBoard (- n 1) (rest rows)))
  )
)
(defun FindPieceAtN (n row)
  (cond
    ((= n 0) (first row))
    (t (FindPieceAtN (- n 1) (rest row)))
  )
)

(defun GetPiece (board row column)
  (FindPieceAtN column (FindRowInBoard row board))
)
(defun CheckIfPlaceEmpty (board row column)
  (cond
    ((string= (GetPiece board row column) "O") t)
    (t nil)))

(defun IsSecondPositionValid (board row column)
  (cond
    ((and (>= row 6) (<= row 12) (>= column 6) (<= column 12)) nil)
    (t t)))
(defun InsertPiece (board row column colour)
  (cond
    ((or (< row 0) (> row 19) (< column 0) (> column 19))
     (format t "Invalid row or column."))
    (t (InsertRow board row column colour))))


;; (defun CalculatePointAndCapture(board row column colour)
;;     (CountPoints row column colour board)
;; )
; returns total black and white pieces currently in the board
(defun CalculateTotalMove (board humanColour computerColour totalHumanMove totalComputerMove rowCounter humanCaptureScore computerCaptureScore)
  (cond
    ((< rowCounter 19)
     (let* ((result (CalculateTotalMovesColumn board humanColour computerColour totalHumanMove totalComputerMove rowCounter 0)))
       (CalculateTotalMove board humanColour computerColour
                          (first result) (second result) ; Separate human and computer move totals
                          (+ rowCounter 1) humanCaptureScore computerCaptureScore))) ; No changes to capture scores
    (t
     (list (+ totalHumanMove (* computerCaptureScore 2)) (+ totalComputerMove (* humanCaptureScore 2)))
    )
  )
)

(defun CalculateTotalMovesColumn (board humanColour computerColour totalHumanMove totalComputerMove rowCounter columnCounter)
  (cond
    ((< columnCounter 19)
     (let ((piece (GetPiece board rowCounter columnCounter)))
       (cond
         ((string= piece humanColour)
          (CalculateTotalMovesColumn board humanColour computerColour
                                     (+ totalHumanMove 1) totalComputerMove
                                     rowCounter (+ columnCounter 1)))
         ((string= piece computerColour)
          (CalculateTotalMovesColumn board humanColour computerColour
                                     totalHumanMove (+ totalComputerMove 1)
                                     rowCounter (+ columnCounter 1)))
         (t
          (CalculateTotalMovesColumn board humanColour computerColour
                                     totalHumanMove totalComputerMove
                                     rowCounter (+ columnCounter 1)))
         )))
    (t
     (list totalHumanMove totalComputerMove)) ; Explicitly creating a list
    )
  )

(defun CalculateRoundPointsInLoadedGame(board humanColour computerColour)
  (RoundPointCalculator board humanColour computerColour 0 0 0)
)

(defun RoundPointCalculator(board humanColour computerColour humanPoint computerPoint rowCounter)
  (cond
    ((< rowCounter 19)
     (let* ((result (CalculateTotalRoundPointColumn board humanColour computerColour humanPoint computerPoint rowCounter 0)))
       (RoundPointCalculator board humanColour computerColour
                            (nth 0 result) (nth 1 result) ; Separate human and computer points
                            (+ rowCounter 1)))
    )
    (t
     (list humanPoint computerPoint)
    )
  )
)

(defun CalculateTotalRoundPointColumn(board humanColour computerColour humanPoint computerPoint rowCounter columnCounter)
  (cond
    ((< columnCounter 19)
     (let ((piece (GetPiece board rowCounter columnCounter)))
       (cond
         ((string= piece humanColour)
          (let ((newBoard (InsertPiece board rowCounter columnCounter "O")))
            (CalculateTotalRoundPointColumn newBoard humanColour computerColour
                                          (+ humanPoint (CountPoints rowCounter columnCounter humanColour newBoard))
                                          computerPoint rowCounter (+ columnCounter 1))
          )
         )
         ((string= piece computerColour)
          (let ((newBoard (InsertPiece board rowCounter columnCounter "O")))
            (CalculateTotalRoundPointColumn newBoard humanColour computerColour
                                          humanPoint
                                          (+ computerPoint (CountPoints rowCounter columnCounter computerColour newBoard))
                                          rowCounter (+ columnCounter 1))
          )
         )
         (t
          (CalculateTotalRoundPointColumn board humanColour computerColour
                                        humanPoint computerPoint rowCounter (+ columnCounter 1))
         )
       )
     )
    )
    (t
     (list humanPoint computerPoint)
    )
  )
)