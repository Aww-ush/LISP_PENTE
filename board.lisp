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


(defun PrintBoardWithIndex(board)
  (princ "   ")
  (PrintColumnIndex 19)
  (terpri)
  (PrintBoard board 19))

(defun CheckIfPieceWithInBoard( row column)
  (cond
  ((or (< row 0) (> row 18)) nil)
  ((or (< column 0) (> column 18
  )) nil)
  (t t)
  )
)
(defun CheckIfPieceValid(colour)
    (cond
      ((or(string= colour "W")(string= colour "B")) t)
      (t nil)
    )
)


(defun InsertColumn (row column color)
  (cond
    ((= column 0)
     (cons color (rest row)))
    (t (cons (car row) (InsertColumn (rest row) (- column 1) color)))))

(defun InsertRow (board row column color)
  (cond
    ((= row 0)
     (cons (InsertColumn (car board) column color) (rest board)))
    (t (cons (car board) (InsertRow (rest board) (- row 1) column color)))))

(defun InsertPiece (board row column color)
  (cond
    ((or (< row 0) (> row 19) (< column 0) (> column 19))
     (format t "Invalid row or column."))
    (t (InsertRow board row column color))))


(defun ChangeToRowColumn (position)
  (let ((row (ConvertAlphaToRow (subseq position 1)))
        (column (ConvertAlphaToColumn (char position 0))))
    (cond
      ((and row column) (cons row column))
      (t nil))))


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
    ( (CheckIfPieceWithInBoard row column) nil)
    ((and (>= row 6) (<= row 12) (>= column 6) (<= column 12)) nil)
    (t t)))