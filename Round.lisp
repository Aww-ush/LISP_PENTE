(load "/Users/aayushshrestha/Desktop/LispProject/board.lisp")
(load "/Users/aayushshrestha/Desktop/LispProject/Player.lisp")
; human and player will be list that has following element (colour totalPoints Points Capture and TotalMoves)
; this should return human and computer in a list
(defun PlayRound (human computer board nextPlayer)
  (PrintBoardStatus board human computer nextPlayer)
  (cond
    ((string= nextPlayer "H")
      (let ((newBoardPlayer (MakeMove board human)))
        (cond 
            ((DoesUserWantToContinue)
                (PlayRound (rest newBoardPlayer) computer (first newBoardPlayer) "C")
            )
            (t nil)
        )
      ))
    (t 
      (let ((newBoardPlayer (MakeMove board computer)))
            (cond 
            ((DoesUserWantToContinue)
                (PlayRound human (rest newBoardPlayer)  (first newBoardPlayer) "H")   
            )
            (t nil)
        )
      ))
  )
)

; position contains eg A19 where A is the column and 19 is the row 
; need to seperate the user string for position
;; /* *********************************************************************
;; Function Name: AskForPosition
;; Purpose: To prompt the user to enter a position on the board and validate the input
;; Parameters:
;;     board, a 2D game board represented as a list
;; Return Value: A valid position on the board as a list (row column)
;; Algorithm:
;;    1) Display a prompt asking the user to enter a position on the board (e.g., 'A1').
;;    2) Read the user's input and convert it to uppercase.
;;    3) Check the length of the input:
;;       - If it's less than 2 characters, prompt the user to enter both column and row.
;;       - If it's more than 3 characters, prompt the user to enter both column and row without spaces.
;;    4) Convert the input to a row-column pair using the ChangeToRowColumn function.
;;    5) Check the validity of the position:
;;       - If it's invalid, prompt the user to enter a valid row or column.
;;       - If there's already a piece at that position, prompt the user to enter an empty position.
;;    6) Return the valid position as a list (row column).
;; Assistance Received: none
;; ********************************************************************* */
(defun AskForPosition (board)
  (princ "Please enter the position on the board where you want to move your piece (e.g., 'A1'): ")
  (terpri)
  (let ((input (string-upcase (read))))
    (cond
      ((< (length input) 2)
       (print "The position is too short. Please enter both column and row.")
       (terpri)
       (AskForPosition board))
      ((> (length input) 3)
       (print "The position is too long. Please enter both column and row without spaces.")
       (terpri)
       (AskForPosition board))
      (t
       (let ((position (ChangeToRowColumn input)))
        ; Local Variable: position, a list containing the row and column of the input position
        (cond
          ((not position)
           (print "Invalid row or column.")
           (terpri)
           (AskForPosition board))
          ((not (CheckIfPlaceEmpty board (first position) (rest position)))
           (print "Piece already exists at this position.")
           (terpri)
           (AskForPosition board))
          (t position)))))))

(defun AskForSecondPosition(board)
    (print "Since this is second position, please enter 3 intersection away from center!!")
    (terpri)
    (let ((position(AskForPosition board)))
        (cond 
            ((not (IsSecondPositionValid board (first position) (rest position)))(AskForSecondPosition board))
            (t position)
        )
    )
)
(defun DoesUserWantToContinue ()
  (princ "Do you want to continue? Please enter \"N\" for 'No' and \"Y\" for 'Yes': ")
  (terpri)
  (let ((input (string-upcase (read))))
    (cond
      ((string= input "Y") t)
      ((string= input "N") nil)
      (t (DoesUserWantToContinue)))))

(defun PrintBoardStatus (board human computer nextPlayer)
    (terpri)
    (princ "The current status is:")
    (terpri)
    (PrintBoardWithIndex board)
    (terpri)
    (princ "Human:")
    (terpri)
    (princ "Round Score: ")
    (print (GetPlayerRoundPoints human))
    (terpri)
    (princ "Total Capture Point: ")
    (print (GetPlayerCapturePoints human))
    (terpri)
    (princ "Computer:")
    (terpri)
    (princ "Round Score: ")
    (print (GetPlayerRoundPoints computer))
    (terpri)
    (princ "Total Capture Point: ")
    (print (GetPlayerCapturePoints computer))
    (terpri)
    (princ "Next player: ")
    (cond
        ((string= nextPlayer "H")( print "Human"))
        (t (print "Computer"))
    )
    (terpri)

)
(defun MakeMove(board player)
  (cond
    ((and (= (GetPlayerTotalMove player) 0) (string= (GetPlayerColour player) "W"))
     (print "Since this is the first move of \"White\" placing the piece in the center")
     (terpri)
     (cons (InsertPiece board 9 9 (GetPlayerColour player))
           (IncreaseMove player)
        ))
    ((and (= (GetPlayerTotalMove player) 1) (string= (GetPlayerColour player) "W"))
     (let ((position (AskForSecondPosition board)))
       (cons (InsertPiece board (first position) (rest position) (GetPlayerColour player))
             (IncreaseMove player))))
    (t
     (let ((position (AskForPosition board)))
       (cons (InsertPiece board (first position) (rest position) (GetPlayerColour player))
             (IncreaseMove player))))))
