(load "/Users/aayushshrestha/Desktop/LispProject/board.lisp")
(load "/Users/aayushshrestha/Desktop/LispProject/Player.lisp")
(load "/Users/aayushshrestha/Desktop/LispProject/Strategy.lisp")
; human and player will be list that has following element (colour totalPoints Points Capture and TotalMoves)
; return currentBoard human computer nextPlayer and ifWon
(defun PlayRound (human computer board nextPlayer)
  (PrintBoardStatus board human computer nextPlayer)
  (print "Human")
  (print human)
  (print "Computer")
  (print computer)
  (cond
    ((string= nextPlayer "H")
      (let ((BoardPlayerIfWinList (MakeMove board human (GetPlayerRoundPoints computer) nextPlayer)))
        (cond 
            ((= (nth 2  BoardPlayerIfWinList) 1) (AnnounceWinner "H")(list board human computer nextPlayer 1))
            ((DoesUserWantToContinue)
                (PlayRound (first (rest BoardPlayerIfWinList)) computer (first BoardPlayerIfWinList) "C")
            )
            (t (list board human computer nextPlayer 0))
        )
      ))
    (t 
      (let ((BoardPlayerIfWinList (MakeMove board computer (GetPlayerRoundPoints human) nextPlayer)))
        (cond 
            ((= (nth 2 BoardPlayerIfWinList) 1) (AnnounceWinner "C")(list board human computer nextPlayer 1))
            ((DoesUserWantToContinue)
                (PlayRound human (first (rest BoardPlayerIfWinList)) (first BoardPlayerIfWinList) "H")
            )
            (t (list board human computer nextPlayer 0))
        )
      ))
  )
)
(defun AnnounceWinner(playerType)
    (cond
        ((string= playerType "H")(print "Congratulation! You have won this round"))
        (t (print "Oh no! You have lost this round"))
    )
)
(defun PrintUserPosition (row column)
  (let ((alpha-col (code-char (+ column (char-code #\A))))
        (user-row (- 19 row)))
    (princ alpha-col)
    (princ user-row)
  )
)

(defun UserWantToMovePosition (generatedPosition)
  (princ "Do you want to move to this position: ")
  (PrintUserPosition (nth 0 generatedPosition) (nth 1 generatedPosition))
  (print "Please enter \"N\" for 'No' and \"Y\" for 'Yes': ")
  (terpri)
  (let ((input (string-upcase (read))))
    (cond
      ((string= input "Y") 1)
      ((string= input "N") 0)
      (t (UserWantToMovePosition generatedPosition))))
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
(defun AskForPosition (board player totalOpponentScore)
  (princ "Please enter the position on the board where you want to move your piece (e.g., 'A1')? Or type help for help")
  (terpri)
  (let ((input (string-upcase (read))))
    (cond
      ((string= input "HELP")
       (cond
            ((and (= (GetPlayerTotalMove player) 1) (string= (GetPlayerColour player) "W"))
                (let ((generatedPosition (GenerateValidSecondPosition board))
                    )
                (cond
                  ((= (UserWantToMovePosition generatedPosition) 1) generatedPosition)
                  (t (AskForPosition board player totalOpponentScore)))
                ) 
            
            )
            (t 
              (let ((generatedPosition (GetBestPosition board (GetPlayerRoundPoints player) (GetPlayerColour player) totalOpponentScore))
                    )
                (cond
                  ((= (UserWantToMovePosition generatedPosition) 1) generatedPosition)
                  (t (AskForPosition board player totalOpponentScore)))
                ) 
            )
        )
       )
      (t (cond
         ((< (length input) 2)
          (print "The position is too short. Please enter both column and row.")
          (AskForPosition board player totalOpponentScore)
         )
         ((> (length input) 3)
          (print "The position is too long. Please enter both column and row without spaces.")
          (AskForPosition board player totalOpponentScore)
          )
         ((alpha-char-p (char (string input) 0))
          (print "The column position is not valid; it must be an alphabet character.")
          (AskForPosition board player totalOpponentScore)
          )
         (t
            (let ((position (ChangeToRowColumn input)))
              (cond
                ((not position)
                  (print "Invalid row or column.")
                  (AskForPosition board player totalOpponentScore)
                )
                ((not (CheckIfPlaceEmpty board (first position) (rest position)))
                (print "Piece already exists at this position.")
                (AskForPosition board player totalOpponentScore))
                (t (AskForPosition board player totalOpponentScore)))
            )
          )
        )
      )
    )
  )

)




(defun AskForSecondPosition (board player totalOpponentScore)
  (print "Since this is the second position, the piece must be placed 3 intersections away from the center!!")
  (terpri)
  (let ((position (AskForPosition board player totalOpponentScore)))
       (cond
         ((not (IsSecondPositionValid board (nth 0 position) (nth 1 position))) (AskForSecondPosition board player totalOpponentScore))
         (t position))
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
        ((string= nextPlayer "H")( princ "Human ")(terpri))
        (t (princ "Computer")(terpri))
    )

)
; return board player and if win
(defun MakeMove(board player totalOpponentScore playerType)
  (cond
      ((string= playerType "H")
        (print "Human's Turn")
        (cond
        ((and (= (GetPlayerTotalMove player) 0) (string= (GetPlayerColour player) "W"))
        (print "Since this is the first move of \"White\" placing the piece in the center")
        (terpri)
        (list (InsertPiece board 9 9 (GetPlayerColour player)) (IncreaseMove player) 0)
        )
        ((and (= (GetPlayerTotalMove player) 1) (string= (GetPlayerColour player) "W"))
        (let ((position (AskForSecondPosition board player totalOpponentScore)))

          (GetNewBoardAndPointsAndCheckIfWin (nth 0 position) (nth 1 position) player board)
        ))
        (t
        (let ((position (AskForPosition board player totalOpponentScore)))
          (GetNewBoardAndPointsAndCheckIfWin (nth 0 position) (nth 1 position) player board)
                
        ))))
      (t 
        (print "Computer's Turn")
        (let ((position (GetBestPosition board (GetPlayerRoundPoints player) (GetPlayerColour player) totalOpponentScore)))
                (princ "Computer is moving its piece to: ")
                (PrintUserPosition (nth 0 position) (nth 1 position))
                (terpri)
               (GetNewBoardAndPointsAndCheckIfWin (nth 0 position) (nth 1 position) player board)

        )
      )
    )
    
)
;returns board updatedPlayer and ifWon
(defun GetNewBoardAndPointsAndCheckIfWin(row column player board)
  (let* ((newBoard (InsertPiece board row column (GetPlayerColour player)))
         (totalPointsScored(CountPoints row column (GetPlayerColour player) newBoard))
         (newPlayer (IncreaseRoundPoint player totalPointsScored))
         (boardCapturePoint (CalculateTotalCapturePoint row column (GetPlayerColour newPlayer) newBoard 0 0))
         (newPlayer1(IncreaseCapturePoint newPlayer (rest boardCapturePoint)))
        )
    (list (first boardCapturePoint) (IncreaseMove newPlayer1) (CheckIfWin newPlayer1 totalPointsScored))
    ))

(defun CheckIfWin(player recentScore)
    (cond
       ( (or(>= recentScore 5)(>= (GetPlayerCapturePoints player) 5)) 1)
       (t 0)
    )
)