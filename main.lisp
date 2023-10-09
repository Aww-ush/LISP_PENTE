(load "/Users/aayushshrestha/Desktop/LispProject/board.lisp")
(load "/Users/aayushshrestha/Desktop/LispProject/CoinToss.lisp")
(load "/Users/aayushshrestha/Desktop/LispProject/Round.lisp")
; human and computer both store data in this way
; (colour totalPoints Points CapturePoints TotalMoves) => ("W" 0 0 0 0 0)
(defun doesUserWantToLoadGame ()
  (princ "Do you want to load the game? Please enter \"N\" for 'No' and \"Y\" for 'Yes': ")
  (terpri)
  (let ((input (string-upcase (read))))
    (cond
      ((string= input "Y") (StartFromLoadedGame))
      ((string= input "N") (StartNewGame))
      (t (doesUserWantToLoadGame)))))
(defun StartFromLoadedGame()
  (print "Please enter a valid file path")
)
(defun startNewGame ()
  (princ "Starting a new game ")
  (terpri)
  (let ((result (CoinToss)))
    (cond
      ((string= result "H") (princ "You will be going first!"))
      ((string= result "C") (princ "You will be going second!"))
      (t (princ "Invalid result from CoinToss!")))
    (let ((board (GetEmptyBoard)))
      (PlayRound '("W" 0 0 0 0 0) '("B" 0 0 0 0 0) board result))))




(doesUserWantToLoadGame)



    ;;   (let ((newBoard(InsertPiece board 0 0 "W")))
    ;;     (PrintBoardWithIndex newBoard)
    ;;     (let ((human' ("W" 0 0 0 0)))
    ;;       (let ((newBoardPlayer(MakeMove newBoard human)))
    ;;         (PrintBoardWithIndex (first newBoardPlayer))
    ;;         (terpri)
    ;;         (print (rest newBoardPlayer))
    ;;     )
    ;; )
    ;; )
    ;;    (let((board(GetEmptyBoard)))
    ;;  (PlayRound '("W" 0 0 0 0 0) '("B" 0 0 0 0 0) board result))