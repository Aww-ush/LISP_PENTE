(load "/Users/aayushshrestha/Desktop/LispProject/CoinToss.lisp")
; start the tournament
;print the winner of round
; print the tournament winner
; ask if the player want to play another round

(defun Tournament (board human computer isLoaded nextPlayer)
  (cond 
    ((= isLoaded 1)
      (print "Continuing Game!")
      (let ((roundResult (PlayRound human computer board nextPlayer)))
        (AfterRoundEnd roundResult)))
    (t
      (print "Starting a new round")
      (let ((roundResult (PlayRoundWithNewBoard human computer)))
        (AfterRoundEnd roundResult))))
  )


(defun AfterRoundEnd(roundResult)
    (let* ((newHumanComputer(SaveTotalScore (nth 1 roundResult) (nth 2 roundResult)))
          )
        (cond
            ((DoesUserWantToPlayRound)
                (Tournament (GetEmptyBoard) (ResetRoundPoint (ResetCapturePoint (first newHumanComputer))) (ResetRoundPoint (ResetCapturePoint (rest newHumanComputer))) 0 "")
            )
            (t

                (AskToSaveGame)
            )
        )
    )
)
(defun SaveTotalScore(human computer)
    (list (IncreaseTotalPoint human (+ (GetPlayerRoundPoints human) (GetPlayerCapturePoints human))) (IncreaseTotalPoint computer (+ (GetPlayerRoundPoints computer) (GetPlayerCapturePoints computer))))
)
(defun PlayRoundWithNewBoard(human computer)
(cond 
        ((= (GetPlayerPoints human) (GetPlayerPoints computer))
            (let ((result (CoinToss)))
                    (cond
                    ((string= result "H") (princ "You will be white and you will be going first!")(PlayRound (SetPlayerColour human "W") (SetPlayerColour computer "B") (GetEmptyBoard) result))
                    ((string= result "C") (princ "You will be black and you will be going second!")(PlayRound (SetPlayerColour human "B") (SetPlayerColour computer "W") (GetEmptyBoard) result))
                    (t (princ "Invalid result from CoinToss!")))
        
            )
        )
        ((> (GetPlayerPoints human) (GetPlayerPoints computer))
            (princ "You will be going first!")
            (PlayRound (SetPlayerColour human "W") (SetPlayerColour computer "B") (GetEmptyBoard) "H")
        )
       ((> (GetPlayerPoints computer) (GetPlayerPoints human))
            (princ "You will be going second!")
            (PlayRound (SetPlayerColour human "B") (SetPlayerColour computer "W") (GetEmptyBoard) "C")
        )
        (t (print "Something went wrong while deciding first player") nil)
    )
)
;; /* *********************************************************************
;; Function Name: DoesUserWantToPlayRound
;; Purpose: To prompt the user to play another round and get their input
;; Parameters: None
;; Return Value: True if the user wants to play another round, False if not
;; Algorithm:
;;    1) Display a prompt asking if the user wants to play another round.
;;    2) Read the user's input and convert it to uppercase.
;;    3) Check the input:
;;       - If it's "Y", return True, indicating the user wants to play another round.
;;       - If it's "N", return False, indicating the user doesn't want to play another round.
;;       - If it's anything else, repeat the process until a valid input is received.
;; Assistance Received: none
;; ********************************************************************* */
(defun DoesUserWantToPlayRound ()
  (princ "Do you want to start a fresh new round? Please enter \"N\" for 'No' and \"Y\" for 'Yes': ")
  (terpri)
  (let ((input (string-upcase (read))))
    (cond
      ((string= input "Y") t)
      ((string= input "N") nil)
      (t (DoesUserWantToPlayRound)))))




; add more to this function to save the game
(defun AskToSaveGame()
  (princ "Do you want to save the game? Please enter \"N\" for 'No' and \"Y\" for 'Yes': ")
  (terpri)
  (let ((input (string-upcase (read))))
    (cond
      ((string= input "Y") (WriteToFile '("this is shit")))
      ((string= input "N") (print "Thank you for playing!"))
      (t (AskToSaveGame))))
)