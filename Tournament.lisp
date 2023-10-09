(load "/Users/aayushshrestha/Desktop/LispProject/CoinToss.lisp")
; start the tournament
;print the winner of round
; print the tournament winner
; ask if the player want to play another round

(defun Torunament(board human computer)
    (cond 
        ((= (GetPlayerPoints human) (GetPlayerPoints computer))
            (let ((result (CoinToss)))
                    (cond
                    ((string= result "H") (princ "You will be going first!"))
                    ((string= result "C") (princ "You will be going second!"))
                    (t (princ "Invalid result from CoinToss!")))
                (let ((board (GetEmptyBoard)))
                (PlayRound human computer board result))
            )
        )
        ((> (GetPlayerPoints human) (GetPlayerPoints computer))
            (princ "You will be going first!")
            (let ((board (GetEmptyBoard)))
                (PlayRound human computer board "H"))
        )
       ((> (GetPlayerPoints computer) (GetPlayerPoints human))
            (princ "You will be going second!")
            (let ((board (GetEmptyBoard)))
                (PlayRound human computer board "C"))
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
  (princ "Do you want to play another round? Please enter \"N\" for 'No' and \"Y\" for 'Yes': ")
  (terpri)
  (let ((input (string-upcase (read))))
    (cond
      ((string= input "Y") t)
      ((string= input "N") nil)
      (t (DoesUserWantToPlayRound)))))