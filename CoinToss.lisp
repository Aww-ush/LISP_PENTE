(defun AskUserForHeadOrTail ()
  (princ "Please choose head or tail? Please enter \"H\" for head and \"T\" for tail: ")
  (terpri)
  (let ((input (string-upcase (read))))
    (cond
      ((string= (string input) "H") "H")
      ((string= (string input) "T") "T")
      (t (AskUserForHeadOrTail)))))

(defun CoinToss ()
  (print "Coin Toss is happening")
  (terpri)
  (let ((userChoice (AskUserForHeadOrTail))
        (winner (Winner)))
    (cond
      ((string= userChoice winner) "H")
      (t "C")))
      )

(defun Winner ()
  (let ((randomNumber (random 1)))
      (cond
        ((= randomNumber 0) "H")
        ((= randomNumber 1) "T")
        (t (princ "Something went wrong while generating the winner") nil))
  )
  )

