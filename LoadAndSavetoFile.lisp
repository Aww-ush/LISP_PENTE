(load "/Users/aayushshrestha/Desktop/LispProject/board.lisp")
(load "/Users/aayushshrestha/Desktop/LispProject/Strategy.lisp")
(defun ReadFromFile()
    (let* ((path (AskUserForPath))
            (inputFile (open path))
            (result(GetBoardAndScores (read inputFile) 0 '() 0 0 0 0 "" ""))
        )
        (close inputFile)
         result
    )
)
; return board humanTotalScore humanCaptureScore computerTotalScore computerCaptureScore nextplayerChar nextPLayerColour
(defun GetBoardAndScores(inputFile counter board humanTotalScore humanCaptureScore computerTotalScore computerCaptureScore nextPlayerColour nextPlayerChar)
      (cond
          ((= counter 0)(GetBoardAndScores inputFile (+ counter 1) (nth counter inputFile) humanTotalScore humanCaptureScore computerTotalScore computerCaptureScore nextPlayerColour nextPlayerChar))
          ((= counter 1)(GetBoardAndScores inputFile (+ counter 1) board humanTotalScore (nth counter inputFile)  computerTotalScore computerCaptureScore nextPlayerColour nextPlayerChar))
          ((= counter 2)(GetBoardAndScores inputFile (+ counter 1) board (nth counter  inputFile) humanCaptureScore computerTotalScore computerCaptureScore nextPlayerColour nextPlayerChar))
          ((= counter 3)(GetBoardAndScores inputFile (+ counter 1) board humanTotalScore humanCaptureScore computerTotalScore (nth counter inputFile)  nextPlayerColour nextPlayerChar))
          ((= counter 4)(GetBoardAndScores inputFile (+ counter 1) board humanTotalScore humanCaptureScore (nth counter inputFile) computerCaptureScore nextPlayerColour nextPlayerChar))
          ((= counter 5)(GetBoardAndScores inputFile (+ counter 1) board humanTotalScore humanCaptureScore computerTotalScore computerCaptureScore (nth counter inputFile) nextPlayerChar))
          ((= counter 6)(GetBoardAndScores inputFile (+ counter 1) board humanTotalScore humanCaptureScore computerTotalScore computerCaptureScore nextPlayerColour (nth counter inputFile)))
          (t 
            (let ((nexPlayerDetails(GetNextPlayer nextPlayerChar nextPlayerColour)))
                (list board humanTotalScore humanCaptureScore computerTotalScore computerCaptureScore (nth 0 nexPlayerDetails) (nth 1 nexPlayerDetails))
            )
          )
      ) 
)
(defun WriteToFile(fileData)
  (let* ((path "Game.txt")
         (output-file (open path :direction :output))
         (text fileData)) ; file data contains the list to save
    (format output-file "~a" text) ; Write the text to the file
    (close output-file) 
    (format t "Successfully written to ~a.~%" path) 
    )
  )
(defun GetNextPlayer(nextPlayer nextPlayerColour)
  (cond
    ((string= nextPlayer "Human")
      (cond 
        ((string= nextPlayerColour "White") '( "H" "W"))
        (t '("H" "B"))
      )
    )
    (t
      (cond 
        ((string= nextPlayerColour "White") '( "C" "W"))
        (t '( "C" "B"))
      )
    )
  )
)


(defun AskUserForPath ()
  (print "Please enter the path for the file: ")
  (terpri)
  (let ((input (read-line)))
    (cond
      ((probe-file input) input)
      (t
       (print "Please enter valid file")
       (AskUserForPath))))
) 