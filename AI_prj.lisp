;;*******************************************************************
;;*                                                                 *  
;;*                         AI Poker Project                        *
;;*  Swapnil Shinde                                                 *
;;*                                                                 *
;;*******************************************************************


(defun PokerMaster (NO_OF_PLAYERS NO_OF_CHIPS NO_OF_GAMES)
  (let ((pot 0)
        (Status-P nil)
        (Chips-P nil)
        (Dealer-Temp nil)
        (Com-Temp nil)
        (temp 0)
       )
    ;;Distribute equal chips to all players
    (dotimes (i NO_OF_PLAYERS)
      (setf Chips-P (append Chips-P (list NO_OF_CHIPS)))
    )
    (dotimes (i NO_OF_PLAYERS)
      (setf Status-P (append Status-P (list nil)))
    )
    ;;Play the number of games specified in the input
    (dotimes (Games-Temp NO_OF_GAMES)
  ;;    (format t "************************************************************************** ~%")
      (format t "Game ~a ~%" (+ Games-Temp 1))
      (dotimes (i NO_OF_PLAYERS)
        (setf (nth i Status-P) nil)
      )
      ;;Call the dealer function to retrieve the hole cards and communal cards
      (setf Dealer-Temp (Dealer NO_OF_PLAYERS))
      ;;4 rounds in each game
      (dotimes (tempRound 4)
        (progn
           (format t "-------------------------------------- ~%")
          (format t "Round ~a:~%" (+ tempRound 1))
          ;;Maintain communal cards. Release one communal card each time
          (cond ((eq tempRound 1)(setf Com-Temp (butlast (second Dealer-Temp) 2)))
                ((eq tempRound 2)(setf Com-Temp (append Com-Temp (list (nth 3 (second Dealer-Temp))))))
                ((eq tempRound 3)(setf Com-Temp (append Com-Temp (list (nth 4 (second Dealer-Temp))))))
                (t (setf Com-Temp nil))
          )
          (format t "Communal Cards: ~a~%" Com-Temp)
          ;;In first round, no communal cards. Poker agents are called, with hole cards
          (cond ((= tempRound 0)
                 (dotimes (i NO_OF_PLAYERS)
                   (setf (nth i Status-P) 
                         (playerFunc i (rest (nth i (first Dealer-Temp))) Com-Temp (nth i Chips-P)))
                 )
                )
                ;;In subsequent rounds, player chips and pot are maintained.
                ;;Poker Agent is called based on the hole cards and communal cards
                (t (progn
                     (dotimes (i NO_OF_PLAYERS)
                       (if (And (eq (second (nth i Status-P)) 1)
                                (not (eq (count 1 Status-P :key #'second) 1)))
                           (progn
                             (setf (nth i Chips-P) (- (nth i Chips-P) 1))
                             (incf pot)
                           )
                       )
                     )
                     (format t "Pot : ~a ~%" pot)    
                     (dotimes (i NO_OF_PLAYERS)
                       (if (And (eq (second (nth i Status-P)) 1)
                                (not (eq (count 1 Status-P :key #'second) 1)))
                           (setf (nth i Status-P)
                                 (playerFunc i (rest (nth i (first Dealer-Temp))) Com-Temp (nth i Chips-P)))
                       )
                     )
                   )
                )
          )
        )
      )
      ;;Finalize the pot in after round 4
      (dotimes (i NO_OF_PLAYERS)
        (if (eq (second (nth i Status-P)) 1)
            (progn
              (setf (nth i Chips-P)(- (nth i Chips-P) 1))
              (incf pot)
            )
        )
      )
      ;;If there is only one player remaining, then the chips in pot goes to the player.
      (cond ((eq (Count 1 Status-P :key #'second) 1)
             (dotimes (i NO_OF_PLAYERS)
               (if (eq (second (nth i Status-P)) 1) 
                   (progn
                     (setf temp (+ (nth i Chips-P) pot))
                     (setf (nth i Chips-P) temp)
                     (setf pot 0)
                     (format t "~%Game ~a Result:~%" (+ Games-Temp 1))
                     (format t "Player ~a has won ~% - Chip Count: ~a~%" (+ i 1) (nth i Chips-P))
                     (return)
                   )
               )
             )
            )
            ((eq (Count 1 Status-P :key #'second) 0) (setf pot 0)
            )
            
            (t
             (let ((tempRank 0)
                   (maxRank 9999)
                   (maxPerson 0)
                  )
               (dotimes (i NO_OF_PLAYERS)
                 (if (eq (second (nth i Status-P)) 1)
                     (progn
                       (setf tempRank (HandRank7 (rest (nth i (first Dealer-Temp))) Com-Temp))
                       (if (< tempRank maxRank)
                           (progn
                             (setf maxRank tempRank)
                             (setf maxPerson i)
                           )
                       )
                     )
                 )
               )
               (setf temp (+ (nth maxperson Chips-P) pot))
               (setf (nth maxPerson Chips-P) temp)
               (setf pot 0)
               (format t "~%Game ~a FUNC-BETTING:~%" (+ Games-Temp 1))
               (format t "Player ~a has won ~% - Chip Count:~a~%" (+ maxPerson 1)(nth maxPerson Chips-P))
             )
           )             
      )        
      (format t "Game ~a ended~%~%" (+ Games-Temp 1))
(format t "************************************************************************** ~%")
;;      (format t "Chips ~a" (nth 1 Chips-P))
    )
  )
)





;;Input: Player Number, Hole Cards of the player, Communal Cards, Current Chips count of player
;;Output: list of Player Number and the FUNC-BETTING of the player in that particular round


(defun playerFunc (playerNum holeCards commCards currChips)
  (let ((tempProb 0)
        (tempDec 0)
       )
    ;;If chip count is 3, player cannot play further. Hence, player folds.
    (cond ((or (< currChips 3) (= currChips 3)) (setf tempProb (PROB-CALC 1 1)))
          ;;Hole cards are distributed to the players
          ((null commCards) (setf tempProb (HandRank2 holeCards)))
          ;;3 Communal Cards released by dealer
          ((eq (Length commCards) 3)
           (setf tempProb (PROB-CALC (HandRank5 holeCards commCards) 1081))
          )
          ;;4th Communal Card is released by dealer
          ((eq (Length commCards) 4)
           (setf tempProb (PROB-CALC (HandRank6 holeCards commCards) 1035))
          )
          ;;5th communal card is released by dealer
          ((eq (Length commCards) 5)
           (setf tempProb (PROB-CALC (HandRank7 holeCards commCards) 990))
          )
          (t (setf tempProb (PROB-CALC 1 1)))
    )
    ;;Based on probability, Make the FUNC-BETTING of the player
    (setf tempDec (FUNC-BETTING tempProb))
    (if (eq tempDec 0) 
        (format t "Player ~a fold ~% - Hole Cards: ~a ~% - Chip Count: ~a ~% " 
                (+ playerNum 1) holeCards currChips)
      (format t "Player ~a Continue ~% - Hole Cards: ~a ~% - Chip Count: ~a ~%" 
              (+ playerNum 1) holeCards currChips)
    )
    (list playerNum tempDec)
  )
)




;;Function: Func-Dist
;;Input: Takes an element and a list
;;Output: A list comrising left distributed elements
;;Example: Func-Dist 1 '(1 2 3 4) gives ((1 1)(1 2)(1 3)(1 4))
;;Approach: Recurse through each element of the rightLst making a product.

(defun Func-Dist (left rightLst)
  (cond
   ((null rightLst) nil)
   (t (cons (list left (car rightLst))(Func-Dist left (cdr rightLst))))
  )
)


;;Function: CartesianProduct
;;Input: Two lists for which the Cartesian Product need to be Prepared
;;Output: Cartesian Product of two lists


(defun Multiply-Func (leftLst rightLst)
  (cond
   ((null leftLst) nil)
   (t (append (Func-Dist (car leftLst) rightLst)
            (Multiply-Func (cdr leftLst) rightLst)))
  )
)



;;Function: Random-shuf
;;Input: A list from which element need to be picked randomly
;;Output: Randomly picked element
;;Approach: Create a temporary variable index which uses random function to 
;;generate a random number. Using this index, the element is picked from the 
;;list and returned as output

(defun Random-shuf (lst)
  (let ((index (random (length lst))))
    (if (eql (length lst) 1) (nth 0 lst)
      (nth index lst)
    )
  )
)



;;Function: ShuffledDeck
;;Input: nil
;;Output: List of Random Shuffled deck.
;;Approach: The function considers cards and suits, and calls the cartesian
;;product function to have the ordered cards. Random pick function is called
;;iteratively to pick a card in random and is added to the shuffled cards list.
;;The function ShuffledDeck returns the shuffled cards.

(defun Deck-shuffle ()
  (let ((Cards '(A 2 3 4 5 6 7 8 9 10 J Q K))
        (Suits '(S H D C))
        (OrderedCards ())
        (ShuffledCards ())
        (temp ())
       )
    (setq OrderedCards (Multiply-Func Cards Suits))
    (loop
     (when (null OrderedCards) (return))
     (setq temp (Random-shuf OrderedCards))
     (setq ShuffledCards (append ShuffledCards (list temp)))
     (setq OrderedCards (remove temp OrderedCards :test 'equal))
    )
    (return-from Deck-shuffle ShuffledCards)
  )
)



;;Approach - (1) Call the function Deck-shuffle to get the shuffled sequence 
;;of cards. (2) Taking number of players as input, iteratively distribute the
;;hole cards, communcal cards and remaining cards are put on to the discard 
;;Pile. 

;;Input: number of players
;;Output: A list of Hole cards, communal cards and discard pile
;;Approach: Hole cards use the following data structure.
;;(<player num> <Hole card 1> <Hole card 2>)
;;Eg: (2 (K Spades) (J Clubs))
;;Communal Cards picks 5 cards from the shuffled pile.
;;After picking the Hole cards and communal cards, the remaining cards form
;;the discard pil

(defun Dealer (num)
  (let ((Deck (Deck-shuffle))
        (HoleCards ())
        (CommunalCards ())
        (DiscardPile ())
       )
    (dotimes (i num HoleCards)
      (setq HoleCards (append HoleCards (list 
                                         (list (+ i 1) (pop Deck)(pop Deck)))))
    )
    (setq DiscardPile (append DiscardPile (list (pop Deck))))
    (dotimes (i 3 CommunalCards)
      (setq CommunalCards (append CommunalCards (list (pop Deck))))
    )
    (setq DiscardPile (append DiscardPile (list (pop Deck))))
    (setq CommunalCards (append CommunalCards (list (pop Deck))))
    (setq DiscardPile (append DiscardPile (list (pop Deck))))
    (setq CommunalCards (append CommunalCards (list (pop Deck))))
    (setq DiscardPile (append DiscardPile Deck))
    (return-from Dealer (list HoleCards CommunalCards DiscardPile))
  )
)



(defun SortHand (hand)
  (sort hand #'(lambda (x y) (< (first x) (first y))))
)





(defun NumericalHand (hand)
  (let ((newHand nil)
        (cardVals nil)
        (straight nil))
    (setf newHand 
          (mapcar #'(lambda (x)
                      (cond ((eq (first x) 'A) (list 1 (second x)))
                            ((eq (first x) 'J) (list 11 (second x)))
                            ((eq (first x) 'Q) (list 12 (second x)))
                            ((eq (first x) 'K) (list 13 (second x)))
                            (t x)
                      )
                    )
                  hand
          )
    )
    (setf cardVals (sort (mapcar #'(lambda (x) (first x)) newHand) #'<))
    (setf straight (and (eq (first cardVals) 1) (eq (second cardVals) 2) 
                        (eq (third cardVals) 3) (eq (fourth cardVals) 4)
                        (eq (fifth cardVals) 5)))
    (if straight (SortHand newHand)
      (SortHand (mapcar #'(lambda (x) (if (eq (first x) 1) 
                                          (list 14 (second x)) x)) 
                        newHand
                )
      )
    )
  )
)



;;Input - Sorted Hand
;;Output - (Straight HighCard) if true else nil


(defun Straight (hand)
  (let ((temp nil)
        (result nil)
        (high nil))
    (setf temp (first (first hand)))
    (if (and (eq (car (second hand)) (+ temp 1))
             (eq (car (third hand)) (+ temp 2))
             (eq (car (fourth hand)) (+ temp 3))
             (eq (car (fifth hand)) (+ temp 4)))
        (progn
          (setf high (car (fifth hand)))
          (setf result (list 'Straight high))
        )
      nil
    )
    result
  )
)


;;Input - Sorted Hand
;;Output - (Flush HighCard Suit) if true else nil


(defun Flush (hand)
  (let ((temp nil)
        (isFlush nil))
    (setf temp (second (first hand)))
    (setf isFlush (and (eq (second (second hand)) temp)
                       (eq (second (third hand)) temp)
                       (eq (second (fourth hand)) temp)
                       (eq (second (fifth hand)) temp))
    )
    (if isFlush 
        (list 'Flush (first (fifth hand)) (second (fifth hand)))
      nil
    )
  )
)



;;Input - Sorted Hand
;;Output - (StraightFlush HighCard Suit) if true else nil


(defun StraightFlush (hand)
  (let ((temp nil)
        (flushHand nil)
        (straightHand nil))
    (setf temp (second (first hand)))
    (setf flushHand (Flush hand))
    (setf straightHand (Straight hand))
    (if (and (consp flushHand) (consp straightHand))
        (list 'StraightFlush (second straightHand) temp)
      nil
    )
  )
)



;;Input - Sorted Hand
;;Output - (RoyalFlush Suit) if true else nil

(defun RoyalFlush (hand)
  (let ((isRoyal nil)
        (straightFlush nil))
    (setf straightFlush (StraightFlush hand))
    (setf isRoyal (eq (car (first hand)) 10))
    (if (and (consp straightFlush) isRoyal)
        (list 'RoyalFlush (third straightFlush))
      nil
    )
  )
)



;;Input - Sorted Hand
;;Output - (FourOfAKind CardVal) if true else nil


(defun  FourofKind(hand)
  (let ((temp ()))
    (dotimes (i 4)
      (if (eq (first (nth i hand)) (first (nth (+ i 1) hand)))
          (setf temp (append temp (list (first (nth i hand)))))
      )
    )
    (setf temp (delete-duplicates temp :test-not #'eq))
    (if (eq (length temp) 3)
        (list 'FourOfAKind (first temp))
      nil
    )
  )
)



;;Input - Sorted Hand
;;Output - (ThreeOfAKind CardVal) if true else nil


(defun ThreeofKind (hand)
  (let ((temp nil)
        (count 1))
    (setf temp (first (first hand)))
    (dotimes (i 4)
      (if (not (eq count 3))
          (if (eq temp (first (nth (+ i 1) hand)))
              (incf count)
            (progn 
              (setf temp (first (nth (+ i 1) hand)))
              (setf count 1)
            )
          )
        nil
      )
    )
    (if (eq count 3) 
        (list 'ThreeOfAKind temp)
      nil
    )
  )
)



;;Input - Sorted Hand
;;Output - (Pair CardVal) if true else nil


(defun OnePair (hand)
  (let ((temp nil)
        (count 1))
    (setf temp (first (first hand)))
    (dotimes (i 4)
      (if (not (eq count 2))
          (if (eq temp (first (nth (+ i 1) hand)))
              (incf count)
            (progn 
              (setf temp (first (nth (+ i 1) hand)))
              (setf count 1)
            )
          )
        nil
      )
    )
    (if (eq count 2) 
        (list 'Pair temp)
      nil
    )
  )
)



;;Input - Sorted Hand
;;Output - (TwoPair HighCardVal LowCardVal) if true else nil


(defun TwoPairs (hand)
  (let ((temp ()))
    (dotimes (i 4)
      (if (eq (first (nth i hand)) (first (nth (+ i 1) hand)))
          (setf temp (append temp (list (first (nth i hand)))))
      )
    )
    (setf temp (Delete-Duplicates temp))
    (if (eq (length temp) 2)
        (list 'TwoPair (second temp) (first temp))
      nil
    )
  )
)



;;Input - Sorted Hand
;;Output - (FullHouse HighCardVal LowCardVal) if true else nil


(defun FullHouse (hand)
  (let ((twoPair nil)
        (threeKind nil))
    (setf twoPair (TwoPairs hand))
    (setf threeKind (ThreeofKind hand))
    (if (and (consp twoPair) (consp threeKind))
        (if (eq (second twoPair) (second threeKind))
            (list 'FullHouse (second twoPair) (third twoPair))
          (list 'FullHouse (third twoPair) (second twoPair))
        )
      nil
    )
  )
)


;;Function: Evaluate Hand


;;Input - Sorted Hand
;;Output - Returns Hand strength of the Hand


(defun EvaluateHand (hand)
  (let ((temp ())
        (flag 0)
       )
    (setf temp (append temp (list (RoyalFlush hand))))
    (setf temp (append temp (list (StraightFlush hand))))
    (setf temp (append temp (list (FourofKind hand))))
    (setf temp (append temp (list (FullHouse hand))))
    (setf temp (append temp (list (Flush hand))))
    (setf temp (append temp (list (straight hand))))
    (setf temp (append temp (list (ThreeofKind hand))))
    (setf temp (append temp (list (TwoPairs hand))))
    (setf temp (append temp (list (onePair hand))))
    (setf temp (append temp (list (first (fifth hand)))))
    (dotimes (i (- (length temp) 1))
      (if (eq flag 1)
          (if (not (eq (nth i temp) nil))
              (setf (nth i temp) nil)
          )
        (if (not (eq (nth i temp) nil))
            (setf flag 1)
        )
      )
    )
    temp
  )
)


;;Function: Tie Break


;;Input - Card Values of same strength
;;Output - Returns the winner hand


(defun BreakTies (cvals1 cvals2)
  (cond ((or (null cvals1) (null cvals2)) 0)
        ((> (first cvals1) (first cvals2)) 1)
        ((< (first cvals1) (first cvals2)) -1)
	(t (BreakTies (rest cvals1) (rest cvals2)))
  )
)  


;;Function: All Diff


(defun All-Diff (cardvals)
  (and (< (first cardvals) (second cardvals)) (< (second cardvals) (third cardvals))
       (< (third cardvals) (fourth cardvals)) (< (fourth cardvals) (fifth cardvals))
  )
)


;;Compare Hands


;;Input - 2 Sorted Hand
;;Output - 1 if hand 1 is strong, 0 if both are equal, -1 of hand 2 is strong


(defun CompareHands (hand1 hand2)
  (let ((nhand1 (NumericalHand hand1))
	(nhand2 (NumericalHand hand2)) (hs1 nil) (hs2 nil) (cvals1 nil) (cvals2))
    (setf hs1 (EvaluateHand nhand1))
    (setf hs2 (EvaluateHand nhand2))
    (setf cvals1 (reverse (mapcar #'(lambda(x) (first x)) nhand1)))
    (setf cvals2 (reverse (mapcar #'(lambda(x) (first x)) nhand2)))

    (cond ((or (first hs1) (first hs2))
	   ;; one player has a RoyalFlush
	   (cond ((null (first hs1)) -1)  ;; hand2 is better
		 ((null (first hs2))  1)  ;; hand1 is better
		 (t 0)))
	  ;; one player has a StraightFlush
	  ((or (second hs1) (second hs2))
	   (cond ((null (second hs1)) -1)   ;; player2 has SF
		 ((null (second hs2))  1)   ;; player1 has SF
		 ((> (second (second hs1)) (second (second hs2))) 1)  ;; higher card
		 ((< (second (second hs1)) (second (second hs2))) -1) ;; higher card
		 (t 0)))  ;; hands equal
	   ;; one player has Four of a Kind
	  ((or (third hs1) (third hs2))
	   (cond ((null (third hs1)) -1)   ;; player2 has FofaK
		 ((null (third hs2))  1)   ;; player1 has FofaK
		 ((> (second (third hs1)) (second (third hs2))) 1)  ;; higher card
		 ((< (second (third hs1)) (second (third hs2))) -1) ;; higher card
		 (t (BreakTies cvals1 cvals2)))) ;; break ties by finding the highest card that differs 
	  ;; one player has Full House
	  ((or (fourth hs1) (fourth hs2))
	   (cond ((null (fourth hs1)) -1)   ;; player2 has FH
		 ((null (fourth hs2))  1)   ;; player1 has FH
		 ((> (second (fourth hs1)) (second (fourth hs2))) 1)  ;; higher card
		 ((< (second (fourth hs1)) (second (fourth hs2))) -1) ;; higher card
		 (t 0)))   ;; hands equal
	  ;; one player has Flush
	  ((or (fifth hs1) (fifth hs2))
	   (cond ((null (fifth hs1)) -1)   ;; player2 has F
		 ((null (fifth hs2))  1)   ;; player1 has F
		 (t (BreakTies cvals1 cvals2))))   ;; break ties by finding the highest card that differs
	  ;; one player has Straight
	  ((or (sixth hs1) (sixth hs2))
	   (cond ((null (sixth hs1)) -1)   ;; player2 has S
		 ((null (sixth hs2))  1)   ;; player1 has S
		 ((> (second (sixth hs1)) (second (sixth hs2))) 1)  ;; higher card
		 ((< (second (sixth hs1)) (second (sixth hs2))) -1) ;; higher card
		 (t 0)))   ;; hands equal
	  ;; one player has Three of a Kind
	  ((or (seventh hs1) (seventh hs2))
	   (cond ((null (seventh hs1)) -1)   ;; player2 has TofaK
		 ((null (seventh hs2))  1)   ;; player1 has TofaK
		 ((> (second (seventh hs1)) (second (seventh hs2))) 1)  ;; higher card
		 ((< (second (seventh hs1)) (second (seventh hs2))) -1) ;; higher card
		 (t (BreakTies cvals1 cvals2))))   ;; hands equal
	  ;; one player has TwoPair
	  ((or (eighth hs1) (eighth hs2))
	   (cond ((null (eighth hs1)) -1)   ;; player2 has TofaK
		 ((null (eighth hs2))  1)   ;; player1 has TofaK
		 ((> (second (eighth hs1)) (second (eighth hs2))) 1)  ;; higher hard card
		 ((< (second (eighth hs1)) (second (eighth hs2))) -1) ;; higher high card
		 ((> (third (eighth hs1)) (third (eighth hs2))) 1)  ;; higher low card
		 ((< (third (eighth hs1)) (third (eighth hs2))) -1) ;; higher low card
		 (t (BreakTies cvals1 cvals2))))  ;; hands equal
	  ;; one player has a Pair
	  ((or (ninth hs1) (ninth hs2))
	   (cond ((null (ninth hs1)) -1)   ;; player2 has Pair
		 ((null (ninth hs2))  1)   ;; player1 has Pair
		 ((> (second (ninth hs1)) (second (ninth hs2))) 1)  ;; higher card
		 ((< (second (ninth hs1)) (second (ninth hs2))) -1) ;; higher card
		 (t (BreakTies cvals1 cvals2))))  ;; hands equal

	  ;; the strongest card wins
	  (t (cond ((< (tenth hs1) (tenth hs2)) -1) ;; player 2 has the high card
		   ((> (tenth hs1) (tenth hs2)) 1)  ;; player 1 has a high card
		   (t (BreakTies (reverse cvals1) (reverse cvals2))))))))  ;; hands equal







;;Input: list of hole cards and list of communal cards
;;Output: Deck without the hole cards and communcal cards


(defun CardSet (holeCards commCards)
  (let ((sortedDeck ()))
    (setf sortedDeck (Multiply-Func '(A 2 3 4 5 6 7 8 9 10 J Q K)
                                       '(S H D C)))
    (dolist (ele holeCards)
      (setf sortedDeck (remove ele sortedDeck :test #'equal))
    )
    (dolist (ele commCards)
      (setf sortedDeck (remove ele sortedDeck :test #'equal))
    )
    sortedDeck
  )
)



;;Input: list of hole cards and communal cards
;;Output: Card pairs combinations 


(defun CardPairs (holeCards commCards)
  (let ((temp ())
        (cardPair ())
       )
    (setf temp (CardSet holeCards commCards))
    (let ((i 0))
      (loop
       (when (> i (Length temp)) (return))
       (let ((j 0))
         (setf j (+ i 1))
         (loop
          (when (> j (- (Length temp) 1)) (return))
          (setf cardPair
                (append cardPair (list (list (nth i temp) (nth j temp))))
          )
          (incf j)
         )
       )
       (incf i)
      )
    )
    cardPair
  )
)



;;Input: List of combinations of 5 cards
;;Output: Best card combination out of card combination


(defun HighRank (lst)
  (let ((hcSet nil)
        (hcVal 0)
        (temp 0)
       )
    (setf hcSet (first lst))
    (setf hcVal -1)
    (dolist (i lst)
      (setf temp (CompareHands i hcSet))
      (if (OR (> temp hcVal) (= temp hcVal))
          (progn 
            (setf hcSet i)
            (setf hcVal temp)
          )
      )
    )
    hcSet
  )
)



;;Input: list of 6 cards
;;Output: Best card combination out of 6 cards


(defun HandRank6-Best (hand)
  (let ((tempHand ()))
    (dotimes (i 6)
      (setf tempHand (append tempHand 
                              (list (remove (nth i hand) hand))))
    )
    (HighRank tempHand)
  )
)



;;Input: list of 7 cards
;;Output: Best card combination out of 7 cards


(defun HandRank7-Best (hand)
  (let ((temp ())
        (temp1 ())
        (tempHand ()))
    (let ((i 0))
      (loop
       (when (> i (Length hand)) (return))
       (setf temp (remove (nth i hand) hand))
       (let ((j 0))
         (setf j i)
         (loop
          (when (> j (- (Length hand) 2)) (return))
          (setf temp1 (remove (nth j temp) temp))
          (setf tempHand (append tempHand (list temp1)))
          (incf j)
          (setf temp1 nil)
         )
       )
       (setf temp nil)
       (incf i)
      )
    )
    (HighRank tempHand)
  )
)



;;Input: List of 2 hand pair and list of sorted1000
;;Output: Probability of losing


(defun HandRank2 (hand)
  (let ((pos nil)
        (sortedPair nil)
       )
    (setf sortedPair (Sorted1000))
    (setf pos (Position hand sortedPair :test #'equal))
    (if (null pos)
        (progn 
          (setf hand (reverse hand))
          (setf pos (Position hand sortedPair :test #'equal))
        )
    )
    (/ (- (Length sortedPair) pos) (* (Length sortedPair) 1.0))
  )
)



;;Input: list of hole cards and communcalcards
;;Output: Number of cards higher than the given hand


(defun HandRank5 (holeCards commCards)
  (let ((rank 0)
        (playerHand ())
        (tempHand ())
        (tempPairs ())
       )
    (setf playerHand (append holeCards commCards))
    (setf tempPairs (CardPairs holeCards commCards))
    (dolist (ele tempPairs)
      (setf tempHand (append ele commCards))
      (if (eq (CompareHands tempHand playerHand) 1)
          (incf rank)
      )
      (setf tempHand nil)
    )
    rank
  )
)



;;Input: list of hole cards and communcalcards
;;Output: Number of cards higher than the given hand


(defun HandRank6 (holeCards commCards)
  (let ((rank 0)
        (playerHand ())
        (tempHand ())
        (tempPairs ())
       )
    (setf tempPairs (CardPairs holeCards commCards))
    (setf playerHand (HandRank6-Best (append holeCards commCards)))
    (dolist (ele tempPairs)
      (setf tempHand (append ele commcards))
      (if (eq (CompareHands (HandRank6-Best tempHand) playerHand) 1)
          (incf rank)
      )
      (setf tempHand nil)
    )
    rank
  )
)



;;Input: list of hole cards and communcalcards
;;Output: Number of cards higher than the given hand

(defun HandRank7 (holeCards commCards)
  (let ((rank 0)
        (playerHand ())
        (tempHand ())
        (tempPairs ())
       )
    (setf tempPairs (CardPairs holeCards commCards))
    (setf playerHand (HandRank7-Best (append holeCards commCards)))
    (dolist (ele tempPairs)
      (setf tempHand (append ele commCards))
      (if (eq (CompareHands (HandRank7-Best tempHand) playerHand) 1)
          (incf rank)
      )
      (setf tempHand nil)
    )
    rank
  )
)



;;Input: Rank of the hand, total number of card pairs
;;Output: Winning probability of the player


(defun Prob-Calc (prop total)
  (/ (- total prop) (* total 1.0))
)



;;Input: Probability of winning of the player
;;Output: FUNC-BETTING; 1 to continue playing, 0 to fold


(defun Func-betting (prob)
  (cond ((or (< prob 0.2) (= prob 0.2)) (return-from Func-betting 0))
        ((and (> prob 0.2) (or (< prob 0.4) (= prob 0.4))) (return-from Func-betting (random 2)))
        (t (return-from Func-betting 1))
  )
)

(defun Sorted1000 ()
  (return-from Sorted1000 
'(((2 D) (3 D)) ((3 C) (2 D)) ((2 C) (3 D)) ((2 C) (3 C)) ((3 H) (2 D)) ((3 H) (2 C)) ((2 H) (3 D)) ((2 H) (3 C))
 ((2 H) (3 H)) ((3 S) (2 D)) ((3 S) (2 C)) ((3 S) (2 H)) ((2 S) (3 D)) ((2 S) (3 C)) ((2 S) (3 H)) ((2 S) (3 S))
 ((2 D) (4 D)) ((4 C) (2 D)) ((2 C) (4 D)) ((2 C) (4 C)) ((4 H) (2 D)) ((4 H) (2 C)) ((2 H) (4 D)) ((2 H) (4 C))
 ((2 H) (4 H)) ((4 S) (2 D)) ((4 S) (2 C)) ((4 S) (2 H)) ((2 S) (4 D)) ((2 S) (4 C)) ((2 S) (4 H)) ((2 S) (4 S))
 ((2 D) (5 D)) ((5 C) (2 D)) ((2 C) (5 D)) ((2 C) (5 C)) ((5 H) (2 D)) ((5 H) (2 C)) ((2 H) (5 D)) ((2 H) (5 C))
 ((2 H) (5 H)) ((5 S) (2 D)) ((5 S) (2 C)) ((5 S) (2 H)) ((2 S) (5 D)) ((2 S) (5 C)) ((2 S) (5 H)) ((2 S) (5 S))
 ((2 D) (6 D)) ((6 C) (2 D)) ((2 C) (6 D)) ((2 C) (6 C)) ((6 H) (2 D)) ((6 H) (2 C)) ((2 H) (6 D)) ((2 H) (6 C))
 ((2 H) (6 H)) ((6 S) (2 D)) ((6 S) (2 C)) ((6 S) (2 H)) ((2 S) (6 D)) ((2 S) (6 C)) ((2 S) (6 H)) ((2 S) (6 S))
 ((2 D) (7 D)) ((7 C) (2 D)) ((2 C) (7 D)) ((2 C) (7 C)) ((7 H) (2 D)) ((7 H) (2 C)) ((2 H) (7 D)) ((2 H) (7 C))
 ((2 H) (7 H)) ((7 S) (2 D)) ((7 S) (2 C)) ((7 S) (2 H)) ((2 S) (7 D)) ((2 S) (7 C)) ((2 S) (7 H)) ((2 S) (7 S))
 ((2 D) (8 D)) ((8 C) (2 D)) ((2 C) (8 D)) ((2 C) (8 C)) ((8 H) (2 D)) ((8 H) (2 C)) ((2 H) (8 D)) ((2 H) (8 C))
 ((2 H) (8 H)) ((8 S) (2 D)) ((8 S) (2 C)) ((8 S) (2 H)) ((2 S) (8 D)) ((2 S) (8 C)) ((2 S) (8 H)) ((2 S) (8 S))
 ((2 D) (9 D)) ((9 C) (2 D)) ((2 C) (9 D)) ((2 C) (9 C)) ((9 H) (2 D)) ((9 H) (2 C)) ((2 H) (9 D)) ((2 H) (9 C))
 ((2 H) (9 H)) ((9 S) (2 D)) ((9 S) (2 C)) ((9 S) (2 H)) ((2 S) (9 D)) ((2 S) (9 C)) ((2 S) (9 H)) ((2 S) (9 S))
 ((2 D) (10 D)) ((10 C) (2 D)) ((2 C) (10 D)) ((2 C) (10 C)) ((10 H) (2 D)) ((10 H) (2 C)) ((2 H) (10 D)) ((2 H) (10 C))
 ((2 H) (10 H)) ((10 S) (2 D)) ((10 S) (2 C)) ((10 S) (2 H)) ((2 S) (10 D)) ((2 S) (10 C)) ((2 S) (10 H)) ((2 S) (10 S))
 ((J H) (2 D)) ((J S) (2 D)) ((2 H) (J D)) ((2 S) (J D)) ((2 S) (J C)) ((2 D) (J D)) ((J C) (2 D)) ((2 C) (J D))
 ((2 C) (J C)) ((J H) (2 C)) ((2 H) (J C)) ((2 H) (J H)) ((2 S) (J H)) ((2 S) (J S)) ((3 H) (4 H)) ((3 D) (4 D))
 ((3 C) (4 C)) ((4 C) (3 D)) ((3 C) (4 D)) ((4 H) (3 D)) ((4 H) (3 C)) ((3 H) (4 D)) ((3 H) (4 C)) ((4 S) (3 D))
 ((4 S) (3 H)) ((3 S) (4 D)) ((3 S) (4 C)) ((3 S) (4 H)) ((3 S) (4 S)) ((4 S) (3 C)) ((3 H) (5 H)) ((3 D) (5 D))
 ((5 C) (3 D)) ((3 C) (5 D)) ((5 S) (3 D)) ((5 S) (3 C)) ((5 S) (3 H)) ((3 S) (5 D)) ((3 S) (5 C)) ((5 H) (3 D))
 ((3 H) (6 H)) ((3 D) (6 D)) ((6 C) (3 D)) ((3 C) (6 D)) ((3 C) (6 C)) ((3 D) (7 D)) ((J H) (3 C)) ((3 D) (J D))
 ((4 D) (5 D)) ((4 D) (6 D)) ((4 C) (6 C)) ((6 C) (4 D)) ((4 H) (6 H)) ((6 H) (4 D)) ((4 D) (7 D)) ((4 C) (7 C))
 ((7 C) (4 D)) ((4 C) (7 D)) ((4 H) (7 H)) ((7 H) (4 D)) ((7 H) (4 C)) ((4 H) (7 D)) ((7 H) (8 D)) ((7 H) (9 D))
 ((7 H) (9 C)) ((Q S) (2 H)) ((5 H) (3 C)) ((3 H) (5 D)) ((3 H) (5 C)) ((3 S) (5 H)) ((3 S) (5 S)) ((6 S) (3 D))
 ((6 S) (3 C)) ((3 S) (6 D)) ((6 S) (3 H)) ((3 S) (6 S)) ((6 H) (3 D)) ((6 H) (3 C)) ((3 H) (6 D)) ((3 H) (6 C))
 ((7 S) (3 D)) ((3 S) (7 D)) ((3 S) (7 H)) ((7 C) (3 D)) ((3 C) (7 D)) ((3 C) (7 C)) ((7 S) (3 C)) ((7 S) (3 H))
 ((3 S) (7 C)) ((3 S) (7 S)) ((7 H) (3 D)) ((7 H) (3 C)) ((3 H) (7 D)) ((3 H) (7 C)) ((3 H) (7 H)) ((3 S) (8 D))
 ((3 S) (8 C)) ((3 S) (8 H)) ((3 S) (8 S)) ((3 C) (8 D)) ((3 C) (8 C)) ((3 D) (8 D)) ((8 C) (3 D)) ((8 H) (3 D))
 ((8 H) (3 C)) ((3 H) (8 D)) ((3 H) (8 C)) ((3 H) (8 H)) ((J S) (2 C)) ((J S) (2 H)) ((8 S) (3 D)) ((8 S) (3 C))
 ((8 S) (3 H)) ((3 S) (9 H)) ((3 S) (9 D)) ((3 S) (9 C)) ((3 S) (9 S)) ((3 C) (9 D)) ((3 C) (9 C)) ((3 D) (9 D))
 ((9 C) (3 D)) ((9 H) (3 D)) ((9 H) (3 C)) ((3 H) (9 D)) ((3 H) (9 C)) ((3 H) (9 H)) ((9 S) (3 D)) ((9 S) (3 C))
 ((9 S) (3 H)) ((3 S) (10 H)) ((3 S) (10 S)) ((3 S) (10 D)) ((3 S) (10 C)) ((3 D) (10 D)) ((3 C) (10 D)) ((3 C) (10 C))
 ((10 C) (3 D)) ((10 H) (3 D)) ((10 H) (3 C)) ((3 H) (10 D)) ((3 H) (10 C)) ((3 H) (10 H)) ((10 S) (3 D)) ((10 S) (3 C))
 ((10 S) (3 H)) ((3 S) (J H)) ((3 S) (J S)) ((3 S) (J D)) ((3 S) (J C)) ((J C) (3 D)) ((3 C) (J D)) ((3 C) (J C))
 ((J H) (3 D)) ((3 H) (J D)) ((J S) (3 D)) ((3 H) (J H)) ((J S) (3 C)) ((J S) (3 H)) ((5 C) (4 D)) ((4 C) (5 D))
 ((5 S) (4 D)) ((5 S) (4 C)) ((5 S) (4 H)) ((4 S) (5 D)) ((4 S) (5 C)) ((4 S) (5 H)) ((4 S) (5 S)) ((4 C) (5 C))
 ((5 H) (4 D)) ((5 H) (4 C)) ((4 H) (5 D)) ((4 H) (5 C)) ((4 C) (6 D)) ((6 S) (4 D)) ((6 S) (4 C)) ((6 S) (4 H))
 ((4 S) (6 D)) ((4 S) (6 C)) ((4 S) (6 H)) ((4 S) (6 S)) ((6 H) (4 C)) ((4 H) (6 D)) ((4 H) (6 C)) ((4 H) (7 C))
 ((7 S) (4 D)) ((7 S) (4 C)) ((7 S) (4 H)) ((4 S) (7 D)) ((4 S) (7 C)) ((4 S) (7 H)) ((4 S) (7 S)) ((4 D) (8 D))
 ((4 C) (8 C)) ((4 C) (8 D)) ((4 H) (8 H)) ((4 H) (8 D)) ((4 H) (8 C)) ((4 S) (8 D)) ((4 S) (8 C)) ((4 S) (8 H))
 ((4 S) (8 S)) ((8 C) (4 D)) ((8 H) (4 D)) ((8 H) (4 C)) ((8 S) (4 D)) ((8 S) (4 C)) ((8 S) (4 H)) ((4 C) (9 C))
 ((4 C) (9 D)) ((4 H) (9 H)) ((4 H) (9 D)) ((4 H) (9 C)) ((4 D) (9 D)) ((9 C) (4 D)) ((9 H) (4 D)) ((4 S) (9 D))
 ((4 S) (9 C)) ((4 S) (9 H)) ((9 H) (4 C)) ((9 S) (4 D)) ((9 S) (4 C)) ((9 S) (4 H)) ((4 D) (10 D)) ((4 C) (10 D))
 ((4 C) (10 C)) ((4 H) (10 H)) ((4 H) (10 D)) ((4 H) (10 C)) ((10 C) (4 D)) ((10 H) (4 D)) ((10 H) (4 C)) ((4 S) (10 D))
 ((4 S) (10 C)) ((4 S) (10 H)) ((10 S) (4 D)) ((3 H) (J C)) ((10 S) (4 C)) ((10 S) (4 H)) ((J H) (4 C)) ((4 D) (J D))
 ((J C) (4 D)) ((4 C) (J D)) ((4 C) (J C)) ((4 S) (J S)) ((4 H) (J D)) ((4 H) (J C)) ((J H) (4 D)) ((4 S) (J D))
 ((4 S) (J C)) ((4 S) (J H)) ((J S) (4 D)) ((J S) (4 H)) ((5 C) (6 C)) ((5 H) (6 C)) ((6 C) (5 D)) ((5 D) (6 D))
 ((5 C) (6 D)) ((6 H) (5 D)) ((6 H) (5 C)) ((6 S) (5 D)) ((6 S) (5 C)) ((5 H) (6 D)) ((6 S) (5 H)) ((5 S) (6 D))
 ((5 S) (6 C)) ((5 S) (6 H)) ((5 S) (6 S)) ((5 C) (7 C)) ((5 H) (7 C)) ((7 C) (5 D)) ((5 C) (7 D)) ((5 D) (7 D))
 ((7 S) (5 D)) ((7 S) (5 C)) ((7 H) (5 D)) ((7 H) (5 C)) ((5 H) (7 D)) ((5 S) (7 H)) ((5 S) (7 S)) ((5 C) (8 C))
 ((5 H) (8 H)) ((5 S) (8 S)) ((5 C) (8 D)) ((5 D) (8 D)) ((8 C) (5 D)) ((5 S) (8 D)) ((4 H) (J H)) ((5 H) (8 D))
 ((5 H) (8 C)) ((8 H) (5 D)) ((8 H) (5 C)) ((8 S) (5 D)) ((8 S) (5 C)) ((8 S) (5 H)) ((5 C) (9 C)) ((5 H) (9 H))
 ((5 S) (9 S)) ((5 D) (9 D)) ((9 C) (5 D)) ((5 C) (9 D)) ((5 H) (9 D)) ((5 H) (9 C)) ((5 S) (9 D)) ((5 S) (9 C))
 ((9 H) (5 D)) ((9 H) (5 C)) ((7 H) (9 H)) ((7 C) (10 C)) ((7 H) (10 D)) ((2 S) (Q H)) ((3 C) (5 C)) ((J S) (4 C))
 ((9 S) (5 D)) ((9 S) (5 C)) ((9 S) (5 H)) ((5 S) (10 S)) ((5 S) (10 D)) ((5 S) (10 C)) ((5 S) (10 H)) ((5 D) (10 D))
 ((10 C) (5 D)) ((5 C) (10 D)) ((5 C) (10 C)) ((10 H) (5 D)) ((10 H) (5 C)) ((10 S) (5 D)) ((10 S) (5 C)) ((10 S) (5 H))
 ((J H) (5 C)) ((5 H) (J D)) ((5 H) (J C)) ((7 H) (10 C)) ((7 H) (10 H)) ((7 S) (10 C)) ((7 S) (10 H)) ((7 S) (10 S))
 ((7 C) (J C)) ((7 H) (J D)) ((7 H) (J C)) ((J H) (7 D)) ((8 S) (9 H)) ((2 S) (Q D)) ((J H) (5 D)) ((J S) (5 D))
 ((J S) (5 C)) ((9 H) (8 D)) ((9 H) (8 C)) ((2 C) (Q D)) ((J S) (5 H)) ((6 D) (7 D)) ((2 S) (Q C)) ((6 H) (7 H))
 ((2 S) (Q S)) ((3 S) (6 C)) ((3 S) (6 H)) ((8 H) (9 D)) ((8 C) (10 C)) ((10 H) (8 D)) ((2 C) (Q C)) ((7 H) (6 D))
 ((6 S) (7 S)) ((6 H) (7 D)) ((5 S) (J S)) ((6 H) (7 C)) ((7 S) (6 D)) ((7 S) (6 C)) ((7 S) (6 H)) ((6 S) (7 D))
 ((10 H) (8 C)) ((8 H) (10 D)) ((8 S) (10 H)) ((3 H) (Q H)) ((6 C) (8 C)) ((6 D) (8 D)) ((6 H) (8 H)) ((6 H) (8 D))
 ((6 H) (8 C)) ((6 S) (8 S)) ((6 S) (8 D)) ((6 S) (8 C)) ((6 S) (8 H)) ((8 S) (6 C)) ((8 S) (6 H)) ((6 C) (9 C))
 ((3 C) (Q D)) ((5 H) (10 D)) ((5 H) (10 C)) ((5 S) (J D)) ((5 S) (J C)) ((5 S) (J H)) ((6 D) (9 D)) ((6 H) (9 D))
 ((6 H) (9 C)) ((8 S) (10 S)) ((10 S) (8 D)) ((8 D) (J D)) ((J H) (8 D)) ((J H) (8 C)) ((8 S) (J H)) ((10 H) (9 D))
 ((3 S) (Q C)) ((4 S) (9 S)) ((4 S) (10 S)) ((7 S) (5 H)) ((5 S) (7 D)) ((5 D) (J D)) ((J C) (5 D)) ((5 C) (J D))
 ((5 C) (J C)) ((6 H) (9 H)) ((10 H) (9 C)) ((9 H) (10 D)) ((9 D) (J D)) ((J H) (9 D)) ((J H) (9 C)) ((J S) (10 C))
 ((10 S) (J C)) ((3 H) (Q C)) ((5 S) (7 C)) ((5 S) (8 C)) ((6 S) (7 C)) ((6 S) (7 H)) ((7 C) (6 D)) ((6 C) (7 D))
 ((6 C) (7 C)) ((8 C) (6 D)) ((6 C) (8 D)) ((6 S) (9 S)) ((10 D) (J D)) ((3 C) (Q C)) ((Q S) (4 D)) ((5 S) (9 H))
 ((8 H) (6 D)) ((8 S) (6 D)) ((6 S) (9 D)) ((6 S) (9 C)) ((6 S) (9 H)) ((9 C) (6 D)) ((6 C) (9 D)) ((9 H) (6 D))
 ((9 S) (6 D)) ((9 H) (6 C)) ((9 S) (6 C)) ((6 C) (10 C)) ((6 D) (10 D)) ((6 H) (10 D)) ((6 H) (10 C)) ((6 H) (10 H))
 ((6 S) (10 S)) ((6 S) (10 D)) ((6 S) (10 C)) ((6 S) (10 H)) ((10 S) (6 D)) ((10 S) (6 C)) ((6 H) (J C)) ((6 H) (J H))
 ((6 D) (J D)) ((J H) (6 D)) ((3 H) (Q D)) ((6 H) (J D)) ((J H) (6 C)) ((6 S) (J D)) ((6 S) (J C)) ((6 S) (J H))
 ((3 S) (Q D)) ((10 C) (6 D)) ((6 C) (10 D)) ((Q H) (2 D)) ((10 H) (6 D)) ((Q S) (4 C)) ((10 H) (6 C)) ((10 S) (6 H))
 ((6 S) (J S)) ((J S) (6 D)) ((Q S) (4 H)) ((J S) (6 C)) ((Q H) (5 C)) ((J S) (6 H)) ((J C) (6 D)) ((6 C) (J D))
 ((6 C) (J C)) ((7 D) (8 D)) ((7 S) (8 S)) ((8 C) (7 D)) ((7 C) (8 D)) ((7 C) (8 C)) ((8 H) (7 D)) ((8 S) (7 D))
 ((8 S) (7 C)) ((8 S) (7 H)) ((8 H) (7 C)) ((7 S) (9 D)) ((7 S) (9 C)) ((7 S) (9 H)) ((7 D) (9 D)) ((9 C) (7 D))
 ((9 S) (7 D)) ((7 C) (9 D)) ((7 C) (9 C)) ((9 S) (7 C)) ((9 S) (7 H)) ((7 C) (10 D)) ((10 C) (7 D)) ((10 S) (7 D))
 ((10 S) (7 C)) ((10 S) (7 H)) ((9 H) (10 H)) ((3 S) (Q S)) ((4 H) (5 H)) ((4 S) (Q H)) ((5 S) (8 H)) ((3 S) (Q H))
 ((8 H) (6 C)) ((9 S) (6 H)) ((4 S) (Q S)) ((Q S) (5 D)) ((Q S) (5 C)) ((Q S) (5 H)) ((5 D) (Q D)) ((8 H) (10 C))
 ((8 C) (J C)) ((6 S) (Q H)) ((7 H) (8 H)) ((7 S) (8 D)) ((7 S) (8 C)) ((7 S) (8 H)) ((9 H) (7 D)) ((9 H) (7 C))
 ((7 S) (9 S)) ((7 D) (10 D)) ((10 H) (7 D)) ((10 H) (7 C)) ((7 S) (10 D)) ((7 D) (J D)) ((J S) (7 C)) ((J C) (7 D))
 ((J S) (7 D)) ((J S) (7 H)) ((8 S) (10 C)) ((8 H) (J D)) ((8 H) (J C)) ((J C) (8 D)) ((Q S) (2 C)) ((7 S) (J D))
 ((Q H) (3 D)) ((5 C) (Q D)) ((9 C) (10 C)) ((3 D) (Q D)) ((4 D) (Q D)) ((4 C) (Q D)) ((4 C) (Q C)) ((Q H) (4 D))
 ((4 H) (Q H)) ((5 H) (6 H)) ((5 H) (7 H)) ((5 H) (10 H)) ((5 H) (J H)) ((4 S) (Q D)) ((5 H) (Q D)) ((7 C) (J D))
 ((9 S) (8 D)) ((4 S) (Q C)) ((9 C) (8 D)) ((Q H) (6 D)) ((9 S) (8 H)) ((8 S) (J S)) ((J S) (8 D)) ((4 H) (Q C))
 ((8 S) (9 D)) ((5 H) (Q C)) ((8 S) (9 C)) ((8 S) (9 S)) ((10 C) (8 D)) ((8 C) (10 D)) ((5 H) (Q H)) ((8 C) (J D))
 ((2 D) (Q D)) ((4 H) (Q D)) ((5 S) (Q D)) ((5 S) (Q C)) ((5 S) (Q H)) ((8 C) (9 D)) ((9 S) (8 C)) ((5 S) (Q S))
 ((7 H) (6 C)) ((Q H) (6 C)) ((8 H) (J H)) ((9 H) (10 C)) ((10 S) (9 D)) ((10 S) (9 C)) ((10 S) (9 H)) ((9 S) (10 C))
 ((J S) (8 C)) ((J S) (8 H)) ((6 S) (Q C)) ((J H) (7 C)) ((8 D) (9 D)) ((8 D) (10 D)) ((8 S) (10 D)) ((9 S) (10 H))
 ((9 S) (10 D)) ((9 C) (10 D)) ((9 H) (J D)) ((5 C) (Q C)) ((9 H) (J C)) ((10 C) (J C)) ((J H) (10 D)) ((10 H) (J C))
 ((J S) (10 D)) ((Q H) (2 C)) ((10 S) (J D)) ((Q S) (2 D)) ((Q H) (5 D)) ((J H) (10 C)) ((6 S) (Q D)) ((6 S) (Q S))
 ((10 H) (J H)) ((Q S) (3 D)) ((Q S) (3 C)) ((Q S) (3 H)) ((Q C) (5 D)) ((8 H) (10 H)) ((Q S) (6 D)) ((Q S) (6 C))
 ((Q S) (6 H)) ((6 D) (Q D)) ((Q C) (6 D)) ((6 C) (Q C)) ((7 H) (J H)) ((Q H) (7 D)) ((8 H) (9 C)) ((10 S) (8 C))
 ((8 S) (J D)) ((9 S) (10 S)) ((J S) (9 D)) ((J S) (9 C)) ((10 H) (J D)) ((Q H) (3 C)) ((Q H) (4 C)) ((6 C) (Q D))
 ((8 S) (J C)) ((J S) (9 H)) ((9 S) (J D)) ((9 S) (J C)) ((Q H) (7 C)) ((Q S) (7 D)) ((9 S) (J H)) ((9 S) (J S))
 ((J C) (9 D)) ((9 C) (J D)) ((J C) (10 D)) ((10 C) (J D)) ((2 H) (Q D)) ((2 H) (Q C)) ((Q C) (2 D)) ((Q C) (3 D))
 ((10 S) (J H)) ((2 H) (Q H)) ((6 H) (Q D)) ((7 S) (J H)) ((7 S) (J S)) ((Q S) (7 C)) ((Q S) (7 H)) ((8 C) (9 C))
 ((10 S) (8 H)) ((7 S) (J C)) ((10 S) (J S)) ((Q C) (4 D)) ((6 H) (Q C)) ((7 H) (8 C)) ((8 H) (9 H)) ((6 H) (Q H))
 ((7 D) (Q D)) ((Q C) (7 D)) ((7 C) (Q D)) ((7 C) (Q C)) ((7 H) (Q D)) ((7 H) (Q C)) ((7 H) (Q H)) ((7 S) (Q D))
 ((7 S) (Q C)) ((7 S) (Q H)) ((9 D) (10 D)) ((10 C) (9 D)) ((9 C) (J C)) ((7 S) (Q S)) ((8 D) (Q D)) ((Q C) (8 D))
 ((Q H) (8 D)) ((Q H) (8 C)) ((9 H) (J H)) ((8 H) (Q D)) ((8 H) (Q C)) ((8 H) (Q H)) ((8 C) (Q D)) ((8 C) (Q C))
 ((Q S) (8 C)) ((Q S) (8 H)) ((Q S) (8 D)) ((8 S) (Q D)) ((8 S) (Q C)) ((8 S) (Q H)) ((8 S) (Q S)) ((9 D) (Q D))
 ((Q C) (9 D)) ((9 C) (Q D)) ((9 C) (Q C)) ((Q H) (9 D)) ((Q H) (9 C)) ((9 H) (Q D)) ((9 H) (Q C)) ((Q S) (9 D))
 ((Q S) (9 C)) ((Q S) (9 H)) ((J S) (10 H)) ((9 H) (Q H)) ((9 S) (Q D)) ((9 S) (Q C)) ((9 S) (Q H)) ((9 S) (Q S))
 ((10 D) (Q D)) ((Q C) (10 D)) ((10 C) (Q D)) ((10 C) (Q C)) ((Q H) (10 D)) ((Q H) (10 C)) ((10 H) (Q D)) ((10 H) (Q C))
 ((10 H) (Q H)) ((Q S) (10 D)) ((Q S) (10 C)) ((Q S) (10 H)) ((10 S) (Q D)) ((10 S) (Q C)) ((10 S) (Q H)) ((10 S) (Q S))
 ((J D) (Q D)) ((Q C) (J D)) ((J C) (Q D)) ((J C) (Q C)) ((Q H) (J D)) ((Q H) (J C)) ((J H) (Q D)) ((J H) (Q C))
 ((J H) (Q H)) ((Q S) (J D)) ((Q S) (J C)) ((Q S) (J H)) ((J S) (Q D)) ((J S) (Q C)) ((J S) (Q H)) ((J S) (Q S))
 ((2 D) (K D)) ((K C) (2 D)) ((2 C) (K D)) ((2 C) (K C)) ((K H) (2 D)) ((K H) (2 C)) ((2 H) (K D)) ((2 H) (K C))
 ((2 H) (K H)) ((K S) (2 D)) ((K S) (2 C)) ((K S) (2 H)) ((2 S) (K D)) ((2 S) (K C)) ((2 S) (K H)) ((2 S) (K S))
 ((3 D) (K D)) ((K C) (3 D)) ((3 C) (K D)) ((3 C) (K C)) ((K H) (3 D)) ((K H) (3 C)) ((3 H) (K D)) ((3 H) (K C))
 ((3 H) (K H)) ((K S) (3 D)) ((K S) (3 C)) ((K S) (3 H)) ((3 S) (K D)) ((3 S) (K C)) ((3 S) (K H)) ((3 S) (K S))
 ((4 D) (K D)) ((K C) (4 D)) ((4 C) (K D)) ((4 C) (K C)) ((K H) (4 D)) ((K H) (4 C)) ((4 H) (K D)) ((4 H) (K C))
 ((4 H) (K H)) ((K S) (4 D)) ((K S) (4 C)) ((K S) (4 H)) ((4 S) (K D)) ((4 S) (K C)) ((4 S) (K H)) ((4 S) (K S))
 ((5 D) (K D)) ((K C) (5 D)) ((5 C) (K D)) ((5 C) (K C)) ((K H) (5 D)) ((K H) (5 C)) ((5 H) (K D)) ((5 H) (K C))
 ((5 H) (K H)) ((K S) (5 D)) ((K S) (5 C)) ((K S) (5 H)) ((5 S) (K D)) ((5 S) (K C)) ((5 S) (K H)) ((5 S) (K S))
 ((6 D) (K D)) ((K C) (6 D)) ((6 C) (K D)) ((6 C) (K C)) ((K H) (6 D)) ((K H) (6 C)) ((6 H) (K D)) ((6 H) (K C))
 ((6 H) (K H)) ((K S) (6 D)) ((K S) (6 C)) ((K S) (6 H)) ((6 S) (K D)) ((6 S) (K C)) ((6 S) (K H)) ((6 S) (K S))
 ((7 D) (K D)) ((K C) (7 D)) ((7 C) (K D)) ((7 C) (K C)) ((K H) (7 D)) ((K H) (7 C)) ((7 H) (K D)) ((7 H) (K C))
 ((7 H) (K H)) ((K S) (7 D)) ((K S) (7 C)) ((K S) (7 H)) ((7 S) (K D)) ((7 S) (K C)) ((7 S) (K H)) ((7 S) (K S))
 ((8 D) (K D)) ((K C) (8 D)) ((8 C) (K D)) ((8 C) (K C)) ((K H) (8 D)) ((K H) (8 C)) ((8 H) (K D)) ((8 H) (K C))
 ((8 H) (K H)) ((K S) (8 D)) ((K S) (8 C)) ((K S) (8 H)) ((8 S) (K D)) ((8 S) (K C)) ((8 S) (K H)) ((8 S) (K S))
 ((9 D) (K D)) ((K C) (9 D)) ((9 C) (K D)) ((9 C) (K C)) ((K H) (9 D)) ((K H) (9 C)) ((9 H) (K D)) ((9 H) (K C))
 ((9 H) (K H)) ((K S) (9 D)) ((K S) (9 C)) ((K S) (9 H)) ((9 S) (K D)) ((9 S) (K C)) ((9 S) (K H)) ((9 S) (K S))
 ((10 D) (K D)) ((K C) (10 D)) ((10 C) (K D)) ((10 C) (K C)) ((K H) (10 D)) ((K H) (10 C)) ((10 H) (K D)) ((10 H) (K C))
 ((10 H) (K H)) ((K S) (10 D)) ((K S) (10 C)) ((K S) (10 H)) ((10 S) (K D)) ((10 S) (K C)) ((10 S) (K H)) ((10 S) (K S))
 ((J D) (K D)) ((K C) (J D)) ((J C) (K D)) ((J C) (K C)) ((K H) (J D)) ((K H) (J C)) ((J H) (K D)) ((J H) (K C))
 ((J H) (K H)) ((K S) (J D)) ((K S) (J C)) ((K S) (J H)) ((J S) (K D)) ((J S) (K C)) ((J S) (K H)) ((J S) (K S))
 ((Q D) (K D)) ((K C) (Q D)) ((Q C) (K D)) ((Q C) (K C)) ((K H) (Q D)) ((K H) (Q C)) ((Q H) (K D)) ((Q H) (K C))
 ((Q H) (K H)) ((K S) (Q D)) ((K S) (Q C)) ((K S) (Q H)) ((Q S) (K D)) ((Q S) (K C)) ((Q S) (K H)) ((Q S) (K S))
 ((A D) (2 D)) ((2 C) (A D)) ((A C) (2 D)) ((A C) (2 C)) ((2 H) (A D)) ((2 H) (A C)) ((A H) (2 D)) ((A H) (2 C))
 ((A H) (2 H)) ((2 S) (A D)) ((2 S) (A C)) ((2 S) (A H)) ((A S) (2 D)) ((A S) (2 C)) ((A S) (2 H)) ((A S) (2 S))
 ((A D) (3 D)) ((3 C) (A D)) ((A C) (3 D)) ((A C) (3 C)) ((3 H) (A D)) ((3 H) (A C)) ((A H) (3 D)) ((A H) (3 C))
 ((A H) (3 H)) ((3 S) (A D)) ((3 S) (A C)) ((3 S) (A H)) ((A S) (3 D)) ((A S) (3 C)) ((A S) (3 H)) ((A S) (3 S))
 ((A D) (4 D)) ((4 C) (A D)) ((A C) (4 D)) ((A C) (4 C)) ((4 H) (A D)) ((4 H) (A C)) ((A H) (4 D)) ((A H) (4 C))
 ((A H) (4 H)) ((4 S) (A D)) ((4 S) (A C)) ((4 S) (A H)) ((A S) (4 D)) ((A S) (4 C)) ((A S) (4 H)) ((A S) (4 S))
 ((A D) (5 D)) ((5 C) (A D)) ((A C) (5 D)) ((A C) (5 C)) ((5 H) (A D)) ((5 H) (A C)) ((A H) (5 D)) ((A H) (5 C))
 ((A H) (5 H)) ((5 S) (A D)) ((5 S) (A C)) ((5 S) (A H)) ((A S) (5 D)) ((A S) (5 C)) ((A S) (5 H)) ((A S) (5 S))
 ((A D) (6 D)) ((6 C) (A D)) ((A C) (6 D)) ((A C) (6 C)) ((6 H) (A D)) ((6 H) (A C)) ((A H) (6 D)) ((A H) (6 C))
 ((A H) (6 H)) ((6 S) (A D)) ((6 S) (A C)) ((6 S) (A H)) ((A S) (6 D)) ((A S) (6 C)) ((A S) (6 H)) ((A S) (6 S))
 ((A D) (7 D)) ((7 C) (A D)) ((A C) (7 D)) ((A C) (7 C)) ((7 H) (A D)) ((7 H) (A C)) ((A H) (7 D)) ((A H) (7 C))
 ((A H) (7 H)) ((7 S) (A D)) ((7 S) (A C)) ((7 S) (A H)) ((A S) (7 D)) ((A S) (7 C)) ((A S) (7 H)) ((A S) (7 S))
 ((A D) (8 D)) ((8 C) (A D)) ((A C) (8 D)) ((A C) (8 C)) ((8 H) (A D)) ((8 H) (A C)) ((A H) (8 D)) ((A H) (8 C))
 ((A H) (8 H)) ((8 S) (A D)) ((8 S) (A C)) ((8 S) (A H)) ((A S) (8 D)) ((A S) (8 C)) ((A S) (8 H)) ((A S) (8 S))
 ((A D) (9 D)) ((9 C) (A D)) ((A C) (9 D)) ((A C) (9 C)) ((9 H) (A D)) ((9 H) (A C)) ((A H) (9 D)) ((A H) (9 C))
 ((A H) (9 H)) ((9 S) (A D)) ((9 S) (A C)) ((9 S) (A H)) ((A S) (9 D)) ((A S) (9 C)) ((A S) (9 H)) ((A S) (9 S))
 ((A D) (10 D)) ((10 C) (A D)) ((A C) (10 D)) ((A C) (10 C)) ((10 H) (A D)) ((10 H) (A C)) ((A H) (10 D)) ((A H) (10 C))
 ((A H) (10 H)) ((10 S) (A D)) ((10 S) (A C)) ((10 S) (A H)) ((A S) (10 D)) ((A S) (10 C)) ((A S) (10 H)) ((A S) (10 S))
 ((A D) (J D)) ((J C) (A D)) ((A C) (J D)) ((A C) (J C)) ((J H) (A D)) ((J H) (A C)) ((A H) (J D)) ((A H) (J C))
 ((A H) (J H)) ((J S) (A D)) ((J S) (A C)) ((J S) (A H)) ((A S) (J D)) ((A S) (J C)) ((A S) (J H)) ((A S) (J S))
 ((A D) (Q D)) ((Q C) (A D)) ((A C) (Q D)) ((A C) (Q C)) ((Q H) (A D)) ((Q H) (A C)) ((A H) (Q D)) ((A H) (Q C))
 ((A H) (Q H)) ((Q S) (A D)) ((Q S) (A C)) ((Q S) (A H)) ((A S) (Q D)) ((A S) (Q C)) ((A S) (Q H)) ((A S) (Q S))
 ((A D) (K D)) ((K C) (A D)) ((A C) (K D)) ((A C) (K C)) ((K H) (A D)) ((K H) (A C)) ((A H) (K D)) ((A H) (K C))
 ((A H) (K H)) ((K S) (A D)) ((K S) (A C)) ((K S) (A H)) ((A S) (K D)) ((A S) (K C)) ((A S) (K H)) ((A S) (K S))
 ((2 C) (2 D)) ((2 H) (2 D)) ((2 H) (2 C)) ((2 S) (2 D)) ((2 S) (2 C)) ((2 S) (2 H)) ((3 C) (3 D)) ((3 H) (3 D))
 ((3 H) (3 C)) ((3 S) (3 D)) ((3 S) (3 C)) ((3 S) (3 H)) ((4 C) (4 D)) ((4 H) (4 D)) ((4 H) (4 C)) ((4 S) (4 D))
 ((4 S) (4 C)) ((4 S) (4 H)) ((5 C) (5 D)) ((5 H) (5 D)) ((5 H) (5 C)) ((5 S) (5 D)) ((5 S) (5 C)) ((5 S) (5 H))
 ((6 C) (6 D)) ((6 H) (6 D)) ((6 H) (6 C)) ((6 S) (6 D)) ((6 S) (6 C)) ((6 S) (6 H)) ((7 C) (7 D)) ((7 H) (7 D))
 ((7 H) (7 C)) ((7 S) (7 D)) ((7 S) (7 C)) ((7 S) (7 H)) ((8 C) (8 D)) ((8 H) (8 D)) ((8 H) (8 C)) ((8 S) (8 D))
 ((8 S) (8 C)) ((8 S) (8 H)) ((9 C) (9 D)) ((9 H) (9 D)) ((9 H) (9 C)) ((9 S) (9 D)) ((9 S) (9 C)) ((9 S) (9 H))
 ((10 C) (10 D)) ((10 H) (10 D)) ((10 H) (10 C)) ((10 S) (10 D)) ((10 S) (10 C)) ((10 S) (10 H)) ((J C) (J D)) ((J H) (J D))
 ((J H) (J C)) ((J S) (J D)) ((J S) (J C)) ((J S) (J H)) ((Q C) (Q D)) ((Q H) (Q D)) ((Q H) (Q C)) ((Q S) (Q D))
 ((Q S) (Q C)) ((Q S) (Q H)) ((K C) (K D)) ((K H) (K D)) ((K H) (K C)) ((K S) (K D)) ((K S) (K C)) ((K S) (K H))
 ((A C) (A D)) ((A H) (A D)) ((A H) (A C)) ((A S) (A D)) ((A S) (A C)) ((A S) (A H))))
)


