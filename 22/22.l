(setq Inp (in "22.txt" (till NIL)))
(setq Blocks (filter 'and (split (split Inp "\n") NIL)))
(for I (mapcar format (cdr (car Blocks)))
  (queue 'Deck1 I))
(for I (mapcar format (cdr (last Blocks)))
  (queue 'Deck2 I))

(setq DemoInp (in "demo.txt" (till NIL)))
(setq DemoBlocks (filter 'and (split (split DemoInp "\n") NIL )))
(for I (mapcar format (cdr (car DemoBlocks)))
  (queue 'DemoDeck1 I))
(for I (mapcar format (cdr (last DemoBlocks)))
  (queue 'DemoDeck2 I))

# 22a
(de play (Deck1 Deck2)
  (let (Deck1 (copy Deck1)
        Deck2 (copy Deck2))
    (while (and Deck1 Deck2)
      #(prin "Deck1: ") (println Deck1)
      #(prin "Deck2: ") (println Deck2)
      (let (C1 (pop 'Deck1)
            C2 (pop 'Deck2))
        #(prinl "C1: " C1)
        #(prinl "C2: " C2)
        (if (> C1 C2)
          (prog
            (queue 'Deck1 C1)
            (queue 'Deck1 C2))
          (prog
            (queue 'Deck2 C2)
            (queue 'Deck2 C1)))))
    (let Deck (if Deck1 Deck1 Deck2)
      (prinl "Score: " (score Deck)))))

(de score (Deck)
  (apply + (mapcar * Deck (reverse (range 1 (length Deck))))))

(test 306 (play DemoDeck1 DemoDeck2))

(play Deck1 Deck2)

# 22b
(de play-rec (Deck1 Deck2 Recur)
  (if Recur (prinl "Playing subgame..."))
  (let (Deck1 (copy Deck1)
        Deck2 (copy Deck2)
        #Rounds NIL
        End NIL)
    (loop
      #(prin "Deck1: ") (println Deck1)
      #(prin "Deck2: ") (println Deck2)
      (NIL (and Deck1 Deck2))
      (T End)
      (let (C1 (pop 'Deck1)
            C2 (pop 'Deck2)
            Pair (cons C1 C2))
        #(prinl "C1: " C1)
        #(prinl "C2: " C2)
        (if (index Pair Rounds)
          (prog
            (prin "Hand ") (print Pair)
            (prinl " was already dealt. P1 wins!")
            (setq End T))
          (prog
            (push 'Rounds Pair)
            (cond
              ((and (>= (length Deck1) C1)
                    (>= (length Deck2) C2))
                 (prinl "Enter subgame")
                 (let Res (play-rec Deck1 Deck2 T)
                   (cond
                     ((=T Res)
                       (queue 'Deck1 C1)
                       (queue 'Deck1 C2))
                     (T
                       (queue 'Deck2 C2)
                       (queue 'Deck2 C1)))))
              ((> C1 C2)
                 (queue 'Deck1 C1)
                 (queue 'Deck1 C2))
              (T
                 (queue 'Deck2 C2)
                 (queue 'Deck2 C1)))))))
    (let Deck (if Deck1 Deck1 Deck2)
      (cond
        (End
          (prinl "Score: " (score Deck))
          (quit))
        (Recur
          (prinl "subgame finished")
          (cond
            (Deck1 T)
            (T NIL)))
        (T (prinl "Score: " (score Deck)))))))

(test 291 (play-rec DemoDeck1 DemoDeck2))

(play-rec Deck1 Deck2)

