(setq Inp (0 5 4 1 10 14 7))

# 15a
(de turn ()
  (let Idx (index Next Spoken)
    (push 'Spoken Next)
    (if Idx
      (setq Next Idx)
      (setq Next 0))))

(de game (Input N)
  (let (Spoken (flip (copy Input))
        Next 0)
    (for I (- N (length Spoken))
      #(prinl I)
      (turn))
    (car Spoken)))

(test 436 (game (0 3 6) 2020))
(test 1836 (game (3 1 2) 2020))

(prinl "2020th: " (game Inp 2020))

# 15b
(class +BTree)
(dm put> (K V)
  (del> This K)
  (idx (:: D) (cons K V) T))
(dm del> (K)
  (if (lup (: D) K)
    (idx (:: D) @ NIL)))
(dm get> (K)
  (cdr (lup (: D) K)))
(dm size> ()
  (length (idx (:: D))))
(dm bal> ()
  (balance (:: D) (idx (:: D))))

# reimplement using binary tree for speed
(de turn (I)
  (let Val (get> Spoken Next)
    (put> Spoken Next I)
    (if Val
      (setq Next (- I Val))
      (setq Next 0))))

(de game (Input N)
  (let (Spoken (new +BTree)
        Last (last Input)
        I (length Input)
        Next 0)
    (mapcar '((X Y) (put> Spoken X Y)) Input (range 1 (length Input)))
    (while (< I (dec N))
      (setq I (inc I))
      (when (=0 (% I 500000))
        (bal> Spoken))
      #(prinl I)
      (turn I))
    Next))

(test 436 (game (0 3 6) 2020))
(test 1836 (game (3 1 2) 2020))

# it takes ~10 mins, reduced to ~3 min with the BTree rebalancing
(prinl "30000000th: " (game Inp 30000000))
