(setq DemoInp (mapcar format (chop "389125467")))
(setq Inp (mapcar format (chop "952438716")))

(setq *Print T)
(setq *Cups (copy DemoInp))
(setq *Curr (car DemoInp))
(setq *Store NIL)
(setq *Dest NIL)

(de print-state ()
  (prin "Cups:")
  (for I *Cups
    (if (= I *Curr)
      (prin " (" I ")")
      (prin " " I)))
  (prinl)
  (prin "Store:")
  (for I *Store
    (prin " " I))
  (prinl)
  (prinl "Dest: " *Dest))

(de take-three ()
  (let (Idx (index *Curr *Cups)
        Len (length *Cups))
    (cond
      ((= Idx (- Len 2)) (setq *Store (append (tail 2 *Cups) (list (car *Cups)))))
      ((= Idx (- Len 1)) (setq *Store (list (last *Cups) (car *Cups) (cadr *Cups))))
      ((= Idx Len) (setq *Store (head 3 *Cups)))
      (T (setq *Store (head 3 (nth *Cups (inc Idx))))))))

(de find-dest ()
  (let red '((X) (let N (dec X)
                   (if (< N 1) 9 N)))
    (setq *Dest (red *Curr))
    (while (member *Dest *Store)
      (setq *Dest (red *Dest)))))

(de put-three ()
  (let Pos (index *Curr *Cups)
    (setq *Cups (make
                  (for C *Cups
                    (if (not (member C *Store))
                      (link C))
                    (if (= C *Dest)
                      (mapcar link *Store)))))
    (let Pos2 (index *Curr *Cups)
      (if (<> Pos Pos2)
        (do (- 9 (- Pos2 Pos)) (rot *Cups))))))

(de find-next ()
  (let (Idx (index *Curr *Cups)
        Next (inc Idx))
    (setq *Curr (car (nth *Cups (if (> Next 9) 1 Next))))))

(de game-round ()
  (take-three)
  (find-dest)
  (if *Print (print-state))
  (put-three)
  (find-next))

(de result ()
  (format (pack (cdr (head 9 (nth (apply circ *Cups) (index 1 *Cups)))))))

(de solve-pt1 (N List)
  (let (*Print T
        *Cups (copy List)
        *Curr (car *Cups)
        *Store NIL
        *Dest NIL)
    (for I N
      (prinl "Round " I)
      (game-round))
    (prinl "Cups: " (result))))

(test 92658374 (solve-pt1 10 DemoInp))
(test 67384529 (solve-pt1 100 DemoInp))

(solve-pt1 100 Inp)

(setq DemoInp2 (append DemoInp (range 10 1000000)))

(de result2 ()
  (apply * (cdr (head 3 (nth (apply circ *Cups) *Curr)))))

(setq *Print NIL)
(setq *Cups (copy DemoInp2))
(setq *Curr (car DemoInp2))
(setq *Store NIL)
(setq *Dest NIL)

(de solve-pt2 (N List)
  (let (*Print NIL
        *Cups (copy List)
        *Curr (car *Cups)
        *Store NIL
        *Dest NIL)
    (for I N
      (prinl "Round " I)
      (game-round))
    (prinl "Cups: " (result2))))

(test 149245887792 (solve-pt2 10000000 DemoInp2))
