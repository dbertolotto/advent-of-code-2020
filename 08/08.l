(setq Inp (in "08.txt" (till NIL T)))
(setq List (filter 'and (split (chop Inp) "\n")))

# 8a
(de parse (List)
  (mapcar '((L) (let ((Op Val) (split L " "))
            (cons (pack Op) (format (pack Val)))))
          List))

(setq List2 (parse List))

(de upd-counters (Row)
  (setq Counters (place Row Counters 1)))

(de runit (Lst)
  (let (Row 1 Acc 0 Counters (need (length Lst) 0))
    (while (=0 (car (nth Counters Row)))
      (let ((Op . Val) (car (nth Lst Row)))
        #(prin "Row: " Row " ") (print Op Val) (prinl " " (car (nth Counters Row)))
        (case Op
          ("acc" (upd-counters Row) (setq Row (inc Row)) (setq Acc (+ Acc Val)))
          ("jmp" (upd-counters Row) (setq Row (+ Row Val)))
          (T     (upd-counters Row) (setq Row (inc Row))))))
    (prinl "accum: " Acc)
    (when (> Row (length Lst)) (println "program terminated!") (quit))))

(runit List2)

# 8b
(for (I . P) List2
  (let ((Op . Val) P
        NewList (case Op
                  ("jmp" (place I List2 (cons "nop" Val)))
                  ("nop" (place I List2 (cons "jump" Val)))
                  (T List2)))
    (runit NewList)))
