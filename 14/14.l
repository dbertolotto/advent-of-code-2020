(setq Inp (in "14.txt" (till NIL)))
(setq All (mapcar 'str (mapcar 'pack (filter 'and (split Inp "\n")))))

(setq Blocks '())

# 14a
(de to-mem (Idx Num Mask)
  (let (N (chop Num)
        M (chop Mask)
        f '((CN CM) (if (= "X" CM) CN CM))
        R (pack (mapcar f N M)))
    (push 'Mem (cons Idx R))))

(setq Mem '())
(for S All
  (cond
    ((match '(mask = @Mask) S)
      (setq Mask (name (car @Mask))))
    ((match '(mem (@Idx) = @Num) S)
      (to-mem (car @Idx) (pad 36 (bin (car @Num))) Mask))))
(setq Set '())
(for M Mem
  (unless (assoc (car M) Set) (push 'Set M)))
(prinl (apply + (mapcar '((Pair) (let ((Idx . Num) Pair) (bin Num))) Set)))

# 14b
(de to-mem-2 (Idx Num Mask)
  (let (I (chop Idx)
        M (chop Mask)
        Cnt 0
        f '((CI CM)
             (case CM
               ("0" CI)
               ("1" CM)
               ("X" (setq Cnt (inc Cnt)) (pack "@" Cnt))))
        FloatIdx (pack (mapcar f I M))
        Bits (bits Cnt)
        Indexes (mapcar '((L) (apply 'text L FloatIdx)) Bits))
    (for In Indexes
      (push 'Mem (cons In Num)))))

(de bits (N)
  (mapcar '((X) (chop (pad N (bin X)))) (range 0 (- (** 2 N) 1))))

(setq Mem '())
(for S All
  (cond
    ((match '(mask = @Mask) S)
      (setq Mask (name (car @Mask))))
    ((match '(mem (@Idx) = @Num) S)
      (to-mem-2 (pad 36 (bin (car @Idx))) (car @Num) Mask))))
(setq Set '())
(for M Mem
  (unless (assoc (car M) Set) (push 'Set M)))
(prinl (apply + (mapcar '((Pair) (let ((Idx . Num) Pair) Num)) Set)))
