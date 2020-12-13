(setq Inp (in "13.txt" (till NIL)))
(setq All (filter 'and (split Inp "\n")))

(setq Timestamp (format (pack (car All))))
(setq Buses (filter 'and (mapcar 'format (split (last All) ","))))

(setq DemoTimestamp 939)
(setq DemoBuses (7 13 59 31 19))

# 13a
(de departures (Ts Buses)
  (let f '((B) (if (=0 (% Ts B))
                 Ts
                 (+ B (* B (/ Ts B)))))
    (mapcar 'f Buses)))

(de result (Ts Buses)
  (let (Deps (departures Ts Buses)
        Mins (mapcar '((D) (- D Ts)) Deps)
        Zip (mapcar 'cons Buses Mins)
        Bus (car (by cdr sort Zip)))
    (* (car Bus) (cdr Bus))))

(test 295 (result DemoTimestamp DemoBuses))
(prinl "result: " (result Timestamp Buses))

# 13b
(setq Buses2 (mapcar 'format (split (last All) ",")))
(setq Offsets (range 0 (length Buses2)))
(setq Pairs (filter 'car (mapcar 'cons Buses2 Offsets)))

(setq DemoPairs1 '((17 . 0) (13 . 2) (19 . 3)))
(setq DemoPairs2 '((67 . 0) (7 . 1) (59 . 2) (61 . 3)))

# brute-force... endless!
(de compute-raw (Pairs)
  (let (f '((P) (=0 (% (+ Ts (cdr P)) (car P))))
        Max (last (by car sort (copy Pairs)))
        Inc (car Max)
        Ts (- (cdr Max)))
    (loop
      (setq Ts (+ Ts Inc))
      (T (fully 'f Pairs))
      (when (=0 (% Ts 10000000)) (prinl Ts)))
    Ts))

# using chinese reminder is fast
# pairs are (m_i . -off_i)
# Timestamp≡-off_i(mod m_i)
(de compute (Pairs)
  (let (Sorted (by car sort (copy Pairs))
        M (apply * (mapcar car Sorted))
        Zs (mapcar '((X) (/ M (car X))) Sorted)
        f '((Z P)
            (let (N 0 X (% Z (car P)))
              (loop
                (setq N (inc N))
                (T (= 1 (% (* X N) (car P))) (* N Z)))))
        Ws (mapcar 'f Zs Sorted)
        Cs (mapcar '((W P) (* W (- (cdr P)))) Ws Sorted)
        Sum (apply + Cs))
    (+ M (% Sum M))))

(test 3417 (compute DemoPairs1))
(test 754018 (compute DemoPairs2))

(prinl "Ts: " (compute Pairs))

