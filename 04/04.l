(setq Inp (in "04.txt" (till NIL T)))
(setq List (filter 'and (split (split (chop Inp) "\n") NIL)))
(de group-card (L)
  (filter 'and (split (mapcan '((X) (insert (inc (length X)) X " ")) L) " ")))
(de parse-pair (S)
  (let ((K V) (split S ":"))
    (cons (pack K) (pack V))))
(de parse (L)
  (mapcar 'parse-pair (group-card L)))
(setq List2 (mapcar 'parse List))

# 4a
(de valid? (Card)
  (or (= 8 (length Card))
      (and (= 7 (length Card)) (not (assoc "cid" Card)))))
(prinl "valid: " (length (filter 'valid? List2)))

# 4b
(de digit? (X) (member X '`(chop "0123456789")))
(de hexa? (X) (member X '`(chop "0123456789abcdef")))
(de meas? (X) (member X '`(chop "cmin")))

(de height-check (V)
  (let (S (chop V)
        N (pack (filter 'digit? S))
        D (format N)
        M (pack (filter 'meas? S)))
    (and (= V (pack N M))
         (cond
           ((= M "cm") (and (>= D 150) (<= D 193)))
           ((= M "in") (and (>= D 59) (<= D 76)))
           (T NIL)))))

(setq Rules '(
 ("byr" . ((X) (let N (format X) (and (>= N 1920) (<= N 2002)))))
 ("iyr" . ((X) (let N (format X) (and (>= N 2010) (<= N 2020)))))
 ("eyr" . ((X) (let N (format X) (and (>= N 2020) (<= N 2030)))))
 ("hgt" . height-check)
 ("hcl" . ((X) (let (S (chop X) D (cdr S))
                  (and (= "#" (car S))
                       (= 6 (length D))
                       (fully 'hexa? D)))))
 ("ecl" . ((X) (member X '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))))
 ("pid" . ((X) (let S (chop X) 
                  (and (= 9 (length S))
                       (fully 'digit? S)))))
 ("cid" . ((X) T))
 ))

(de check (Pair)
  (let ((K . V) Pair
        Fn (cdr (assoc K Rules)))
    (if Fn (Fn V) NIL)))

(de valid2? (Card)
  (and (or (= 8 (length Card))
           (and (= 7 (length Card)) (not (assoc "cid" Card))))
       (fully 'check Card)))

(prinl "valid: " (length (filter 'valid2? List2)))

