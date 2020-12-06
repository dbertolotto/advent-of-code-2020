(setq Inp (in "06.txt" (till NIL T)))
(setq List (filter 'and (split (split (chop Inp) "\n") NIL)))

# 6a
(de anyone (G)
  (length (uniq (fish 'atom G))))

(prinl "total: " (apply + (mapcar 'anyone List)))

# 6b
(de everyone (G)
  (let (Len (length G)
        Grouped (by name group (fish 'atom G)))
    (length (filter '((L) (= Len (length L))) Grouped))))

(prinl "total: " (apply + (mapcar 'everyone List)))
