(setq *CardPK 6930903)
(setq *DoorPK 19716708)

(setq *DemoCardPK 5764801)
(setq *DemoDoorPK 17807724)

(setq *CardSubjNum 7)
(setq *DoorSubjNum 7)

(de key-loop (SubjNum PK)
  (let (LS 0 Val 1)
    (while (<> Val PK)
      (setq LS (inc LS))
      (setq Val (% (* SubjNum Val) 20201227)))
    LS))

(de transform (SubjNum LS)
  (let Val 1
    (for I LS
      (setq Val (% (* SubjNum Val) 20201227)))
    Val))

(test 8 (key-loop *CardSubjNum *DemoCardPK))
(test 11 (key-loop *DoorSubjNum *DemoDoorPK))

(test 14897079 (transform *DemoDoorPK 8))
(test 14897079 (transform *DemoCardPK 11))

(de enc-key (PK1 PK2)
  (transform PK1 (key-loop 7 PK2)))

(prinl "EK: " (enc-key *CardPK *DoorPK))
