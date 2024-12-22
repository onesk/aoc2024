(require 'dash)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-input (input-string)
  (->> (split-string input-string "\n" t)
       (-map #'string-to-number)))

(defun hash-step (secret)
  (-reduce-from
   (lambda (s shift) (logand #xffffff (logxor (lsh s shift) s)))
   secret '(6 -5 11)))

(defun part1 (input)
  (->> input
       (--map (-last-item (-iterate #'hash-step it (1+ 2000))))
       (-sum)))

(defun part2 (input)
  (let ((totals (ht)) prices consecutives freq-key bananas)
    (-each-indexed input
      (lambda (i secret)
        (setq prices
              (->> (-iterate #'hash-step secret (1+ 2000))
                   (--map (% it 10))))

        (setq consecutives
              (--> prices
                   (-zip-lists it (cdr it))
                   (-reductions-from
                    (-lambda (deltas (prev cur)) (-take 4 (cons (- cur prev) deltas)))
                    nil it)))

        (dolist (price-consecs (-zip-pair prices consecutives))
          (-let* (((price . consecs) price-consecs)
                  (totals-key (cons i consecs)))
            (unless (ht-get totals totals-key)
              (ht-set! totals totals-key price))))))

    (setq bananas (ht))
    (ht-aeach (ht-update-with! bananas (cdr key) (-partial #'+ value) 0) totals)

    (->> bananas
         (ht->alist)
         (--keep (and (eql 4 (length (car it))) (cdr it)))
         (-max))))

(setq example (parse-input "
1
10
100
2024"))

(setq example2 (parse-input "
1
2
3
2024"))

(setq input (parse-input (read-input "22.txt")))
(part1 input)
(part2 input)
