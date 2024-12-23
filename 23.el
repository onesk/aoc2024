(require 'dash)
(require 's)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-input (input-string)
  (->> (split-string input-string "\n" t)
       (-map (lambda (line) (split-string line "-")))))

(defun make-graph (edges)
  (let ((adj (ht)))
    (dolist (edge edges)
      (-let [(a b) edge]
        (ht-set! adj a (cons b (ht-get adj a)))
        (ht-set! adj b (cons a (ht-get adj b)))))
    adj))

(defun part1 (edges)
  (let* ((graph (-> (make-graph edges) (ht->alist) (seq-into 'vector)))
         (n (length graph))
         (ans 0)
         na nb nc)
    (dolist (i (-iota (- n 2)))
      (dolist (j (-iota (- n i 1) (1+ i)))
        (dolist (k (-iota (- n j 1) (1+ j)))
          (setq na (aref graph i)
                nb (aref graph j)
                nc (aref graph k))
          (when (and (-contains? (cdr na) (car nb))
                     (-contains? (cdr na) (car nc))
                     (-contains? (cdr nb) (car nc))
                     (--any? (s-starts-with? "t" (car it)) (list na nb nc)))
            (cl-incf ans)))))
    ans))

(defun max-clique (candidates cur-clique culled)
  (if (and (null candidates) (null culled))
      (when (length> cur-clique (length max-clique))
        (setq max-clique cur-clique))
    (dolist (suffix (-drop-last 1 (-tails candidates)))
      (-let* ((node (car suffix))
              (neighbours (ht-get graph node)))
        (max-clique (-intersection neighbours suffix)
                    (cons node cur-clique)
                    (-intersection neighbours culled))
        (push node culled)))))

(defun part2 (edges)
  (let* ((graph (make-graph edges))
         (all-vertices (->> graph (ht->alist) (-map #'car)))
         max-clique)
    (max-clique all-vertices nil nil)
    (s-join "," (-sort #'string< max-clique))))

(setq example (parse-input "
kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn
"))

(setq input (parse-input (read-input "23.txt")))
(part1 input)
(part2 input)
