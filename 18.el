(require 'dash)
(require 's)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-input (input-string)
  (->> (split-string input-string "\n" t)
       (--map (-> it
                  (string-trim)
                  (split-string "," t)
                  ((lambda (coords) (-map #'string-to-number coords)))))))

(defun adjacent (coord)
  (-let [(x y) coord]
    (-map (-lambda ((dx dy)) (list (+ x dx) (+ y dy))) '((0 1) (-1 0) (0 -1) (1 0)))))

(defun find-path (w h sx sy ex ey obstacles)
  (let* ((steps 0)
         (reached `((,sx ,sy)))
         (dirs '((0 1) (-1 0) (0 -1) (1 0)))
         (prev-reached 0))
    (while (and (not (-contains? reached (list ex ey)))
                (> (length reached) prev-reached))
      (cl-incf steps)

      (setq prev-reached (length reached)
            reached (append reached (-mapcat #'adjacent reached)))

      (setq reached (-> reached (-distinct) (-difference obstacles)))
      (setq reached (->> reached (-filter (-lambda ((x y)) (and (>= x 0) (>= y 0) (<= x w) (<= y h)))))))

    (when (-contains? reached (list ex ey)) steps)))

;; (find-path 6 6 0 0 6 6 (-take 12 example))

(defun part1 (input)
  (-let [size 70]
    (find-path size size 0 0 size size (-take 1024 input))))

(defun part2 (input)
  (-let [size 70]
    (let ((low 0) (high (1- (* size size))) mid ans)
      (while (<= low high)
        (setq mid (/ (+ low high) 2))
        (if (not (find-path size size 0 0 size size (-take mid input)))
            (setq high (1- mid))
          (setq ans mid low (1+ mid))))
      (-let [(x y) (nth ans input)] (format "%s,%s" x y)))))

(setq example (parse-input "
5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0"))

(setq input (parse-input (read-input "18.txt")))
(part1 input)
(part2 input)
