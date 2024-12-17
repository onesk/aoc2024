(require 'dash)
(require 'ht)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-input (input-string)
  (-let* ((trimmed (-> input-string (string-trim)
                       (split-string "\n" t)
                       (seq-into 'vector)))
          (height (length trimmed))
          (width (length (aref trimmed 0)))
          (map (-> (-map (lambda (_) (make-vector width nil)) (-iota height))
                   (seq-into 'vector)))
          start end)

    (dotimes (i height)
      (dotimes (j width)
        (setf (aref (aref map i) j)
              (pcase (aref (aref trimmed i) j)
                (?# 'W)
                (?. '_)
                (?S (setq start (cons i j))
                    '_)
                (?E (setq end (cons i j))
                    '_)))))

    (list start end height width map)))

(defun read-map (map-no-markers i j)
  (-let [(height width grid) map-no-markers]
    (and (>= i 0) (>= j 0) (< i height) (< j width)
         (-> grid
             (aref i)
             (aref j)))))

(defun dijkstra (map start-node)
  (-let* ((visited (ht))
          (paths (ht (start-node 0)))
          (inf (expt 10 10))
          shortest-path)

    (while (setq shortest-path (-some->> paths (ht-items) (-min-by (-on #'> #'cadr))))
      (-let* (((node path-len) shortest-path)
              ((cx cy dx dy) node)
              (move-key (list (+ cx dx) (+ cy dy) dx dy))
              (cw-rotate-key (list cx cy dy (- dx)))
              (ccw-rotate-key (list cx cy (- dy) dx)))

        (ht-remove! paths node)
        (ht-set! visited node path-len)

        (when (and (not (ht-contains? visited move-key))
                   (equal '_ (read-map map-no-markers (+ cx dx) (+ cy dy))))
          (ht-update-with! paths move-key (-partial #'min (1+ path-len)) inf))

        (dolist (key (list cw-rotate-key ccw-rotate-key))
          (unless (ht-contains? visited key)
            (ht-update-with! paths key (-partial #'min (+ 1000 path-len)) inf)))))

    visited))

(defun part1 (map)
  (-let* ((((sx . sy) (ex . ey) . map-no-markers) map)
          (final-nodes (--map (append (list ex ey) it) '((0 1) (-1 0) (0 -1) (1 0)))))
    (->> (dijkstra map-no-markers (list sx sy 0 1))
         ((lambda (visited) (--map (ht-get visited it) final-nodes)))
         (-non-nil)
         (-min))))

(defun part2 (map)
  (-let* ((((sx . sy) (ex . ey) . map-no-markers) map)
          (forward-final-nodes (--map (append (list ex ey) it) '((0 1) (-1 0) (0 -1) (1 0))))
          (inf (expt 10 20))
          forward backward best-path fdx fdy both ans)

    (setq forward (dijkstra map-no-markers (list sx sy 0 1)))

    (-setq ((_ _ fdx fdy) . best-path)
      (->>
       (--map (cons it (ht-get forward it)) forward-final-nodes)
       (--keep (and (cdr it) it))
       (-min-by (-on #'> #'cdr))))

    (setq backward (dijkstra map-no-markers (list ex ey (- fdx) (- fdy))))

    (setq both (ht))
    (dolist (kv (ht-items forward))
      (-let* ((((cx cy dx dy) forward-path) kv)
              (backward-path (ht-get backward (list cx cy (- dx) (- dy)) inf)))
        (when (eql best-path (+ forward-path backward-path))
          (ht-set both (list cx cy) t))))

    (ht-size both)))

(setq first-input (parse-input "
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############"))

(setq second-input (parse-input "
#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################"))

(setq input (parse-input (read-input "16.txt")))
(part1 input)
(part2 input)
