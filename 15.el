(require 'dash)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-map (input-string)
  (-let* ((trimmed (-> input-string (string-trim)
                       (split-string "\n" t)
                       (seq-into 'vector)))
          (height (length trimmed))
          (width (length (aref trimmed 0)))
          (map (-> (-map (lambda (_) (make-vector width nil)) (-iota height))
                   (seq-into 'vector)))
          robot)

    (dotimes (i height)
      (dotimes (j width)
        (setf (aref (aref map i) j)
              (pcase (aref (aref trimmed i) j)
                (?# 'W)
                (?O 'C)
                (?. '_)
                (?@ (setq robot (cons i j))
                    '_)))))

    (list robot height width map)))

(defun parse-dirs (input-string)
  (--> input-string
       (seq-into it 'list)
       (-remove (-cut ->> <> (char-to-string) (string-match "[^<>^v]")) it)
       (-map (-cut alist-get <> '((?< . (0 . -1))
                                  (?> . (0 . +1))
                                  (?^ . (-1 . 0))
                                  (?v . (+1 . 0)))) it)))

(defun parse-input (input-string)
  (-let* (((map-s dirs-s) (-> input-string
                              (string-trim)
                              (split-string "\n\n" t)))
          (map (parse-map map-s))
          (dirs (parse-dirs dirs-s)))
    (list map dirs)))

(defun read-map (map-no-robot i j)
  (-let [(height width grid) map-no-robot]
    (and (>= i 0) (>= j 0) (< i height) (< j width)
         (-> grid
             (aref i)
             (aref j)))))

(defun crates-to-move (map-no-robot coord dir)
  (-let (((ci . cj) coord)
         ((di . dj) dir)
         (crates 0))
    (catch 'found
      (while t
        (pcase (read-map map-no-robot (+ ci (* (1+ crates) di)) (+ cj (* (1+ crates) dj)))
          ('C (cl-incf crates))
          ('W (throw 'found nil))
          ('_ (throw 'found crates)))))))

(defun move-robot (map dir)
  (-let (((robot . map-no-robot) map) steps)
    (when (setq crates (crates-to-move map-no-robot robot dir))
      (-let (((ri . rj) robot)
             ((di . dj) dir)
             ((_ _ grid) map-no-robot))
        (setf (aref (aref grid (+ ri (* (1+ crates) di))) (+ rj (* (1+ crates) dj))) 'C
              (aref (aref grid (+ ri di))                 (+ rj dj))                 '_)
        (cl-incf (caar map) di)
        (cl-incf (cdar map) dj)))))

(defun deep-copy (map)
  (-let [((ri . rj) height width map) map]
    `((,ri . ,rj) ,height ,width ,(--> map (seq-map #'seq-copy it) (seq-into it 'vector)))))

(defun dfs (i j)
  (-let* ((sym-l (read-map map-no-robot i j))
          (sym-r (read-map map-no-robot i (1+ j)))
          ((_ _ grid) map-no-robot)
          (jl (1- j))
          (jr (1+ j))
          (id (+ i di))
          (jd (+ j dj))
          (jdr (+ jr dj)))
    (pcase (cons sym-l sym-r)
      (`(R . ,_)
       (dfs i jl))

      (`(L . R)
       (setf (aref (aref grid i) j)  '_
             (aref (aref grid i) jr) '_)
       (and (dfs id jd) (dfs id jdr)
            (setf (aref (aref grid id) jd)  'L
                  (aref (aref grid id) jdr) 'R)))

      (`(_ . ,_) t)
      (`(W . ,_) nil))))

(defun move-robot-part2 (map dir)
  (-let* ((((ri . rj) . map-no-robot) map)
          ((di . dj) dir)
          (ird (+ ri di))
          (jrd (+ rj dj))
          (fallback (deep-copy map)))
    (if (dfs ird jrd)
        (setf (caar map) ird (cdar map) jrd)
      (setf (car map) (car fallback) (cdr map) (cdr fallback)))))

(defun widen-symbol (sym)
  (pcase sym
    ('W [W W])
    ('C [L R])
    ('_ [_ _])))

(defun widen (map)
  (-let [((ri . rj) height width map) map]
    `((,ri . ,(* 2 rj))
      ,height
      ,(* 2 width)
      ,(--> map
            (seq-map (lambda (row) (seq-mapcat #'widen-symbol row 'vector)) it)
            (seq-into it 'vector)))))

(defun rate-map (map sym)
  (-let* ((map-no-robot (cdr map))
          ((height width _) map-no-robot)
          (ans 0))
    (dotimes (i height)
      (dotimes (j width)
        (when (eq sym (read-map map-no-robot i j))
          (cl-incf ans (+ (* 100 i) j)))))
    ans))

(defun part1 (input)
  (-let* (((map-orig dirs) input)
          (map (deep-copy map-orig)))
    (dolist (dir dirs) (move-robot map dir))
    (rate-map map 'C)))

(defun part2 (input)
  (-let* (((map-orig dirs) input)
          (map (widen map-orig)))
    (dolist (dir dirs) (move-robot-part2 map dir))
    (rate-map map 'L)))

(setq smaller-example (parse-input "
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<"))

(setq larger-example (parse-input "
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"))

(setq input (parse-input (read-input "15.txt")))
(part1 input)
(part2 input)
