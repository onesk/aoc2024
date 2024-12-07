;;; -*- lexical-binding: t; -*-

(require 'dash)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-input (input-string)
  (-let* ((lines (split-string input-string "\n" t))
		  (trimmed-lines (--map (-> it (string-trim) (string-to-vector)) lines)))
	(seq-into trimmed-lines 'vector)))

(setq example
      (parse-input
       (mapconcat #'identity
                  '("....#....."
		            ".........#"
		            ".........."
		            "..#......."
			        ".......#.."
			        ".........."
			        ".#..^....."
			        "........#."
			        "#........."
			        "......#...") "\n")))

(defun read-map (map x y)
  (-some--> map
    (when (and (>= x 0) (< x (length it))) (aref it x))
    (when (and (>= y 0) (< y (length it))) (aref it y))))

(defun hits-wall (map x y dx dy)
  (-> map (read-map (+ x dx) (+ y dy)) (equal ?#)))

(defun rotate (dx dy)
  (list dy (- dx)))

(defun loops (map)
  (-let* ((rows (length map))
		  (cols (length (aref map 0)))
		  ((sx sy) (->>
					(-table-flat 'list (-iota rows) (-iota cols))
					(-first (-lambda ((x y)) (equal ?^ (read-map map x y))))))
		  ((dx dy) '(-1 0))
		  (visited (make-hash-table :test 'equal)))

	(while (and (read-map map sx sy) (not (gethash (list sx sy dx dy) visited)))
      (puthash (list sx sy dx dy) t visited)
	  (if (hits-wall map sx sy dx dy)
		  (-setq (dx dy) (rotate dx dy))
        (-setq (sx sy) (list (+ sx dx) (+ sy dy)))))

	(list (read-map map sx sy) visited)))

(defun part1 (map)
  (let ((visited-dirs (cadr (loops map)))
        (visited (make-hash-table :test 'equal)))
    (maphash (-lambda ((sx sy _ _) _) (puthash (list sx sy) t visited)) visited-dirs)
    (hash-table-count visited)))

(defun part2 (map)
  (let ((rows (length map))
        (cols (length (aref map 0)))
        (ans 0)
        ox oy obmap)
    (dolist (obstacle (-table-flat 'list (-iota rows) (-iota cols)) ans)
      (-setq (ox oy) obstacle)
      (when (equal ox 0)
        (message "column: %s" oy)
        (unless (sit-for 0) (keyboard-quit)))
      (when (equal (read-map map ox oy) ?.)
        (setq obmap (--> map (-map 'cl-copy-seq it) (seq-into it 'vector)))
        (aset (aref obmap ox) oy ?#)
        (when (car (loops obmap))
          (cl-incf ans))))))

(native-compile #'part2)
(native-compile #'loops)
(native-compile #'read-map)
(native-compile #'rotate)
(native-compile #'hits-wall)
(setq gc-cons-threshold (* 4096 1024 1024))

(setq input (parse-input (read-input "6.txt")))
(part1 input)
(part2 input)
