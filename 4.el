(require 'dash)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-input (input-string)
  (--> input-string (split-string it "\n" t)
       (-map #'string-trim it)))

(setq example '("MMMSXXMASM"
                "MSAMXMSMSA"
                "AMXSXMAAMM"
                "MSAMASMSMX"
                "XMASAMXAMM"
                "XXAMMXXAMA"
                "SMSMSASXSS"
                "SAXAMASAAA"
                "MAMMMXMMMM"
                "MXMXAXMASX"))

(setq xmas (string-to-list "XMAS"))
(setq dirs (-remove-item '(0 0) (-table-flat 'list '(-1 0 1) '(-1 0 1))))

(defun get-char (matrix a b)
  (-some--> matrix
    (when (and (>= a 0) (< a (length it))) (nth a it))
    (when (and (>= b 0) (< b (length it))) (aref it b))))

(defun xmas? (matrix start dir)
  (-let (((sx sy) start)
         ((dx dy) dir))
    (-all? #'identity
           (--map-indexed
            (equal it
                   (get-char matrix
                             (+ sx (* it-index dx))
                             (+ sy (* it-index dy))))
            xmas))))

(defun x-mas? (matrix start)
  (-let* (((sx sy) start)
          (chars (--map (-let [(dx dy) it] (get-char matrix (+ sx dx) (+ sy dy)))
                              '((0 0) (2 0) (1 1) (0 2) (2 2)))))
    (pcase chars
      (`(?M ?M ?A ?S ?S) t)
      (`(?S ?S ?A ?M ?M) t)
      (`(?M ?S ?A ?M ?S) t)
      (`(?S ?M ?A ?S ?M) t)
      (_ nil))))

(defun part1 (input)
  (let ((rows (length input))
        (cols (length (nth 0 input)))
        (ans 0))
    (dolist (start (-table-flat 'list (-iota rows) (-iota cols)) ans)
      (-let [matches (-count (-partial #'xmas? input start) dirs)]
        (setq ans (+ matches ans))))))

(defun part2 (input)
  (let ((rows (length input))
        (cols (length (nth 0 input))))
    (-count (-partial #'x-mas? input) (-table-flat 'list (-iota rows) (-iota cols)))))

(setq input (parse-input (read-input "4.txt")))
(part1 input)
(part2 input)
