(require 'dash)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-input (input-string)
  (-let* (((edges-str paths-str) (split-string input-string "\n\n"))
          (edges (--map (-map #'string-to-number (split-string it "|")) (split-string edges-str "\n")))
          (paths (--map (-map #'string-to-number (split-string it ",")) (split-string paths-str "\n"))))
    (list edges paths)))

(setq example '(((47 53)
                 (97 13)
                 (97 61)
                 (97 47)
                 (75 29)
                 (61 13)
                 (75 53)
                 (29 13)
                 (97 29)
                 (53 29)
                 (61 53)
                 (97 53)
                 (61 29)
                 (47 13)
                 (75 47)
                 (97 75)
                 (47 61)
                 (75 61)
                 (47 29)
                 (75 13)
                 (53 13))

                ((75 47 61 53 29)
                 (97 61 53 29 13)
                 (75 29 13)
                 (75 97 47 61 53)
                 (61 13 29)
                 (97 13 75 29 47))))

(defun backwards? (edges prefix node)
  (->> edges
       (--filter (equal node (car it)))
       (--any? (-elem-index (cadr it) prefix))))

(defun correct-ordering? (edges path)
  (->> (-iota (length path))
       (-any? (lambda (index) (-let [(prefix suffix) (-split-at index path)]
                                (backwards? edges prefix (car suffix)))))
       (not)))

(defun dfs (node)
  (unless (-elem-index node visited)
    (push node visited)
    (dolist (adj (-keep (-lambda ((a b)) (when (equal b node) a)) edges))
      (dfs adj))
    (push node topsorted)))

(defun topsort (edges path)
  (let ((edges (--filter (-let [(a b) it] (and (-elem-index a path) (-elem-index b path))) edges))
        visited
        topsorted)
    (dolist (node path) (dfs node))
    (nreverse topsorted)))

(defun part1 (edges-and-paths)
  (-let [(edges paths) edges-and-paths]
    (->> paths
         (-filter (-partial #'correct-ordering? edges))
         (--map (nth (/ (length it) 2) it))
         (-sum))))

(defun part2 (edges-and-paths)
  (-let [(edges paths) edges-and-paths]
    (->> paths
         (-filter (-not (-partial #'correct-ordering? edges)))
         (-map (-partial #'topsort edges))
         (--map (nth (/ (length it) 2) it))
         (-sum))))

(setq input (parse-input (read-input "5.txt")))
(part1 input)
(part2 input)
