(require 'dash)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-input (input-string)
  (-let* ((lines (--> input-string (split-string it "\n" t) (-map #'string-trim it)))
          (rows (length lines))
          (cols (length (nth 0 lines))))
    (--> lines
         (-map #'string-to-vector it)
         (-map (-partial #'-map (-rpartial #'- ?0)) it)
         (-map (-rpartial #'seq-into 'vector) it)
         (list rows cols (seq-into it 'vector)))))

(defun make-queue () (cons nil nil))

(defun push-queue (elt queue)
  (cond
   ((and (not (car queue)) (not (car queue)))
    (setf (car queue) (list elt))
    (setf (cdr queue) (car queue)))
   (t (setf (cdr (cdr queue)) (list elt))
      (setf (cdr queue) (cdr (cdr queue))))))

(defun pop-queue (queue)
  (let ((head (caar queue)))
    (if (eq (car queue) (cdr queue))
        (progn
          (setf (car queue) nil)
          (setf (cdr queue) nil))
      (setf (car queue) (cdar queue)))
    head))

(defun read-grid (grid x y)
  (-let [(rows cols matrix) grid]
    (and (>= x 0) (>= y 0) (< x rows) (< y cols)
         (-> matrix
             (aref x)
             (aref y)))))

(defun trailhead-score-and-rating (grid sx sy)
  (let ((queue (make-queue))
        (score 0)
        (rating 0)
        (dp (make-hash-table :test #'equal)))

    (push-queue (cons sx sy) queue)
    (puthash (cons sx sy) 1 dp)

    (while (car queue)
      (-let* ((cc (pop-queue queue))
              ((cx . cy) cc)
              (ch (read-grid grid cx cy))
              (cdp (gethash cc dp)))

        (when (eql ch 9)
          (cl-incf score)
          (cl-incf rating cdp))

        (dolist (dir '((1 0) (0 -1) (-1 0) (0 1)))
          (-let* (((dx dy) dir)
                  (nx (+ cx dx))
                  (ny (+ cy dy))
                  (nh (read-grid grid nx ny))
                  (nc (cons nx ny))
                  (ndp (gethash nc dp 0)))

            (when (eql nh (1+ ch))
              (puthash nc (+ ndp cdp) dp)
              (when (eql ndp 0)
                (push-queue nc queue)))))))

    (list score rating)))

(defun score-and-rating (grid)
  (-let (((rows cols _) grid)
         (score 0)
         (rating 0))
    (dolist (start (-table-flat #'cons (-iota rows) (-iota cols)) (list score rating))
      (-let [(sx . sy) start]
        (when (eql 0 (read-grid grid sx sy))
          (-let [(cur-score cur-rating) (trailhead-score-and-rating grid sx sy)]
            (cl-incf score cur-score)
            (cl-incf rating cur-rating)))))))

(defun part1 (grid)
  (-> grid (score-and-rating) (car)))

(defun part2 (grid)
  (-> grid (score-and-rating) (cadr)))

(setq example
      (parse-input
       (mapconcat #'identity '("89010123"
                               "78121874"
                               "87430965"
                               "96549874"
                               "45678903"
                               "32019012"
                               "01329801"
                               "10456732") "\n")))

(setq input (parse-input (read-input "10.txt")))
(part1 input)
(part2 input)
