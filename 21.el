(require 'dash)
(require 'ht)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-input (input-string)
  (->> (string-split input-string "\n" t) (-map #'string-trim)))

(defun last-numpad (node dir-sym)
  (keypad numeric-keypad #'matcher node dir-sym))

(setq dpad-coords '((?^ (0 . 1))
                    (?A (0 . 2))
                    (?< (1 . 0))
                    (?v (1 . 1))
                    (?> (1 . 2))))

(setq numpad-coords '((?7 (0 . 0))
                      (?8 (0 . 1))
                      (?9 (0 . 2))
                      (?4 (1 . 0))
                      (?5 (1 . 1))
                      (?6 (1 . 2))
                      (?1 (2 . 0))
                      (?2 (2 . 1))
                      (?3 (2 . 2))
                      (?0 (3 . 1))
                      (?A (3 . 2))))

(defun find-button (buttons char)
  (cadr (assoc char buttons)))

(defun both (tuple)
  (-let [(f . s) tuple]
    (list (cons f s) (cons s f))))

(defun signum (x)
  (cond
   ((< x 0) -1)
   ((> x 0) 1)
   (t 0)))

(defun stack (next forbidden memo-sym depth from to)
  (-let* (((fx . fy) from)
          ((tx . ty) to)
          (dx (abs (- tx fx)))
          (dy (abs (- ty fy)))
          (a (find-button dpad-coords ?A))
          (memo-key (list memo-sym depth from to))
          (memo-value (ht-get memo memo-key))
          xm-coord
          ym-coord
          min-cost)

    (catch 'early-return
      (when memo-value
        (throw 'early-return memo-value))

      (when (eql -1 depth)
        (throw 'early-return 0))

      (setq xm-coord (find-button dpad-coords (if (< fx tx) ?v ?^)))
      (setq ym-coord (find-button dpad-coords (if (< fy ty) ?> ?<)))

      (dolist (keyseq (both (cons (list xm-coord dx 0)
                                  (list ym-coord 0 dy))))

        (-let* (((fst fst-dx fst-dy) (car keyseq))
                ((snd snd-dx snd-dy) (cdr keyseq))
                (mid-x (+ fx (* (signum (- tx fx)) fst-dx)))
                (mid-y (+ fy (* (signum (- ty fy)) fst-dy)))
                (prev-coord a)
                (cur-min-cost (+ dx dy)))

          (unless (equal (cons mid-x mid-y) forbidden)
            (unless (zerop (+ fst-dx fst-dy))
              (cl-incf cur-min-cost (funcall next (1- depth) prev-coord fst))
              (setq prev-coord fst))

            (unless (zerop (+ snd-dx snd-dy))
              (cl-incf cur-min-cost (funcall next (1- depth) prev-coord snd))
              (setq prev-coord snd))

            (unless (equal prev-coord a)
              (cl-incf cur-min-cost (funcall next (1- depth) prev-coord a)))

            (when (or (not min-cost) (< cur-min-cost min-cost))
              (setq min-cost cur-min-cost)))))

      (ht-set! memo memo-key min-cost)
      min-cost)))

(defun dpad-stack (depth from to)
  (stack #'dpad-stack '(0 . 0) :dpad-stack depth from to))

(defun numpad-stack (depth from to)
  (stack #'dpad-stack '(3 . 0) :numpad-stack depth from to))

(defun numpad-seq (depth char-seq)
  (-let* ((memo (ht))
          (ans 0)
          (prev (find-button numpad-coords ?A))
          cur)

    (dolist (char char-seq)
      (setq cur (find-button numpad-coords char))
      (cl-incf ans (1+ (numpad-stack depth prev cur)))
      (setq prev cur))

    ans))

(defun solve (depth input)
  (let ((ans 0) sequence-len char-list num-value)
    (dolist (line input)
      (setq num-value (string-to-number (substring line 0 -1)))
      (setq char-list (string-to-list line))
      (setq sequence-len (numpad-seq depth char-list))
      (cl-incf ans (* num-value sequence-len)))
    ans))

(defun part1 (input)
  (solve 2 input))

(defun part2 (input)
  (solve 25 input))

(setq example (parse-input "
029A
980A
179A
456A
379A"))

(setq input (parse-input (read-input "21.txt")))
(part1 input)
(part2 input)
