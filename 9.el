(require 'dash)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (string-trim (buffer-string))))

(defun rle-decode (disk-map spans)
  (let ((digits (->> disk-map (string-to-vector) (-map (-rpartial #'- ?0))))
        (offset 0)
        (id 0)
        (file? t)
        free
        files)
    (dolist (len digits)
      (pcase spans
       (:spans (if file?
                   (push (list offset len id) files)
                 (push (list offset len) free))
               (cl-incf offset len))
       (:cells (dotimes (_ len)
                 (if file?
                     (push (cons offset id) files)
                   (push offset free))
                 (cl-incf offset))))
      (when (setq file? (not file?))
          (cl-incf id)))

    (list (nreverse free) files)))

(defun compact (cells)
  (-let (((free files) cells)
         compacted)
    (while (and (consp free) (consp files) (< (car free) (caar files)))
      (push (cons (car free) (cdar files)) compacted)
      (setq free (cdr free))
      (setq files (cdr files)))
    (->> (-concat compacted files)
         (-sort (-on #'< #'car)))))

(defun move-file (free len)
  (pcase free
    ((and `((,offset ,samelen) . ,rest) (guard (eql len samelen)))
     (cons rest offset))
    ((and `((,offset ,biglen) . ,rest) (guard (< len biglen)))
     (cons (cons (list (+ offset len) (- biglen len)) rest) offset))
    (`(,span . ,rest)
     (-let [(free . offset) (move-file rest len)]
       (cons (cons span free) offset)))))

(defun compact-nofrag (spans)
  (-let (((free files) spans)
         moved)
    (dolist (file files moved)
      (-let* (((offset len id) file)
              dest-offset)
        (-setq (free . dest-offset) (move-file free len))
        (unless (and dest-offset (< dest-offset offset))
          (setq dest-offset offset))
        (dotimes (_ len)
          (push (cons dest-offset id) moved)
          (cl-incf dest-offset))))))

(defun checksum (compacted-map)
  (->> compacted-map
       (-map (-lambda ((offs . id)) (* offs id)))
       (-sum)))

(defun part1 (input)
  (-> input (rle-decode :cells) (compact) (checksum)))

(defun part2 (input)
  (-> input (rle-decode :spans) (compact-nofrag) (checksum)))

(setq example "2333133121414131402")

(setq max-lisp-eval-depth 40000)
(setq input (read-input "9.txt"))
(part1 input)
(part2 input)
