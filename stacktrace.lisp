(in-package :sentry-client)

(defun split-lines (string)
  (uiop:split-string string :separator '(#\newline)))

(defstruct buffer
  content
  (position 0))

(defun load-location-buffer (location-buffer)
  (alexandria:destructuring-case location-buffer
    ((:file filename)
     (make-buffer :content (alexandria:read-file-into-string filename)))
    ((:source-form content)
     (make-buffer :content content))))

(defun move-to-position (buffer position)
  (alexandria:destructuring-case position
    ((:position pos)
     (setf (buffer-position buffer)
           ;; Since pos is in bytes, it is converted to character units.
           (length
            (babel:octets-to-string
             (subseq (babel:string-to-octets (buffer-content buffer))
                     0
                     pos))))
     t)))

(defun current-line-number (buffer)
  (1+ (count #\newline
             (buffer-content buffer)
             :end (buffer-position buffer))))

(defun get-beginning-of-line-pos (buffer &optional (pos (buffer-position buffer)))
  (let ((content (buffer-content buffer)))
    (loop
      (when (= pos 0)
        (return))
      (decf pos)
      (when (char= #\newline (char content pos))
        (incf pos)
        (return)))
    pos))

(defun get-end-of-line-pos (buffer &optional (pos (buffer-position buffer)))
  (let ((content (buffer-content buffer)))
    (loop
      (when (= pos (length content))
        (return))
      (when (char= #\newline (char content pos))
        (return))
      (incf pos))
    pos))

(defun get-previous-line-position (buffer n)
  (let ((pos (buffer-position buffer)))
    (loop :repeat n
          :do (let ((start-pos (get-beginning-of-line-pos buffer pos)))
                (if (= start-pos 0)
                    (return start-pos)
                    (setf pos (1- start-pos))))
          :finally (return pos))))

(defun get-next-line-position (buffer n)
  (let ((pos (buffer-position buffer)))
    (loop :repeat n
          :do (let ((end-pos (get-end-of-line-pos buffer pos)))
                (if (= end-pos (length (buffer-content buffer)))
                    (return end-pos)
                    (setf pos (1+ end-pos))))
          :finally (return pos))))

(defun line-string (buffer)
  (subseq (buffer-content buffer)
          (get-beginning-of-line-pos buffer)
          (get-end-of-line-pos buffer)))

(defun get-pre-lines (buffer n)
  (let ((lines (split-lines (subseq (buffer-content buffer)
                                    0
                                    (buffer-position buffer)))))
    (butlast (last lines n))))

(defun get-post-lines (buffer n)
  (let ((lines (split-lines (subseq (buffer-content buffer)
                                    (buffer-position buffer)))))
    (subseq lines 1
            (if (< (1+ n) (length lines))
                (1+ n)
                nil))))

(defun get-source-contexts (source-location)
  (alexandria:destructuring-case source-location
    ((:location location-buffer position hints)
     (declare (ignore hints))
     (let ((buffer (load-location-buffer location-buffer)))
       (when (and buffer (move-to-position buffer position))
         (values (line-string buffer)
                 (current-line-number buffer)
                 (get-pre-lines buffer 5)
                 (get-post-lines buffer 5)))))))

(defun get-frame-vars (index)
  (loop :for local :in (swank::frame-locals-for-emacs index)
        :collect (cons (getf local :name)
                       (getf local :value))))

(defun get-frame-function (frame)
  (alexandria:if-let
      (fun (sb-di:debug-fun-fun (sb-di:frame-debug-fun frame)))
    (princ-to-string (swank/backend:function-name fun))
    (swank::frame-to-string frame)))

(defun get-source-location-filename (source-location)
  (alexandria:when-let* ((location (getf source-location :location))
                         (filename (getf location :file)))
    filename))

(defstruct stack-frame
  filename
  function
  pre-context
  context-line
  post-context
  line-number
  vars)

(defun collect-stacktrace ()
  (swank/backend:call-with-debugging-environment
   (lambda ()
     (let ((frames (swank/backend:compute-backtrace 0 nil)))
       (loop :for frame :in frames
             :for i :from 0
             :for source-location := (swank:frame-source-location i)
             :collect (multiple-value-bind (context-line line-number pre-context post-context)
                          (get-source-contexts source-location)
                        (make-stack-frame
                         :filename (get-source-location-filename source-location)
                         :function (get-frame-function frame)
                         :pre-context pre-context
                         :context-line context-line
                         :post-context post-context
                         :line-number line-number
                         :vars (get-frame-vars i))))))))

(defun stack-frame-to-json-object (stack-frame)
  `(("function" . ,(stack-frame-function stack-frame))
    ("filename" . ,(stack-frame-filename stack-frame))
    ("lineno" . ,(stack-frame-line-number stack-frame))
    ("pre_context" . ,(coerce (stack-frame-pre-context stack-frame) 'vector))
    ("context_line" . ,(stack-frame-context-line stack-frame))
    ("post_context" . ,(coerce (stack-frame-post-context stack-frame) 'vector))
    ("vars" . ,(stack-frame-vars stack-frame))))

(defun encode-sbcl-stacktrace (json-stream)
  (let ((frames (collect-stacktrace)))
    (json:encode-json `(("frames" . ,(map 'vector
                                          #'stack-frame-to-json-object
                                          (reverse frames))))
                      json-stream)))
