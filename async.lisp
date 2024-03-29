(defpackage :sentry-client.async
  (:use :cl :sentry-client)
  (:export :initialize-async-sentry-client))

(in-package :sentry-client.async)

(defclass async-sentry-client (sentry-client)
  ()
  (:documentation "A sentry client that sends errors to Sentry asynchronously, using a background task."))

(defvar *tasks-runner* nil
  "The task runner")
(defvar *tasks-thread* nil
  "The current runner thread")

(defun maybe-initialize-tasks ()
  "Initialize the background tasks if they are not already."
  (when (null *tasks-runner*)
    (setf *tasks-runner* (make-instance 'simple-tasks:queued-runner))
    (setf *tasks-thread* (simple-tasks:make-runner-thread *tasks-runner*))))

(defun initialize-async-sentry-client (dsn &rest args)
  "Initialize an async sentry client instance."
  (setf *sentry-client*
        (apply #'make-instance 'async-sentry-client :dsn dsn args)))

(defmethod initialize-instance :after ((sentry-client async-sentry-client) &rest initargs)
  (declare (ignore initargs))
  ;; Start background tasks for async client
  (maybe-initialize-tasks))

(defmethod sentry-client::client-capture-exception ((sentry-client async-sentry-client) condition &key extras transaction)
  (when (not *tasks-runner*)
    (error "Sentry async task runner not running"))
  (let ((json (sentry-client::encode-exception-event
               condition
               :sentry-client sentry-client
               :extras extras
               :transaction transaction)))
    (let ((task (make-instance 'simple-tasks:call-task
                               :func (lambda ()
                                       (sentry-client::post-sentry-request json sentry-client)))))
      (simple-tasks:schedule-task task *tasks-runner*))))
