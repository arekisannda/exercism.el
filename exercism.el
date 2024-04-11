;;; exercism.el --- Exercism Client -*- lexical-binding: t -*-

;; Author: Alexander Chan <alex@arekisannda.dev>
;; Maintainer: Alexander Chan
;; Package-Requires: ((emacs "26.1") (aio "1.0"))
;; Version: 0.1.0

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; exercism.el is an unofficial Exercism client.
;; Uses `exercism` cli tool.

;;; Code:

(require 'simple)
(require 'json)
(require 'seq)

(require 'aio)
(require 'persist)
(require 'log4e)

(log4e:deflogger "exercism" "[exercism] %t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                                 (error . "error")
                                                                 (warn  . "warn")
                                                                 (info  . "info")
                                                                 (debug . "debug")
                                                                 (trace . "trace")))
(setq log4e--exercism-log "*exercism-log*")

(defgroup exercism nil
  "Exercism client."
  :prefix 'exercism-
  :group 'tools)

(defgroup exercism-faces nil
  "Exercism client faces."
  :group 'exercism
  :group 'faces)

(defface exercism-easy-face
  '((t (:foreground "green")))
  "Face for easy exercises."
  :group 'exercism-faces)

(defface exercism-medium-face
  '((t (:foreground "yellow")))
  "Face for medium exercises."
  :group 'exercism-faces)

(defface exercism-hard-face
  '((t (:foreground "red")))
  "Face for hard exercises."
  :group 'exercism-faces)

;; API URL
(defconst exercism--url-base "https://exercism.org")
(defconst exercism--url-api (concat exercism--url-base "/api/v2"))
(defconst exercism--url-track-api (concat exercism--url-api "/tracks"))
(defconst exercism--url-solutions-api (concat exercism--url-api "/solutions"))
(defconst exercism--url-fmt-track-exercises-api (concat exercism--url-track-api "/%s/exercises"))
(defconst exercism--url-solutions-api (concat exercism--url-api "/solutions"))
(defconst exercism--url-fmt-solutions-complete-api (concat exercism--url-solutions-api "/%s/complete"))
(defconst exercism--url-fmt-solutions-publish-api (concat exercism--url-solutions-api "/%s/publish"))
(defconst exercism--url-fmt-solutions-unpublish-api (concat exercism--url-solutions-api "/%s/unpublish"))

(defcustom exercism-cmd "exercism"
  "Exercism command."
  :type 'string
  :group 'exercism)

(defcustom exercism-enable-log-to-message-buffer nil
  "Enable Exercism log dump to w*Messages*`."
  :type 'boolean
  :group 'exercise)

(defcustom exercism-open-url-on-submit t
  "Flag indicating wheter to open url in browser on submit."
  :type 'boolean
  :group 'exercise)

(defcustom exercism-config-file-path (expand-file-name "~/.config/exercism/user.json")
  "Exercism configuration file path."
  :type 'string
  :group 'exercism)

(defcustom exercism-workspace-path (expand-file-name "~/exercism")
  "Path for saving Exercism exercises."
  :type 'string
  :group 'exercism)

(defcustom exercism-result-buffer-name "*exercism result*"
  "Buffer name for exercise test."
  :type 'string
  :group 'exercism)

(defvar exercism--api-token)
(defvar exercism--workspace)
(defvar exercism--exercise-metadata nil "Current exercise metadata.")
(defvar exercism--exercise-files nil "Current exercise files.")

(persist-defvar exercism--current-track nil "Current track.")

;; Window/Layout Management
(defvar exercism--description-window nil
  "Holds the reference to description window.")

(defvar exercism--test-window nil
  "Holds the reference to test result window.")

(defvar exercism--code-window nil
  "Holds the reference to code buffer window.")

(defun exercism-layout ()
  "Layout for solving exercises."
  (delete-other-windows)
  (split-window-right)
  (setq exercism--description-window (selected-window))
  (setq exercism--result-window (split-window-below))
  (other-window 1)
  (other-window 1)
  (setq exercism--code-window (selected-window)))

(defun exercism--display-description (buffer &optional alist)
  "Display function for exercise description.
BUFFER is the exercise description.  ALIST is a combined alist
specified in `display-buffer-alist'."
  (set-window-buffer exercism--description-window buffer)
  exercism--description-window)

(defun exercism--display-result (buffer &optional alist)
  "Display function for exercise test result.
BUFFER is the exercise test result.  ALIST is a combined alist
specified in `display-buffer-alist'."
  (set-window-buffer exercism--result-window buffer)
  exercism--result-window)

(defun exercism--display-code (buffer &optional alist)
  "Display function for exercise code.
BUFFER is the exercise code.  ALIST is a combined alist
specified in `display-buffer-alist'."
  (set-window-buffer exercism--code-window buffer)
  exercism--code-window)

(defun exercism--authorization (value)
  "Return an alist as the HTTP Authorization header.
VALUE is the authorization value."
  (cons "Authorization" value))

(defun exercism--authorization-bearer (value)
  "Return bearer string for HTTP Authorization header.
VALUE is the authorization bearer value."
  (format "Bearer %s" value))

;;;###autoload
(defun exercism-toggle-debug ()
  "Toggle debug."
  (interactive)
  (if (exercism--log-debugging-p)
      (progn
        (exercism--log-set-level 'info)
        (exercism--log-disable-debugging)
        (message "exercism disable debug"))
    (progn
      (exercism--log-set-level 'debug)
      (exercism--log-enable-debugging)
      (message "exercism enable debug"))))

(aio-defun exercism--run-command (cmd)
  "Asynchronously run exercism shell CMD."
  (shell-command-to-string cmd))

(defun exercism--setup (&optional config-file-path)
  "Set up exercism client.
Reading from CONFIG-FILE-PATH if provided."
  (unless (file-exists-p config-file-path)
    (exercism--error "Failed to load config: invalid file"))

  (let* ((json-object-type 'hash-table)
         (user-config (json-read-file config-file-path)))
    (setq exercism--api-token (gethash "token" user-config))
    (setq exercism--workspace (gethash "workspace" user-config))
    (exercism--debug "Successfully configured client.")))

;;;###autoload
(defun exercism-setup ()
  "Setup exercism client."
  (interactive)
  (when exercism-enable-log-to-message-buffer
    (exercism--log-enable-messaging))
  (exercism--setup (expand-file-name exercism-config-file-path))
  (exercism--info "Exercism client initialized"))

(defun exercism-configure (&optional api-token)
  "Configure exercism with API-TOKEN."
  (interactive)

  (if api-token
      (setq exercism--api-token api-token)
    (setq exercism--api-token (read-string "API Token: ")))

  (aio-await (exercism--run-command
              (concat (executable-find exercism-cmd)
                      " configure"
                      " --token=" (shell-quote-argument exercism--api-token)
                      " --workspace=" (shell-quote-argument exercism-workspace-path))))

  (exercism-setup (expand-file-name exercism-config-file-path)))

(defun exercism-start-coding ()
  "Set up Exercism coding layout as defined in`exercism-layout`."
  (interactive)

  (unless exercism--workspace (exercism-setup))
  (unless exercism--current-track (aio-await (exercism-set-track)))
  (unless (and exercism--exercise-metadata exercism--exercise-files)
    (aio-await (exercism-set-exercise)))

  (exercism-layout)

  (let* ((track (gethash "track" exercism--exercise-metadata))
         (exercise (gethash "exercise" exercism--exercise-metadata))
         (track-dir (expand-file-name track exercism--workspace))
         (exercise-dir (expand-file-name exercise track-dir))
         (description-buffer (find-file-noselect (expand-file-name "README.md" exercise-dir)))
         (test-buffer (get-buffer-create exercism-result-buffer-name))
         ;; use first file in solution list
         ;; TODO: improve select file logic
         (solution-file-name (aref (gethash "solution" exercism--exercise-files) 0))
         (code-buffer (find-file-noselect (expand-file-name solution-file-name exercise-dir))))

    (with-current-buffer description-buffer
      (markdown-view-mode))
    (display-buffer description-buffer
                    '((display-buffer-reuse-window
                       exercism--display-description)
                      (reusable-frames . visible)))

    (with-current-buffer test-buffer
      (erase-buffer))
    (display-buffer test-buffer
                    '((display-buffer-reuse-window
                       exercism--display-result)
                      (reusable-frames . visible)))

    (display-buffer code-buffer
                    '((display-buffer-reuse-window
                       exercism--display-code)
                      (reusable-frames . visible)))))

(defun exercism--add-font-lock (str face)
  "Add property FACE to STR."
  (prog1 str (add-face-text-property 0 (length str) face nil str)))

(defun exercism--format-difficulty-string (difficulty)
  "Return propertized DIFFICULTY string."
  (let* ((difficulty-tags '("easy" "medium" "hard"))
         (width (seq-reduce (lambda (a s) (max a (length s))) difficulty-tags 0)))
    (exercism--add-font-lock (s-pad-right width " " difficulty)
                             (cond
                              ((equal difficulty "easy") 'exercism-easy-face)
                              ((equal difficulty "medium") 'exercism-medium-face)
                              ((equal difficulty "hard") 'exercism-hard-face)
                              (t 'default)))))

(defun exercism--exercise-annotation-fn (exercise)
  "Annotate EXERCISE option with the difficulty and description."
  ;; `minibuffer-completion-table` entries should be in formation of '(exercise-lable option)
  (let* ((option (car (last (assoc exercise minibuffer-completion-table))))
         (difficulty (gethash "difficulty" option))
         (blurb (gethash "blurb" option)))
    (concat " " (exercism--format-difficulty-string difficulty)
            "   " (exercism--add-font-lock blurb 'font-lock-comment-face))))

(aio-defun exercism--download-exercise (track exercise &optional force-download)
  "Download exercise locally using TRACK and EXERCISE."
  (unless exercism--workspace (exercism-setup))

  (if-let* ((workspace exercism--workspace)
            (track-dir (expand-file-name track workspace))
            (exercise-dir (expand-file-name exercise track-dir))
            (perform-download (or force-download (not (file-exists-p exercise-dir))))
            (result (aio-await (exercism--run-command
                                (concat (executable-find exercism-cmd)
                                        " download"
                                        " --track=" (shell-quote-argument track)
                                        " --exercise=" (shell-quote-argument exercise)
                                        " --force")))))
      (if (string-match "^Error:.*" result)
          (progn
            (exercism-debug "Download failed: %s" result)
            (list :error result))
        (exercism--debug "Download result: %s" result)
        exercise-dir)
    (exercism--debug "Exercise already exists, skipping download")
    exercise-dir))

(defun exercism--get-exercise-files (exercise-dir)
  "Return hash-table containing file information under EXERCISE-DIR."
  (let* ((json-object-type 'hash-table)
         (exercise-config-path (expand-file-name ".exercism/config.json" exercise-dir)))
    (when (file-exists-p exercise-config-path)
      (let ((config (json-read-file exercise-config-path)))
        (gethash "files" config)))))

(defun exercism--get-exercise-metadata (exercise-dir)
  "Return hash-table containing metadata information under EXERCISE-DIR."
  (let* ((json-object-type 'hash-table)
         (exercise-metadata-path (expand-file-name ".exercism/metadata.json" exercise-dir)))
    (when (file-exists-p exercise-metadata-path)
      (json-read-file exercise-metadata-path))))

(aio-defun exercism--exercise-setup (track exercise)
  "Set up EXERCISE for the selected TRACK, downloading the exercise if it does not exist."
  ;; result: error or path to exercise directory
  (let* ((result (aio-await (exercism--download-exercise track exercise))))

    (if-let ((error-info (plist-get result :error)))
        (progn
          (setq exercism--exercise-metadata nil)
          (setq exercism--exercise-files nil)
          result)

      (setq exercism--exercise-metadata (exercism--get-exercise-metadata result))
      (setq exercism--exercise-files (exercism--get-exercise-files result)))))

(aio-defun exercism--list-exercises (track)
  "List exercises for the currently selected TRACK."
  (let* ((json-object-type 'hash-table)
         (url-request-method "GET")
         (api-url (format exercism--url-fmt-track-exercises-api track))
         (result (aio-await (aio-url-retrieve api-url))))

    (if-let ((error-info (plist-get (car result) :error)))
        (progn
          (switch-to-buffer (cdr result))
          (exercism--error "List exercises failed: %S" error-info))

      (exercism--debug "Successfully retrieved available exercises")
      (with-current-buffer (cdr result)
        (goto-char url-http-end-of-headers)
        (let* ((resp (json-read))
               (exercises (gethash "exercises" resp))
               ;; get width of longest exercise slug for alignment
               (width (seq-reduce (lambda (a e)
                                    (max a (length (gethash "slug" e))))
                                  exercises 0)))
          ;; generate minibuffer list
          (seq-map (lambda (e)
                     (list (s-pad-right width " " (gethash "slug" e)) e))
                   exercises))
        ))))

(aio-defun exercism-set-exercise ()
  "Choose an exercise from the current track."
  (interactive)
  (unless exercism--current-track (aio-await (exercism-set-track)))

  (if-let* ((track exercism--current-track)
            (exercises (aio-await (exercism--list-exercises track))))

      (let* ((completion-extra-properties '(:annotation-function exercism--exercise-annotation-fn))
             (prompt (format "Choose exercise [%s]: " track))
             (exercise (s-trim (completing-read prompt exercises (-const t) t))))

        (let ((result (aio-await (exercism--exercise-setup track exercise))))
          (if-let ((error-info (plist-get result :error)))
              (progn
                (exercism--error "Unable to retrieve exercise: %s" error-info))

            (exercism--debug "Solving %s/%s" track exercise)
            (exercism-start-coding))))

    (exercism--error "Failed to retrieve exercise list")
    (setq exercism--exercise-files nil)
    (setq exercism--exercise-metadata nil)))

(aio-defun exercism--list-tracks ()
  "List all available Exercism tracks."
  (let* ((json-object-type 'hash-table)
         (url-request-method "GET")
         (url-request-extra-headers
          `(,(exercism--authorization (exercism--authorization-bearer exercism--api-token))))
         (result (aio-await (aio-url-retrieve exercism--url-track-api))))

    (if-let ((error-info (plist-get (car result) :error)))
        (progn
          (switch-to-buffer (cdr result))
          (exercism--error "List tracks failed: %S" error-info))

      (exercism--debug "Successfully retrieved available tracks")
      (with-current-buffer (cdr result)
        (goto-char url-http-end-of-headers)
        (let* ((resp (json-read))
               (tracks (gethash "tracks" resp)))
          (seq-map (lambda (t) (gethash "slug" t)) tracks))
        ))))

(aio-defun exercism--track-setup (track)
  "Set up TRACK, initializing track if it does not exist."
  (unless exercism--workspace (exercism-setup))
  (let* ((current-track exercism--current-track)
         (workspace exercism--workspace)
         (track-dir (expand-file-name track workspace)))

    (unless (file-exists-p track-dir)
      (aio-await (exercism--exercise-setup track "hello-world")))))

(aio-defun exercism-set-track ()
  "Set the current exercism track."
  (interactive)
  (if-let* ((workspace exercism--workspace)
            (track-slugs (aio-await (exercism--list-tracks)))
            (track (completing-read "Set track: " track-slugs (-const t) t)))

      (let* ((result (aio-await (exercism--track-setup track))))
        (if-let ((error-info (plist-get result :error)))
            (progn
              (exercism--error "Unable to set track %s" error-info)
              (setq exercism--current-track nil))

          (exercism--info "Track set to %s" track)
          (setq exercism--current-track track)))

    (exercism--error "Failed to retrieve track list")))

(aio-defun exercism-run-tests ()
  "Run test suite for the currently selected exercise."
  (interactive)
  (if-let ((_ (and exercism--exercise-metadata exercism--exercise-files)))
      (let* ((track (gethash "track" exercism--exercise-metadata))
             (exercise (gethash "exercise" exercism--exercise-metadata))
             (track-dir (expand-file-name track exercism--workspace))
             (exercise-dir (expand-file-name exercise track-dir))
             (default-directory exercise-dir)
             (test-buffer (get-buffer-create exercism-result-buffer-name))
             (result (aio-await (exercism--run-command
                                 (concat (executable-find exercism-cmd) " test")))))

        (with-current-buffer test-buffer
          (erase-buffer)
          (insert result))

        (display-buffer test-buffer
                        '((display-buffer-reuse-window
                           exercism--display-result)
                          (reusable-frames . visible))))

    (exercism--error "Exercise is not set")))

(aio-defun exercism-submit-solution ()
  "Submit exercise solution."
  (interactive)
  (if-let ((_ (and exercism--exercise-metadata exercism--exercise-files)))
      (let* ((track (gethash "track" exercism--exercise-metadata))
             (exercise (gethash "exercise" exercism--exercise-metadata))
             (exercise-url (gethash "url" exercism--exercise-metadata))
             (track-dir (expand-file-name track exercism--workspace))
             (exercise-dir (expand-file-name exercise track-dir))
             (default-directory exercise-dir)
             (result (aio-await (exercism--run-command
                                 (concat (executable-find exercism-cmd) " submit")))))

        (if (string-match "^Error:.*" result)
            (exercism--error "Failed to submit solution: %s" result)
          (if (and exercism-open-url-on-submit
                   (y-or-n-p "Open submission in browser? "))
              (browse-url exercise-url))))

    (exercism--error "Exercise not set")))

(aio-defun exercism--execute-solutions-api (action)
  "Perform Exercism solution API request for ACTION."
  (if-let ((_ (and exercism--exercise-metadata exercism--exercise-files)))
      (if-let* ((action-alist
                 `((complete . ,(list :url-fmt exercism--url-fmt-solutions-complete-api
                                      :error-fmt "Complete request failed: %s"
                                      :success-fmt "Marked %s/%s as completed"))

                   (publish . ,(list :url-fmt exercism--url-fmt-solutions-publish-api
                                     :error-fmt "Publish request failed: %s"
                                     :success-fmt "Published %s/%s solution"))

                   (unpublish . ,(list :url-fmt exercism--url-fmt-solutions-unpublish-api
                                       :error-fmt "Unpublish request failed: %s"
                                       :success-fmt "Unpublished %s/%s solution"))
                   ))
                (opt (assoc action action-alist))
                (opt (cdr opt)))
          (let* ((json-object-type 'hash-table)
                 (track (gethash "track" exercism--exercise-metadata))
                 (exercise-name (gethash "exercise" exercism--exercise-metadata))
                 (exercise-id (gethash "id" exercism--exercise-metadata))
                 (api-url (format (plist-get opt :url-fmt) exercise-id))
                 (url-request-method "PATCH")
                 (url-request-extra-headers
                  `(,(exercism--authorization (exercism--authorization-bearer exercism--api-token))))
                 (result (aio-await (aio-url-retrieve api-url))))
            (if-let ((error-info (plist-get (car result) :error)))
                (exercism--error (plist-get opt :error-fmt) error-info)
              (exercism--info (plist-get opt :success-fmt) track exercise-name)))
        (exercism--error "Invalid action %s" (symbol-name action)))
    (exercism--error "Exercise not set")))

(defun exercism-mark-completed ()
  "Mark exercise as complete."
  (interactive)
  (exercism--execute-solutions-api 'complete))

(aio-defun exercism-publish-solution ()
  "Publish exercise solution."
  (interactive)
  (exercism--execute-solutions-api 'publish))

(aio-defun exercism-unpublish-solution ()
  "Publish exercise solution."
  (interactive)
  (exercism--execute-solutions-api 'unpublish))

(defun exercism-open-readme ()
  "Open exercise `README.md`."
  (interactive)
  (if-let ((_ (and exercism--exercise-metadata exercism--exercise-files)))

      (let* ((track (gethash "track" exercism--exercise-metadata))
             (exercise (gethash "exercise" exercism--exercise-metadata))
             (track-dir (expand-file-name track exercism--workspace))
             (exercise-dir (expand-file-name exercise track-dir))
             (description-buffer (find-file-noselect (expand-file-name "README.md" exercise-dir))))

        (with-current-buffer description-buffer
          (markdown-view-mode))
        (display-buffer description-buffer
                        '((display-buffer-reuse-window
                           exercism--display-description)
                          (reusable-frames . visible))))

    (exercism--error "Exercise not set")))

(defun exercism-open-help ()
  "Open exercise `HELP.md`."
  (interactive)
  (if-let ((_ (and exercism--exercise-metadata exercism--exercise-files)))

      (let* ((track (gethash "track" exercism--exercise-metadata))
             (exercise (gethash "exercise" exercism--exercise-metadata))
             (track-dir (expand-file-name track exercism--workspace))
             (exercise-dir (expand-file-name exercise track-dir))
             (description-buffer (find-file-noselect (expand-file-name "HELP.md" exercise-dir))))

        (with-current-buffer description-buffer
          (markdown-view-mode))
        (display-buffer description-buffer
                        '((display-buffer-reuse-window
                           exercism--display-description)
                          (reusable-frames . visible))))

    (exercism--error "Exercise not set")))

(defun exercism-open-tests ()
  "Open exercise test suite."
  (interactive)
  (if-let ((_ (and exercism--exercise-metadata exercism--exercise-files)))

      (let* ((track (gethash "track" exercism--exercise-metadata))
             (exercise (gethash "exercise" exercism--exercise-metadata))
             (track-dir (expand-file-name track exercism--workspace))
             (exercise-dir (expand-file-name exercise track-dir))
             (tests-file-name (aref (gethash "test" exercism--exercise-files) 0))
             (tests-buffer (find-file-noselect (expand-file-name tests-file-name exercise-dir))))

        (display-buffer tests-buffer
                        '((display-buffer-reuse-window
                           exercism--display-result)
                          (reusable-frames . visible)))
        )

    (exercism--error "Exercise not set")))

(provide 'exercism)

;;; exercism.el ends here
