;;; -*- lexical-binding: t; -*-

(require 'tabulated-list)
(require 'seq)

(defvar-local sj/buffer-container "")
(defconst sj/max-entries 5000)

(define-derived-mode sj/journal-mode tabulated-list-mode "Journal"
  (setq tabulated-list-format
        [("Timestamp" 19 t)
         ("Priority" 10 t)
         ("Unit" 30 t)
         ("Message" 80 nil)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "Timestamp" nil))
  (tabulated-list-init-header)
  (use-local-map (let ((map (make-sparse-keymap)))
                   (set-keymap-parent map tabulated-list-mode-map)
                   (define-key map "f" #'sj/filter-by-unit)
                   (define-key map "F" #'sj/filter-by-priority)
                   (define-key map "c" #'sj/clear)
                   (define-key map "k" #'sj/kill-process)
                   (define-key map "a" #'sj/toggle-autoscroll)
                   map))
  (setq-local sj/autoscroll t)
  (setq-local sj/all-entries nil)
  (setq-local sj/unit-filter nil)
  (setq-local sj/priority-filter nil))

(defvar-local sj/autoscroll t)
(defvar-local sj/all-entries nil)
(defvar-local sj/unit-filter nil)
(defvar-local sj/priority-filter nil)

(defconst sj/priority-alist
  '(("0" . (Emergency . error))
    ("1" . (Alert . error))
    ("2" . (Critical . error))
    ("3" . (Error . error))
    ("4" . (Warning . warning))
    ("5" . (Notice . success))
    ("6" . (Info . success))
    ("7" . (Debug . shadow))))

(defun sj/parse-insert (json)
  (condition-case e
      (let* ((entry (json-parse-string json :object-type 'alist))
             (ts-us (alist-get '_SOURCE_REALTIME_TIMESTAMP entry)))
        (when ts-us
          (let* ((priority (or (alist-get 'PRIORITY entry) "—"))
                 (pinfo (alist-get priority sj/priority-alist nil nil #'equal))
                 (pname (if pinfo (symbol-name (car pinfo)) priority))
                 (pface (if pinfo (cdr pinfo) 'default))
                 (unit (or (alist-get '_SYSTEMD_UNIT entry) "—"))
                 (message (replace-regexp-in-string
                           "\n" " " (or (alist-get 'MESSAGE entry) "—")))
                 (ts-num (/ (string-to-number ts-us) 1e6))
                 (ts-str (format-time-string "%Y-%m-%d %H:%M:%S"
                                             (seconds-to-time ts-num))))
            (list ts-str (vector ts-str
                                 (propertize pname 'face pface)
                                 unit message)))))
    (error (message "sj: JSON parse error: %S" e) nil)))

(defun sj/filter-process (p o)
  (setq sj/buffer-container (concat sj/buffer-container o))
  (let ((parts (split-string sj/buffer-container "\x1e")))
    (setq sj/buffer-container (car (last parts)))
    (with-current-buffer (process-buffer p)
      (let ((new-entries
             (delq nil
                   (mapcar (lambda (part)
                             (let ((trimmed (string-trim part)))
                               (unless (string-empty-p trimmed)
                                 (sj/parse-insert trimmed))))
                           (butlast parts)))))
        (when new-entries
          (setq sj/all-entries (append sj/all-entries new-entries))
          (when (> (length sj/all-entries) sj/max-entries)
            (setq sj/all-entries (last sj/all-entries sj/max-entries)))
          (sj/apply-filters)
          (when sj/autoscroll
            (dolist (w (get-buffer-window-list (current-buffer) nil t))
              (set-window-point w (point-max)))))))))

(defun sj/kill-process ()
  (interactive)
  (let ((p (get-buffer-process (current-buffer))))
    (if p (delete-process p) (message "no process"))))

(defun sj/clear ()
  (interactive)
  (setq sj/all-entries nil)
  (setq tabulated-list-entries nil)
  (tabulated-list-print t))

(defun sj/toggle-autoscroll ()
  (interactive)
  (setq sj/autoscroll (not sj/autoscroll))
  (message "Autoscroll is now %s" (if sj/autoscroll "on" "off")))

(defun sj/filter-by-unit (unit)
  (interactive "sFilter by unit (regexp, empty=reset): ")
  (setq sj/unit-filter (unless (string-empty-p unit) unit))
  (sj/apply-filters))

(defun sj/filter-by-priority (priority)
  (interactive "sFilter by priority (regexp, empty=reset): ")
  (setq sj/priority-filter (unless (string-empty-p priority) priority))
  (sj/apply-filters))

(defun sj/apply-filters ()
  (setq tabulated-list-entries
        (seq-filter (lambda (e)
                      (let ((unit (aref (cadr e) 2))
                            (prio (aref (cadr e) 1)))
                        (and (or (null sj/unit-filter)
                                 (string-match-p sj/unit-filter unit))
                             (or (null sj/priority-filter)
                                 (string-match-p sj/priority-filter prio)))))
                    sj/all-entries))
  (tabulated-list-print t))

;;;###autoload
(defun systemd-journal ()
  (interactive)
  (let* ((buf (get-buffer-create "*systemd-journal*"))
         (existing (get-buffer-process buf)))
    (when existing (delete-process existing))
    (with-current-buffer buf
      (sj/journal-mode)
      (let ((p (make-process
                :name "systemd-journal"
                :buffer buf
                :command '("journalctl" "-n" "200" "-f" "-o" "json-seq")
                :filter #'sj/filter-process)))
        (setq sj/buffer-container "")))
    (display-buffer buf '((display-buffer-pop-up-window)
                          (window-height . 0.3)))))

(provide 'systemd-journal)
