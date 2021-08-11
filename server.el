(require 'sqlite3)

(eserver-register-site "/weibo"
  "微博热搜历史")

(defcustom eserver-weibo (expand-file-name "weibo" eserver-root)
  "Root directory of weibo."
  :group 'eserver
  :type 'directory)

(defcustom eserver-weibo-db-name "hot-second.sqlite3"
  "Database file name to store weibo hot topics statistics."
  :group 'eserver
  :type 'file)

;;; open database readonly for query
(setq eserver-weibo-db (sqlite3-open
                        (expand-file-name eserver-weibo-db-name eserver-weibo)
                        sqlite-open-readonly))

(defun eserver-weibo-close-db ()
  (sqlite3-close eserver-weibo-db))

(add-hook 'httpd-stop-hook #'eserver-weibo-close-db)

(defun eserver-weibo-start-page ()
  "Generate weibo start page form database."
  (let ((stmt (sqlite3-prepare eserver-weibo-db "SELECT * FROM topics"))
        (content "<ol>\n"))
    (while (= sqlite-row (sqlite3-step stmt))
      (let ((topic (sqlite3-column-text stmt 0))
            (index (sqlite3-column-int64 stmt 1)))
        (setq content (concat content
                              (format "  <li><a href=\"weibo/%d\">%s</a></li>\n"
                                      index topic)))))
    (sqlite3-finalize stmt)
    (setq content (concat content "</ol>"))))

(defun eserver-weibo-topic-tuple (topic)
  (let (column value stmt)
    (if (> (string-to-number topic) 0)
        (setq column "idx"
              value (string-to-number topic))
      (setq column "topic"
            value topic))
    (setq stmt (sqlite3-prepare eserver-weibo-db
                                (format "SELECT * FROM topics WHERE %s == ?"
                                        column)))
    (sqlite3-bind-multi stmt value)
    (when (= sqlite-row (sqlite3-step stmt))
      (cons (sqlite3-column-text stmt 0)      ; topic name
            (sqlite3-column-int64 stmt 1))))) ; topic id

(defun eserver-weibo-time-string (time)
  (let ((x (decode-time time)))
    (format "%04d/%02d/%02d %02d:%02d:%02d"
            (cl-sixth x) (cl-fifth x) (cl-fourth x)
            (cl-third x) (cl-second x) (cl-first x))))

(defun eserver-weibo-time-chart (topic)
  (let* ((topic-tuple (eserver-weibo-topic-tuple topic))
         (topic-name (car topic-tuple))
         (topic-id (cdr topic-tuple)))
    (if (not topic-id)
        "ERROR: URL invalid."
      (let ((stmt (sqlite3-prepare
                   eserver-weibo-db
                   "SELECT timestamp, hit FROM hotsec WHERE topichash == ?"))
            timestamp hit content)
        (sqlite3-bind-multi stmt topic-id)
        (while (= sqlite-row (sqlite3-step stmt))
          (setq timestamp (sqlite3-column-int64 stmt 0))
          (setq hit (sqlite3-column-int64 stmt 1))
          (setq content (concat content
                                (format "[\"%s\", %d],\n"
                                        (eserver-weibo-time-string timestamp)
                                        hit))))
        (sqlite3-finalize stmt)
        (with-temp-buffer
          (insert-file-contents (expand-file-name "chart-template.html"
                                                  eserver-weibo))
          (replace-string ":tbt:replace:title:" topic-name)
          (replace-string ":tbt:replace:data:" content)
          (buffer-string))))))

(defun httpd/weibo (proc path arguments &rest args)
  (with-httpd-buffer proc "text/html"
    (if (or (string-equal path "/weibo")
            (string-equal path "/weibo/")
            (string-equal path "/weibo/topics"))
        (insert (eserver-weibo-start-page))
      (insert (eserver-weibo-time-chart (file-name-base path))))))

(defun httpd/weibo/time (proc path &rest args)
  (with-httpd-buffer proc "text/plain"
    (insert "This is weibo/time.")))
