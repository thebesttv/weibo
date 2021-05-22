(require 'sqlite3)

(eserver-register-site "/weibo"
                       "微博热搜历史")

(defcustom eserver-weibo (expand-file-name "weibo" eserver-root)
  "Root directory of weibo."
  :group 'eserver
  :type 'directory)

(setq weibo-db (sqlite3-open
                (expand-file-name "hot-second.bk.sqlite3" eserver-weibo)
                sqlite-open-readonly))

(defun refresh-database ()
  (sqlite3-close weibo-db)
  (copy-file (expand-file-name "hot-second.sqlite3" eserver-weibo)
             (expand-file-name "hot-second.bk.sqlite3" eserver-weibo)
             t)
  (setq weibo-db (sqlite3-open
                  (expand-file-name "hot-second.bk.sqlite3" eserver-weibo)
                  sqlite-open-readonly)))

(defun get-topics ()
  (let ((stmt (sqlite3-prepare weibo-db "SELECT * FROM topics"))
        (content "<ol>\n"))
    (while (= sqlite-row (sqlite3-step stmt))
      (let ((topic (sqlite3-column-text stmt 0))
            (idx   (sqlite3-column-int64 stmt 1)))
        (setq content (concat content
                              (format "  <li><a href=\"weibo/%d\">%s</a></li>\n"
                                      idx topic)))))
    (setq content (concat content "</ol>"))))

(defun get-topic-hash (topic)
  (if (> (string-to-number topic) 0)
      (string-to-number topic)
    (let ((stmt (sqlite3-prepare weibo-db "SELECT * FROM topics WHERE topic == ?"))
          topic-hash)
      (sqlite3-bind-multi stmt topic)
      (if (= sqlite-row (sqlite3-step stmt))
          (setq topic-hash (sqlite3-column-int64 stmt 1))))))

(defun get-topic-tuple (topic)
  (let (column value)
    (if (> (string-to-number topic) 0)
        (setq column "idx"
              value (string-to-number topic))
      (setq column "topic"
            value topic))
    (let ((stmt (sqlite3-prepare
                 weibo-db (format "SELECT * FROM topics WHERE %s == ?" column))))
      (sqlite3-bind-multi stmt value)
      (when (= sqlite-row (sqlite3-step stmt))
        (setq topic-string (sqlite3-column-text stmt 0))
        (setq topic-hash (sqlite3-column-int64 stmt 1))
        (cons topic-string topic-hash)))))

(defun time-string (time)
  (let ((x (decode-time time)))
    (format "%04d/%02d/%02d %02d:%02d:%02d"
            (cl-sixth x) (cl-fifth x) (cl-fourth x)
            (cl-third x) (cl-second x) (cl-first x))))

(defun get-time-chart (topic)
  (let* ((topic-tuple (get-topic-tuple topic))
         (topic-string (car topic-tuple))
         (topic-hash (cdr topic-tuple)))
    (when topic-hash
      (let ((stmt (sqlite3-prepare
                   weibo-db
                   "SELECT timestamp, hit FROM hotsec WHERE topichash == ?"))
            timestamp hit
            content)
        (sqlite3-bind-multi stmt topic-hash)
        (while (= sqlite-row (sqlite3-step stmt))
          (setq timestamp (sqlite3-column-int64 stmt 0))
          (setq hit (sqlite3-column-int64 stmt 1))
          (setq content (concat content
                                (format "[\"%s\", %d],\n"
                                        (time-string timestamp)
                                        hit))))
        (with-temp-buffer
          (insert-file-contents (expand-file-name
                                 "chart-template.html" eserver-weibo))
          (replace-string ":tbt:replace:title:" topic-string)
          (replace-string ":tbt:replace:data:" content)
          (buffer-string))))))

(defun httpd/weibo (proc path arguments &rest args)
  (when (equal '("t")
               (alist-get "refresh" arguments nil nil 'string=))
    (httpd-log `(resresh weibo database at ,(current-time-string)))
    (refresh-database)
    (httpd-redirect proc "/weibo"))
  (with-httpd-buffer proc "text/html"
    (if (or (string-equal path "/weibo")
            (string-equal path "/weibo/")
            (string-equal path "/weibo/topics"))
        (insert (get-topics))
      (insert (get-time-chart (file-name-base path))))))
