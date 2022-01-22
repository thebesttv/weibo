;;; Parsing HTML and XML
;;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-HTML_002fXML.html

;;; Document Object Model
;;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Document-Object-Model.html

;;; Timers for Delayed Execution
;;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Timers.html

(require 'dom)
(require 'sqlite3)

(defvar hot-last-result nil
  "CDR of the last return value of `hot-topics'.")

(defvar hot-results nil
  "A list of results to be committed to database.")

(defvar hot-topic-hashtable (make-hash-table :test 'equal
                                             :size 4000)
  "Hash table to store topic ids.")

(defvar hot-db-path "/home/thebesttv/eserver/weibo/hot.sqlite3"
  "Path to database.")

(defvar hot-db nil
  "Database.")

;;; functions

(defun hot-open-db ()
  (setq hot-db (sqlite3-open hot-db-path
                             sqlite-open-readwrite sqlite-open-create)))

(defun hot-close-db ()
  (sqlite3-close hot-db))

(add-hook 'kill-emacs-hook #'hot-close-db)

(defun hot-db-tables ()
  "Return a list of `hot-db' tables."
  (let ((stmt (sqlite3-prepare
               hot-db
               "SELECT name from sqlite_master where type= \"table\""))
        name tables)
    (while (= sqlite-row (sqlite3-step stmt))
      (setq name (sqlite3-column-text stmt 0)
            tables (cons name tables)))
    (sqlite3-finalize stmt)
    tables))

(defun hot-init-db ()
  "Init `hot-db'."
  (let ((tables (hot-db-tables)))
    (unless (member "topics" tables)
      (sqlite3-exec hot-db
                    "CREATE TABLE topics (name text primary key, id int)"))
    (unless (member "hot" tables)
      (sqlite3-exec hot-db
                    "CREATE TABLE hot (time int, rank int, id int, hits int)"))))

(defun hot-init-topic-hashtable ()
  "Init `hot-topic-hashtable'."
  (let ((stmt (sqlite3-prepare hot-db "SELECT name, id FROM topics")))
    (while (= sqlite-row (sqlite3-step stmt))
      (let ((name (sqlite3-column-text stmt 0))
            (id   (sqlite3-column-int64 stmt 1)))
        (setf (gethash name hot-topic-hashtable) id)))
    (sqlite3-finalize stmt)))

(defun hot-topic-id (name)
  (let* ((ht hot-topic-hashtable)
         (id (gethash name ht)))
    (if id
        id
      ;; If id not found in hashtable
      (setq id (1+ (hash-table-count ht)))
      (setf (gethash name ht) id)
      ;; save to database
      (let ((stmt (sqlite3-prepare hot-db
                                   "INSERT INTO topics VALUES (?, ?)")))
        (sqlite3-bind-multi stmt name id)
        (sqlite3-step stmt)
        (sqlite3-finalize stmt))
      id)))

(defun hot-current-topics ()
  "Return a cons cell (LIST . TIME) representing current weibo hot topics.
TIME is the time at retrieval (in seconds) since epoch.  LIST is
made up of 50 cons cells (TOPIC . HITS) representing weibo's hot
topics at the moment.  TOPIC is obtained by `hot-topic-id'."
  (with-temp-buffer
    (shell-command "curl -s -m 2 https://s.weibo.com/top/summary"
                   (current-buffer))
    (let* ((time (time-convert (current-time) 'integer))
           (dom (libxml-parse-html-region (point-min) (point-max)))
           (trs (dom-search dom
                            (lambda (node)
                              (when (eq (dom-tag node) 'tr)
                                (let* ((tds (dom-by-tag node 'td))
                                       (td1 (car tds)))
                                  (and (= (length tds) 3)
                                       (string-equal (dom-attr td1 'class)
                                                     "td-01 ranktop")
                                       (> (string-to-number
                                           (dom-text td1)) 0))))))))
      (when (= (length trs) 50)
        (cons time
              (mapcar (lambda (node)
                        (let* ((tds (dom-by-tag node 'td))
                               (td2 (cadr tds))
                               (topic (dom-text (dom-by-tag td2 'a)))
                               (hits (string-to-number
                                      (dom-text (dom-by-tag td2 'span)))))
                          (cons (hot-topic-id topic)
                                hits)))
                      trs))))))

(defun hot-main ()
  (let ((res (hot-current-topics)))
    (message (number-to-string (car res)))
    (unless (equal (cdr res) hot-last-result)
      (message "new")
      (setq hot-last-result (cdr res))
      (setq hot-results (cons res hot-results)))))

;;; start collecting

(hot-open-db)
(hot-init-db)
(hot-init-topic-hashtable)

(hot-current-topics)
