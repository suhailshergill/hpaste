;;; hpaste.el -- Integration with hpaste: http://hpaste.org.
 
;; Author: David House <dmhouse@gmail.com>
;; Created: 14th April 2007
;; Version: 1.0
;; License: GPL
 
(require 'url)
 
(defgroup hpaste nil "Integration with the hpaste pastebin")
(defcustom hpaste-server "http://hpaste.org" 
  "Base URL for the hpaste server."
  :type '(string)
  :group 'hpaste)
(defcustom hpaste-default-nick nil
  "What to tell the server your nick is. If NIL, then prompt every time."
  :type '(choice (string) (const :tag "Ask every time" nil))
  :group 'hpaste)
(defcustom hpaste-blank-title nil
  "If non-NIL, don't send a title to the server."
  :type '(boolean)
  :group 'hpaste)
(defcustom hpaste-announce 'ask
  "Whether to announce the paste in the #haskell channel on
Freenode. If ALWAYS, then announce every time. If ASK, then
prompt every time. If NEVER, then never announce."
  :type '(choice (const :tag "Always announce" always)
                 (const :tag "Ask each time" ask) 
                 (const :tag "Never announce" never))
  :group 'hpaste)
 
(defvar hpaste-last-paste-id nil
  "Numerical ID of the last paste.")
 
(defun hpaste-after-paste (&optional redirect)
  "Callback that runs after a paste is made. Messages the user
and tell them that everything went smoothly, and save the paste
ID for use as a default ID for annotations."
  (message "Paste successful: " (cadr redirect))
  (kill-new (format (cadr redirect)))
  (if (eq (car redirect) ':redirect)
      (progn 
        (setq url (cadr redirect))
        (string-match "/\\([0-9]*\\)\\(#.*\\)?$" url)
        (let ((id (match-string 1 url)))
          (if id (setq hpaste-last-paste-id id))))))
 
(defun hpaste-prompt-for-annotate ()
  "Ask the user whether they want to send the paste as an
annotation, and if so, the ID of the paste to
annotate (defaulting to the last paste made through this
interface)."
  (if (y-or-n-p "Send as annotation? ")
      (let* ((prompt
              (if hpaste-last-paste-id
                  (format "Paste to annotate (default %s): "
                          hpaste-last-paste-id)
                "Paste to annotate: "))
             (input (read-from-minibuffer prompt)))
        (if (> (length input) 0) input hpaste-last-paste-id))))
 
(defun hpaste-paste-region (beg end)
  "Send the region to the hpaste server specified in
`hpaste-server'. Use the nick in `hpaste-default-nick', or prompt
for one if that is NIL. You can still appear as (anonymous) by
just not filling out a nick when prompted (just hit RET). Prompt
for a title, unless `hpaste-blank-title' is non-NIL, in which
case just send a blank title. Pastes will be announced in
#haskell on Freenode according to `hpaste-announce', see the
docstring of that variable for more information.
 
For more information on hpaste, see http://hpaste.org"
  (interactive "r")
  (let* ((nick (or hpaste-default-nick (read-from-minibuffer "Nick: ")))
         (title (if hpaste-blank-title "" (read-from-minibuffer "Title: ")))
         (annot-id (hpaste-prompt-for-annotate))
         (announce (if (or (eq hpaste-announce 'always)
                           (and (eq hpaste-announce 'ask)
                                (y-or-n-p "Announce paste? ")))
                       "&announce=true"
                     ""))
 
         (url (concat hpaste-server
                      (if annot-id (concat "/annotate/" annot-id)
                        "/new")))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-mime-accept-string "*/*")
         (url-request-data
          (format "content=%s&nick=%s&title=%s%s&x=0&y=0\r\n"
                  (url-hexify-string (buffer-substring-no-properties beg end))
                  (url-hexify-string nick)
                  (url-hexify-string title)
                  announce)))
    (url-retrieve url 'hpaste-after-paste)))

(defun hpaste-get-paste (id)
  "Fetch the contents of the paste from hpaste into a new buffer."
  (interactive "nPaste #: ")

  (let ((url-request-method "GET")
        (url-request-extra-headers nil)
        (url-mime-accept-string "*/*")
        (url (url-generic-parse-url
	      (format "http://hpaste.org/raw/%s" id))))
    (setq hpaste-buffer (url-retrieve-synchronously url))
   (setq hpaste-last-paste-id id)

     (with-current-buffer hpaste-buffer
       (progn
         (set-visited-file-name (format "hpaste #%s" id))
	 (goto-char (point-min))
         (search-forward-regexp "\n\n")
         (delete-region (point-min) (point))
         (set-buffer-modified-p nil)
         (switch-to-buffer hpaste-buffer)
         (if haskell-version
             (haskell-mode)
           (normal-mode))))))

(defun hpaste-paste-buffer ()
  "Like `hpaste-paste-region', but paste the entire buffer instead."
  (interactive)
  (hpaste-paste-region (point-min) (point-max)))
 
(provide 'hpaste)
