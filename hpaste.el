;;; hpaste.el -- Integration with hpaste: http://hpaste.org.
 
;; Authors: David House <dmhouse@gmail.com>, 
;;          Andrew Sackville-West <andrew@swclan.homelinux.org>,
;;          and others.
;;
;; 
;; Version 1.1 -- 16 Sep, 2010:
;;   fix hpaste-get-paste and hpaste-paste-region to handle new hpaste.org
;; 
;; Created: 14th April 2007
;; Version: 1.0
;; License: GPL

 
;; Require
(require 'url)

;; Definitions
 
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

(defcustom hpaste-lang 'ask
  "Whether to set the language tag in the paste. If ASK, then
  prompt every time. If ALWAYS, then the value of
  `hpaste-default-lang' will be silently and automatically
  used. If NEVER, then the paste will never be tagged with a
  language."
  :type '(choice (const :tag "Always tag the language" always)
		 (const :tag "Ask whether to tag the language" ask)
		 (const :tag "Never tag the language" never))
  :group 'hpaste)


(defcustom hpaste-channel 0
  "The channel to use for making announcements. Specifying 0, No
Channel, has the effect of having your post not announced ever,
regardless of the setting of `hpaste-announce'. There is
currently no prompting for which channel to announce to, so
beware."
  :type '(choice (const :tag "No channel" 0)
		 (const :tag "#haskell" 1)
		 (const :tag "#xmonad" 2))
  :group 'hpaste)

(defconst hpaste-langs-alist '(("None" . 0)
			       ("Bash/shell" . 1)
			       ("C" . 2)
			       ("C++" . 3)
			       ("Common Lisp" . 4)
			       ("D" . 5)
			       ("Erlang" . 6)
			       ("Haskell" . 7)
			       ("Java" . 8)
			       ("JavaScript" . 9)
			       ("Literate Haskell" . 10)
			       ("Lua" . 11)
			       ("Objective-C" . 12)
			       ("OCaml" . 13)
			       ("Perl" . 14)
			       ("Perl" . 15) ;; sic
			       ("Prolog" . 16)
			       ("Python" . 17)
			       ("Ruby" . 18)
			       ("Scala" . 19)
			       ("XML" . 20))
  "The list of available language tags on hpaste.org. This list
  is subject to change without notice, possibly causing erroneous
  tagging. This should really be replaced with some function to
  actually parse the list of languages from the html...")

(defcustom hpaste-default-lang 0
  "The default language tag to use when pasting. If `hpaste-lang'
  is set to ALWAYS, then this value will be used silently. If
  `hpaste-lang' is set to ASK, the user selects yes, and this is
  set to None, 0, then the user will be prompted for a language
  to use. The user can always select 0, None, again to force no
  language tag."
  :type (cons 'choice (mapcar (lambda (x)
				(list 'const ':tag (car x)
				      (cdr x)))
			      hpaste-langs-alist))
  :group 'hpaste)

(defun hpaste-prompt-for-lang ()
   (or (cdr (assoc
	     (completing-read (format "Enter the language [%s]:" hpaste-default-lang) 
			      hpaste-langs-alist) hpaste-langs-alist))
       hpaste-default-lang)) 
 
(defvar hpaste-last-paste-id nil
  "Numerical ID of the last paste.")
 
(defun hpaste-after-paste (&optional redirect)
  "Callback that runs after a paste is made. Messages the user
and tell them that everything went smoothly, and save the paste
ID for use as a default ID for annotations."
  (if redirect
      (progn
	(message "Paste successful: %s" (cadr redirect))
	(kill-new (format (cadr redirect)))
	(if (eq (car redirect) ':redirect)
	    (progn 
	      (setq url (cadr redirect))
;;	      (string-match "/\\([0-9]*\\)\\(#.*\\)?$" url) ;; original regex
	      (string-match ".*/\\([0-9]*\\)/.*$" url) ;; a hack of a regex 
	      (let ((id (match-string 1 url)))
		(if id (setq hpaste-last-paste-id id))))))
    (message "%s" "No result from server.")))
 
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
case just send a blank title. Pastes will be announced on
Freenode in the channel as specified in `hpaste-channel', per the
value of `hpaste-announce'. See the docstring of those variables
for more information.

This function does not currently implement the selection of
source code language as is available on hpaste.
 
For more information on hpaste, see http://hpaste.org"
  (interactive "r")
  (let* ((nick (or hpaste-default-nick (read-from-minibuffer "Nick: ")))
         (title (if hpaste-blank-title "" (read-from-minibuffer "Title: ")))
         (annot-id (hpaste-prompt-for-annotate))
         (announce (or (eq hpaste-announce 'always)
		       (and (eq hpaste-announce 'ask)
			    (y-or-n-p "Announce paste? "))))

         (url (concat hpaste-server "/control"))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-mime-accept-string "*/*")
         (url-request-data (concat 
			    (if annot-id
				(format "annotation_of=%s&" annot-id)
			      "") 
			    (format "fval[1]=%s&fval[2]=%s&fval[3]=0&fval[4]=%d&fval[5]=%s&email=&submit=true\r\n" 
				    (url-hexify-string title)
				    (url-hexify-string nick)
				    (if announce hpaste-channel 0)
				    (url-hexify-string (buffer-substring-no-properties beg end))))))
(message "%s" url-request-data)))
;;    (url-retrieve url 'hpaste-after-paste)))

;; new hpaste.org form fields
;;
;; fval[1] = Title:, String
;; fval[2] = Author:, String
;; fval[3]*= Language, Int 0=no language, 1=C, 2=Haskell, 3=JavaScrip, 4=OCaml, 5=Perl, 6=Python
;; fval[4] = Channel: Int 0=no channel, 1=#haskell 2=#xmonad
;; fval[5] = Paste; String
;;
;; * already out of date!


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
