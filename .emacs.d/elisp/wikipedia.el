;;; wikipedia.el --- Emacs Wikipedia mode.

;; Copyright (C) 2006 Matthew Mundell
;; Copyright (C) 2004, 2005 Paul Pogonyshev

;; Author: Paul Pogonyshev
;; Keywords: wikipedia wiki internet

;; Contact address for bug-reporting and general questions:
;;
;;			wikipedia-el-dev@gna.org

;; This file is not part of either GNU Emacs or XEmacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;; PLEASE NOTE: This code is in alpha stage.  It has bugs, even known
;;		bugs.
;;
;;              The 2006 additions (local database, maths conversion)
;;              are even pre-alpha -- please take care.
;;
;; MAKE SURE YOU UNDERSTAND WHAT YOU ARE DOING BEFORE SUBMITTING
;; EDITED ARTICLES TO WIKIPEDIA.  WHEN YOU COPY TEXT WITH HIDDEN
;; MARKUP TO A BROWSER, YOU *LOSE* ALL THE MARKUP.
;;
;; There is not enough documentation yet.


;; QUICK START INFORMATION.
;;
;; Customize `wikipedia-default-language-domain' if needed.
;;
;; Type `M-x wikipedia-visit-article RET' and input an article name.
;; Type `C-h m' to get a feeling of keyboard shortcuts.
;;
;; To save an edited article (workaround missing functionality):
;;
;; * Hit `C-c C-m' to show hidden markup.  Text becomes unreadable,
;;   but that is what we need now.
;;
;; * Open the article in a browser for editing.  Then replace the text
;;   in the browser with the edited text from Emacs and save the page.
;;
;; * Hope this buggy code didn't spoil the article ;-) Don't worry
;;   this really doesn't happen all that often :-)


;; PARTICULARLY USEFUL CUSTOMIZABLE FEATURES
;;
;; * `wikipedia-default-language-domain' mentioned above should be set
;;   to whatever domain you use (most often.)
;;
;; * You may want to enable `wikipedia-turn-on-eldoc-mode' for
;;   `wikipedia-mode-hook'.  This way you will always see where a
;;   particular link leads to.
;;
;; * Similarly, enabling `wikipedia-turn-on-longlines-mode' will
;;   automatically and transparently break lines in articles.
;;
;; * If you include `wikipedia-mode' into `which-func-modes' list, you
;;   will see the current article subsection name in the mode line.
;;   In any case you can use `imenu'.


;; HINTS
;;
;; * You can save articles to files without manually showing markup:
;;   the mode will do this for you, transparently.  When loading from
;;   a file, you will need to activate Wikipedia mode manually,
;;   however.
;;
;; * Wikipedia mode tries to be smart and replaces certain input with
;;   special characters (e.g. `<=' with `â‰¤'.)  There are cases when
;;   this is not desirable.  Use `C-c C-e' (`wikipedia-electric-keys')
;;   to temporarily disable all Wikipedia mode electric keys and then
;;   `C-c C-e' again to enable them.  Another way out is to use `C-q'
;;   to quote the last character in sequence.


;; KNOWN BUGS
;;
;; * Links and emphasis don't interact well when they share some text.
;;   This can result in something like
;;
;;	''[[this'' is a link]]
;;
;;   A must to fix.


;; TODO
;;
;; * Support for saving articles in addition to visiting them.
;;
;; * Properly handle markup-inhibiting constructions:
;;   `<nowiki>...</nowiki>' and `<math>...</math>'.  Don't insert
;;   special characters in the latter.
;;
;; * Break this into parts.  At least two: the mode itself and HTTP
;;   access stuff.  Maybe domain data could be a part on its own too.


;; CURRENT PAGE LAYOUT OF THIS FILE
;;
;; 0. License, short info and documentation.
;; 1. Description of supported language domains.
;; 2. Customizable variables and faces.
;; 3. Internal variables.
;; 4. Interactive commands and special options for Wikipedia mode
;;    hook.
;; 5. Internal functions.
;; 6. Debug function(s).


(require 'url)
(require 'font-lock)



;;;; Description of supported language domains.

;; This is of course an overkill, but Lisp is such a fun
;; language...  Test yourself, read this! ;-)
;;
(let ((properties '(;; The domain, e.g. "en".
		    domain
		    ;; Human-readable domain language name,
		    ;; e.g. "English"
		    domain-language-name
		    ;; Domain coding, e.g. `utf-8' or `iso-8859-1'.
		    domain-coding
		    ;; The prefix used for special page addresses,
		    ;; "Special:" for English domain.
		    special-page-prefix
		    ;; This must be a list of special namespaces in
		    ;; the following order: "Talk:", "User:", "User
		    ;; talk:", "Wikipedia:", "Wikipedia talk:",
		    ;; "Category:", "Category talk:", "Image:", "Image
		    ;; talk:".
		    list-of-namespaces
		    ;; String of language-specific proper quotes.
		    ;; Must be exactly two or four characters.  The
		    ;; first two characters are the opening and the
		    ;; closing quotes correspondingly.  The last two
		    ;; characters, if present, are alternative opening
		    ;; and closing quotes, used for nested quotes.
		    proper-quotes
		    ;; Whether the domain's language uses en-dash.
		    ;; Set to `nil' if it only uses em-dash.
		    use-en-dash))
      (property-index 0))
  (mapc (lambda (property)
	  (let ((function-symbol (intern (concat "wikipedia-get-"
						 (symbol-name property)))))
	    (eval `(defsubst ,function-symbol (&optional domain)
		     (aref (if domain (cdr (assoc domain
						  wikipedia-language-domains))
			     wikipedia-article-domain-description)
			   ,property-index))))
	  (setq property-index (1+ property-index)))
	properties)
  (eval `(defmacro wikipedia-describe-language-domain
	   (domain . ,(cdr properties))
	   (declare (indent 2) (debug 0))
	   (list 'cons domain (list 'vector domain ,@(cdr properties))))))

(defconst wikipedia-language-domains
  (eval-when-compile
    (let ((english-list-of-namespaces '("Talk:"
					"User:"      "User talk:"
					"Wikipedia:" "Wikipedia talk:"
					"Category:"  "Category talk:"
					"Image:"     "Image talk:")))
      ;; Note: the sorting policy is like this: English first, Meta
      ;; second (special), all other languages sorted alphabetically
      ;; by their English names.
      (list (wikipedia-describe-language-domain
		"en"
		"English"
	      'utf-8
	      "Special"
	      english-list-of-namespaces
	      "â€œâ€â€˜â€™"
	      t)
	    (wikipedia-describe-language-domain
		"meta"
		"Meta-Wiki (English)"
	      'utf-8
	      "Special"
	      english-list-of-namespaces
	      "â€œâ€â€˜â€™"
	      t)
	    (wikipedia-describe-language-domain
		"ru"
		"Russian"
	      'utf-8
	      "Ð¡Ð»ÑƒÐ¶ÐµÐ±Ð½Ð°Ñ:"
	      '("ÐžÐ±ÑÑƒÐ¶Ð´ÐµÐ½Ð¸Ðµ:"
		"Ð£Ñ‡Ð°ÑÑ‚Ð½Ð¸Ðº:"    "ÐžÐ±ÑÑƒÐ¶Ð´ÐµÐ½Ð¸Ðµ ÑƒÑ‡Ð°ÑÑ‚Ð½Ð¸ÐºÐ°:"
		"Ð’Ð¸ÐºÐ¸Ð¿ÐµÐ´Ð¸Ñ:"   "ÐžÐ±ÑÑƒÐ¶Ð´ÐµÐ½Ð¸Ðµ Ð’Ð¸ÐºÐ¸Ð¿ÐµÐ´Ð¸Ð¸:"
		"ÐšÐ°Ñ‚ÐµÐ³Ð¾Ñ€Ð¸Ñ:"   "ÐžÐ±ÑÑƒÐ¶Ð´ÐµÐ½Ð¸Ðµ ÐºÐ°Ñ‚ÐµÐ³Ð¾Ñ€Ð¸Ð¸:"
		"Ð˜Ð·Ð¾Ð±Ñ€Ð°Ð¶ÐµÐ½Ð¸Ðµ:" "ÐžÐ±ÑÑƒÐ¶Ð´ÐµÐ½Ð¸Ðµ Ð¸Ð·Ð¾Ð±Ñ€Ð°Ð¶ÐµÐ½Ð¸Ñ:")
	      "Â«Â»â€žâ€œ"
	      nil)
	    (wikipedia-describe-language-domain
		"simple"
		"Simple English"
	      'utf-8
	      "Special"
	      english-list-of-namespaces
	      "â€œâ€â€˜â€™"
	      t))))

  "Alist with each element describing a language domain.
Elements has the format of (DOMAIN . PROPERTIES) where DOMAIN is
a string (i.e. \"en\") and PROPERTIES is a vector.

This constant must not be modified by hands.  When redefining it,
always use `wikipedia-describe-language-domain' macro.")

(defconst wikipedia-language-domains-for-customization
  (eval-when-compile
    (mapcar (lambda (domain-cons)
	      (let ((domain (car domain-cons)))
		(list 'const
		      :tag
		      (format "%s (%s)"
			      (wikipedia-get-domain-language-name domain)
			      domain)
		      domain)))
	    wikipedia-language-domains))
  "List of customization options, one per language domain.")



;;;; Customizable variables and faces.

(defgroup wikipedia nil
  "Major mode for editing Wikipedia articles."
  :group 'wp)

(defcustom wikipedia-default-language-domain "en"
  "Default language domain.
Value of this variable is taken into account when visiting
Wikipedia articles or when activating this mode for a text with
unknown domain."
  :type  (eval-when-compile
	   (cons 'radio wikipedia-language-domains-for-customization))
  :group 'wikipedia)

(defcustom wikipedia-use-link-associations t
  "Whether to use link associations.
If set to nil, link associations are turned off for all domains,
t or any other non-list value turns them all on.  Finally, this
variable can be set to a list of domain shortnames for which to
use link associations.

Link association is a pair of link text and link address.  When
associations are turned on for the domain the current article
belongs to, Wikipedia mode will suggest addresses from stored
link associations when creating new links.  For instance, if you
created a link from text \"Einstein\" to article \"Albert
Einstein\", next time you hit `C-c C-l' on word \"Einstein\",
Wikipedia mode will automatically suggest \"Albert Einstein\" as
link destination.  This is particularly useful for languages with
grammatical cases and genders.

Note that link associations are stored in files between Emacs
sessions.  The files are placed in the directory specified by
`wikipedia-mode-directory' variable."
  :type  (eval-when-compile
	   `(choice (const :tag "on for all domains" t)
		    (set   :tag "domain-specific"
			   ,@wikipedia-language-domains-for-customization)
		    (const :tag "off for all domains" nil)))
  :group 'wikipedia)

(defcustom wikipedia-auto-rescan-sections t
  "Whether to handle changed section headers automatically in Which Function mode.

When non-nil, Which Function mode will not depend on Imenu and
will handle added, removed or edited section headers
automatically.  Otherwise, changes in section headers will be
taken into account only after an Imenu rescan.

Hint: you should add `wikipedia-mode' to `which-func-modes' list
if you like Which Function mode in Wikipedia buffers."
  :type  '(choice (other :tag "on (non-nil)" t)
		  (const :tag "off (nil)" nil))
  :group 'wikipedia)

(defcustom wikipedia-mode-hook nil
  "Hook run when entering Wikipedia mode."
  :type    'hook
  :options '(wikipedia-turn-on-eldoc-mode
	     wikipedia-turn-on-longlines-mode
	     wikipedia-add-menubar-index
	     wikipedia-harvest-link-associations)
  :group   'wikipedia)


;;; Faces.

(defface wikipedia-default-face
  '((t (:inherit variable-pitch)))
  "Default face for Wikipedia mode.  Variable-pitch by default."
  :group 'wikipedia)

(defface wikipedia-fixed-pitch-face
  '((t (:foreground "gray40" :inherit fixed-pitch)))
  "Face for text on preformatted lines (those starting with a space.)"
  :group 'wikipedia)

(defface wikipedia-link-face
  '((t (:inherit (font-lock-string-face) :underline t)))
  "Face for hypertext links."
  :group 'wikipedia)

(defface wikipedia-header-level-1-face
  '((t (:weight bold :height 1.9 :inherit wikipedia-default-face)))
  "Face for level 1 headers (not recommended for use in articles.)
Headers of this level are used to display article titles and so
you are advised to avoid them in article text."
  :group 'wikipedia)

(defface wikipedia-header-level-2-face
  '((t (:weight bold :height 1.6 :inherit wikipedia-default-face)))
  "Face for level 2 headers (highest level section headers.)
Level 1 headers are used to display article titles and so you are
advised to start with level 2 for titles of your article
sections.  I.e. you should put two equal-to signs around
top-level section headers."
  :group 'wikipedia)

(defface wikipedia-header-level-3-face
  '((t (:weight bold :height 1.4 :inherit wikipedia-default-face)))
  "Face for level 3 headers."
  :group 'wikipedia)

(defface wikipedia-header-level-4-face
  '((t (:weight bold :height 1.2 :inherit wikipedia-default-face)))
  "Face for level 4 headers."
  :group 'wikipedia)

(defface wikipedia-header-level-5-face
  '((t (:weight bold :height 1.1 :inherit wikipedia-default-face)))
  "Face for level 5 headers."
  :group 'wikipedia)

(defface wikipedia-header-level-6-face
  '((t (:weight bold :height 1.0 :inherit wikipedia-default-face)))
  "Face for level 6 headers."
  :group 'wikipedia)

(defface wikipedia-non-breaking-space-face
  '((t (:background "gray90")))
  "Face for highlighting non-breaking spaces.
You can remove all attributes from this space to make
non-breaking spaces visually indistinguishable from normal
spaces.  Note that Wikipedia mode disables escaping of
non-breaking spaces done by certain newer emacsen."
  :group 'wikipedia)


;;; Low-priority customizable variables.

(defcustom wikipedia-special-characters
  '(("dash"			.	?â€”)
    ("em-dash"			.	?â€”)
    ("en-dash"			.	?â€“)
    ("ellipsis"			.	?â€¦)
    ("apostrophe"		.	?â€™)
    ("minus sign"		.	?âˆ’)
    ("plus-minus sign"		.	?Â±)
    ("multiplication sign"	.	?Ã—)
    ("division sign"		.	?Ã·)
    ("less-or-equal sign"	.	?â‰¤)
    ("greater-or-equal sign"	.	?â‰¥)
    ("not-equal sign"		.	?â‰ )
    ("identical sign"		.	?â‰¡)
    ("degree sign"		.	?Â°)
    ("euro sign"		.	?â‚¬)
    ("section sign"		.	?Â§)
    ("number sign"		.	?â„–)
    ("copyright sign"		.	?Â©)
    ("trade mark sign"		.	?â„¢)
    ("registered sign"		.	?Â®)
    ("non-breaking space"	.	?Â ))
  "Alist of characters available for insertion with `wikipedia-insert-special-character'."
  :type  '(alist :key-type (string :tag "Character name")
		 :value-type character)
  :group 'wikipedia)

(defcustom wikipedia-mode-directory (convert-standard-filename' "~/.wikipedia")
  "Directory where Wikipedia mode saves internal files.
In particular, link associations are saved in this directory."
  :type  'directory
  :group 'wikipedia)



;;;; Internal variables.

(defvar wikipedia-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\M-\S- ]	'wikipedia-insert-non-breaking-space)
    (define-key map [3696]	'wikipedia-electric-pseudo-number-sign)
    (define-key map "9"		'wikipedia-electric-digit)
    (define-key map "8"		'wikipedia-electric-digit)
    (define-key map "7"		'wikipedia-electric-digit)
    (define-key map "6"		'wikipedia-electric-digit)
    (define-key map "5"		'wikipedia-electric-digit)
    (define-key map "4"		'wikipedia-electric-digit)
    (define-key map "3"		'wikipedia-electric-digit)
    (define-key map "2"		'wikipedia-electric-digit)
    (define-key map "1"		'wikipedia-electric-digit)
    (define-key map "0"		'wikipedia-electric-digit)
    (define-key map "."		'wikipedia-electric-dot)
    (define-key map "'"		'wikipedia-electric-apostrophe)
    (define-key map "\""	'wikipedia-electric-quote)
    (define-key map "-"		'wikipedia-electric-hyphen)
    (define-key map "="		'wikipedia-electric-equal-sign)
    (define-key map " "		'wikipedia-electric-space)
    (define-key map "\C-c\C-s"	'wikipedia-insert-special-character)
    (define-key map "\C-c\C-b"	'wikipedia-add-or-remove-strong-emphasis)
    (define-key map "\C-c\C-i"	'wikipedia-add-or-remove-emphasis)
    (define-key map "\C-c\C-l"	'wikipedia-create-alter-or-delete-link)
    (define-key map "\C-c\C-c"  'wikipedia-follow-link)
    (define-key map "\C-c\C-m"  'wikipedia-show-buffer-markup)
    (define-key map "\C-c\C-r"  'wikipedia-refresh-buffer)
    (define-key map "\C-c\C-e"	'wikipedia-electric-keys)
    map))


(defconst wikipedia-property-faces
  '((wikipedia-emphasis		  . (:slant  italic))
    (wikipedia-strong-emphasis	  . (:weight bold))
    (wikipedia-link-address	  . wikipedia-link-face)))

(defconst wikipedia-header-faces
  [ wikipedia-header-level-1-face
    wikipedia-header-level-2-face
    wikipedia-header-level-3-face
    wikipedia-header-level-4-face
    wikipedia-header-level-5-face
    wikipedia-header-level-6-face ])


(defvar wikipedia-link-addresses-history nil
  "History variable used by `wikipedia-create-alter-or-delete-link'.")


(defvar wikipedia-html-entities
  '(("mdash"	.	?â€”)
    ("ndash"	.	?â€“)
    ("hellip"	.	?â€¦)
    ("minus"	.	?âˆ’)
    ("plusmn"	.	?Â±)
    ("times"	.	?Ã—)
    ("divide"	.	?Ã·)
    ("le"	.	?â‰¤)
    ("ge"	.	?â‰¥)
    ("ne"	.	?â‰ )
    ("equiv"	.	?â‰¡)
    ("deg"	.	?Â°)
    ("euro"	.	?â‚¬)
    ("sect"	.	?Â§)
    ("nbsp"	.	?Â )
    ("copy"	.	?Â©)
    ("trade"	.	?â„¢)
    ("reg"	.	?Â®)
    ("alpha"	.	?Î±)
    ("beta"	.	?Î²)
    ("gamma"	.	?Î³)
    ("delta"	.	?Î´)
    ("epsilon"	.	?Îµ)
    ("zeta"	.	?Î¶)
    ("eta"	.	?Î·)
    ("theta"	.	?Î¸)
    ("iota"	.	?Î¹)
    ("kappa"	.	?Îº)
    ("mu"	.	?Î¼)
    ("nu"	.	?Î½)
    ("xi"	.	?Î¾)
    ("omicron"	.	?Î¿)
    ("pi"	.	?Ï€)
    ("rho"	.	?Ï)
    ("sigma"	.	?Ïƒ)
    ("tau"	.	?Ï„)
    ("upsilon"	.	?Ï…)
    ("phi"	.	?Ï†)
    ("chi"	.	?Ï‡)
    ("psi"	.	?Ïˆ)
    ("omega"	.	?Ï‰)
    ("Alpha"	.	?Î‘)
    ("Beta"	.	?Î’)
    ("Gamma"	.	?Î“)
    ("Delta"	.	?Î”)
    ("Epsilon"	.	?Î•)
    ("Zeta"	.	?Î–)
    ("Eta"	.	?Î—)
    ("Theta"	.	?Î˜)
    ("Iota"	.	?Î™)
    ("Kappa"	.	?Îš)
    ("Mu"	.	?Îœ)
    ("Nu"	.	?Î)
    ("Xi"	.	?Îž)
    ("Omicron"	.	?ÎŸ)
    ("Pi"	.	?Î )
    ("Rho"	.	?Î¡)
    ("Sigma"	.	?Î£)
    ("Tau"	.	?Î¤)
    ("Upsilon"	.	?Î¥)
    ("Phi"	.	?Î¦)
    ("Chi"	.	?Î§)
    ("Psi"	.	?Î¨)
    ("Omega"	.	?Î©))
  "Alist of HTML entities and corresponding characters.")

(defvar wikipedia-known-html-entities-regexp
  (concat "&\\(" (regexp-opt (mapcar 'car wikipedia-html-entities) t)
	  "\\|#[1-9][0-9]*\\);")
  "Regexp to match all HTML entities listed in `wikipedia-html-entities'.")

(defconst wikipedia-header-regexp
  "^\\(=\\{1,6\\}\\)[[:blank:]]*\\(.+?\\)[[:blank:]]*\\1")


;; See http://meta.wikimedia.org/wiki/Complete_list_of_language_wikis_available
(defconst wikipedia-all-language-domains
  '("aa" "ab" "af" "ak" "als" "am" "an" "ang" "ar" "arc" "as" "ast" "av" "ay"
         "az"
    "ba" "be" "bg" "bh" "bi" "bn" "bo" "br" "bs"
    "ca" "ce" "ch" "cho" "chr" "chy" "co" "cr" "cs" "csb" "cv" "cy"
    "da" "de" "dv" "dz"
    "ee" "el" "en" "eo" "es" "et" "eu"
    "fa" "ff" "fi" "fj" "fo" "fr" "fy"
    "ga" "gd" "gl" "gn" "got" "gu" "gv"
    "ha" "haw" "he" "hi" "ho" "hr" "ht" "hu" "hy" "hz"
    "ia" "id" "ie" "ig" "ii" "ik" "io" "is" "it" "iu"
    "ja" "jbo" "jv"
    "ka" "kg" "ki" "kj" "kk" "kl" "km" "kn" "ko" "kr" "ks" "ku" "kv" "kw" "ky"
    "la" "lb" "lg" "li" "ln" "lo" "lt" "lv"
    "meta" "mg" "mh" "mi" "minnan" "mk" "ml" "mn" "mo" "mr" "ms" "mt" "mus"
         "my"
    "na" "nah" "nds" "ne" "ng" "nl" "nn" "no" "nv" "ny"
    "oc" "om" "or"
    "pa" "pi" "pl" "ps" "pt"
    "qu"
    "rm" "rn" "ro" "roa-rup" "ru" "rw"
    "sa" "sc" "scn" "sd" "se" "sg" "sh" "si" "simple" "sk" "sl" "sm" "sn" "so"
         "sq" "sr" "ss" "st" "su" "sv" "sw"
    "ta" "te" "tg" "th" "ti" "tk" "tl" "tlh" "tn" "to" "tokipona" "tpi" "tr"
         "ts" "tt" "tw" "ty"
    "ug" "uk" "ur" "uz"
    "ve" "vi" "vo"
    "wa" "wo"
    "xh"
    "yi" "yo"
    "za" "zh" "zu")
  "List of all registered Wikipedia domains.")

(defconst wikipedia-interwiki-link-regexp
  (eval-when-compile
    (concat "^" (regexp-opt wikipedia-all-language-domains t) ":\\(.*\\)$"))
  "Regexp to match an interwiki link address.")


(defvar wikipedia-link-associations-alist nil
  "Alist of domains and corresponding hash tables of link associations.")

;; The hook takes care of saving link associations.
(unless (assq 'wikipedia-kill-emacs-hook kill-emacs-hook)
  (add-hook 'kill-emacs-hook 'wikipedia-kill-emacs-hook))


;;; State variables.

(defvar wikipedia-electric-keys-enabled t
  "When non-nil (default), various electric keys work in special ways.
When nil, they behave normally (i.e. invoke `self-insert-command'.)

You can toggle or set this variable with
`wikipedia-electric-keys' command, which is normally bound to
`C-c C-e'.")

(make-variable-buffer-local 'wikipedia-electric-keys-enabled)


;; Used by `wikipedia-write-contents' to prevent recursion.
(defvar wikipedia-in-write-contents nil)



;;;; Interactive commands and special options for Wikipedia mode hook.

(put 'wikipedia-article-domain-description 'permanent-local t)
(put 'wikipedia-article-name               'permanent-local t)

(define-derived-mode wikipedia-mode text-mode "Wikipedia"
  "Major mode for editing articles with Wikipedia markup."
  (unless (boundp 'wikipedia-article-domain-description)
    (set (make-local-variable 'wikipedia-article-domain-description)
	 (cdr (assoc wikipedia-default-language-domain
		     wikipedia-language-domains))))
  (set (make-local-variable 'paragraph-start)    "[ \t*#=:;]")
  (set (make-local-variable 'paragraph-separate) "[ \t]*$")
  (set (make-local-variable 'comment-start)      "<!-- ")
  (set (make-local-variable 'comment-end)        " -->")
  ;;
  (set (make-local-variable 'show-nonbreak-escape) nil)
  ;;
  ;; This is the older name.
  (set (make-local-variable 'eldoc-print-current-symbol-info-function)
       'wikipedia-print-current-link-destination)
  ;; This is the newer name.
  (set (make-local-variable 'eldoc-documentation-function)
       'wikipedia-print-current-link-destination)
  ;;
  (set (make-local-variable 'imenu-generic-expression)
       (list (list nil wikipedia-header-regexp 2)))
  ;; Outline minor mode support.
  (set (make-local-variable 'outline-regexp) "=\\{1,6\\}")
  (set (make-local-variable 'outline-heading-end-regexp) "\n")
  (set (make-local-variable 'outline-level)
       (lambda ()
	 (save-match-data
	   (looking-at wikipedia-header-regexp)
	   (- (match-end 1) (match-beginning 1)))))
  (set (make-local-variable 'outline-heading-alist)
       '(("="    . 1) ("=="    . 2) ("==="    . 3)
	 ("====" . 4) ("=====" . 5) ("======" . 6)))
  ;; Hooks.
  (add-hook 'after-change-functions   'wikipedia-after-change   nil t)
  (add-hook 'write-contents-functions 'wikipedia-write-contents nil t)
  (add-hook 'which-func-functions     'wikipedia-which-func     nil t)
  ;; Turn on Which Function minor mode if we are listed in
  ;; `which-func-modes' variable. Which Function mode does that
  ;; itself, but only when opening files and Wikipedia mode is usually
  ;; activated in other ways.
  (when (and (member 'wikipedia-mode which-func-modes)
	     (null which-func-mode))
    (which-func-mode 1))
  ;; Hide all markup.
  (wikipedia-hide-region-markup (point-min) (point-max)))


(defun wikipedia-turn-on-eldoc-mode ()
  "Turn on ElDoc mode.
In Wikipedia mode this causes link destinations to be shown in
the echo area."
  (require 'eldoc)
  (eldoc-mode 1))

(defun wikipedia-turn-on-longlines-mode ()
  "Turn on Long Lines mode."
  (require 'longlines)
  (longlines-mode 1))

(defun wikipedia-add-menubar-index ()
  "Add an Imenu \"Index\" entry on the menu bar for the current buffer.
In Wikipedia mode this menu entry contains a list of artcile's
subsections."
  (require 'imenu)
  (imenu-add-menubar-index))


(defun wikipedia-harvest-link-associations ()
  "Find all links in the current buffer and store them as link association.
This function never overwrites existing link associations.

It may be especially useful when added to `wikipedia-mode-hook'."
  (interactive)
  (if (wikipedia-use-link-associations-for-domain)
      (let ((position   (point-min))
	    (limit      (point-max))
	    (hash-table (wikipedia-get-link-associations)))
	(while (< (progn (unless (get-text-property position
						    'wikipedia-link-address)
			   (setq position (next-single-property-change
					   position 'wikipedia-link-address
					   nil limit)))
			 position)
		  limit)
	  (let* ((link-address (get-text-property position
						  'wikipedia-link-address))
		 (link-text
		  (buffer-substring-no-properties
		   position
		   (setq position (next-single-property-change
				   position 'wikipedia-link-address
				   nil limit))))
		 (link-text-downcased (downcase link-text)))
	    (unless (or (string= link-text-downcased (downcase link-address))
			(gethash link-text-downcased hash-table))
	      (puthash link-text-downcased link-address hash-table)))))
    (when (interactive-p)
      (error "Link associations are turned off\
 (see `wikipedia-use-link-associations' variable)"))))


;;; Markup mode switching.

(defun wikipedia-refresh-buffer (&optional arg)
  "Refresh current buffer's text by hiding any (new) markup.
With prefix argument, refresh only the current paragraph."
  (interactive "P")
  (if arg
      (wikipedia-hide-region-markup (point) (point))
    (wikipedia-hide-region-markup (point-min) (point-max))))

(defun wikipedia-show-buffer-markup (&optional arg)
  "Show current buffer's text markup (e.g. for copying to a browser.)
With prefix argument, show only the markup of the current
paragraph."
  (interactive "P")
  (if arg
      (wikipedia-show-region-markup (point) (point))
    (wikipedia-show-region-markup (point-min) (point-max))))


;; Visiting articles.

(defvar wikipedia-article-database nil
  "Records the database used, if any, for each buffer.")
(make-variable-buffer-local 'wikipedia-article-database)

(defun wikipedia-follow-link ()
  "Follow link under point.
If there is no link under point, use `wikipedia-visit-article' to
prompt for an article to visit."
  (interactive)
  (let ((link-address (wikipedia-link-address-at (point))))
    (if link-address
	(wikipedia-visit-article link-address nil wikipedia-article-database)
      (call-interactively 'wikipedia-visit-article))))

(defcustom wikipedia-database "wikidb"
  "Database holding the Wikipedia.")

(defcustom wikipedia-database-user "root"
  "User to use when accessing a wikipedia database directly.")

(defun wikipedia-visit-local-article (name &optional no-redirection)
  ;; FIX same as wp-v-art interactive clause
  (interactive (list (let ((default-domain (wikipedia-get-domain-safe))
			   (keymap         (make-sparse-keymap)))
		       (set-keymap-parent keymap
					  minibuffer-local-completion-map)
		       (define-key keymap " " nil)
		       (let ((minibuffer-local-completion-map keymap))
			 (completing-read
			  (format "Visit (%s) Wikipedia article (%s): "
				  default-domain (word-at-point))
			  (wikipedia-get-list-of-namespaces default-domain)
			  nil nil nil nil (word-at-point) t)))
		     current-prefix-arg))
  (wikipedia-visit-article name no-redirection wikipedia-database))

(defun wikipedia-visit-article (name &optional no-redirection database)
  "Visit given Wikipedia article.
When called interactively, read article name in the minibuffer.

If DATABASE is non-nil then it is taken as the name of a MySQL
database from which the article must be retrieved.

By default, this function follows Wikipedia redirections.
However, you can specify a non-nil NO-REDIRECTION
variable (prefix argument when called interactively) to change
this behaviour."
  (interactive (list (let ((default-domain (wikipedia-get-domain-safe))
			   (keymap         (make-sparse-keymap)))
		       (set-keymap-parent keymap
					  minibuffer-local-completion-map)
		       (define-key keymap " " nil)
		       (let ((minibuffer-local-completion-map keymap))
			 (completing-read
			  (format "Visit (%s) Wikipedia article (%s): "
				  default-domain (word-at-point))
			  (wikipedia-get-list-of-namespaces default-domain)
			  nil nil nil nil (word-at-point) t)))
		     current-prefix-arg))
  (let ((domain (if (let ((case-fold-search nil))
		      (string-match wikipedia-interwiki-link-regexp name))
		    (prog1
			(match-string 1 name)
		      (setq name (match-string 2 name)))
		  (wikipedia-get-domain-safe))))
    (unless (assoc domain wikipedia-language-domains)
      (error "Sorry, language domain `%s' is not supported" domain))

    (let ((blist (buffer-list))
	  (need-article t))
      (while (and blist need-article)
	(let ((buffer-name (buffer-name (car blist)))
	      (name-width (string-width name)))
	  (if (and (eq (compare-strings buffer-name 0 name-width
					name 0 name-width)
		       t)
		   (or (eq (string-width buffer-name) name-width)
		       (eq (aref buffer-name name-width)
			   ;; FIX could check for entire "<n>" suffix
			   ?<))
		   (with-current-buffer (car blist)
		     (eq major-mode 'wikipedia-mode)))
	      (progn
		(switch-to-buffer (car blist))
		(setq need-article nil)))
	  (setq blist (cdr blist))))
      (if need-article
	  (if database
	      (progn
		(switch-to-buffer (concat "*wp:" name "*"))
		(setq emacs-wiki-database database)
		;; 		 ;; use first part of "piped link"
		;; 		 (if (string-match "^\\(.*\\)|" name)
		;; 		     (setq name (match-string-no-properties 1 name)))
		;; clip trailing whitespace
		;; FIX what was wrong with this?
		;; 	(if (string-match "^\\(.*\\)[:blank:]$" name)
		;; 	    (setq name (match-string-no-properties 1 name)))
		(if (string-match "\\s-+$" name)
		    (setq name (replace-match "" t t name)))
		;; convert spaces in name to underscores
		(while (string-match " " name)
		  (setq name (replace-match "_" t t name)))
		;; name in db seems to always begin capitalized
		(aset name 0 (upcase (aref name 0)))
		(let* ((command-begin (concat "mysql -u"
					      wikipedia-database-user
					      " -D"
					      database
					      " --skip-column-names --disable-pager --raw -e \"select cur_text from cur where cur_title = \\\""))
		       (command-end "\\\" limit 2\"")
		       (command (concat command-begin
					name
					command-end)))
		  (shell-command command (current-buffer))
		  (when (eq (point-max) 1)
		    ;; Try again with name in upper case, e.g. for acronyms.
		    (setq command (concat command-begin
					  (upcase name)
					  command-end))
		    (shell-command command (current-buffer)))
		  (when (eq (point-max) 1)
		    ;; Try again with each word in article name capitalised,
		    ;; e.g. for people's names.
		    (setq command (concat command-begin
					  (capitalize name)
					  command-end))
		    (shell-command command (current-buffer)))
		  (when (eq (point-max) 1)
		    (kill-this-buffer)
		    (message command)
		    (error "Query result empty.")))
		(wikipedia-handle-visited-article domain name no-redirection database))
	    (url-retrieve (concat "http://" domain ".wikipedia.org/w/index.php?title="
				  (wikipedia-hexify-string name domain)
				  "&action=raw")
			  'wikipedia-handle-visited-article (list domain name
								  no-redirection)))))))

;;; Adding, removing and otherwise altering markup.

(defun wikipedia-create-alter-or-delete-link (arg)
  "Create, alter or delete a hyperetext link.

This command behaviour depends on whether Transient Mark mode is
active.  In both cases link's destination article name is read
from the minibuffer.  When creating a link from a region of text,
any links that might already be there are deleted.

* In Transient Mark mode

If the mark is active, create a link from text in the selected
region.  The region is first shrinked to exclude leading and
terminal whitespace and then expanded to include only complete
words, if needed.  With prefix argument, delete any links that
intersect with the region.

If the mark is inactive and the point is outside any existing
links, create a link from the word under point (with prefix
argument, that many words, see `forward-word'.)

If the mark is inactive and the point is on an existing link,
change that link's destination article.  When called with a
prefix argument, delete the link under point.

* Not in Transient Mark mode

If there is an existing link under the point, change that link's
destination article name or, with a prefix argument, delete the
link.

If there is no link under the point, create a new one from the
text between the point and the mark.  With a prefix argument,
create a link from that many words.  (See Transient Mark mode
section for details.)"
  (interactive "*P")
  (let* ((link-region (wikipedia-markup-region-at (point) t
						  'wikipedia-link-address))
	 (link-start  (car link-region))
	 (link-end    (cdr link-region)))
    (wikipedia-without-after-change-hook
      (if (and link-region (not (and transient-mark-mode mark-active)))
	  (if arg
	      ;; Delete the link.
	      (wikipedia-delete-markup-region link-start link-end
					      'wikipedia-link-address)
	    ;; Change link's address.
	    (let ((current-link-address (get-text-property
					 link-start 'wikipedia-link-address)))
	      (put-text-property
	       link-start link-end 'wikipedia-link-address
	       (wikipedia-make-all-spaces-normal
		(read-string "Make the link point to: " current-link-address
			     'wikipedia-link-addresses-history nil t)))))
	(if mark-active
	    (setq link-start (region-beginning)
		  link-end   (region-end))
	  (setq link-start (point)
		link-end   (point)))
	(if (and arg transient-mark-mode mark-active)
	    ;; Delete all links intersecting with the region.
	    (wikipedia-delete-markup-subregions link-start link-end t
						'wikipedia-link-address)
	  ;; Create a new link.
	  (save-excursion
	    (when arg
	      ;; A new link from `arg' words.
	      (let ((word-count (prefix-numeric-value arg)))
		(if (>= word-count 0)
		    (setq link-start (point)
			  link-end   (progn (forward-word word-count)
					    (point)))
		  (setq link-end   (point)
			link-start (progn (forward-word word-count)
					  (point))))))
	    ;; Shrink/expand region as explained in the doc string.
	    (setq link-start (progn
			       (goto-char link-start)
			       (skip-syntax-forward " " link-end)
			       (unless (looking-at "\\<")
				 (skip-syntax-backward "w"))
			       (point))
		  link-end   (progn
			       (goto-char link-end)
			       (skip-syntax-backward " " link-start)
			       (unless (looking-at "\\>")
				 (skip-syntax-forward "w"))
			       (point))))
	  (when (= link-start link-end)
	    (error "Nothing to set a link from"))
	  (let* ((link-text
		  (wikipedia-make-all-spaces-normal
		   (buffer-substring-no-properties link-start link-end)))
		 (link-text-without-quotes
		  (let ((proper-quotes (wikipedia-get-proper-quotes)))
			(if (or (and (= (aref link-text 0)
					(aref proper-quotes 0))
				     (= (aref link-text (1- (length
							     link-text)))
					(aref proper-quotes 1)))
				(and (= (length proper-quotes) 4)
				     (= (aref link-text 0)
					(aref proper-quotes 2))
				     (= (aref link-text (1- (length
							     link-text)))
					(aref proper-quotes 3))))
			    (substring link-text 1 -1)
			link-text)))
		 (link-text-downcased      (downcase link-text-without-quotes))
		 (hash-table               (wikipedia-get-link-associations))
		 (initial-address
		  (or (when (wikipedia-use-link-associations-for-domain)
			(let ((associated-address (gethash link-text-downcased
							   hash-table)))
			  (when associated-address
			    (aset associated-address 0
				  (funcall
				   (if (= (aref link-text-without-quotes 0)
					  (downcase
					   (aref link-text-without-quotes 0)))
				       'downcase
				     'upcase)
				   (aref associated-address 0)))
			    associated-address)))
		      link-text-without-quotes))
		 (link-address
		  (wikipedia-make-all-spaces-normal
		   (read-string (format "Create a link from %s to: " link-text)
				initial-address
				'wikipedia-link-addresses-history nil t))))
	    (wikipedia-delete-markup-subregions link-start link-end t
						'wikipedia-link-address)
	    (wikipedia-create-markup-region link-start link-end
					    'wikipedia-link-address
					    link-address)
	    (when (wikipedia-use-link-associations-for-domain)
	      (if (string= (downcase link-address)
			   (downcase link-text-without-quotes))
		  (remhash (downcase link-text) hash-table)
		(puthash (downcase link-text) link-address hash-table)))))))))


(defun wikipedia-add-or-remove-emphasis (arg)
  "Add or remove emphasis (italic text.)

If the mark is active and not all characters in the selected
region are emphasized, then emphasize them all.  Alternatively,
if they are already emphasized or if a prefix argument is
specified, remove emphasis from the region.

If the mark is inactive (in Transient Mark mode) and the point is
within an emphasized region, remove that emphasis.  Else
emphasize text from the point position to the end of word (with
prefix argument, that many words, see `forward-word') and move
the point there."
  (interactive "*P")
  (wikipedia-do-add-or-remove-emphasis 'wikipedia-emphasis arg))

(defun wikipedia-add-or-remove-strong-emphasis (arg)
  "Add or remove strong emphasis (bold text.)
The behaviour of this function is the same as of
`wikipedia-add-or-remove-emphasis', except that it deals with
strong emphasis."
  (interactive "*P")
  (wikipedia-do-add-or-remove-emphasis 'wikipedia-strong-emphasis arg))

;; This function is actually internal, but since it does all the work
;; of the interactive functions above, let's keep it here.
(defun wikipedia-do-add-or-remove-emphasis (property arg)
  (let ((emphasis-region (wikipedia-markup-region-at (point) nil property)))
    (wikipedia-without-after-change-hook
      (if mark-active
	  (let* ((start (region-beginning))
		 (end   (region-end)))
	    (if (and (null arg)
		     (text-property-any start end property nil))
		(wikipedia-create-markup-region start end property t)
	      (wikipedia-delete-markup-subregions start end nil property)))
	(let ((emphasis-region (wikipedia-markup-region-at (point) nil
							   property)))
	  (if emphasis-region
	      (wikipedia-delete-markup-region (car emphasis-region)
					      (cdr emphasis-region)
					      property)
	    (wikipedia-create-markup-region
	     (point)
	     (progn (forward-word (prefix-numeric-value arg))
		    (point))
	     property t)))))))


;;; Special characters and electric keys.

(defun wikipedia-insert-special-character (arg)
  "Prompt for a special character name and insert that character.
Quotes depend on the article language.  However, you probably
don't want to insert quotes with this function, they are also
inserted with `wikipedia-electric-quote', normally bound to
`\"' key.

This function can be used in non-Wikipedia mode buffers too.  In
this case proper quotes will not be included in the list of
characters.

With prefix argument, insert that many characters at once."
  (interactive "*P")
  (let* ((character-alist
	  (if (boundp 'wikipedia-article-domain-description)
	      (let ((proper-quotes (wikipedia-get-proper-quotes)))
		(append (list (cons "left quote" (aref proper-quotes 0))
			      (cons "right quote" (aref proper-quotes 1)))
			(when (= (length proper-quotes) 4)
			  (list (cons "alternative left quote"
				      (aref proper-quotes 2))
				(cons "alternative right quote"
				      (aref proper-quotes 3))))
			wikipedia-special-characters))
	    wikipedia-special-characters))
	 (character (cdr (assoc (completing-read "Insert special character: "
						 character-alist nil t)
				character-alist))))
    (when character
      (insert-char character (setq arg (prefix-numeric-value arg)))
      (when (= character ?Â )
	(wikipedia-without-after-change-hook
	  (wikipedia-highlight-non-breaking-spaces arg))))))


(defun wikipedia-electric-space (arg)
  "Insert a space character.

Only for UTF-8 language domains: when there is whitespace, dot or
comma followed by a hyphen right before the point, replace the
hyphen with dash.  A space before the new dash is made
non-breaking.  This functionality is only active when
`wikipedia-electric-keys-enabled' is non-nil.

This function treats prefix argument in the standard way."
  (interactive "*P")
  (when (and wikipedia-electric-keys-enabled
	     (eq (wikipedia-get-domain-coding) 'utf-8)
	     (= (preceding-char) ?-)
	     (memq (char-after (- (point) 2)) '(?  ?Â  ?\t ?\n ?. ?, nil)))
    (wikipedia-replace-preceding-character ?â€”)
    (save-excursion
      (backward-char)
      (when (and (= (preceding-char) ? )
		 (/= (char-syntax (char-before (1- (point)))) ? ))
	(wikipedia-replace-preceding-character ?Â ))))
  (self-insert-command (prefix-numeric-value arg)))

(defun wikipedia-electric-hyphen (arg)
  "Insert a hyphen.

\(Only for UTF-8 language domains.)  When there is another hyphen
right before the point, combine it with the new hyphen into
em-dash or en-dash depending on article's language.  When there
is en-dash before the point, combine into em-dash
unconditionally.

When there is a plus sign before the point, combine it with the
hyphen into a plus-minus sign.

Functionality described above is disabled if
`wikipedia-electric-keys-enabled' is nil.

This function treats prefix argument in the standard way except
that any prefix argument disables special functionality described
above."
  (interactive "*P")
  (if (or arg (not wikipedia-electric-keys-enabled))
      (self-insert-command (prefix-numeric-value arg))
    (cond ((and (eq (wikipedia-get-domain-coding) 'utf-8)
		(memq (preceding-char) '(?- ?â€“)))
	   (wikipedia-replace-preceding-character
	    (if (and (= (preceding-char) ?-)
		     (wikipedia-get-use-en-dash))
		?â€“ ?â€”)))
	  ((= (preceding-char) ?+)
	   (wikipedia-replace-preceding-character ?Â±))
	  (t
	   (insert ?-)))))

(defun wikipedia-electric-equal-sign (arg)
  "Insert an equal (to) sign.

When there is a slash, another equal (to) sign, less-than sign or
greater-than sign right before point, replace it with not-equal,
identical (to), less-or-equal or greater-or-equal sign
correspondingly.  However, this is only enabled if
`wikipedia-electric-keys-enabled' variable is non-nil.

This function treats prefix argument in the standard way except
that any prefix argument disables special functionality described
above."
  (interactive "*P")
  (let ((preceding-char (preceding-char)))
    (if (or arg
	    (not wikipedia-electric-keys-enabled)
	    (not (memq preceding-char '(?/ ?< ?>))))
	(self-insert-command (prefix-numeric-value arg))
      (wikipedia-replace-preceding-character
       (cdr (assoc preceding-char '((?/ . ?â‰ ) (?< . ?â‰¤) (?> . ?â‰¥))))))))

(defun wikipedia-electric-apostrophe (arg)
  "Insert a proper apostrophe sign.

Since typewriter's apostrophe is used as Wikipedia markup for
emphasis, this function tries to allow both usage as a text
symbol and as a markup symbol, despite the mode's handling of
emphasis markup.  It works like this: if the characters before
and after the point are not apostrophes (proper or typewriter's),
a proper apostrophe sign is inserted.  Otherwise, it inserts a
typewriter's apostrophe and additionally converts all preceding
and following proper apostrophes to typewriter's variant.

The described functionality is only active when
`wikipedia-electric-keys-enabled' is non-nil and domain's coding
system is UTF-8.

When a prefix argument is specified, insert a number of
typewriter's apostrophes unconditionally."
  (interactive "*P")
  (if (or arg
	  (not wikipedia-electric-keys-enabled)
	  (not (eq (wikipedia-get-domain-coding) 'utf-8)))
      (self-insert-command (prefix-numeric-value arg))
    (if (or (memq (preceding-char) '(?' ?â€™))
	    (memq (following-char) '(?' ?â€™)))
	(let* ((current-point      (point))
	       (end-of-apostrophes (progn
				     (skip-chars-forward "'â€™")
				     (point)))
	       (apostrophes-string (progn
				     (skip-chars-backward "'â€™")
				     (buffer-substring (point)
						       end-of-apostrophes))))
	  ;; Replace all apostrophes with typewriter's apostrophes.
	  (store-substring apostrophes-string 0
			   (make-string (length apostrophes-string) ?'))
	  (wikipedia-without-after-change-hook
	    (delete-char (length apostrophes-string))
	    (insert apostrophes-string))
	  (goto-char current-point)
	  (insert ?'))
      (insert ?â€™))))

(defun wikipedia-electric-quote (arg)
  "Insert the proper quote based on article's language and context.
Instead of of typewriter's pseudo-quote (`\"') the proper opening
or closing quote is inserted.  Different nested quotes are
supported for the languages that have them.  Note, that this
functionality works only for UTF-8 language domains and only when
`wikipedia-electric-keys-enabled' is non-nil.

Wikipedia mode tries to guess which proper quote to insert based
on context.  It is usually right, but not always.  If it inserts
not the quote you want, you try using a minus as a prefix
argument: this flips the inserted quote (i.e. inserts the opening
quote if the mode thinks a closing quote is appropriate and vice
versa.)  If this is still not enough, you can always use
`wikipedia-insert-special-character', normaly bound to C-c C-s.

When a prefix argument other than a single minus is specified,
insert a number of typewriter's pseudo-quotes."
  (interactive "*P")
  (if (or (and arg (not (eq arg '-)))
	  (not wikipedia-electric-keys-enabled)
	  (not (eq (wikipedia-get-domain-coding) 'utf-8)))
      (self-insert-command (prefix-numeric-value arg))
    (let ((proper-quotes (wikipedia-get-proper-quotes))
	  (quote-index   (if (memq (char-before)
				   '(? ?Â  ?\t ?\n ?- ?â€” ?â€“ ?/ ?\( ?\[ ?{ nil))
			     0 1)))
      (when (= (length proper-quotes) 4)
	(save-excursion
	  (skip-chars-backward (concat "^" proper-quotes))
	  (when (= (preceding-char) (aref proper-quotes
					  (+ quote-index quote-index)))
	    (setq quote-index (+ quote-index 2)))))
      (when (eq arg '-)
	(setq quote-index (logxor quote-index 1)))
      (insert (aref proper-quotes quote-index)))))

(defun wikipedia-electric-dot (arg)
  "Insert a dot.

Only for UTF-8 language domains: when there are two dots right
before the point, combine them together with the new one into
ellipsis.  When `wikipedia-electric-keys-enabled' is nil, this
functionality is disabled.

This function treats prefix argument in the standard way except
that any prefix argument disables ellipsis functionality."
  (interactive "*P")
  (if (and (null arg)
	   wikipedia-electric-keys-enabled
	   (eq (wikipedia-get-domain-coding) 'utf-8)
	   (looking-back "\\.\\."))
      (progn
	(delete-backward-char 1)
	(wikipedia-replace-preceding-character ?â€¦))
    (self-insert-command (prefix-numeric-value arg))))

(defun wikipedia-electric-digit (arg)
  "Insert a digit.

When there is whitespace and a hyphen right before the point,
replace the hyphen with the minus sign.  When there is a digit,
number sign or section sign followed by a space right before the
point, make the space non-breaking.  This is disabled when
`wikipedia-electric-keys-enabled' is nil.

This function treats prefix argument in the standard way."
  (interactive "*P")
  (when wikipedia-electric-keys-enabled
    (cond ((and (= (preceding-char) ?-)
		(memq (char-after (- (point) 2)) '(? ?Â   ?\t ?\n nil)))
	   (wikipedia-replace-preceding-character ?âˆ’))
	  ((let ((search-space-regexp nil))
	     (looking-back "[[:digit:]Â§â„–] "))
	   (wikipedia-replace-preceding-character ?Â ))))
  (self-insert-command (prefix-numeric-value arg)))

(defun wikipedia-electric-pseudo-number-sign (arg)
  "Insert UTF-8 number sign.
This is disabled if `wikipedia-electric-keys-enabled' is set to
nil."
  (interactive "*P")
  (if wikipedia-electric-keys-enabled
      (insert-char ?â„– (prefix-numeric-value arg))
    (self-insert-command (prefix-numeric-value arg))))

(defun wikipedia-electric-keys (arg)
  "Toggle or set `wikipedia-electric-keys-enabled' variable for the current buffer.

When called without a prefix argument, toggle the variable.
Otherwise set `wikipedia-electric-keys-enabled' to t if and only
if the prefix argument is positive.

This function is useful to temporarily disable electric keys (for
instance, for entering sample code, which should not contain
fancy characters) and then enable it back."
  (interactive "P")
  (setq wikipedia-electric-keys-enabled
	(if arg
	    (> arg 0)
	  (not wikipedia-electric-keys-enabled)))
  (when (called-interactively-p)
    (message (if wikipedia-electric-keys-enabled
		 "Electric keys enabled"
	       "Electric keys disabled"))))


(defun wikipedia-insert-non-breaking-space (arg)
  "Insert a non-breaking space."
  (interactive "*P")
  (insert-char ?Â  (setq arg (prefix-numeric-value arg)))
  (wikipedia-without-after-change-hook
    (wikipedia-highlight-non-breaking-spaces arg)))


(defun wikipedia-next-reference ()
  "Move to next link in buffer."
  (interactive)
  (let ((first t) pos)
	;; Search for the next reference, one paragraph at a time.
	(while (and (eq pos nil)
				(eq (let ((case-fold-search nil))
					  (while (looking-at "\n")
						(forward-char))
					  (let ((point (point)) (end (make-marker)))
						(save-excursion
						  (forward-paragraph)
						  (while (looking-at "[^\n]")
							(forward-paragraph))
						  (set-marker end (point)))
						(wikipedia-show-region-markup point end)
						(setq point (point))
						(save-excursion
						  ;; FIX always set first nil here?
						  (when (and first (looking-at "'*\\[\\["))
							(goto-char (match-end 0))
							(setq first nil))
						  (if (re-search-forward "\\[\\[" end t)
							  (setq pos (match-beginning 0))))
						(if pos
							(goto-char pos))
						(wikipedia-hide-region-markup point end))
					  pos)
					nil)
				;; FIX on last jump
				(re-search-forward "\n\n" (point-max) t)))))


(defun wikipedia-previous-reference ()
  "Move to previous link in buffer."
  (interactive)
  (let ((first t) pos)
	(while (and (eq pos nil)
				(eq (let ((case-fold-search nil))
					  (while (looking-at "\n")
						(backward-char))
					  (let ((end (point)) start)
						(save-excursion
						  (backward-paragraph)
						  (or (looking-at "\n")
							  (backward-paragraph))
						  (setq start (point)))
						(or (eq start end)
							(progn
							  (wikipedia-show-region-markup start end)
							  (setq end (point))
							  (save-excursion
								(when (and first (looking-at "'*\\[\\["))
								  (backward-char)
								  (setq first nil))
								;; FIX limit search according to end
								(if (re-search-backward "\\[\\[" nil t)
									(setq pos (match-beginning 0))))
							  (if pos
								  (goto-char pos))
							  (wikipedia-hide-region-markup start end)))
						pos))
					nil)
				;; FIX on last jump
				(re-search-backward "\n\n" (point-min) t)))))



;;;; Internal functions.

;;; Visiting articles.

(defvar wikipedia-convert-symbols t)
(defun wikipedia-convert-symbols ()
  "Convert &lambda; style symbols in the current buffer into letters.
Conversion is only performed if `wikipedia-convert-symbols' is
non-nil.  All such symbols from point to the end of the buffer
are converted."
  (when wikipedia-convert-symbols
    (message "Converting symbols...")
    ;; FIX these conversions rule out wikipedia editing
    ;;         could convert back on save
    ;; FIX compare with emacs-wiki-changelog-markup
    ;; first try, now a substitution
    ;; 	    (mapc (lambda (assoc)
    ;; 		    (goto-char (point-min))
    ;; 		    (while (re-search-forward (car assoc) nil t)
    ;; 		      (replace-match (match-string-no-properties 1) t)
    ;; 		      (funcall (cadr assoc))))
    ;; 		  '(
    ;; 		    ("<[Ii]>\\(.*?\\)</[Ii]>"
    ;; 		     (lambda ()
    ;; 		       (add-text-properties (match-beginning 1)
    ;; 					    (- (match-end 1) 7)
    ;; 					    '(face italic))))
    ;; 		    ("<[Bb]>\\(.*?\\)</[Bb]>"
    ;; 		     (lambda ()
    ;; 		       (add-text-properties (match-beginning 1)
    ;; 					    (- (match-end 1) 7)
    ;; 					    '(face bold))))))
    (mapc (lambda (assoc)
	    (goto-char (point-min))
	    (let ((case-fold-search nil))
	      ;; FIX skip <code>...</code> areas
	      ;; 			(while (re-search-forward (concat (car assoc)
	      ;; 							  "\|<code>.*?</code>")
	      ;; 						  nil t)
	      ;; 			  ;;(buffer-substring (- (point) 7) (point))
	      ;; 			  (or (string= (substring (match-string 0) 0 7)
	      ;; 				       "<code>")
	      ;; 			      (replace-match (eval (eval (cadr assoc))) t)))))
	      (while (re-search-forward (car assoc) nil t)
		(replace-match (eval (eval (cadr assoc))) t))))
	  ;; FIX use a hashtable
	  '( ;; FIX maybe do this in grey/white
	    ;;("<code>\\s-*\\(.*?\\)</code>" "\\1")
	    ("&alpha;" "Î±")
	    ("&Alpha;" "Î‘")
	    ("&beta;" "Î²")
	    ("&Beta;" "Î’")
	    ("&chi;" "Ï‡")
	    ("&Chi;" "Î§")
	    ("&delta;" "Î´")
	    ("&eta;" "Î·")
	    ("&epsilon;" "Îµ")
	    ("&gamma;" "Î³")
	    ("&Gamma;" "Î“")
	    ("&iota;" "Î¹")
	    ("&kappa;" "Îº")
	    ("&lambda;" "Î»")
	    ("&Lambda;" "Î›")
	    ("&mu;" "Î¼")
	    ("&nu;" "Î½")
	    ("&omega;" "Ï‰")
	    ("&Omega;" "Î©")
	    ("&phi;" "Ï†")
	    ("&pi;" "Ï€")
	    ("&rho;" "Ï")
	    ("&sigma;" "Ïƒ")
	    ("&tau;" "Ï„")
	    ("&Tau;" "Î¤")
	    ("&theta;" "Î¸")
	    ("&Theta;" "Î˜")
	    ("&upsilon;" "Ï…")
	    ("&xi;" "Î¾")
	    ("&spades;" "â™ ")
	    ("&hearts;" "â™¥")
	    ("&diams;" "â™¦")
	    ("&clubs;" "â™£")
	    ("&eacute;" "Ã©")
	    ("&deg;" "Â°")
	    ("&times;" "Ã—")
	    ("&rarr;" "â†’")
	    ("&rArr;" "â†’")
	    ("&larr;" "â†")
	    ("&lArr;" "â†")
	    ("&hArr;" "â‡”")
	    ("&harr;" "â†”")
	    ("&uarr;" "â†‘")
	    ("&mdash;" "â€•")
	    ("&le;" "<=")
	    ("&ge;" ">=")
	    ("&lt;" "<")
	    ("&gt;" ">")
	    ("&ne;" "!=")
	    ("&equiv;" "â‰¡")
	    ("&asymp;" "â‰ˆ")
	    ("&empty;" "âˆ…")
	    ("&isin;" "âˆˆ")
	    ("&notin;" "âˆ‰")
	    ("&sube;" "âŠ†")
	    ("&supe;" "âŠ‡")
	    ("&cup;" "âˆª")
	    ("&sub;" "âŠ‚")
	    ("&sup;" "âŠƒ")
	    ("&prod;" "âˆ")
	    ("&int;" "âˆ«")
	    ("&exist;" "âˆƒ")
	    ("&minus;" "-")
	    ("&divide;" "Ã·")
	    ("&ldquo;" "â€œ")
	    ("&rdquo;" "â€")
	    ("&rsquo;" "â€™")
	    ("&lsquo;" "â€˜")
	    ("&prime;" "â€²")
	    ("&Prime;" "â€³")
	    ("&quot;" "\"")
	    ("&and;" "âˆ§")
	    ("&or;" "âˆ¨")
	    ("&not;" "Â¬")
	    ("&perp;" "âŠ¥")
	    ("&cap;" "âˆ©")
	    ("&infin;" "âˆž")
	    ("&forall;" "âˆ€")
	    ("&part;" "âˆ‚")
	    ("&plusmn;" "Â±")
	    ("&sup2;" "Â²")
	    ("&sup3;" "Â³")
	    ("&psi;" "Ïˆ")
	    ("&prime;" "â€²")
	    ("&radic;" "âˆš")
	    ("&sum;" "âˆ‘")
	    ("&ndash;" "ï¿½")
	    ("&apos;" "'")
	    ("&otimes;" "âŠ—")
	    ("&amp;" "&")
	    ("&ouml;" "Ã¶")
	    ("<sup>\\s-*\\(.*?\\)</sup>" "^\\1")
	    ("<sub>\\s-*\\(.*?\\)</sub>" "[\\1]")
	    ("<var>\\s-*\\(.*?\\)</var>" "\\1")
	    ("<pre>\n?\\(\\(.\\|\n\\)*?\\)</pre>" "\\1")
	    ("<center>\n?\\(\\(.\\|\n\\)*?\\)</center>" "\\1")
	    ;; FIX maybe do this in grey/white
	    ;; FIX maybe add a post replace eval element to the list
	    ;;   (add-text-properties (+ 7 (match-beginning 0))
	    ;;                        (line-end-position)
	    ;;                        '(face emacs-wiki-header-1)))
	    ("<tt>\n?\\(\\(.\\|\n\\)*?\\)</tt>" "\\1")
	    ;; FIX insert \\1 in a slightly smaller font
	    ;; FIX maybe add a post replace eval element to the list
	    ;;("<small>\n?\\(\\(.\\|\n\\)*?\\)</small>" "\\1")
	    ("<[iI]>\\s-*\\(.*?\\)</[iI]>" "''\\1''")
	    ("<[bB]>\\s-*\\(.*?\\)</[bB]>" "'''\\1'''")
	    ("&nbsp;" " ")
;;; unicode
	    ("&#\\([0-9]+\\);"
	     '(char-to-string
	       (or (decode-char 'ucs
				(string-to-number (match-string 1)))
		   ??)))
	    ;; FIX merge with above
	    ("&#x\\([0-9ABCDEFabcdef]+\\);"
	     '(char-to-string
	       (or (decode-char 'ucs
				(string-to-number (match-string 1) 16))
		   ??)))
;;; Maths mode.
	    ("\\\\hat" "\\\\hat{}")))
    (message "Converting symbols... done.")))

(defvar wikipedia-convert-maths nil)
(defun wikipedia-convert-maths ()
  "Convert <maths>...</maths> items in the current buffer into images.
Conversion is only performed if `wikipedia-convert-maths' is
non-nil.  All items from point to the end of the buffer are
converted."
  (when wikipedia-convert-maths
    (message "Converting maths environments...")
    (save-excursion
      ;; FIX check pngs available
      (goto-char (point-min))
      (while (re-search-forward "\\(?::?:\\s-*\\)?<maths?>\\(\\(\n\\|.\\)*?\\)<\\(/\\|\\\\\\)maths?>"
				nil t)
	;; FIX defun?
	(let* ((math (match-string-no-properties 1))
	       (file (expand-file-name
		      (make-temp-file "wikipedia" nil ".tex")))
	       (file-body (file-name-sans-extension file)))
;;; FIX here could replace \begin{matrix}...\end{matrix} in math (?)
	  (save-match-data
	    (with-temp-file file
	      (insert "\\documentclass{article}
\\usepackage{preview}
\\usepackage{amsmath}
\\usepackage{amsfonts}
\\usepackage{amssymb}
\\usepackage{amscd}
\\usepackage{amsopn}
\\usepackage{amstext}
\\usepackage{amsxtra}
\\pagestyle{empty}
\\newcommand\\infin{\\infty}
\\newcommand\\rarr{\\to}
\\newcommand\\rang{\\rangle}
\\newcommand\\lang{\\langle}
\\newcommand\\exist{\\exists}
\\newcommand\\sgn{\\mathop{\\mathrm{sgn}}}
\\newcommand\\sub{\\subset}
%\\newcommand\\or{\\lor}
% FIX are these correct?
\\newcommand\\R{\\\\R}
\\newcommand\\N{\\\\N}
\\begin{document}
$")
	      ;; Flush blank lines from `math'.
	      ;; FIX only from end?
	      (let ((start 0))
		;; FIX repeat one replace loop
		(while (setq start (string-match "^[ \t]*\n" math start))
		  (setq math (replace-match "" nil nil math)))
		(setq start 0)
		;; Replace, for e.g., x^\sqrt{2} with x^{\sqrt{2}}.
		(while (setq start (string-match "\\^\\\\sqrt{\\(.+\\)}" math start))
		  (setq math (replace-match "^{\\\\sqrt{\\1}}" nil nil math))
		  (setq start (+ start 10
				 (- (match-end 1) (match-beginning 1)))))
		(setq start 0)
		(while (setq start (string-match "\\part" math start))
		  (setq start (+ start 8))
		  (setq math (replace-match "\\\\partial" nil nil math)))
		(setq start 0)
		(while (setq start (string-match "\\\\and" math start))
		  (setq start (+ start 8))
		  (setq math (replace-match "\\\\land" nil nil math)))
		(setq start 0)
		(while (setq start (string-match "\\\\or" math start))
		  (setq start (+ start 4))
		  (setq math (replace-match "\\\\lor" nil nil math))))
	      (insert math)
	      (insert "$\n\\end{document}\n"))
;;;		    ;; FIX check return
;;;		    (shell-command (concat "wikipedia-texpng " file-body)))
	    (or (equal (call-process-shell-command "emacs-wiki-texpng"
						   nil nil nil file-body)
		       0)
		(error (concat "emacs-wiki-texpng (" file-body ".tex) failed"))))

	  ;; 	      (replace-match (concat "[[" file-body
	  ;; 				     ;; FIX maybe put the mths envirmnt in a .txt file
	  ;; 				     ".png][FIX_maths_environment]]")
	  ;; ;;; FIX emacs-wiki requires multi-line (single-line?) image names
	  ;; ;;;					 ".png][$ " math " $]]")
	  ;; 			     t t)
	  (replace-match "" t t)
	  (insert-image (create-image
			 (concat file-body ".png")
			 'png))

	  ;; FIX this slows it down
	  ;; FIX delete on buffer kill? checking at emacs exit?
	  (delete-file file)
	  (delete-file (concat file-body ".log"))
	  (delete-file (concat file-body ".aux"))
	  (delete-file (concat file-body ".dvi"))
	  ))
      ;; FIX replace other occurrences of the match with the img
      ;; FIX free the imgs,files
      (message "Converting maths environments... done."))))

(defun wikipedia-handle-visited-article (domain name no-redirection &optional database keep-headers)
  (goto-char (point-min))
  (or database
      keep-headers
      (search-forward "\n\n" nil t)) ; Skip HTTP headers.
  (let ((text (decode-coding-string (buffer-substring-no-properties
				     (point) (point-max))
				    (wikipedia-get-domain-coding domain) t)))
    (kill-buffer (current-buffer))
    (if (and (not no-redirection)
	     (let ((case-fold-search t))
	       (string-match "^\\s-*#\\s-*redirect\\s-*\\[\\[\\(.+\\)\\]\\]"
			     text)))
	(let ((wikipedia-article-domain-description
	       (cdr (assoc domain wikipedia-language-domains))))
	  (wikipedia-visit-article (match-string 1 text) nil database))
      (let ((buffer (generate-new-buffer name)))
	(set-buffer buffer)
	(insert text)
	(unless (memq (char-before) '(?\n nil))
	  (insert ?\n))
	(goto-char (point-min))
	(set-buffer-modified-p nil)
	(set (make-local-variable
	      'wikipedia-article-domain-description)
	     (cdr (assoc domain wikipedia-language-domains)))
	(set (make-local-variable 'wikipedia-article-name) name)
	(wikipedia-mode)
	(save-excursion
	  (goto-char (point-min))
	  (delete-trailing-whitespace))
	(wikipedia-convert-symbols)
	(wikipedia-convert-maths)
	(setq wikipedia-article-database database)
	(switch-to-buffer buffer)))))


;; Note: not used currently, but may become used again for saving
;; articles support, unless Wikimedia team adds a non-HTML-friendly
;; way for that.
;;
;; FIXME: Add support for redirection if you revive this function.
(defun wikipedia-extract-article-text (domain name)
  (goto-char (point-min))
  (unless
      (and (search-forward "<textarea" nil t)
	   (search-forward ">" nil t)
	   (let ((text-start (point)))
	     (when (search-forward "</textarea>" nil t)
	       (let ((text   (buffer-substring-no-properties
			      text-start (- (point) (length "</textarea>"))))
		     (buffer (generate-new-buffer name)))
		 (kill-buffer (current-buffer))
		 (set-buffer buffer)
		 (insert (decode-coding-string
			  text (wikipedia-get-domain-coding domain) t))
		 (goto-char (point-min))
		 (let ((case-fold-search nil))
		   (while (re-search-forward "&\\(quot\\|amp\\|lt\\|gt\\);"
					     nil t)
		     (replace-match (cdr (assq (aref (match-string 1) 0)
					       '((?q . "\"")
						 (?a . "&")
						 (?l . "<")
						 (?g . ">"))))
				    t t)))
		 (goto-char (point-min))
		 (set-buffer-modified-p nil)
		 (set (make-local-variable
		       'wikipedia-article-domain-description)
		      (cdr (assoc domain wikipedia-language-domains)))
		 (set (make-local-variable 'wikipedia-article-name)   name)
		 (wikipedia-mode)
		 (display-buffer buffer)))))
    (pop-to-buffer (current-buffer))
    (error "Unable to locate article text")))


;;; Information/state retrieving.

;; Get language domain of current buffer or default language domain.
(defsubst wikipedia-get-domain-safe ()
  (if (boundp 'wikipedia-article-domain-description)
      (wikipedia-get-domain)
    wikipedia-default-language-domain))

;; Whether to use link associations for the DOMAIN.  If DOMAIN is nil
;; or omitted, use current domain instead.
(defsubst wikipedia-use-link-associations-for-domain (&optional domain)
  (or (not (listp wikipedia-use-link-associations))
      (member (or domain (wikipedia-get-domain))
	      wikipedia-use-link-associations)))


(defun wikipedia-get-link-associations-filename (domain)
  (expand-file-name (concat (file-name-as-directory wikipedia-mode-directory)
			    (convert-standard-filename
			     (format "%s-link-associations" domain)))))

(defun wikipedia-get-link-associations ()
  (let* ((domain     (wikipedia-get-domain))
	 (hash-table (cdr (assoc domain wikipedia-link-associations-alist))))
    (unless hash-table
      (setq wikipedia-link-associations-alist
	    (cons (cons domain
			(setq hash-table (make-hash-table :test 'equal)))
		  wikipedia-link-associations-alist))
      (let ((filename (wikipedia-get-link-associations-filename domain)))
	(when (file-readable-p filename)
	  (save-excursion
	    (progn
	      (let ((buffer (set-buffer (find-file-noselect filename t))))
		(goto-char (point-min))
		(condition-case nil
		    (mapc (lambda (key-value-cons)
			    (puthash (car key-value-cons) (cdr key-value-cons)
				     hash-table))
			  (read buffer))
		  (error nil))
		(kill-buffer buffer)))))))
    hash-table))


(defun wikipedia-link-address-at (position)
  (let ((link-region (wikipedia-markup-region-at position t
						 'wikipedia-link-address)))
    (when link-region
      (get-text-property (car link-region) 'wikipedia-link-address))))

(defun wikipedia-markup-region-at (position look-back property)
  (or (and (get-text-property position property)
	   (cons (previous-single-property-change (1+ position) property
						  nil (point-min))
		 (next-single-property-change position property
					      nil (point-max))))
      (and look-back
	   (> position (point-min))
	   (get-text-property (1- position) property)
	   (cons (previous-single-property-change position property
						  nil (point-min))
		 position))))


;;; Various hooks and related functions.

;; Evaluate BODY with `wikipedia-after-change' hook temporarily
;; disabled.  NOTE: this macro should be used in most functions that
;; modify buffer's text or properties.
(defmacro wikipedia-without-after-change-hook (&rest body)
  (declare (indent 0) (debug t))
  `(progn
     (remove-hook 'after-change-functions 'wikipedia-after-change t)
     ,@body
     (add-hook 'after-change-functions 'wikipedia-after-change nil t)))

;; A hook for `after-change-functions'.  Currently it has two duties:
;;
;; * Updating fixed/variable pitch face depending on lines' first
;;   characters.
;;
;; * Spreading special properties onto inserted text.  When a special
;;   property has the same value both to the left and to the right of
;;   an inserted text chunk, the whole chunk inherits the property.
;;   In addition, continous region of characters with word syntax
;;   which begins the chunk inherits special properties from the left.
;;   This last case is also checked for `wikipedia-link-address' after
;;   text deletion.
;;
(defun wikipedia-after-change (start end deleted-length)
  ;; Undo will do fine on its own, don't spoil its operations.
  (when (not undo-in-progress)
    (wikipedia-set-region-primary-face start end)
    (when (> start (point-min))
      (save-excursion
	(if (> deleted-length 0)
	    (let ((link-address (get-text-property (1- start)
						   'wikipedia-link-address)))
	      (when (and link-address
			 (> (skip-syntax-forward "w") 0)
			 (not (text-property-not-all start (point)
						     'wikipedia-link-address
						     nil)))
		(wikipedia-create-markup-region start (point)
						'wikipedia-link-address
						link-address)))
	  (goto-char start)
	  (while (re-search-forward "Â +" end t)
	    (wikipedia-highlight-non-breaking-spaces (- (point)
							(match-beginning 0))))
	  (goto-char start)
	  (skip-syntax-forward "w" end)
	  ;; Loop through all special properties.
	  (dolist (description wikipedia-property-faces)
	    (let* ((property (car description))
		   (value    (get-text-property (1- start) property)))
	      (when value
		(if (equal value (get-text-property end property))
		    (wikipedia-create-markup-region start end property value)
		  (when (> (point) start)
		    (wikipedia-create-markup-region start (point)
						    property value)))))))))))


(defun wikipedia-write-contents ()
  (when (and (buffer-modified-p) (not wikipedia-in-write-contents))
    (let ((buffer-undo-list            t) ; Temporarily disable undo.
	  (wikipedia-in-write-contents t) ; Avoid infinite recursion.
	  (buffer-contents             (buffer-substring (point-min)
							 (point-max))))
      (goto-char
       (prog1 (point)
	 (wikipedia-show-region-markup (point-min) (point-max))
	 (save-buffer)
	 (delete-region (point-min) (point-max))
	 (insert buffer-contents))))
    (set-buffer-modified-p nil)
    t))


(defun wikipedia-which-func ()
  (when wikipedia-auto-rescan-sections
    (save-excursion
      (end-of-line)
      (save-match-data
	(when (re-search-backward wikipedia-header-regexp nil t)
	  (match-string-no-properties 2))))))

(defun wikipedia-print-current-link-destination ()
  (let ((link-address (wikipedia-link-address-at (point))))
    (when link-address
      (concat "Link points to "
	      (propertize link-address 'face 'wikipedia-link-face)))))


(defun wikipedia-kill-emacs-hook ()
  (make-directory wikipedia-mode-directory t)
  (dolist (link-associations wikipedia-link-associations-alist)
    (let* ((filename   (wikipedia-get-link-associations-filename
			(car link-associations)))
	   (hash-table (cdr link-associations)))
      (when (find-buffer-visiting filename)
	(kill-buffer (find-buffer-visiting filename)))
      (when (file-exists-p filename)
	(delete-file filename))
      (save-excursion
	(set-buffer (find-file-noselect filename t))
	(erase-buffer)
	(insert ";; -*-coding: utf-8;-*-\n(")
	(maphash (lambda (key value)
		   (when (bolp)
		     (insert " "))
		   (insert (format "(%S\t.\t%S)\n" key value)))
		 hash-table)
	(backward-char)
	(insert ")")
	(write-file filename nil)
	(kill-buffer (current-buffer))))))


;;; Hiding/showing the markup.

(defun wikipedia-hide-region-markup (start end)
  (let ((end-marker	      (set-marker (make-marker) end))
	(paragraph-end-marker (make-marker)))
    (set-marker-insertion-type paragraph-end-marker t)
    (wikipedia-show-region-markup start end)
    (set-buffer-modified-p
     (prog1 (buffer-modified-p)
       (save-excursion
	 (goto-char start)
	 (backward-paragraph)
	 (setq start (point))
	 (wikipedia-without-after-change-hook
	   (wikipedia-set-region-primary-face start end)
	   (while (and (< start end-marker) (not (eobp)))
	     (forward-paragraph)
	     (wikipedia-hide-paragraph-markup start
					      (set-marker paragraph-end-marker
							  (point)))
	     (setq start (marker-position paragraph-end-marker)))))))
    (set-marker end-marker nil)
    (set-marker paragraph-end-marker nil)))

(defun wikipedia-show-region-markup (start end)
  (set-buffer-modified-p
   (prog1 (buffer-modified-p)
     (save-excursion
       (goto-char start)
       (backward-paragraph)
       (let ((paragraph-start-point (setq start (point)))
	     (end-marker            (set-marker (make-marker) end))
	     (paragraph-end-marker  (make-marker)))
	 (set-marker-insertion-type paragraph-end-marker t)
	 (wikipedia-without-after-change-hook
	   (while (progn
		    (forward-paragraph)
		    (set-marker paragraph-end-marker (point))
		    (wikipedia-show-paragraph-markup paragraph-start-point
						     paragraph-end-marker)
		    (when (> paragraph-end-marker end-marker)
		      (move-marker end-marker paragraph-end-marker))
		    (goto-char (setq paragraph-start-point
				     (marker-position paragraph-end-marker)))
		    (and (< paragraph-start-point end-marker) (not (eobp)))))
	   (remove-list-of-text-properties
	    start end-marker
	    (cons 'face (mapcar 'car wikipedia-property-faces)))
	   (wikipedia-set-region-primary-face start end-marker))
	 (set-marker end-marker nil)
	 (set-marker paragraph-end-marker nil))))))

(defun wikipedia-hide-paragraph-markup (start end-marker)
  (goto-char start)
  (skip-chars-forward "\n\r" end-marker)
  (when (looking-at wikipedia-header-regexp)
    (wikipedia-add-or-replace-face (match-beginning 2) (match-end 2)
				   (aref wikipedia-header-faces
					 (- (match-end 1)
					    (match-beginning 1) 1))))
  (while (let ((case-fold-search nil))
	   (re-search-forward wikipedia-known-html-entities-regexp
			      end-marker t))
    (let* ((entity     (match-string 1))
	   (character  (if (= (aref entity 0) ?#)
			   (let ((unicode-index (string-to-number
						 (substring entity 1))))
			     (aref (decode-coding-string
				    (string #xfe #xff
					    (% unicode-index #x100)
					    (/ unicode-index #x100))
				    'utf-16-le t)
				   0))
			 (cdr (assoc entity wikipedia-html-entities))))
	   (properties (text-properties-at (match-beginning 0))))
      (replace-match (apply 'propertize (char-to-string character)
			     properties))))
  (goto-char start)
  (while (re-search-forward "Â +" end t)
    (wikipedia-highlight-non-breaking-spaces (- (point) (match-beginning 0))))
  (goto-char start)
  (let ((italic-start-marker (make-marker))
	(bold-start-marker   (make-marker)))
    (while (progn (skip-chars-forward "^'" end-marker)
		  (< (point) end-marker))
      (let* ((before-apostrophes (point))
	     (num-apostrophes    (skip-chars-forward "'")))
	(when (or (= num-apostrophes 2) (= num-apostrophes 5))
	  (if (marker-buffer italic-start-marker)
	      (progn
		(wikipedia-create-markup-region (+ italic-start-marker 2)
						before-apostrophes
						'wikipedia-emphasis t)
		(delete-backward-char 2)
		(delete-region italic-start-marker (+ italic-start-marker 2))
		(set-marker italic-start-marker nil)
		(setq before-apostrophes (- before-apostrophes 2)))
	    (set-marker italic-start-marker before-apostrophes)))
	(when (or (= num-apostrophes 3) (= num-apostrophes 5))
	  (if (marker-buffer bold-start-marker)
	      (progn
		(wikipedia-create-markup-region (+ bold-start-marker 3)
						before-apostrophes
						'wikipedia-strong-emphasis t)
		(delete-backward-char 3)
		(delete-region bold-start-marker (+ bold-start-marker 3))
		(set-marker bold-start-marker nil))
	    (set-marker bold-start-marker before-apostrophes)))))
    (set-marker italic-start-marker nil)
    (set-marker bold-start-marker nil))
  (goto-char start)
  (let ((image-link-regexp
	 (regexp-opt (list "Image:"
			   (nth 7 (wikipedia-get-list-of-namespaces))))))
    (while (search-forward "[[" end-marker 1)
      (unless (let ((case-fold-search nil))
		(looking-at image-link-regexp))
	(let ((link-start (- (point) 2)))
	  (when (search-forward "]]" end-marker t)
	    (let* ((link-end     (- (point) 2))
		   (link-address (progn
				   (goto-char link-start)
				   (skip-chars-forward "^|" link-end)
				   (buffer-substring-no-properties
				    (+ link-start 2) (point)))))
	      (delete-region link-start
			     (if (= (following-char) ?|)
				 (prog1 (1+ (point))
				   (goto-char link-end))
			       (+ link-start 2)))
	      (delete-char 2)
	      (skip-syntax-forward "w")
	      (wikipedia-create-markup-region link-start (point)
					      'wikipedia-link-address
					      link-address))))))))

(defun wikipedia-show-paragraph-markup (start end-marker)
  (let ((markup-start))
    (mapc
     (lambda (description)
       (goto-char start)
       (while (or (get-text-property (setq markup-start (point))
				     (car description))
		  (< (setq markup-start (next-single-property-change
					 markup-start (car description)
					 nil end-marker))
		     end-marker))
	 (goto-char markup-start)
	 (funcall (cdr description))))
     '((wikipedia-link-address    . wikipedia-show-links-markup)
       (wikipedia-emphasis        . wikipedia-show-emphasis-markup)
       (wikipedia-strong-emphasis . wikipedia-show-strong-emphasis-markup))))
  (goto-char start)
  (while (search-forward "Â " end-marker t)
    (replace-match "&nbsp;" t t))
  (let ((coding (wikipedia-get-domain-coding)))
    (when (not (eq coding 'utf-8))
      (goto-char start)
      (while (setq start (unencodable-char-position (point) end-marker coding))
	(goto-char start)
	(let ((html-entity (car (rassoc (following-char)
					wikipedia-html-entities))))
	  (if html-entity
	      (insert ?& html-entity ?\;)
	    (let ((character-in-utf-16
		   (encode-coding-string (char-to-string (following-char))
					 'utf-16-le)))
	      (insert ?& ?#
		      (number-to-string
		       (+ (* (aref character-in-utf-16 3) #x100)
			  (aref character-in-utf-16 2)))
		      ?\;))))
	(delete-char 1)))))

(defun wikipedia-show-links-markup ()
  (let* ((link-end            (next-single-property-change
			       markup-start 'wikipedia-link-address
			       nil (point-max)))
	 (link-text           (buffer-substring-no-properties markup-start
							      link-end))
	 (link-text-length    (length link-text))
	 (link-address        (get-text-property markup-start
						 'wikipedia-link-address))
	 (link-address-length (length link-address)))
    (if (and (<= link-address-length link-text-length)
	     (string= (substring link-text 0 link-address-length)
		      link-address)
	     (string-match "^[[:word:]]*$"
			   (substring link-text link-address-length)))
	(progn
	  (wikipedia-sticky-insert "[[")
	  (forward-char link-address-length)
	  (wikipedia-front-sticky-insert "]]")
	  (forward-char (- link-text-length link-address-length)))
      (wikipedia-sticky-insert (concat "[[" link-address "|"))
      (forward-char link-text-length)
      (wikipedia-front-sticky-insert "]]"))))

(defun wikipedia-show-emphasis-markup ()
  (wikipedia-sticky-insert "''")
  (goto-char (next-single-property-change (point) 'wikipedia-emphasis
					  nil (point-max)))
  (wikipedia-front-sticky-insert "''"))

(defun wikipedia-show-strong-emphasis-markup ()
  (wikipedia-sticky-insert "'''")
  (goto-char (next-single-property-change (point) 'wikipedia-strong-emphasis
					  nil (point-max)))
  (wikipedia-front-sticky-insert "'''"))


;;; Low-level markup properties manipulation.

(defun wikipedia-create-markup-region (start end property value)
  (add-text-properties start end (list property value 'rear-nonsticky t))
  (let ((face (cdr (assq property wikipedia-property-faces))))
    (while (let ((next-property-change (next-single-property-change
					start 'face nil end)))
	     (wikipedia-add-or-replace-face start next-property-change
					    face)
	     (< (setq start next-property-change) end)))))

(defun wikipedia-delete-markup-region (start end property)
  (remove-list-of-text-properties start end (list property))
  (let ((face (cdr (assq property wikipedia-property-faces))))
    (while (let ((next-property-change (next-single-property-change
					start 'face nil end)))
	     (put-text-property start next-property-change
				'face
				(remove face (get-text-property start 'face)))
	     (< (setq start next-property-change) end)))))

(defun wikipedia-delete-markup-subregions (start end expand property)
  (if (get-text-property start property)
      (when expand
	(setq start (previous-single-property-change (1+ start) property
						     nil (point-min))))
    (setq start (next-single-property-change start property nil end)))
  (while (< start end)
    (let ((subregion-end (next-single-property-change start property nil
						      (if expand
							  (point-max)
							end))))
      (wikipedia-delete-markup-region start subregion-end property)
      (setq start (next-single-property-change subregion-end property
					       nil end)))))


;;; Faces manipulation.

(defun wikipedia-set-region-primary-face (start end)
  (save-excursion
    (goto-char start)
    (wikipedia-beginning-of-line)
    (while (progn
	     (unless (eolp)
	       (let ((face)
		     (other-face))
		 (if (= (following-char) ? )
		     (setq face       'wikipedia-fixed-pitch-face
			   other-face 'wikipedia-default-face)
		   (setq other-face 'wikipedia-fixed-pitch-face
			 face       'wikipedia-default-face))
		 (wikipedia-end-of-line)
		 (while (let ((next-property-change
			       (next-single-property-change start 'face
							    nil (point))))
			  (wikipedia-add-or-replace-face start
							 next-property-change
							 face other-face)
			  (< (setq start next-property-change) (point))))))
	     (when (not (eobp))
	       (forward-char))
	     (< (point) end)))))

(defun wikipedia-add-or-replace-face (start end face-to-add
					    &optional face-to-replace)
  (let ((face-list (get-text-property start 'face)))
    (when (nlistp face-list)
      (setq face-list (list face-list)))
    (unless (memq face-to-add face-list)
      (if (and face-to-replace
	       (memq face-to-replace face-list))
	  (progn (setq face-list (copy-sequence face-list))
		 (setcar (memq face-to-replace face-list) face-to-add))
	(setq face-list (cons face-to-add face-list)))
      (unless (cdr face-list)
	(setq face-list (car face-list)))
      (put-text-property start end 'face face-list))))

(defsubst wikipedia-highlight-non-breaking-spaces (num-spaces)
  (wikipedia-add-or-replace-face (- (point) num-spaces) (point)
				 'wikipedia-non-breaking-space-face)
  (put-text-property (- (point) num-spaces) (point) 'rear-nonsticky t))


;;; Miscellaneous utility functions.

;; Go to the beginning of current line, treating soft newlines as
;; spaces.  E.g. in Long Lines minor mode it will work differently
;; than plain `beginning-of-line'.
(defun wikipedia-beginning-of-line ()
  (beginning-of-line)
  (when use-hard-newlines
    (while (and (not (bobp))
		(null (get-text-property (1- (point)) 'hard)))
      (backward-char)
      (beginning-of-line))))

;; Go to the end of current line, treating soft newlines as spaces.
;; E.g. in Long Lines minor mode it will work differently than plain
;; `end-of-line'.
(defun wikipedia-end-of-line ()
  (end-of-line)
  (when use-hard-newlines
    (while (and (not (eobp))
		(null (get-text-property (point) 'hard)))
      (forward-char)
      (end-of-line))))


;; Replace all non-breaking space in the STRING with normal spaces.
(defsubst wikipedia-make-all-spaces-normal (string)
  (dotimes (k (length string))
    (when (= (aref string k) ?Â )
      (aset string k ? )))
  string)


(defsubst wikipedia-sticky-insert (string)
  (insert (apply 'propertize string (text-properties-at (point)))))

(defsubst wikipedia-front-sticky-insert (string)
  (insert (if (bobp)
	      string
	    (apply 'propertize string (text-properties-at (1- (point)))))))

;; Replace the character preceding the point with NEW-CHARACTER.  The
;; purpose of this function is to avoid unexpected property changes
;; when the old character is deleted.
(defun wikipedia-replace-preceding-character (new-character)
  (let ((text-properties (text-properties-at (1- (point)))))
    (wikipedia-without-after-change-hook
      (delete-backward-char 1)
      (insert (apply 'propertize (char-to-string new-character)
		     text-properties))
      (when (= new-character ?Â )
	(wikipedia-highlight-non-breaking-spaces 1)))))


(defsubst wikipedia-hexify-string (string domain &optional space-replacement)
  (url-hexify-string
   (encode-coding-string (if space-replacement
			     (subst-char-in-string ?  space-replacement string)
			   string)
			 (wikipedia-get-domain-coding domain))))



;;;; Debug function(s).

;;Replace "http://127.0.0.1/" and "ru" and "Ð“Ð¾Ñ€Ð¸Ð»Ð»Ð°" if you'd like to
;; use it too.
(defun wikipedia-test-locally ()
  (interactive)
  (url-retrieve "http://127.0.0.1/"
		'wikipedia-extract-article-text '("ru" "Ð“Ð¾Ñ€Ð¸Ð»Ð»Ð°")))


(provide 'wikipedia)


;; Last updated: $Date: 2006/06/21 20:08:14 $, $Author: doublep $

;; Local variables:
;; coding: utf-8
;; End: