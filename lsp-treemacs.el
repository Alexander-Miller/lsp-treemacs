;; lsp-treemacs.el --- treemacs-based extensions for lsp-mode -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "25.2") (dash "2.11.0") (s "1.10.0") (f "0.11.0") (ht "2.2"))
;; Homepage: https://github.com/Alexander-Miller/treemacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'lsp)
(require 'treemacs)
(require 'cl-lib)

(defgroup lsp-treemacs nil
  "lsp-treemacs configuration options."
  :group 'lsp-treemacs
  :prefix "lsp-treemacs-")

(defface lsp-treemacs-project-root-error
  '((t :inherit font-lock-keyword-face :underline (:style wave :color "Red1")))
  "Face used for highlighting symbols being read."
  :group 'lsp-faces)

(defface lsp-treemacs-project-root-info
  '((t :inherit font-lock-keyword-face :underline (:style wave :color "yellow green")))
  "Face used for highlighting symbols being read."
  :group 'lsp-faces)

(defface lsp-treemacs-project-root-warn
  '((t :inherit font-lock-keyword-face  :underline (:style wave :color "Orange")))
  "Face used for highlighting symbols being read."
  :group 'lsp-faces)

(defface lsp-treemacs-file-error
  '((t :inherit error))
  "Face used for highlighting symbols being read."
  :group 'lsp-faces)


(defface lsp-treemacs-file-info
  '((t :inherit success))
  "Face used for highlighting symbols being read."
  :group 'lsp-faces)

(defface lsp-treemacs-file-warn
  '((t :inherit warning))
  "Face used for highlighting symbols being read."
  :group 'lsp-faces)

(treemacs--setup-icon lsp-treemacs-error "icons/vscode/error.png")
(treemacs--setup-icon lsp-treemacs-warning "icons/vscode/warning.png")
(treemacs--setup-icon lsp-treemacs-info "icons/vscode/info.png")

(defcustom lsp-treemacs-face-map
  '((1 . lsp-treemacs-project-root-error)
    (2 .  lsp-treemacs-project-root-warn)
    (3 .  lsp-treemacs-project-root-info))
  "Alist diagnostics to face."
  :type 'alist
  :group 'lsp-treemacs)

(defcustom lsp-treemacs-icons-map
  `((1 . ,lsp-treemacs-error)
    (2 . ,lsp-treemacs-warning)
    (3 . ,lsp-treemacs-info))
  "Alist diagnostics to face."
  :type 'alist
  :group 'lsp-treemacs)

(defcustom lsp-treemacs-file-face-map
  '((1 . lsp-treemacs-file-error)
    (2 . lsp-treemacs-file-warn)
    (3 . lsp-treemacs-file-info))
  "Alist diagnostics to face."
  :type 'alist
  :group 'lsp-treemacs)

(defun lsp-treemacs--root-folders ()
  "Get a list of lsp root folders.
Will return an alist mapping display names to absolute paths."
  (let ((diagnostics (lsp-diagnostics)))
    (->> (lsp-session)
         lsp-session-folders
         (-filter (lambda (folder-name)
                    (-some (-partial 's-starts-with? folder-name)
                           (ht-keys diagnostics))))
         (--map (cons (propertize (f-filename it)
                                  'face (lsp-treemacs--face it diagnostics))
                      it)))))

(defun lsp-treemacs-quick-fix ()
  "Select the element under cursor."
  (interactive)
  (-let (((file . diag) (button-get (treemacs-node-at-point) :key))
         (session (lsp-session)))
    (with-current-buffer (find-file-noselect file)
      (with-lsp-workspaces (gethash
                            (lsp-find-session-folder session file)
                            (lsp-session-folder->servers session))
        (save-excursion
          (goto-char (point-min))
          (forward-line (lsp-diagnostic-line diag))
          (call-interactively #'lsp-execute-code-action))))))

(defun lsp-treemacs--open (&rest _)
  "Open the file at point."
  (interactive)
  (find-file (button-get (treemacs-node-at-point) :key)))

(defun lsp-treemacs--open-error (&rest _)
  "Open the error at point."
  (interactive)
  (-let [(file . diag) (button-get (treemacs-node-at-point) :key)]
    (find-file file)
    (goto-char (point-min))
    (forward-line (lsp-diagnostic-line diag))
    (forward-char (lsp-diagnostic-column diag))))

(defun lsp-treemacs--face (root-folder diagnostics)
  "Get face for ROOT-FOLDER based on DIAGNOSTICS."
  (--> diagnostics
       ht->alist
       (-keep (-lambda ((file-name . file-diagnostics))
                (when (s-starts-with? root-folder file-name)
                  (lsp-diagnostic-severity
                   (--min-by (> (lsp-diagnostic-severity it)
                                (lsp-diagnostic-severity other))
                             file-diagnostics))))
              it)
       -min
       (assoc it lsp-treemacs-face-map)
       cl-rest))

(defun lsp-treemacs--diag-statistics (file-diagnostics)
  "Format FILE-DIAGNOSTICS for display."
  (s-join
   ":"
   (-map (-lambda ((severity . diagnostics))
           (propertize (f-filename (number-to-string (length diagnostics)))
                       'face (cl-rest (assoc severity lsp-treemacs-file-face-map))))
         (-group-by 'lsp-diagnostic-severity file-diagnostics))))

(defun lsp-treemacs--get-files (project-root)
  "Get files under PROJECT-ROOT."
  (--> (lsp-diagnostics)
       ht->alist
       (-keep (-lambda ((file-name . file-diagnostics))
                (when (s-starts-with? project-root file-name)
                  (cons file-name
                        (format (propertize "%s [%s] %s" 'face 'default)
                                (propertize (f-filename file-name)
                                            'face 'default
                                            ;; (rest (assoc (lsp-diagnostic-severity
                                            ;;               (--min-by (> (lsp-diagnostic-severity it)
                                            ;;                            (lsp-diagnostic-severity other))
                                            ;;                         file-diagnostics))
                                            ;;              lsp-treemacs-file-face-map))
                                            )
                                (lsp-treemacs--diag-statistics file-diagnostics)
                                (propertize (f-dirname (f-relative file-name project-root))
                                            'face 'lsp-lens-face)))))
              it)))

(defconst treemacs-icon-info    (treemacs-as-icon "I "))
(defconst treemacs-icon-warning (treemacs-as-icon "W "))
(defconst treemacs-icon-error   (treemacs-as-icon "E "))

(defun lsp-treemacs--errors (file-name)
  "Get diagnostics for FILE-NAME."
  (--map (cons file-name it) (gethash file-name (lsp-diagnostics))))

(treemacs-define-expandable-node lsp-error
  :icon-open treemacs-icon-root
  :icon-closed treemacs-icon-root
  :query-function (lsp-treemacs--errors (treemacs-button-get btn :key))
  :ret-action 'lsp-treemacs--open-error
  :render-action
  (treemacs-render-node
   :icon (treemacs-as-icon ". " 'face 'font-lock-string-face)
   :label-form (propertize (lsp-diagnostic-message item) 'face 'default)
   :state (with-no-warnings treemacs-lsp-files-closed-state)
   :key-form item))

(treemacs-define-expandable-node lsp-files
  :icon-open-form  (treemacs-icon-for-file (treemacs-button-get btn :key))
  :icon-closed-form  (treemacs-icon-for-file (treemacs-button-get btn :key))
  :query-function (lsp-treemacs--errors (treemacs-button-get btn :key))
  :ret-action 'lsp-treemacs--open
  :render-action
  (treemacs-render-node
   :icon (cl-case (lsp-diagnostic-severity (cl-rest item))
           (1 treemacs-icon-error)
           (2 treemacs-icon-warning)
           (t treemacs-icon-info))
   :label-form (propertize (lsp-diagnostic-message (cl-rest item)) 'face 'default)
   :state treemacs-lsp-error-open-state
   :key-form item))

(treemacs-define-expandable-node lsp-projects
  :icon-open treemacs-icon-open
  :icon-closed treemacs-icon-closed
  :query-function (lsp-treemacs--get-files (treemacs-button-get btn :key))
  :ret-action 'lsp-treemacs--open
  :render-action
  (treemacs-render-node
   :icon (treemacs-icon-for-file (cl-first item))
   :label-form (cl-rest item)
   :state treemacs-lsp-files-closed-state
   :key-form (cl-first item)))

(treemacs-define-expandable-node lsp-error-list
  :icon-open (treemacs-as-icon "⚠ ")
  :icon-closed (treemacs-as-icon "⚠ ")
  :query-function (lsp-treemacs--root-folders)
  :render-action
  (treemacs-render-node
   :icon treemacs-icon-lsp-projects-closed
   :label-form (cl-first item)
   :state treemacs-lsp-projects-closed-state
   ;; :face 'default
   :key-form (cl-rest item))
  :top-level-marker t
  :root-label "Errors"
  :root-face 'font-lock-type-face
  :root-key-form 'lsp-error-list)

(let* ((buffer (get-buffer-create "*LSP Diagnostics*"))
       (window (display-buffer-in-side-window buffer '((side . bottom)))))
  (select-window window)
  (treemacs-initialize)

  (treemacs-LSP-ERROR-LIST-extension)

  (treemacs-expand-lsp-error-list))

(defun lsp-treemacs--after-diagnostics ()
  "Update the diagnostics view."
  (with-demoted-errors "%s"
    (with-current-buffer (get-buffer-create "*LSP Diagnostics*")
      (treemacs-update-node '(:custom lsp-error-list)))))

(add-hook 'lsp-after-diagnostics-hook 'lsp-treemacs--after-diagnostics)

(provide 'lsp-treemacs)

;;; lsp-treemacs.el ends here
