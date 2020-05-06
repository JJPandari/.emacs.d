(use-package projectile
  :custom (projectile-current-project-on-switch 'move-to-end)
  :demand t
  :config
  (projectile-mode 1)
  (jester/with-leader
   "p K" 'projectile-kill-buffers
   "p s" 'jester/select-project-and-search
   "p g" 'jester/select-project-and-magit-status
   "p F" 'jester/select-project-and-open-file)
  (defvar jester-projectile-project-history nil "recent projects (used through projectile)")
  (defun jester/select-project-and-do (action)
    "Select a project with projectile, then do `ACTION' in the project.
`ACTION' takes one arg, the selected project dir."
    (let ((projects (projectile-relevant-known-projects)))
      (if projects
          (ivy-read "in project: " projects
                    :history 'jester-projectile-project-history
                    :action action)
        (user-error "There are no known projects"))))
  (defun jester/select-project-and-search ()
    "Select a project with projectile, and search it."
    (interactive)
    (jester/select-project-and-do
     (lambda (project-dir) (let ((default-directory project-dir))
                        (counsel-rg)))))
  (defun jester/select-project-and-magit-status ()
    "Select a project with projectile, and open magit status."
    (interactive)
    (jester/select-project-and-do
     (lambda (project-dir) (let ((default-directory project-dir))
                        (magit-status)))))
  (defun jester/select-project-and-open-file ()
    "Select a project with projectile, and open a file."
    (interactive)
    (jester/select-project-and-do
     (lambda (project-dir) (let ((default-directory project-dir))
                        (counsel-git))))))


(provide 'init-projects)
