(require 'thingatpt)
(require 'dash)

(defun prelude-search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defun prelude-google ()
  "Googles a query or region if any."
  (interactive)
  (prelude-search "http://www.google.com/search?q=" "Google: "))

(provide 'core)
