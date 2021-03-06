(deftheme garden
  "Garden theme")

(let (
      (monokai-blue-light "#89BDFF")
      (monokai-gray "#595959")
      (monokai-gray-darker "#383830")
      (monokai-gray-darkest "#141411")
      (monokai-gray-lightest "#595959")
      (monokai-gray-light "#E6E6E6")
      (monokai-green "#A6E22A")
      (monokai-green-light "#A6E22E")
      (monokai-grey-dark "#272822")
      (monokai-magenta "#F92672")
      (monokai-purple "#AE81FF")
      (monokai-purple-light "#FD5FF1")
      (monokai-yellow "#E6DB74")
      (monokai-yellow-dark "#75715E")
      (monokai-yellow-light "#F8F8F2")
      (garden-pre "#0F192a")
      (garden-com "#428BDD")
      (garden-typ "#FFAA3E")
      (garden-lit "#D1EDFF")
      (garden-pun "#D1EDFF")
      (garden-pln "#FFAA3E")
      (garden-atn "#606")
      (garden-str "#1DC116")
      (garden-atv "#080")
      (garden-kwd "#E83737")
      (garden-tag "#008")
      (garden-dec "#606")
      )
  (custom-theme-set-faces
   `garden
   ;; Frame
   `(default ((t (:foreground ,garden-pun :background ,garden-pre))))
   `(cursor ((t (:foreground ,monokai-magenta))))
   `(hl-line ((t (:background ,monokai-gray-darkest))))
   `(minibuffer-prompt ((t (:foreground ,monokai-yellow-dark))))
   `(modeline ((t (:background ,monokai-gray-lightest :foreground ,monokai-gray-light))))
   `(region ((t (:background ,monokai-gray-darker))))
   `(show-paren-match-face ((t (:background ,monokai-gray-lightest))))
   ;; Main
   `(font-lock-builtin-face       ((t (:foreground ,monokai-green))))
   `(font-lock-comment-face       ((t (:foreground ,garden-com))))
   `(font-lock-constant-face      ((t (:foreground ,monokai-purple))))
   `(font-lock-doc-string-face    ((t (:foreground ,monokai-yellow))))
   `(font-lock-function-name-face ((t (:foreground ,garden-pln))))
   `(font-lock-keyword-face       ((t (:foreground ,garden-kwd))))
   `(font-lock-string-face        ((t (:foreground ,garden-str))))
   `(font-lock-type-face          ((t (:foreground ,garden-typ))))
   `(font-lock-variable-name-face ((t (:foreground ,garden-pln))))
   `(font-lock-warning-face       ((t (:bold t :foreground ,monokai-purple-light))))
   ;; CUA
   `(cua-rectangle ((t (:background ,monokai-gray-darkest))))
   ;; IDO
   `(ido-first-match ((t (:foreground ,monokai-purple))))
   `(ido-only-match ((t (:foreground ,monokai-green))))
   `(ido-subdir ((t (:foreground ,monokai-blue-light))))
   ;; ECB
   `(ecb-default-highlight-face ((t (:foreground ,monokai-green))))
   ;; Whitespace
   `(whitespace-space ((t (:foreground ,monokai-gray))))
   ;; Yasnippet
   `(yas/field-highlight-face ((t (:background ,monokai-gray-darker))))
   )
)
