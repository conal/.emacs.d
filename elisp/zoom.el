;;; zoom.el --- font zooming

;;; From http://www.users.sbg.ac.at/~jack/misc/emacs.el

;;; I don't have the required fonts.  Play with another time.


;; ===== Enable Firefox-like zooming of fonts =====
;;
 (setq default-font-zoom-index 1)
 (setq font-zoom-index default-font-zoom-index)
 ;
 (setq font-zoom-list
       (list "-*-*-medium-r-normal--10-*-*-*-*-*-fontset-osaka"
             "-*-*-medium-r-normal--12-*-*-*-*-*-fontset-osaka"
             "-*-*-medium-r-normal--14-*-*-*-*-*-fontset-osaka"
             "-*-*-medium-r-normal--16-*-*-*-*-*-fontset-osaka"
             "-*-*-medium-r-normal--18-*-*-*-*-*-fontset-osaka"
             "-*-*-medium-r-normal--24-*-*-*-*-*-fontset-osaka"
))

 ;
 (defun font-zoom-increase-font-size ()
   (interactive)
   (progn
     (setq font-zoom-index (min (- (length font-zoom-list) 1)
                                (+ font-zoom-index 1)))
     (set-frame-font (nth font-zoom-index font-zoom-list))))
 ;
 (defun font-zoom-decrease-font-size ()
   (interactive)
   (progn
     (setq font-zoom-index (max 0
                                (- font-zoom-index 1)))
     (set-frame-font (nth font-zoom-index font-zoom-list))))
 ;
 (defun font-zoom-reset-font-size ()
   (interactive)
   (progn
     (setq font-zoom-index default-font-zoom-index)
     (set-frame-font (nth font-zoom-index font-zoom-list))))
 ;
 (define-key global-map (read-kbd-macro "C--") 'font-zoom-decrease-font-size)
 (define-key global-map (read-kbd-macro "C-=") 'font-zoom-increase-font-size)
 (define-key global-map (read-kbd-macro "C-0") 'font-zoom-reset-font-size)
 ;
 (set-frame-font (nth font-zoom-index font-zoom-list))




(provide 'zoom)
;;; zoom.el ends here
