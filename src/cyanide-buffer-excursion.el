;; This file is part of CyanIDE.
;;
;; CyanIDE is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; CyanIDE is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with CyanIDE.  If not, see <http://www.gnu.org/licenses/>.

(defun cyanide-buffer-excursion (func
                                     sought-buffer
                                     &optional all-frames)
      "Place cursor in buffer-window, eval code, go
             back to starting location, return output of
             funcall func.

            - If window is visible, switch to it.

            - If window is not visible, switch to buffer.

            - If buffer does not exist, create it.

            - If all-frames is t, consider all frames. See
              `get-buffer-window' for details regarding the specific
              behavior of that arg. In this case, if a buffer is open
              in multiple frames, cyanide-buffer-excursion will
              prefer to select the window of the buffer in the current
              frame."
      (let ((starting-buffer (current-buffer))
            (starting-window (selected-window))
            (starting-frame  (selected-frame)))
        (progn
          (message
           (concat
            "Beginning cyanide buffer excursion with sought-buffer "
            sought-buffer))
          (cyanide-select-buffer-window-worker sought-buffer all-frames)
          (let ((return (funcall func)))
            (progn
              (select-frame-set-input-focus starting-frame)
              (select-window starting-window)
              (message
               (concat
                "Finished cyanide buffer excursion with sought-buffer "
                sought-buffer))
              return)))))

(provide 'cyanide-buffer-excursion)
