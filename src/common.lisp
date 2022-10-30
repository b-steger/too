;;;; TOO (The Offline Oriented) creates ZIM files with offline available maps
;;;; Copyright (C) 2022 Benedikt Steger <b.steger@protonmail.ch>
;;;; 
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :too)

(defparameter *connection* nil)

;;Authority: bsbase:asplice.
(defun asplice (&rest lists-or-atoms-that-will-be-wrapped-in-lists)
  "(append ,@LISTS-OR-ATOMS-THAT-WILL-BE-WRAPPED-IN-LISTS), but nest element in (list element) if it is not a list.
   Useful since less (list ...) have to be written in order to make #'append happy.
   Ignores nil in LISTS-OR-ATOMS-THAT-WILL-BE-WRAPPED-IN-LISTS."
  (loop for item in lists-or-atoms-that-will-be-wrapped-in-lists
        append (if (listp item) item (list item))))

